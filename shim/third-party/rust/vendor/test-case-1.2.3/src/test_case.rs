use crate::expected::Expected;
use crate::utils::fmt_syn;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{parse_quote, Error, Expr, Ident, ItemFn, LitStr, Token};

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct TestCase {
    test_case_name: String,
    args: Vec<Expr>,
    expected: Option<Expected>,
    case_desc: Option<LitStr>,
}

impl Parse for TestCase {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let mut test_case_name = String::new();

        let mut args = vec![];
        loop {
            let exp: Expr = input.parse()?;
            test_case_name += &format!(" {}", fmt_syn(&exp));
            args.push(exp);
            if !input.peek(Token![,]) {
                break;
            }
            let _comma: Token![,] = input.parse()?;
        }

        let arrow: Option<Token![=>]> = input.parse()?;

        let expected = if arrow.is_some() {
            let expected: Expected = input.parse()?;

            test_case_name += &format!(" {}", expected);

            Some(expected)
        } else {
            None
        };

        let semicolon: Option<Token![;]> = input.parse()?;
        let case_desc = if semicolon.is_some() {
            let desc: LitStr = input.parse()?;
            Some(desc)
        } else {
            None
        };

        Ok(Self {
            test_case_name,
            args,
            expected,
            case_desc,
        })
    }
}

impl TestCase {
    pub fn test_case_name(&self) -> Ident {
        let case_desc = self
            .case_desc
            .as_ref()
            .map(LitStr::value)
            .unwrap_or_else(|| self.test_case_name.clone());
        crate::utils::escape_test_name(case_desc)
    }

    #[cfg(not(feature = "allow_result"))]
    pub fn expects_return(&self) -> bool {
        self.expected.is_some()
    }

    pub fn render(&self, mut item: ItemFn) -> TokenStream2 {
        let item_name = item.sig.ident.clone();
        let arg_values = self.args.iter();
        let test_case_name = self.test_case_name();
        let inconclusive = self
            .case_desc
            .as_ref()
            .map(|cd| cd.value().to_lowercase().contains("inconclusive"))
            .unwrap_or_default();

        let mut attrs = vec![];

        let expected = self.expected.as_ref().and_then(|expected| {
            let case = expected.case();

            if let Some(attr) = case.attr() {
                attrs.push(attr);
            }

            case.body()
        });

        if inconclusive {
            attrs.push(parse_quote! { #[ignore] })
        }

        attrs.push(parse_quote! { #[allow(clippy::bool_assert_comparison)] });

        attrs.append(&mut item.attrs);

        if let Some(_asyncness) = item.sig.asyncness {
            quote! {
                #(#attrs)*
                async fn #test_case_name() {
                    let _result = #item_name(#(#arg_values),*).await;
                    #expected
                }
            }
        } else {
            quote! {
                #[test]
                #(#attrs)*
                fn #test_case_name() {
                    let _result = #item_name(#(#arg_values),*);
                    #expected
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod expected {
        use super::*;

        mod parse {
            use super::*;
            use crate::expected::expr_case::ExprCase;
            use crate::expected::ignore_case::IgnoreCase;
            use crate::expected::panic_case::PanicCase;
            use crate::expected::pattern_case::PatternCase;
            use syn::parse_quote;

            #[test]
            fn parses_expression() {
                let actual: Expected = parse_quote! { 2 + 3 };

                assert_eq!(Expected::Expr(ExprCase::new(parse_quote!(2 + 3))), actual);
            }

            #[test]
            fn parses_panic() {
                let actual: Expected = parse_quote! { panics "Error msg" };

                assert_eq!(
                    Expected::Panic(PanicCase::new(parse_quote!("Error msg"))),
                    actual
                );
            }

            #[test]
            fn parses_panic_without_msg() {
                let actual: Expected = parse_quote! { panics };

                assert_eq!(Expected::Panic(PanicCase::new(None)), actual);
            }

            #[test]
            fn parses_pattern() {
                let actual: Expected = parse_quote! { matches Some(_) };

                assert_eq!(
                    Expected::Pattern(PatternCase::new(parse_quote!(Some(_)))),
                    actual
                );
            }

            #[test]
            fn parses_inconclusive() {
                let actual: Expected = parse_quote! { inconclusive "Ignore this" };

                assert_eq!(
                    Expected::Ignore(IgnoreCase::new(parse_quote!("Ignore this"))),
                    actual
                );
            }
        }
    }

    mod test_case {
        use super::*;

        mod parse {
            use super::*;
            use syn::parse_quote;

            #[test]
            fn parses_basic_input() {
                let actual: TestCase = parse_quote! {
                    2, 10
                };

                assert_eq!(
                    TestCase {
                        test_case_name: " 2 10".to_string(),
                        args: vec![parse_quote!(2), parse_quote!(10),],
                        expected: None,
                        case_desc: None,
                    },
                    actual
                );
            }

            #[test]
            fn parses_input_with_expectation() {
                let actual: TestCase = parse_quote! {
                    2, 10 => 12
                };

                assert_eq!(
                    TestCase {
                        test_case_name: " 2 10 expects 12".to_string(),
                        args: vec![parse_quote!(2), parse_quote!(10),],
                        expected: Some(parse_quote!(12)),
                        case_desc: None,
                    },
                    actual
                );
            }

            #[test]
            fn parses_input_with_description() {
                let actual: TestCase = parse_quote! {
                    2, 10; "basic addition"
                };

                assert_eq!(
                    TestCase {
                        test_case_name: " 2 10".to_string(),
                        args: vec![parse_quote!(2), parse_quote!(10),],
                        expected: None,
                        case_desc: parse_quote!("basic addition"),
                    },
                    actual
                );
            }
        }
    }
}
