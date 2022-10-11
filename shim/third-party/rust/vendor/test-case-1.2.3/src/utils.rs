use proc_macro2::{Ident, Span};
use quote::ToTokens;

pub fn escape_test_name(input: impl AsRef<str>) -> Ident {
    if input.as_ref().is_empty() {
        return Ident::new("_empty", Span::call_site());
    }

    let mut last_under = false;
    let mut ident: String = input
        .as_ref()
        .to_ascii_lowercase()
        .chars()
        .filter_map(|c| match c {
            c if c.is_alphanumeric() => {
                last_under = false;
                Some(c.to_ascii_lowercase())
            }
            _ if !last_under => {
                last_under = true;
                Some('_')
            }
            _ => None,
        })
        .collect();

    if !ident.starts_with(|c: char| c == '_' || c.is_ascii_alphabetic()) {
        ident = format!("_{}", ident);
    }

    Ident::new(&ident, Span::call_site())
}

pub fn fmt_syn(syn: &(impl ToTokens + Clone)) -> String {
    syn.clone().into_token_stream().to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    mod escape_test_name {
        use super::*;

        #[test]
        fn converts_arbitrary_test_names() {
            assert_eq!(
                escape_test_name("word"),
                Ident::new("word", Span::call_site())
            );
            assert_eq!(
                escape_test_name("a simple sentence"),
                Ident::new("a_simple_sentence", Span::call_site())
            );
            assert_eq!(
                escape_test_name("extra  spaces  inbetween"),
                Ident::new("extra_spaces_inbetween", Span::call_site())
            );
            assert_eq!(
                escape_test_name(" extra end and start spaces "),
                Ident::new("_extra_end_and_start_spaces_", Span::call_site())
            );
            assert_eq!(
                escape_test_name("abcdefghijklmnoqprstuwvxyz1234567890"),
                Ident::new("abcdefghijklmnoqprstuwvxyz1234567890", Span::call_site())
            );
        }

        #[test]
        fn converts_to_lowercase() {
            assert_eq!(
                escape_test_name("ALL UPPER"),
                Ident::new("all_upper", Span::call_site())
            );
            assert_eq!(
                escape_test_name("MiXeD CaSe"),
                Ident::new("mixed_case", Span::call_site())
            );
        }

        #[test]
        fn handles_numeric_first_char() {
            assert_eq!(
                escape_test_name("1test"),
                Ident::new("_1test", Span::call_site())
            );
        }

        #[test]
        fn omits_unicode() {
            assert_eq!(
                escape_test_name("from⟶to"),
                Ident::new("from_to", Span::call_site())
            );
        }

        #[test]
        fn handles_empty_input() {
            assert_eq!(
                escape_test_name(""),
                Ident::new("_empty", Span::call_site())
            );
        }
    }
}
