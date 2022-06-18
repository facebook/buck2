//! Derives the registration for starlark documentation generation

#[allow(unused_extern_crates)] // proc_macro is very special
extern crate proc_macro;

use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{
    parse::ParseStream, parse_macro_input, spanned::Spanned, Attribute, DeriveInput, LitStr, Token,
};

struct DocDeriveOptions {
    module: Ident,
    name_override: Option<String>,
}

/// Derive the starlark documentation registration
#[proc_macro_derive(Buck2Docs, attributes(buck2_docs))]
pub fn derive_buck2_docs(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match derive_buck2_docs_impl(input) {
        Ok(r) => r.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn derive_buck2_docs_impl(input: DeriveInput) -> syn::Result<TokenStream> {
    let opts = parse_opts(&input.attrs, input.span())?;

    render(input, opts)
}

fn render(input: DeriveInput, opts: DocDeriveOptions) -> syn::Result<TokenStream> {
    let name = &input.ident;
    let doc_name = if let Some(name_override) = opts.name_override {
        name_override
    } else {
        name.to_string()
    };

    let module = opts.module;

    let gen_docs_fun = quote! {
        || {
            let mut builder = buck2_docs_gen::__derive_refs::starlark::environment::MethodsBuilder::new();
            super::#module(&mut builder);
            builder.build().documentation()
        }
    };

    let module_name = Ident::new(&format!("__derive_impl_docs_{}", name), input.span());
    Ok(quote! {
        #[allow(non_snake_case)]
        mod #module_name {
            use buck2_docs_gen::__derive_refs::inventory as inventory;
            inventory::submit! {
                buck2_docs_gen::StarlarkObject {
                    name: #doc_name,
                    module: Box::new(#gen_docs_fun),
                }
            }
        }
    })
}

fn parse_opts(attrs: &[Attribute], span: Span) -> syn::Result<DocDeriveOptions> {
    syn::custom_keyword!(builder);
    syn::custom_keyword!(name);

    let mut module = None;
    let mut name_override = None;

    let mut first = true;

    for attr in attrs.iter() {
        if !attr.path.is_ident("buck2_docs") {
            continue;
        }

        attr.parse_args_with(|input: ParseStream| {
            loop {
                if first {
                    if let Ok(ident) = input.parse::<Ident>() {
                        module = Some(ident);
                    } else {
                        return Err(input.error(
                            "first argument to `buck2_docs` must be the starlark module function",
                        ));
                    }
                    first = false;
                } else {
                    // no longer the first iteration, so we parse the rest of the attributes
                    if input.parse::<name>().is_ok() {
                        if name_override.is_some() {
                            return Err(input.error("`name` was set twice"));
                        }
                        input.parse::<Token![=]>()?;
                        name_override = Some(input.parse::<LitStr>()?.value());
                    } else {
                        return Err(input.lookahead1().error());
                    }
                }

                if input.parse::<Option<Token![,]>>()?.is_none() {
                    break;
                }
            }

            Ok(())
        })?;
    }

    if first {
        return Err(syn::Error::new(
            span,
            "`buck2_docs` attribute is not specified",
        ));
    }

    Ok(DocDeriveOptions {
        module: module.unwrap(),
        name_override,
    })
}
