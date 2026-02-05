/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use proc_macro2::Span;
use quote::quote;
use syn::Data;
use syn::DeriveInput;
use syn::Fields;
use syn::Ident;
use syn::spanned::Spanned;

pub(crate) fn derive_variant_names(input: DeriveInput) -> syn::Result<proc_macro::TokenStream> {
    if let Data::Enum(data_enum) = input.data {
        let mut variant_body = Vec::new();
        let mut variant_lowercase_body = Vec::new();
        for variant in data_enum.variants {
            let variant_name = &variant.ident;
            let patterns = match variant.fields {
                Fields::Unit => quote! {},
                Fields::Named(_) => quote! { {..} },
                Fields::Unnamed(_) => quote! { (..) },
            };
            let variant_name_str = variant_name.to_string();
            let variant_name_lowercase_str = to_snake_case(&variant_name_str);
            variant_body.push(quote! {
                Self::#variant_name #patterns => #variant_name_str
            });
            variant_lowercase_body.push(quote! {
                Self::#variant_name #patterns => #variant_name_lowercase_str
            });
        }

        let name = &input.ident;
        let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

        let r#gen = quote! {
            impl #impl_generics gazebo::variants::VariantName for #name #ty_generics #where_clause {
                fn variant_name(&self) -> &'static str {
                    match self {
                        #(#variant_body,)*
                    }
                }

                fn variant_name_lowercase(&self) -> &'static str {
                    match self {
                        #(#variant_lowercase_body,)*
                    }
                }
            }
        };

        Ok(r#gen.into())
    } else {
        Err(syn::Error::new(
            input.span(),
            "Can only derive variant name on enums",
        ))
    }
}

pub(crate) fn derive_unpack_variants(input: DeriveInput) -> syn::Result<proc_macro::TokenStream> {
    if let Data::Enum(data_enum) = input.data {
        let mut variant_fns = Vec::new();
        for variant in data_enum.variants {
            let variant_name = &variant.ident;

            let mut count = 0;
            let mut patterns = Vec::new();
            let mut inner_type = Vec::new();

            for field in variant.fields.iter() {
                patterns.push(field.ident.clone().unwrap_or_else(|| {
                    let id = Ident::new(&format!("_v{count}"), Span::call_site());
                    count += 1;
                    id
                }));

                inner_type.push(&field.ty);
            }

            let (patterned_out, borrowed_inner_type, owned_inner_type) =
                if variant.fields.len() == 1 {
                    let patterned_out = quote! { #(#patterns)* };
                    let borrowed_inner_type = quote! { #(&'__gazebo_variant_a #inner_type)*  };
                    let owned_inner_type = quote! { #(#inner_type)*  };

                    (patterned_out, borrowed_inner_type, owned_inner_type)
                } else {
                    let patterned_out = quote! { (#(#patterns,)*) };
                    let borrowed_inner_type = quote! { (#(&'__gazebo_variant_a #inner_type,)*) };
                    let owned_inner_type = quote! { (#(#inner_type,)*) };

                    (patterned_out, borrowed_inner_type, owned_inner_type)
                };
            let patterns = match variant.fields {
                Fields::Named(_) => quote! { { #(#patterns,)*} },
                Fields::Unnamed(_) => quote! { ( #(#patterns,)* ) },
                Fields::Unit => quote!(),
            };

            let variant_unpack_fn_name = Ident::new(
                &format!("unpack_{}", to_snake_case(&variant_name.to_string())),
                Span::call_site(),
            );

            let variant_into_fn_name = Ident::new(
                &format!("into_{}", to_snake_case(&variant_name.to_string())),
                Span::call_site(),
            );

            variant_fns.push(quote! {
                pub fn #variant_unpack_fn_name<'__gazebo_variant_a>(&'__gazebo_variant_a self) -> Option<#borrowed_inner_type> {
                    match self {
                       Self::#variant_name #patterns => Some(#patterned_out),
                       _ => None
                    }
                }

                pub fn #variant_into_fn_name(self) -> Option<#owned_inner_type> {
                    match self {
                       Self::#variant_name #patterns => Some(#patterned_out),
                       _ => None
                    }
                }
            });
        }

        let name = &input.ident;
        let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

        let r#gen = quote! {
            impl #impl_generics #name #ty_generics #where_clause {
                #(#variant_fns)*
            }
        };

        Ok(r#gen.into())
    } else {
        Err(syn::Error::new(
            input.span(),
            "Can only derive variant name on enums",
        ))
    }
}

fn to_snake_case(s: &str) -> String {
    let mut out = String::new();
    let mut is_first = true;
    for c in s.chars() {
        if c.is_ascii_uppercase() {
            if !is_first {
                out.push('_');
            }
            out.push(c.to_ascii_lowercase());
        } else {
            out.push(c);
        }
        is_first = false;
    }

    out
}
