/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;
use syn::spanned::Spanned;
use syn::DeriveInput;
use syn::Fields;
use syn::Ident;

#[proc_macro_derive(StrongHash)]
pub fn derive_hash(input: TokenStream) -> TokenStream {
    // TODO(scottcao): Make derive work with generics that implement StrongHash
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();
    let output = match input.data {
        syn::Data::Struct(data) => {
            let members = data.fields.members();
            quote! {
                impl #impl_generics strong_hash::StrongHash for #name #type_generics #where_clause {
                    fn strong_hash<H: strong_hash::StrongHasher>(&self, state: &mut H) {
                        #(StrongHash::strong_hash(&self.#members, state));*
                    }
                }
            }
        }
        syn::Data::Enum(data) => {
            let variants = data.variants;
            let hash_impl = variants
                .iter()
                .map(|variant| {
                    let variant_name = &variant.ident;
                    let variant_name_str = variant_name.to_string();
                    match &variant.fields {
                        Fields::Named(fields) => {
                            let field_names: Vec<_> = fields
                                .named
                                .iter()
                                .map(|field| field.ident.to_owned().unwrap())
                                .collect();
                            quote! {
                                #name::#variant_name { #(#field_names),* } => {
                                    StrongHash::strong_hash(#variant_name_str, state);
                                    #(StrongHash::strong_hash(#field_names, state);)*
                                }
                            }
                        }
                        Fields::Unnamed(fields) => {
                            let vars: Vec<_> = fields
                                .unnamed
                                .iter()
                                .enumerate()
                                .map(|(index, field)| {
                                    Ident::new(&format!("x{}", index), field.span())
                                })
                                .collect();
                            quote! {
                                #name::#variant_name(#(#vars),*) => {
                                    StrongHash::strong_hash(#variant_name_str, state);
                                    #(StrongHash::strong_hash(#vars, state);)*
                                }
                            }
                        }
                        Fields::Unit => {
                            quote! {
                                #name::#variant_name => {
                                    StrongHash::strong_hash(#variant_name_str, state);
                                }
                            }
                        }
                    }
                })
                .collect::<Vec<_>>();
            quote! {
                impl #impl_generics strong_hash::StrongHash for #name #type_generics #where_clause {
                    fn strong_hash<H: strong_hash::StrongHasher>(&self, state: &mut H) {
                        match self {
                            #(#hash_impl),*
                        }
                    }
                }
            }
        }
        _ => panic!("Only structs and enums are supported"),
    };

    output.into()
}
