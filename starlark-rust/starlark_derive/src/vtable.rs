/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use proc_macro2::TokenStream;
use quote::quote;
use quote::quote_spanned;
use syn::parse_macro_input;
use syn::spanned::Spanned;
use syn::FnArg;
use syn::ItemTrait;
use syn::Pat;
use syn::ReturnType;
use syn::TraitItem;
use syn::TraitItemMethod;

struct Gen {
    starlark_value: ItemTrait,
}

struct VTableEntry {
    field: TokenStream,
    init: TokenStream,
    init_for_black_hole: TokenStream,
}

impl Gen {
    fn vtable_entry(&self, method: &TraitItemMethod) -> syn::Result<VTableEntry> {
        let fn_name = &method.sig.ident;
        let fn_ret_type = &method.sig.output;
        let mut field_fn_param_types = Vec::new();
        let mut field_params_names = Vec::new();
        let mut field_init_args = Vec::new();
        for param in &method.sig.inputs {
            match param {
                FnArg::Receiver(_) => {
                    field_fn_param_types.push(quote_spanned! {method.sig.span()=>
                        crate::values::layout::vtable::StarlarkValueRawPtr<'a, 'v>
                    });
                    field_params_names.push(quote_spanned! {method.sig.span()=>
                        this
                    });
                    field_init_args.push(quote_spanned! {method.sig.span()=>
                        this.value_ref::<T>()
                    });
                }
                FnArg::Typed(p) => {
                    let name = match &*p.pat {
                        Pat::Ident(p) => p.ident.clone(),
                        _ => return Err(syn::Error::new(p.span(), "parameter must be identifier")),
                    };
                    let ty = &p.ty;
                    field_fn_param_types.push(quote_spanned! {method.sig.span()=>
                        #ty
                    });
                    field_params_names.push(quote_spanned! {method.sig.span()=>
                        #name
                    });
                    field_init_args.push(quote_spanned! {method.sig.span()=>
                        // We do `transmute` to get rid of lifetimes, see below.
                        std::mem::transmute(#name)
                    });
                }
            }
        }
        let field_init_param_pairs: Vec<TokenStream> = field_fn_param_types
            .iter()
            .zip(field_params_names.iter())
            .map(|(ty, name)| {
                quote_spanned! {method.sig.span()=>
                    #name: #ty
                }
            })
            .collect();
        let ret = match &method.sig.output {
            ReturnType::Default => quote! {},
            ReturnType::Type(_, ty) => {
                quote_spanned! {method.sig.span()=>
                    -> #ty
                }
            }
        };
        let field = quote_spanned! {method.sig.span()=>
            pub(crate) #fn_name: for<'a, 'v> fn(
                #(#field_fn_param_types),*
            ) #ret
        };
        let init = quote_spanned! {method.sig.span()=>
            #fn_name: {
                // It is important to put vtable entry into named function
                // instead of anonymous callback so function name is meaningful in profiler output.
                fn #fn_name<'a, 'v, 'v2, T: StarlarkValue<'v2>>(#(#field_init_param_pairs),*) #fn_ret_type {
                    unsafe {
                        // The problem is that it is concrete `'v` in
                        // ```
                        // struct StarlarkValueVTableGet<'v, T: StarlarkValue<'v>
                        // ```
                        // but we must generate vtable entry `for<'v>`.
                        //
                        // If Rust supported something like:
                        // ```
                        // struct StarlarkValueVTableGet<for<'v> T<'v>: StarlarkValue<'v>>
                        // ```
                        // it would be possible. But it doesn't. Hence all the transmutes.
                        std::mem::transmute(
                            T::#fn_name(#(#field_init_args),*)
                        )
                    }
                }
                #fn_name::<T>
            }
        };
        let init_for_black_hole = quote_spanned! {method.sig.span()=>
            #fn_name: |#(#field_params_names),*| {
                panic!("BlackHole")
            }
        };
        Ok(VTableEntry {
            field,
            init,
            init_for_black_hole,
        })
    }

    fn gen_starlark_value_vtable(&self) -> syn::Result<TokenStream> {
        let mut fields = Vec::new();
        let mut inits = Vec::new();
        let mut init_black_holes = Vec::new();
        for item in &self.starlark_value.items {
            let m = match item {
                TraitItem::Method(m) => m,
                _ => continue,
            };
            // Don't need it.
            if m.sig.ident == "is_special" || m.sig.ident == "please_use_starlark_type_macro" {
                continue;
            }
            let VTableEntry {
                field,
                init,
                init_for_black_hole,
            } = self.vtable_entry(m)?;
            fields.push(field);
            inits.push(init);
            init_black_holes.push(init_for_black_hole);
        }

        Ok(quote_spanned! {
            self.starlark_value.span() =>
            pub(crate) struct StarlarkValueVTable {
                #(#fields),*
            }

            #[allow(clippy::all)]
            #[allow(unused_variables)]
            impl StarlarkValueVTable {
                pub(crate) const BLACK_HOLE: StarlarkValueVTable = StarlarkValueVTable {
                    #(#init_black_holes),*
                };
            }

            pub(crate) struct StarlarkValueVTableGet<'v, T: StarlarkValue<'v>>(std::marker::PhantomData<&'v T>);

            #[allow(clippy::all)]
            #[allow(unused_variables)]
            impl<'v, T: StarlarkValue<'v>> StarlarkValueVTableGet<'v, T> {
                pub(crate) const VTABLE: StarlarkValueVTable = StarlarkValueVTable {
                    #(#inits),*
                };
            }
        })
    }
}

/// Generates the `StarlarkValueVTable` and friends.
/// At the moment of writing this macro generated: P499428269,
/// output of `cargo expand -p starlark --lib values::traits`.
pub(crate) fn starlark_internal_vtable(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input_clone = input.clone();
    let starlark_value = parse_macro_input!(input_clone as ItemTrait);

    let gen = Gen { starlark_value };
    let generated = match gen.gen_starlark_value_vtable() {
        Ok(generated) => generated,
        Err(error) => {
            return error.to_compile_error().into();
        }
    };

    let input = TokenStream::from(input);

    proc_macro::TokenStream::from(quote! {
        #input
        #generated
    })
}
