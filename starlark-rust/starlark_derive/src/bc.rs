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

//! Generate stubs for bytecode interpreter.

use gazebo::prelude::*;
use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote_spanned;
use syn::parse_macro_input;
use syn::spanned::Spanned;
use syn::Fields;
use syn::ItemEnum;
use syn::Variant;

struct BcOpcodeEnum {
    span: Span,
    variants: Vec<Ident>,
}

impl BcOpcodeEnum {
    fn parse_variant(input: &Variant) -> Ident {
        assert!(
            input.discriminant.is_none(),
            "BcOpcode relies on sequential numbering of opcodes"
        );
        assert!(
            matches!(input.fields, Fields::Unit),
            "BcOpcode variants must be unit"
        );
        input.ident.clone()
    }

    fn parse(input: ItemEnum) -> BcOpcodeEnum {
        BcOpcodeEnum {
            span: input.span(),
            variants: input
                .variants
                .iter()
                .map(BcOpcodeEnum::parse_variant)
                .collect(),
        }
    }

    fn render_dispatch_variant(&self, variant: &Ident) -> TokenStream {
        let instr = format_ident!("Instr{}", variant);
        quote_spanned! {
            variant.span()=>
            BcOpcode::#variant => handler.handle::<#instr>(),
        }
    }

    fn render_dispatch(&self) -> TokenStream {
        let variants = self.variants.map(|v| self.render_dispatch_variant(v));
        quote_spanned! {
            self.span=>
            impl BcOpcode {
                #[inline(always)]
                fn do_dispatch<R>(self, handler: impl BcOpcodeHandler<R>) -> R {
                    match self {
                        #(#variants)*
                    }
                }
            }
        }
    }

    fn render_dispatch_all_variant(&self, variant: &Ident) -> TokenStream {
        let instr = format_ident!("Instr{}", variant);
        quote_spanned! {
            variant.span()=>
            handler.handle::<#instr>(BcOpcode::#variant);
        }
    }

    fn render_dispatch_all(&self) -> TokenStream {
        let variants = self.variants.map(|v| self.render_dispatch_all_variant(v));
        quote_spanned! {
            self.span=>
            impl BcOpcode {
                #[inline(always)]
                fn do_dispatch_all(handler: &mut impl BcOpcodeAllHandler) {
                    #(#variants)*
                }
            }
        }
    }
}

pub(crate) fn starlark_internal_bc(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input_clone = input.clone();
    let bc_opcode_enum = parse_macro_input!(input_clone as ItemEnum);
    let bc_opcode_enum = BcOpcodeEnum::parse(bc_opcode_enum);
    let input = TokenStream::from(input);
    let dispatch = bc_opcode_enum.render_dispatch();
    let dispatch_all = bc_opcode_enum.render_dispatch_all();
    proc_macro::TokenStream::from(quote_spanned! {
        input.span()=>
        #input
        #dispatch
        #dispatch_all
    })
}
