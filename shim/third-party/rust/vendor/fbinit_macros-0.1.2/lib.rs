/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

extern crate proc_macro;

mod expand;

use self::expand::{expand, Mode};
use proc_macro::TokenStream;
use syn::parse::Nothing;
use syn::parse_macro_input;

// Expand from:
//
//     #[fbinit::main]
//     fn main(fb: FacebookInit) {
//         ...
//     }
//
// to:
//
//     fn main() {
//         let fb: FacebookInit = fbinit::r#impl::perform_init();
//         ...
//     }
//
// If async, also add a #[tokio::main] attribute.
#[proc_macro_attribute]
pub fn main(args: TokenStream, input: TokenStream) -> TokenStream {
    parse_macro_input!(args as Nothing);
    expand(Mode::Main, parse_macro_input!(input))
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

// Same thing, expand:
//
//     #[fbinit::test]
//     fn name_of_test(fb: FacebookInit) {
//         ...
//     }
//
// to:
//
//     #[test]
//     fn name_of_test() {
//         let fb: FacebookInit = fbinit::r#impl::perform_init();
//         ...
//     }
//
// with either #[test] or #[tokio::test] attribute.
#[proc_macro_attribute]
pub fn test(args: TokenStream, input: TokenStream) -> TokenStream {
    parse_macro_input!(args as Nothing);
    expand(Mode::Test, parse_macro_input!(input))
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

#[proc_macro_attribute]
pub fn compat_test(args: TokenStream, input: TokenStream) -> TokenStream {
    parse_macro_input!(args as Nothing);
    expand(Mode::CompatTest, parse_macro_input!(input))
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}
