/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use proc_macro2::TokenStream;
use quote::quote;
use syn::punctuated::Punctuated;
use syn::{parse_quote, Error, ItemFn, Result};

#[derive(Copy, Clone, PartialEq)]
pub enum Mode {
    Main,
    Test,
    CompatTest,
}

pub fn expand(mode: Mode, mut function: ItemFn) -> Result<TokenStream> {
    if function.sig.inputs.len() > 1 {
        return Err(Error::new_spanned(
            function.sig,
            "expected one argument of type fbinit::FacebookInit",
        ));
    }

    if mode == Mode::Main && function.sig.ident != "main" {
        return Err(Error::new_spanned(
            function.sig,
            "#[fbinit::main] must be used on the main function",
        ));
    }

    let guard = match mode {
        Mode::Main => Some(quote! {
            if module_path!().contains("::") {
                panic!("fbinit must be performed in the crate root on the main function");
            }
        }),
        Mode::Test | Mode::CompatTest => None,
    };

    let assignment = function.sig.inputs.first().map(|arg| quote!(let #arg =));
    function.sig.inputs = Punctuated::new();

    let block = function.block;

    let body = match (function.sig.asyncness.is_some(), mode) {
        (true, Mode::CompatTest) => quote! {
            tokio_compat::runtime::current_thread::Runtime::new()
                .unwrap()
                .block_on_std(async #block)
        },
        (true, Mode::Test) => quote! {
            tokio::runtime::Builder::new()
                .basic_scheduler()
                .enable_all()
                .build()
                .unwrap()
                .block_on(async #block)
        },
        (true, Mode::Main) => quote! {
            tokio::runtime::Builder::new()
                .threaded_scheduler()
                .enable_all()
                .build()
                .unwrap()
                .block_on(async #block)
        },
        (false, Mode::CompatTest) => {
            return Err(Error::new_spanned(
                function.sig,
                "#[fbinit::compat_test] should be used only on async functions",
            ));
        }
        (false, _) => {
            let stmts = block.stmts;
            quote! { #(#stmts)* }
        }
    };

    function.block = parse_quote!({
        #guard
        #assignment unsafe {
            fbinit::r#impl::perform_init()
        };
        let destroy_guard = unsafe { fbinit::r#impl::DestroyGuard::new() };
        #body
    });

    function.sig.asyncness = None;

    if mode == Mode::Test || mode == Mode::CompatTest {
        function.attrs.push(parse_quote!(#[test]));
    }

    Ok(quote!(#function))
}
