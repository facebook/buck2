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

mod fun;

use gazebo::prelude::*;
use proc_macro2::TokenStream;
use quote::{format_ident, quote_spanned};

use crate::module::{
    render::fun::render_fun,
    typ::{SpecialParam, StarAttr, StarConst, StarModule, StarStmt},
    util::ident_string,
};

pub(crate) fn render(x: StarModule) -> syn::Result<TokenStream> {
    let span = x.span();
    let StarModule {
        name,
        globals_builder,
        visibility,
        docstring,
        stmts,
        module_kind,
    } = x;
    let statics = format_ident!("{}", module_kind.statics_type_name());
    let stmts = stmts.into_try_map(render_stmt)?;
    let set_docstring =
        docstring.map(|ds| quote_spanned!(span=> globals_builder.set_docstring(#ds);));
    Ok(quote_spanned! {
        span=>
        #visibility fn #name(globals_builder: #globals_builder) {
            fn build(globals_builder: #globals_builder) {
                #set_docstring
                #( #stmts )*
                // Mute warning if stmts is empty.
                let _ = globals_builder;
            }
            static RES: starlark::environment::#statics = starlark::environment::#statics::new();
            RES.populate(build, globals_builder);
        }
    })
}

fn render_stmt(x: StarStmt) -> syn::Result<TokenStream> {
    match x {
        StarStmt::Const(x) => Ok(render_const(x)),
        StarStmt::Attr(x) => Ok(render_attr(x)),
        StarStmt::Fun(x) => render_fun(x),
    }
}

fn render_const(x: StarConst) -> TokenStream {
    let StarConst { name, ty, value } = x;
    let span = name.span();
    let name = ident_string(&name);
    quote_spanned! {
        span=>
        globals_builder.set::<#ty>(#name, #value);
    }
}

fn render_attr(x: StarAttr) -> TokenStream {
    let span = x.span();
    let StarAttr {
        name,
        arg,
        heap,
        attrs,
        return_type,
        return_type_arg,
        speculative_exec_safe,
        body,
        docstring,
    } = x;
    let name_str = ident_string(&name);
    let docstring = match docstring {
        Some(d) => quote_spanned!(span=> Some(#d.to_owned())),
        None => quote_spanned!(span=> None),
    };

    let let_heap = if let Some(SpecialParam { ident, ty }) = heap {
        Some(quote_spanned! { span=> let #ident: #ty = heap; })
    } else {
        None
    };

    quote_spanned! {
        span=>
        #( #attrs )*
        #[allow(non_snake_case)] // Starlark doesn't have this convention
        fn #name<'v>(
            #[allow(unused_variables)]
            this: starlark::values::Value<'v>,
            heap: &'v starlark::values::Heap,
        ) -> anyhow::Result<starlark::values::Value<'v>> {
             fn inner<'v>(
                this: starlark::values::Value<'v>,
                #[allow(unused_variables)]
                heap: &'v starlark::values::Heap,
            ) -> #return_type {
                #[allow(unused_variables)]
                let this: #arg = match starlark::values::UnpackValue::unpack_value(this) {
                    None => return Err(starlark::values::ValueError::IncorrectParameterTypeNamedWithExpected(
                        "this".to_owned(),
                        <#arg as starlark::values::UnpackValue>::expected(),
                        this.get_type().to_owned(),
                    ).into()),
                    Some(v) => v,
                };
                #let_heap
                #body
            }
            Ok(heap.alloc(inner(this, heap)?))
        }
        globals_builder.set_attribute_fn(#name_str, #speculative_exec_safe, #docstring, stringify!(#return_type_arg).to_owned(), #name);
    }
}
