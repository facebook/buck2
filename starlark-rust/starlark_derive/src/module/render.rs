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

use proc_macro2::TokenStream;
use quote::ToTokens;
use quote::format_ident;
use quote::quote;

use crate::module::render::fun::render_fun;
use crate::module::simple_param::SimpleParam;
use crate::module::typ::SpecialParam;
use crate::module::typ::StarAttr;
use crate::module::typ::StarConst;
use crate::module::typ::StarGenerics;
use crate::module::typ::StarModule;
use crate::module::typ::StarStmt;
use crate::module::util::ident_string;

pub(crate) fn render(x: StarModule) -> syn::Result<TokenStream> {
    Ok(render_impl(x)?.to_token_stream())
}

fn render_impl(x: StarModule) -> syn::Result<syn::ItemFn> {
    let StarModule {
        mut input,
        docstring,
        stmts,
        module_kind,
        generics,
    } = x;
    let statics = format_ident!("{}", module_kind.statics_type_name());
    let stmts: Vec<_> = stmts
        .into_iter()
        .map(|s| render_stmt(s, &generics))
        .collect::<syn::Result<_>>()?;
    let set_docstring = docstring.map(|ds| quote!(globals_builder.set_docstring(#ds);));

    let inner_fn = syn::ItemFn {
        attrs: Default::default(),
        vis: syn::Visibility::Inherited,
        sig: syn::Signature {
            ident: syn::Ident::new("build", input.sig.ident.span()),
            ..input.sig.clone()
        },
        block: syn::parse_quote! {
            {
                #set_docstring
                #( #stmts )*
                // Mute warning if stmts is empty.
                let _ = globals_builder;
            }
        },
    };
    let turbofish = generics.turbofish();
    input.block = syn::parse_quote! {
        {
            #inner_fn
            static RES: starlark::environment::#statics = starlark::environment::#statics::new();
            RES.populate(build #turbofish, globals_builder);
        }
    };
    Ok(input)
}

fn render_stmt(x: StarStmt, generics: &StarGenerics) -> syn::Result<syn::Stmt> {
    match x {
        StarStmt::Const(x) => Ok(render_const(x)),
        StarStmt::Attr(x) => Ok(render_attr(x, generics)),
        StarStmt::Fun(x) => render_fun(x, generics),
    }
}

fn render_const(x: StarConst) -> syn::Stmt {
    let StarConst { name, ty, value } = x;
    let name = ident_string(&name);
    syn::parse_quote! {
        globals_builder.set::<#ty>(#name, #value);
    }
}

fn render_attr(x: StarAttr, generics: &StarGenerics) -> syn::Stmt {
    let StarAttr {
        name,
        this,
        heap,
        attrs,
        return_type,
        speculative_exec_safe,
        body,
        docstring,
    } = x;
    let name_str = ident_string(&name);
    let name_inner = syn::Ident::new(&format!("{name_str}__inner"), name.span());
    let docstring: syn::Expr = match docstring {
        Some(d) => render_some(syn::parse_quote! { #d.to_owned() }),
        None => render_none(),
    };

    let generic_decls = generics.decls();
    let where_clause = generics.where_clause();
    let turbofish = generics.turbofish();

    let let_heap = if let Some(SpecialParam {
        param: SimpleParam { ident, ty, .. },
    }) = heap
    {
        Some(quote! { let #ident: #ty = __heap; })
    } else {
        None
    };

    let this_value: syn::Ident = syn::parse_quote! { s_this_value };
    let this_return_type: &syn::Type = &this.param.ty;

    let unpack = this.render_prepare(&this.param.ident, &this_value);

    let inner: syn::ItemFn = syn::parse_quote! {
        #( #attrs )*
        #[allow(non_snake_case)] // Starlark doesn't have this convention
        #[allow(unused_variables)]
        fn #name_inner #generic_decls(
            this: #this_return_type,
            __heap: starlark::values::Heap<'v>,
        ) -> #return_type
        #where_clause
        {
            #let_heap
            #body
        }
    };

    let outer: syn::ItemFn = syn::parse_quote! {
        #[allow(non_snake_case)]
        fn #name #generic_decls(
            _ignored: std::option::Option<starlark::values::FrozenValue>,
            #this_value: starlark::values::Value<'v>,
            heap: starlark::values::Heap<'v>,
        ) -> starlark::Result<starlark::values::Value<'v>>
        #where_clause
        {
            #unpack
            Ok(heap.alloc(#name_inner #turbofish(this, heap)?))
        }
    };

    syn::parse_quote! {
        {
            #inner
            #outer

            globals_builder.set_attribute_fn(
                #name_str,
                #speculative_exec_safe,
                #docstring,
                starlark::values::type_repr::type_repr_from_attr_impl(#name_inner #turbofish),
                #name #turbofish
            );
        }
    }
}

pub(crate) fn render_none() -> syn::Expr {
    syn::parse_quote! { std::option::Option::None }
}

pub(crate) fn render_some(expr: syn::Expr) -> syn::Expr {
    syn::parse_quote! { std::option::Option::Some(#expr) }
}
