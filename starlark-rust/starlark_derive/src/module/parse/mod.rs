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
use syn::{
    spanned::Spanned, Attribute, FnArg, Item, ItemConst, ItemFn, Meta, MetaNameValue, PatType,
    Stmt, Type, TypeReference,
};

use crate::module::{
    parse::fun::parse_fun,
    typ::{StarConst, StarModule, StarStmt},
    util::is_type_name,
};

#[derive(Debug, Copy, Clone, Dupe, PartialEq, Eq)]
pub(crate) enum ModuleKind {
    Globals,
    Methods,
}

impl ModuleKind {
    pub(crate) fn statics_type_name(self) -> &'static str {
        match self {
            ModuleKind::Globals => "GlobalsStatic",
            ModuleKind::Methods => "MethodsStatic",
        }
    }
}

pub(crate) fn parse(mut input: ItemFn) -> syn::Result<StarModule> {
    let module_docstring = parse_module_docstring(&input);
    let visibility = input.vis;
    let sig_span = input.sig.span();
    let name = input.sig.ident;

    if input.sig.inputs.len() != 1 {
        return Err(syn::Error::new(
            sig_span,
            "function must have exactly one argument",
        ));
    }
    let arg = input.sig.inputs.pop().unwrap();
    let arg_span = arg.span();

    let (ty, module_kind) = match arg.into_value() {
        FnArg::Typed(PatType { ty, .. }) if is_mut_globals_builder(&ty) => {
            (ty, ModuleKind::Globals)
        }
        FnArg::Typed(PatType { ty, .. }) if is_mut_methods_builder(&ty) => {
            (ty, ModuleKind::Methods)
        }
        _ => {
            return Err(syn::Error::new(
                arg_span,
                "Expected a mutable globals or methods builder",
            ));
        }
    };
    Ok(StarModule {
        module_kind,
        visibility,
        globals_builder: *ty,
        name,
        docstring: module_docstring,
        stmts: input.block.stmts.into_try_map(parse_stmt)?,
    })
}

fn is_attribute_docstring(x: &Attribute) -> Option<String> {
    if x.path.is_ident("doc") {
        if let Ok(Meta::NameValue(MetaNameValue {
            lit: syn::Lit::Str(s),
            ..
        })) = x.parse_meta()
        {
            return Some(s.value());
        }
    }
    None
}

fn parse_module_docstring(input: &ItemFn) -> Option<String> {
    let mut doc_attrs = Vec::new();
    for attr in &input.attrs {
        if let Some(ds) = is_attribute_docstring(attr) {
            doc_attrs.push(ds);
        }
    }
    if doc_attrs.is_empty() {
        None
    } else {
        Some(doc_attrs.join("\n"))
    }
}

fn parse_stmt(stmt: Stmt) -> syn::Result<StarStmt> {
    match stmt {
        Stmt::Item(Item::Fn(x)) => parse_fun(x),
        Stmt::Item(Item::Const(x)) => Ok(StarStmt::Const(parse_const(x))),
        s => Err(syn::Error::new(
            s.span(),
            "Can only put constants and functions inside a #[starlark_module]",
        )),
    }
}

fn parse_const(x: ItemConst) -> StarConst {
    StarConst {
        name: x.ident,
        ty: *x.ty,
        value: *x.expr,
    }
}

fn is_mut_something(x: &Type, smth: &str) -> bool {
    match x {
        Type::Reference(TypeReference {
            mutability: Some(_),
            elem: x,
            ..
        }) => is_type_name(x, smth),
        _ => false,
    }
}

fn is_ref_something(x: &Type, smth: &str) -> bool {
    match x {
        Type::Reference(TypeReference {
            mutability: None,
            elem: x,
            ..
        }) => is_type_name(x, smth),
        _ => false,
    }
}

// Is the type `&mut GlobalsBuilder`
fn is_mut_globals_builder(x: &Type) -> bool {
    is_mut_something(x, "GlobalsBuilder")
}

// Is the type `&mut MethodsBuilder`
fn is_mut_methods_builder(x: &Type) -> bool {
    is_mut_something(x, "MethodsBuilder")
}
