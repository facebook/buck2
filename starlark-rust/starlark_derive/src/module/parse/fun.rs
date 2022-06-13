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

use proc_macro2::{Ident, Span, TokenStream};
use syn::{
    parse::ParseStream, spanned::Spanned, visit::Visit, Attribute, Expr, FnArg, GenericArgument,
    GenericParam, Generics, ItemFn, Lifetime, Meta, NestedMeta, Pat, PatType, PathArguments,
    ReturnType, Token, Type,
};

use crate::module::{
    parse::{is_attribute_docstring, is_mut_something, is_ref_something, ModuleKind},
    typ::{
        SpecialParam, StarArg, StarArgPassStyle, StarArgSource, StarAttr, StarFun, StarFunSource,
        StarStmt,
    },
};

struct ProcessedAttributes {
    is_attribute: bool,
    type_attribute: Option<Expr>,
    speculative_exec_safe: bool,
    docstring: Option<String>,
    /// Rest attributes
    attrs: Vec<Attribute>,
}

#[derive(Default)]
struct FnParamAttrs {
    default: Option<Expr>,
    this: bool,
    pos_only: bool,
    named_only: bool,
    args: bool,
    kwargs: bool,
    unused_attrs: Vec<Attribute>,
}

/// Parse `#[starlark(...)]` fn param attribute.
fn parse_starlark_fn_param_attr(
    tokens: &Attribute,
    param_attrs: &mut FnParamAttrs,
) -> syn::Result<()> {
    assert!(tokens.path.is_ident("starlark"));
    let parse = |parser: ParseStream| -> syn::Result<()> {
        let mut first = true;
        while !parser.is_empty() {
            if !first {
                parser.parse::<Token![,]>()?;
                // Allow trailing comma.
                if parser.is_empty() {
                    break;
                }
            }
            first = false;

            let ident = parser.parse::<Ident>()?;
            if ident == "default" {
                parser.parse::<Token![=]>()?;
                param_attrs.default = Some(parser.parse::<Expr>()?);
                continue;
            } else if ident == "this" {
                param_attrs.this = true;
                continue;
            } else if ident == "args" {
                param_attrs.args = true;
                continue;
            } else if ident == "kwargs" {
                param_attrs.kwargs = true;
                continue;
            } else if ident == "require" {
                parser.parse::<Token!(=)>()?;
                let require = parser.parse::<Ident>()?;
                if require == "pos" {
                    param_attrs.pos_only = true;
                    continue;
                } else if require == "named" {
                    param_attrs.named_only = true;
                    continue;
                }
            }

            return Err(syn::Error::new(
                ident.span(),
                "Expecting \
                    `#[starlark(default = expr)]`, \
                    `#[starlark(require = pos)]`, \
                    `#[starlark(require = named)]`, \
                    `#[starlark(this)]` attribute",
            ));
        }
        Ok(())
    };
    tokens.parse_args_with(parse)
}

/// Parse fn param attributes: parse `#[starlark(...)]` and take others as is.
fn parse_fn_param_attrs(attrs: Vec<Attribute>) -> syn::Result<FnParamAttrs> {
    let mut param_attrs = FnParamAttrs::default();
    for attr in attrs {
        if attr.path.is_ident("starlark") {
            parse_starlark_fn_param_attr(&attr, &mut param_attrs)?;
        } else {
            param_attrs.unused_attrs.push(attr);
        }
    }
    Ok(param_attrs)
}

/// Parse `#[starlark(...)]` attribute.
fn process_attributes(span: Span, xs: Vec<Attribute>) -> syn::Result<ProcessedAttributes> {
    const ERROR: &str = "Couldn't parse attribute. \
        Expected `#[starlark(type = \"ty\")]`, \
        `#[starlark(attribute)]` or `#[starlark(speculative_exec_safe)]`";

    let mut attrs = Vec::with_capacity(xs.len());
    let mut is_attribute = false;
    let mut type_attribute = None;
    let mut speculative_exec_safe = false;
    let mut doc_attrs = Vec::new();
    for x in xs {
        if x.path.is_ident("starlark") {
            if let Some(ty) = parse_starlark_type_eq(&x)? {
                type_attribute = Some(ty);
            } else {
                match x.parse_meta()? {
                    Meta::List(list) => {
                        assert!(list.path.is_ident("starlark"));
                        for nested in list.nested {
                            match nested {
                                NestedMeta::Lit(lit) => {
                                    return Err(syn::Error::new(lit.span(), ERROR));
                                }
                                NestedMeta::Meta(meta) => {
                                    if meta.path().is_ident("attribute") {
                                        is_attribute = true;
                                    } else if meta.path().is_ident("speculative_exec_safe") {
                                        speculative_exec_safe = true;
                                    } else {
                                        return Err(syn::Error::new(meta.span(), ERROR));
                                    }
                                }
                            }
                        }
                    }
                    _ => return Err(syn::Error::new(x.span(), ERROR)),
                }
            }
        } else if let Some(ds) = is_attribute_docstring(&x) {
            doc_attrs.push(ds);
            // Important the attributes remain tagged to the function, so the test annotations
            // are present, and thus the doc test works properly.
            attrs.push(x);
        } else {
            attrs.push(x);
        }
    }
    if is_attribute && type_attribute.is_some() {
        return Err(syn::Error::new(span, "Can't be an attribute with a .type"));
    }
    let docstring = if !doc_attrs.is_empty() {
        Some(doc_attrs.join("\n"))
    } else {
        None
    };
    Ok(ProcessedAttributes {
        is_attribute,
        type_attribute,
        speculative_exec_safe,
        docstring,
        attrs,
    })
}

/// Check if given type is `anyhow::Result<T>`, and if it is, return `T`.
fn is_anyhow_result(t: &Type) -> Option<Type> {
    let path = match t {
        Type::Path(p) => p,
        _ => return None,
    };
    if path.qself.is_some() {
        return None;
    }
    let mut segments = path.path.segments.iter();
    match segments.next() {
        None => return None,
        Some(s) if s.ident != "anyhow" => return None,
        _ => {}
    };
    let result = match segments.next() {
        None => return None,
        Some(s) if s.ident != "Result" => return None,
        Some(result) => result,
    };
    if segments.next().is_some() {
        return None;
    }
    let result_arguments = match &result.arguments {
        PathArguments::AngleBracketed(args) => args,
        _ => return None,
    };
    let mut result_arguments = result_arguments.args.iter();
    let t = match result_arguments.next() {
        None => return None,
        Some(t) => t,
    };
    match t {
        GenericArgument::Type(t) => Some(t.clone()),
        _ => None,
    }
}

// Add a function to the `GlobalsModule` named `globals_builder`.
pub(crate) fn parse_fun(func: ItemFn, module_kind: ModuleKind) -> syn::Result<StarStmt> {
    let sig_span = func.sig.span();

    let ProcessedAttributes {
        is_attribute,
        type_attribute,
        speculative_exec_safe,
        docstring,
        attrs,
    } = process_attributes(func.span(), func.attrs)?;

    let has_v = parse_fn_generics(&func.sig.generics)?;

    let (return_type, return_type_arg) = parse_fn_output(&func.sig.output, func.sig.span(), has_v)?;

    let mut eval = None;
    let mut heap = None;

    let mut seen_star_args = false;
    let mut args = Vec::new();
    for (i, arg) in func.sig.inputs.into_iter().enumerate() {
        let span = arg.span();
        let parsed_arg = parse_arg(arg, has_v, seen_star_args, module_kind, i)?;
        match parsed_arg {
            StarArgOrSpecial::Heap(special) => {
                if heap.is_some() {
                    return Err(syn::Error::new(span, "Repeated `&Heap` parameter"));
                }
                heap = Some(special);
            }
            StarArgOrSpecial::Eval(special) => {
                if eval.is_some() {
                    return Err(syn::Error::new(span, "Repeated `&mut Evaluator` parameter"));
                }
                eval = Some(special);
            }
            StarArgOrSpecial::StarArg(arg) => {
                if arg.pass_style == StarArgPassStyle::Args {
                    seen_star_args = true;
                }
                args.push(arg);
            }
        }
    }

    if eval.is_some() && heap.is_some() {
        return Err(syn::Error::new(
            sig_span,
            "Can't have both `&mut Evaluator` and `&Heap` parameters",
        ));
    }

    if is_attribute {
        if eval.is_some() {
            return Err(syn::Error::new(
                sig_span,
                "Attributes cannot have `&mut Evaluator` parameter",
            ));
        }

        if args.len() != 1 {
            return Err(syn::Error::new(
                sig_span,
                "Attribute function must have single parameter",
            ));
        }
        let arg = args.pop().unwrap();
        if arg.pass_style != StarArgPassStyle::This {
            return Err(syn::Error::new(
                sig_span,
                "Attribute function must have `this` as the only parameter",
            ));
        }
        if arg.default.is_some() {
            return Err(syn::Error::new(
                sig_span,
                "Attribute function `this` parameter have no default value",
            ));
        }
        Ok(StarStmt::Attr(StarAttr {
            name: func.sig.ident,
            arg: arg.ty,
            heap,
            attrs,
            return_type,
            return_type_arg,
            speculative_exec_safe,
            body: *func.block,
            docstring,
        }))
    } else {
        let is_method = !args.is_empty() && args[0].pass_style == StarArgPassStyle::This;
        if is_method != (module_kind == ModuleKind::Methods) {
            return Err(syn::Error::new(
                sig_span,
                "Methods can only be defined in methods module",
            ));
        }

        let source = resolve_args(&mut args)?;

        let fun = StarFun {
            name: func.sig.ident,
            type_attribute,
            attrs,
            args,
            heap,
            eval,
            return_type,
            return_type_arg,
            speculative_exec_safe,
            body: *func.block,
            source,
            docstring,
        };
        Ok(StarStmt::Fun(fun))
    }
}

#[allow(clippy::branches_sharing_code)] // False positive
fn resolve_args(args: &mut [StarArg]) -> syn::Result<StarFunSource> {
    if args.len() == 1 && args[0].pass_style == StarArgPassStyle::Arguments {
        args[0].source = StarArgSource::Parameters;
        Ok(StarFunSource::Parameters)
    } else if args.len() == 2
        && args[0].pass_style == StarArgPassStyle::This
        && args[1].pass_style == StarArgPassStyle::Arguments
    {
        args[0].source = StarArgSource::This;
        args[1].source = StarArgSource::Parameters;
        Ok(StarFunSource::ThisParameters)
    } else {
        let use_arguments = args
            .iter()
            .filter(|x| x.pass_style != StarArgPassStyle::This)
            .any(|x| x.requires_signature());
        if use_arguments {
            let mut argument = 0;
            for x in args.iter_mut() {
                if x.pass_style == StarArgPassStyle::This {
                    x.source = StarArgSource::This;
                } else {
                    x.source = StarArgSource::Argument(argument);
                    argument += 1;
                }
            }
            Ok(StarFunSource::Argument(argument))
        } else {
            let mut required = 0;
            let mut optional = 0;
            for x in args.iter_mut() {
                if x.pass_style == StarArgPassStyle::This {
                    x.source = StarArgSource::This;
                    continue;
                }
                if optional == 0 && x.default.is_none() && !x.is_option() {
                    x.source = StarArgSource::Required(required);
                    required += 1;
                } else {
                    x.source = StarArgSource::Optional(optional);
                    optional += 1;
                }
            }
            Ok(StarFunSource::Positional(required, optional))
        }
    }
}

/// Check there are no undeclared lifetiems in return type.
fn check_lifetimes_in_return_type(return_type: &ReturnType, has_v: bool) -> syn::Result<()> {
    match return_type {
        ReturnType::Default => Ok(()),
        ReturnType::Type(_, ty) => check_lifetimes_in_type(ty, has_v),
    }
}

/// Check there are no undeclared lifetiems in type.
fn check_lifetimes_in_type(ty: &Type, has_v: bool) -> syn::Result<()> {
    struct VisitImpl {
        has_v: bool,
        result: syn::Result<()>,
    }

    impl<'ast> Visit<'ast> for VisitImpl {
        #[allow(clippy::collapsible_if)]
        fn visit_lifetime(&mut self, lifetime: &'ast Lifetime) {
            if self.result.is_ok() {
                if lifetime.ident != "_" {
                    if lifetime.ident != "v" {
                        self.result = Err(syn::Error::new(
                            lifetime.span(),
                            "Only 'v lifetime is allowed",
                        ));
                    }
                    if !self.has_v {
                        self.result = Err(syn::Error::new(lifetime.span(), "Undeclared lifetime"));
                    }
                }
            }
        }
    }

    let mut visit = VisitImpl {
        has_v,
        result: Ok(()),
    };

    visit.visit_type(ty);

    visit.result
}

fn parse_fn_output(return_type: &ReturnType, span: Span, has_v: bool) -> syn::Result<(Type, Type)> {
    check_lifetimes_in_return_type(return_type, has_v)?;
    match return_type {
        ReturnType::Default => Err(syn::Error::new(span, "Function must have a return type")),
        ReturnType::Type(_, x) => match is_anyhow_result(x) {
            Some(return_arg_type) => Ok(((**x).clone(), return_arg_type)),
            None => Err(syn::Error::new(
                return_type.span(),
                "Function return type must be precisely `anyhow::Result<...>`",
            )),
        },
    }
}

fn parse_fn_generics(generics: &Generics) -> syn::Result<bool> {
    let mut seen_v = false;
    for param in &generics.params {
        match param {
            GenericParam::Type(..) => {
                return Err(syn::Error::new(
                    param.span(),
                    "Function cannot have type parameters",
                ));
            }
            GenericParam::Const(..) => {
                return Err(syn::Error::new(
                    param.span(),
                    "Function cannot have const parameters",
                ));
            }
            GenericParam::Lifetime(lifetime) => {
                if lifetime.lifetime.ident != "v" {
                    return Err(syn::Error::new(
                        lifetime.lifetime.span(),
                        "Function cannot have lifetime parameters other than `v",
                    ));
                }
                if !lifetime.bounds.is_empty() {
                    return Err(syn::Error::new(
                        lifetime.span(),
                        "Function lifetime params must not have bounds",
                    ));
                }
                if seen_v {
                    return Err(syn::Error::new(lifetime.span(), "Duplicate `v parameters"));
                }
                seen_v = true;
            }
        }
    }
    Ok(seen_v)
}

/// Parse `#[starlark(type = expr)]`.
fn parse_starlark_type_eq(tokens: &Attribute) -> syn::Result<Option<Expr>> {
    assert!(tokens.path.is_ident("starlark"));
    let parse = |parser: ParseStream| -> syn::Result<Option<Expr>> {
        if parser.parse::<Token![type]>().is_err() {
            parser.parse::<TokenStream>()?;
            return Ok(None);
        }
        parser.parse::<Token![=]>()?;
        parser.parse::<Expr>().map(Some)
    };
    tokens.parse_args_with(parse)
}

#[allow(clippy::large_enum_variant)]
enum StarArgOrSpecial {
    StarArg(StarArg),
    /// `&mut Evaluator`.
    Eval(SpecialParam),
    /// `&Heap`.
    Heap(SpecialParam),
}

/// Function parameter is `eval: &mut Evaluator`.
fn is_eval(ident: &Ident, ty: &Type) -> syn::Result<Option<SpecialParam>> {
    if is_mut_something(ty, "Evaluator") {
        Ok(Some(SpecialParam {
            ident: ident.clone(),
            ty: ty.clone(),
        }))
    } else {
        Ok(None)
    }
}

/// Function parameter is `heap: &Heap`.
fn is_heap(ident: &Ident, ty: &Type) -> syn::Result<Option<SpecialParam>> {
    if is_ref_something(ty, "Heap") {
        Ok(Some(SpecialParam {
            ident: ident.clone(),
            ty: ty.clone(),
        }))
    } else {
        Ok(None)
    }
}

#[allow(clippy::collapsible_else_if)]
fn parse_arg(
    x: FnArg,
    has_v: bool,
    seen_star_args: bool,
    module_kind: ModuleKind,
    param_index: usize,
) -> syn::Result<StarArgOrSpecial> {
    let this = module_kind == ModuleKind::Methods && param_index == 0;

    let span = x.span();
    match x {
        FnArg::Typed(PatType {
            attrs,
            pat: box Pat::Ident(ident),
            ty: box ty,
            ..
        }) => {
            if let Some(heap) = is_heap(&ident.ident, &ty)? {
                if this {
                    return Err(syn::Error::new(
                        span,
                        "Receiver parameter cannot be `&Heap`",
                    ));
                }
                return Ok(StarArgOrSpecial::Heap(heap));
            } else if let Some(eval) = is_eval(&ident.ident, &ty)? {
                if this {
                    return Err(syn::Error::new(
                        span,
                        "Receiver parameter cannot be `&mut Evaluator`",
                    ));
                }
                return Ok(StarArgOrSpecial::Eval(eval));
            }

            if ident.subpat.is_some() {
                return Err(syn::Error::new(
                    ident.span(),
                    "Function arguments cannot use patterns",
                ));
            }

            if ident.by_ref.is_some() {
                return Err(syn::Error::new(
                    ident.span(),
                    "Function arguments cannot have `ref` modifier",
                ));
            }

            check_lifetimes_in_type(&ty, has_v)?;
            let param_attrs = parse_fn_param_attrs(attrs)?;

            if this {
                if !param_attrs.this && ident.ident != "this" {
                    return Err(syn::Error::new(
                        span,
                        "Receiver parameter must be named `this` \
                            or have `#[starlark(this)]` annotation",
                    ));
                }
            } else {
                if param_attrs.this {
                    return Err(syn::Error::new(
                        span,
                        "Receiver parameter can be only first",
                    ));
                }
            }

            let arguments = is_ref_something(&ty, "Arguments");
            let args = ident.ident == "args" && !arguments;
            let kwargs = ident.ident == "kwargs" && !arguments;

            if param_attrs.args && !args {
                return Err(syn::Error::new(
                    span,
                    "`#[starlark(args)]` annotation can only be used \
                        on parameter named `args`",
                ));
            }
            if param_attrs.kwargs && !kwargs {
                return Err(syn::Error::new(
                    span,
                    "`#[starlark(kwargs)]` annotation can only be used \
                        on parameter named `kwargs`",
                ));
            }

            let pass_style = match (
                this,
                args,
                kwargs,
                seen_star_args,
                param_attrs.pos_only,
                param_attrs.named_only,
                arguments,
            ) {
                (true, _, _, _, _, _, false) => StarArgPassStyle::This,
                (false, true, _, _, _, _, false) => StarArgPassStyle::Args,
                (false, _, true, _, _, _, false) => StarArgPassStyle::Kwargs,
                (false, _, _, true, true, _, false) => {
                    return Err(syn::Error::new(
                        span,
                        "Positional-only arguments cannot follow *args",
                    ));
                }
                (false, false, false, true, false, _, false) => StarArgPassStyle::NamedOnly,
                (false, false, false, false, false, false, false) => StarArgPassStyle::PosOrNamed,
                (false, false, false, false, true, false, false) => StarArgPassStyle::PosOnly,
                (false, false, false, false, false, true, false) => StarArgPassStyle::NamedOnly,
                (false, false, false, false, true, true, false) => {
                    return Err(syn::Error::new(
                        span,
                        "Function parameter cannot be both positional-only and named-only",
                    ));
                }
                (false, false, false, _, false, false, true) => StarArgPassStyle::Arguments,
                (_, _, _, _, _, _, true) => {
                    return Err(syn::Error::new(
                        span,
                        "`&Arguments` parameter type is incompatible with annotations",
                    ));
                }
            };
            Ok(StarArgOrSpecial::StarArg(StarArg {
                span,
                attrs: param_attrs.unused_attrs,
                mutable: ident.mutability.is_some(),
                name: ident.ident,
                pass_style,
                ty,
                default: param_attrs.default,
                source: StarArgSource::Unknown,
            }))
        }
        FnArg::Typed(PatType { .. }) => Err(syn::Error::new(
            span,
            "Function parameter pattern must be identifier",
        )),
        FnArg::Receiver(..) => Err(syn::Error::new(
            span,
            "Function cannot have `self` parameters",
        )),
    }
}
