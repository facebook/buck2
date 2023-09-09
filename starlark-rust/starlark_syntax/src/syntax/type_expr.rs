/*
 * Copyright 2018 The Starlark in Rust Authors.
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

use crate::codemap::CodeMap;
use crate::codemap::Spanned;
use crate::eval_exception::EvalException;
use crate::slice_vec_ext::SliceExt;
use crate::syntax::ast::AstExprP;
use crate::syntax::ast::AstIdentP;
use crate::syntax::ast::AstLiteral;
use crate::syntax::ast::AstPayload;
use crate::syntax::ast::BinOp;
use crate::syntax::ast::ExprP;

#[derive(Debug, thiserror::Error)]
enum TypeExprUnpackError {
    #[error("{0} expression is not allowed in type expression")]
    InvalidType(&'static str),
    #[error("Empty list is not allowed in type expression")]
    EmptyListInType,
    #[error("Only dot expression of form `ident.ident` is allowed in type expression")]
    DotInType,
    #[error(r#"`""` or `"_xxx"` is not allowed in type expression, use `typing.Any` instead"#)]
    EmptyStrInType,
    #[error(r#"`"{0}"` is not allowed in type expression, use `{1}` instead"#)]
    StrBanReplace(&'static str, &'static str),
    #[error(r#"`{0}.type` is not allowed in type expression, use `{0}` instead"#)]
    DotTypeBan(String),
}

/// Types that are `""` or start with `"_"` are wildcard - they match everything
/// (also deprecated).
pub fn type_str_literal_is_wildcard(s: &str) -> bool {
    s == "" || s.starts_with('_')
}

/// This type should be used instead of `TypeExprP`, but a lot of code needs to be updated.
#[derive(Debug)]
pub enum TypeExprUnpackP<'a, P: AstPayload> {
    Path(&'a AstIdentP<P>, Vec<Spanned<&'a str>>),
    /// `list[str]`.
    Index(&'a AstIdentP<P>, Box<Spanned<TypeExprUnpackP<'a, P>>>),
    /// `dict[str, int]`.
    Index2(
        &'a AstIdentP<P>,
        Box<Spanned<TypeExprUnpackP<'a, P>>>,
        Box<Spanned<TypeExprUnpackP<'a, P>>>,
    ),
    /// `tuple[str, ...]`.
    Index2Ellipsis(&'a AstIdentP<P>, Box<Spanned<TypeExprUnpackP<'a, P>>>),
    Union(Vec<Spanned<TypeExprUnpackP<'a, P>>>),
    Tuple(Vec<Spanned<TypeExprUnpackP<'a, P>>>),
    Literal(Spanned<&'a str>),
}

/// List of builtin types which are converted to proper types.
/// First is the type name, second is the symbol.
const BAN_REPLACE_TYPES: &[(&str, &str)] = &[
    ("str", "str"),
    ("string", "str"),
    ("int", "int"),
    ("float", "float"),
    ("bool", "bool"),
    ("list", "list"),
    ("dict", "dict"),
    ("tuple", "tuple"),
    ("range", "range"),
    ("struct", "struct"),
    ("NoneType", "None"),
    ("None", "None"),
    ("function", "typing.Callable"),
    // Following do not belong to starlark, but this code will go away
    // after we finish migration from string-based types.
    ("actions", "AnalysisActions"),
    ("context", "AnalysisContext"),
    ("artifact", "Artifact"),
    ("artifact_tag", "ArtifactTag"),
    ("dependency", "Dependency"),
    ("provider", "Provider"),
    ("selector", "Select"),
    ("transitive_set", "TransitiveSet"),
    (
        "transitive_set_args_projection",
        "TransitiveSetArgsProjection",
    ),
    ("transitive_set_definition", "TransitiveSetDefinition"),
    (
        "transitive_set_json_projection",
        "TransitiveSetJsonProjection",
    ),
    ("transitive_set_iterator", "TransitiveSetIterator"),
    (
        "transitive_set_args_projection_iterator",
        "TransitiveSetArgsProjectionIterator",
    ),
    ("target_label", "TargetLabel"),
    ("configured_target_label", "ConfiguredTargetLabel"),
    ("providers_label", "ProvidersLabel"),
    ("label", "ConfiguredProvidersLabel"),
];

impl<'a, P: AstPayload> TypeExprUnpackP<'a, P> {
    pub fn unpack(
        expr: &'a AstExprP<P>,
        codemap: &CodeMap,
        allow_string_literals_in_type_expr: bool,
    ) -> Result<Spanned<TypeExprUnpackP<'a, P>>, EvalException> {
        let span = expr.span;
        let err = |t| {
            Err(EvalException::new(
                TypeExprUnpackError::InvalidType(t).into(),
                expr.span,
                codemap,
            ))
        };

        match &expr.node {
            ExprP::Tuple(xs) => {
                let xs = xs.try_map(|x| {
                    TypeExprUnpackP::unpack(x, codemap, allow_string_literals_in_type_expr)
                })?;
                Ok(Spanned {
                    span,
                    node: TypeExprUnpackP::Tuple(xs),
                })
            }
            ExprP::Dot(object, field) => {
                let mut current: &AstExprP<P> = object;
                let mut rem: Vec<Spanned<_>> = vec![field.as_ref().map(|x| x.as_str())];
                loop {
                    match &current.node {
                        ExprP::Dot(o, f) => {
                            current = o;
                            rem.push(f.as_ref().map(|x| x.as_str()));
                        }
                        ExprP::Identifier(i) => {
                            rem.reverse();
                            if let Some((last, but_last)) = rem.split_last() {
                                if last.node == "type" {
                                    let mut full_path = i.node.ident.clone();
                                    for elem in but_last {
                                        full_path.push_str(&format!(".{}", elem.node));
                                    }
                                    // TODO(nga): allow it after we prohibit
                                    //   string constants as types.
                                    return Err(EvalException::new(
                                        TypeExprUnpackError::DotTypeBan(full_path).into(),
                                        current.span,
                                        codemap,
                                    ));
                                }
                            }
                            return Ok(Spanned {
                                span,
                                node: TypeExprUnpackP::Path(i, rem),
                            });
                        }
                        _ => {
                            return Err(EvalException::new(
                                TypeExprUnpackError::DotInType.into(),
                                current.span,
                                codemap,
                            ));
                        }
                    }
                }
                // We would also want to ban expressions like `x.y` where `x` is not `type`,
                // or `x.y.z` but these are used now.
                // Try `xbgs metalos.ProvisioningConfig`.
                // That expression has type string which is the type name.
            }
            ExprP::Call(..) => err("call"),
            ExprP::Index(a_i) => {
                let (a, i) = &**a_i;
                match &a.node {
                    ExprP::Identifier(ident) => {
                        let i = TypeExprUnpackP::unpack(
                            i,
                            codemap,
                            allow_string_literals_in_type_expr,
                        )?;
                        Ok(Spanned {
                            span,
                            node: TypeExprUnpackP::Index(ident, Box::new(i)),
                        })
                    }
                    _ => err("array indirection where array is not an identifier"),
                }
            }
            ExprP::Index2(a_i0_i1) => {
                let (a, i0, i1) = &**a_i0_i1;
                let ExprP::Identifier(ident) = &a.node else {
                    return err("array indirection 2 where array is not an identifier");
                };
                if let ExprP::Literal(AstLiteral::Ellipsis) = &i1.node {
                    let i0 =
                        TypeExprUnpackP::unpack(i0, codemap, allow_string_literals_in_type_expr)?;
                    Ok(Spanned {
                        span,
                        node: TypeExprUnpackP::Index2Ellipsis(ident, Box::new(i0)),
                    })
                } else {
                    let i0 =
                        TypeExprUnpackP::unpack(i0, codemap, allow_string_literals_in_type_expr)?;
                    let i1 =
                        TypeExprUnpackP::unpack(i1, codemap, allow_string_literals_in_type_expr)?;
                    Ok(Spanned {
                        span,
                        node: TypeExprUnpackP::Index2(ident, Box::new(i0), Box::new(i1)),
                    })
                }
            }
            ExprP::Slice(..) => err("slice"),
            ExprP::Identifier(ident) => Ok(Spanned {
                span,
                node: TypeExprUnpackP::Path(ident, Vec::new()),
            }),
            ExprP::Lambda(..) => err("lambda"),
            ExprP::Literal(AstLiteral::String(s)) => {
                if !allow_string_literals_in_type_expr {
                    return err("string literal");
                }
                if type_str_literal_is_wildcard(s) {
                    return Err(EvalException::new(
                        TypeExprUnpackError::EmptyStrInType.into(),
                        expr.span,
                        codemap,
                    ));
                }
                for (ban, replace) in BAN_REPLACE_TYPES {
                    if s.as_str() == *ban {
                        return Err(EvalException::new(
                            TypeExprUnpackError::StrBanReplace(ban, replace).into(),
                            expr.span,
                            codemap,
                        ));
                    }
                }
                Ok(Spanned {
                    span,
                    node: TypeExprUnpackP::Literal(s.as_ref().map(|x| x.as_str())),
                })
            }
            ExprP::Literal(AstLiteral::Int(_)) => err("int"),
            ExprP::Literal(AstLiteral::Float(_)) => err("float"),
            ExprP::Literal(AstLiteral::Ellipsis) => err("ellipsis"),
            ExprP::Not(..) => err("not"),
            ExprP::Minus(..) => err("minus"),
            ExprP::Plus(..) => err("plus"),
            ExprP::BitNot(..) => err("bit not"),
            ExprP::Op(a, BinOp::BitOr, b) => {
                let a = TypeExprUnpackP::unpack(a, codemap, allow_string_literals_in_type_expr)?;
                let b = TypeExprUnpackP::unpack(b, codemap, allow_string_literals_in_type_expr)?;
                Ok(Spanned {
                    span,
                    node: TypeExprUnpackP::Union(vec![a, b]),
                })
            }
            ExprP::Op(..) => err("bin op except `|`"),
            ExprP::If(..) => err("if"),
            ExprP::List(xs) => {
                if xs.is_empty() {
                    Err(EvalException::new(
                        TypeExprUnpackError::EmptyListInType.into(),
                        expr.span,
                        codemap,
                    ))
                } else if xs.len() == 1 {
                    err("list of 1 element")
                } else {
                    let xs = xs.try_map(|x| {
                        TypeExprUnpackP::unpack(x, codemap, allow_string_literals_in_type_expr)
                    })?;
                    Ok(Spanned {
                        span,
                        node: TypeExprUnpackP::Union(xs),
                    })
                }
            }
            ExprP::Dict(..) => err("dict"),
            ExprP::ListComprehension(..) => err("list comprehension"),
            ExprP::DictComprehension(..) => err("dict comprehension"),
            ExprP::FString(..) => err("f-string"),
        }
    }
}
