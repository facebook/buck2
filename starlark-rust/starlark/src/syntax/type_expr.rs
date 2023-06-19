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
use crate::eval::compiler::EvalException;
use crate::slice_vec_ext::SliceExt;
use crate::syntax::ast::AstExprP;
use crate::syntax::ast::AstIdentP;
use crate::syntax::ast::AstLiteral;
use crate::syntax::ast::AstPayload;
use crate::syntax::ast::ExprP;

#[derive(Debug, thiserror::Error)]
enum TypeExprUnpackError {
    #[error("{0} expression is not allowed in type expression")]
    InvalidType(&'static str),
    #[error("Empty list is not allowed in type expression")]
    EmptyListInType,
    #[error("Only dict literal with single entry is allowed in type expression")]
    DictNot1InType,
    #[error("Only dot expression of form `ident.ident` is allowed in type expression")]
    DotInType,
}

/// This type should be used instead of `TypeExprP`, but a lot of code needs to be updated.
#[derive(Debug)]
pub(crate) enum TypeExprUnpackP<'a, P: AstPayload> {
    Path(&'a AstIdentP<P>, Vec<Spanned<&'a str>>),
    Any(Vec<Spanned<TypeExprUnpackP<'a, P>>>),
    ListOf(Box<Spanned<TypeExprUnpackP<'a, P>>>),
    DictOf(
        Box<Spanned<TypeExprUnpackP<'a, P>>>,
        Box<Spanned<TypeExprUnpackP<'a, P>>>,
    ),
    Tuple(Vec<Spanned<TypeExprUnpackP<'a, P>>>),
    Literal(Spanned<&'a str>),
}

impl<'a, P: AstPayload> TypeExprUnpackP<'a, P> {
    pub(crate) fn unpack(
        expr: &'a AstExprP<P>,
        codemap: &CodeMap,
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
                let xs = xs.try_map(|x| TypeExprUnpackP::unpack(x, codemap))?;
                Ok(Spanned {
                    span,
                    node: TypeExprUnpackP::Tuple(xs),
                })
            }
            ExprP::Dot(object, field) => {
                let mut current: &AstExprP<P> = object;
                let mut rem: Vec<Spanned<_>> = vec![field.as_ref().into_map(|x| x.as_str())];
                loop {
                    match &current.node {
                        ExprP::Dot(o, f) => {
                            current = o;
                            rem.push(f.as_ref().into_map(|x| x.as_str()));
                        }
                        ExprP::Identifier(i) => {
                            rem.reverse();
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
            ExprP::ArrayIndirection(..) => err("array indirection"),
            ExprP::Slice(..) => err("slice"),
            ExprP::Identifier(ident) => Ok(Spanned {
                span,
                node: TypeExprUnpackP::Path(ident, Vec::new()),
            }),
            ExprP::Lambda(..) => err("lambda"),
            ExprP::Literal(AstLiteral::String(s)) => Ok(Spanned {
                span,
                node: TypeExprUnpackP::Literal(s.as_ref().into_map(|x| x.as_str())),
            }),
            ExprP::Literal(AstLiteral::Int(_)) => err("int"),
            ExprP::Literal(AstLiteral::Float(_)) => err("float"),
            ExprP::Not(..) => err("not"),
            ExprP::Minus(..) => err("minus"),
            ExprP::Plus(..) => err("plus"),
            ExprP::BitNot(..) => err("bit not"),
            ExprP::Op(..) => err("bin op"),
            ExprP::If(..) => err("if"),
            ExprP::List(xs) => {
                if xs.is_empty() {
                    Err(EvalException::new(
                        TypeExprUnpackError::EmptyListInType.into(),
                        expr.span,
                        codemap,
                    ))
                } else if xs.len() == 1 {
                    Ok(Spanned {
                        span,
                        node: TypeExprUnpackP::ListOf(Box::new(TypeExprUnpackP::unpack(
                            &xs[0], codemap,
                        )?)),
                    })
                } else {
                    let xs = xs.try_map(|x| TypeExprUnpackP::unpack(x, codemap))?;
                    Ok(Spanned {
                        span,
                        node: TypeExprUnpackP::Any(xs),
                    })
                }
            }
            ExprP::Dict(xs) => {
                if xs.len() != 1 {
                    Err(EvalException::new(
                        TypeExprUnpackError::DictNot1InType.into(),
                        expr.span,
                        codemap,
                    ))
                } else {
                    let (k, v) = &xs[0];
                    Ok(Spanned {
                        span,
                        node: TypeExprUnpackP::DictOf(
                            Box::new(TypeExprUnpackP::unpack(k, codemap)?),
                            Box::new(TypeExprUnpackP::unpack(v, codemap)?),
                        ),
                    })
                }
            }
            ExprP::ListComprehension(..) => err("list comprehension"),
            ExprP::DictComprehension(..) => err("dict comprehension"),
        }
    }
}
