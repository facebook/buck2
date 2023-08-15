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
use crate::syntax::ast::BinOp;
use crate::syntax::ast::ExprP;
use crate::values::typing::type_compiled::compiled::TypeCompiled;

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
}

/// This type should be used instead of `TypeExprP`, but a lot of code needs to be updated.
#[derive(Debug)]
pub(crate) enum TypeExprUnpackP<'a, P: AstPayload> {
    Path(&'a AstIdentP<P>, Vec<Spanned<&'a str>>),
    /// `list[str]`.
    Index(&'a AstIdentP<P>, Box<Spanned<TypeExprUnpackP<'a, P>>>),
    /// `dict[str, int]`.
    Index2(
        &'a AstIdentP<P>,
        Box<Spanned<TypeExprUnpackP<'a, P>>>,
        Box<Spanned<TypeExprUnpackP<'a, P>>>,
    ),
    Union(Vec<Spanned<TypeExprUnpackP<'a, P>>>),
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
                let mut rem: Vec<Spanned<_>> = vec![field.as_ref().map(|x| x.as_str())];
                loop {
                    match &current.node {
                        ExprP::Dot(o, f) => {
                            current = o;
                            rem.push(f.as_ref().map(|x| x.as_str()));
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
            ExprP::Index(a_i) => {
                let (a, i) = &**a_i;
                match &a.node {
                    ExprP::Identifier(ident) => {
                        let i = TypeExprUnpackP::unpack(i, codemap)?;
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
                match &a.node {
                    ExprP::Identifier(ident) => {
                        let i0 = TypeExprUnpackP::unpack(i0, codemap)?;
                        let i1 = TypeExprUnpackP::unpack(i1, codemap)?;
                        Ok(Spanned {
                            span,
                            node: TypeExprUnpackP::Index2(ident, Box::new(i0), Box::new(i1)),
                        })
                    }
                    _ => err("array indirection 2 where array is not an identifier"),
                }
            }
            ExprP::Slice(..) => err("slice"),
            ExprP::Identifier(ident) => Ok(Spanned {
                span,
                node: TypeExprUnpackP::Path(ident, Vec::new()),
            }),
            ExprP::Lambda(..) => err("lambda"),
            ExprP::Literal(AstLiteral::String(s)) => {
                if TypeCompiled::is_wildcard(s) {
                    return Err(EvalException::new(
                        TypeExprUnpackError::EmptyStrInType.into(),
                        expr.span,
                        codemap,
                    ));
                }
                let ban_replace = [
                    ("str", "str"),
                    ("string", "str"),
                    ("int", "int"),
                    ("float", "float"),
                    ("bool", "bool"),
                    ("list", "list"),
                    ("dict", "dict"),
                    ("tuple", "tuple"),
                    ("NoneType", "None"),
                    ("None", "None"),
                    // TODO(nga): ban `"function"` too.
                    // ("function", "typing.Callable"),
                ];
                for (ban, replace) in ban_replace {
                    if s.as_str() == ban {
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
            ExprP::Op(a, op, b) if op == &BinOp::BitOr => {
                let a = TypeExprUnpackP::unpack(a, codemap)?;
                let b = TypeExprUnpackP::unpack(b, codemap)?;
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
                    let xs = xs.try_map(|x| TypeExprUnpackP::unpack(x, codemap))?;
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
