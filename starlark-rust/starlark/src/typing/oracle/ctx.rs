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

use std::fmt::Display;

use dupe::Dupe;

use crate::codemap::CodeMap;
use crate::codemap::Span;
use crate::codemap::Spanned;
use crate::typing::error::TypingError;
use crate::typing::function::Arg;
use crate::typing::function::Param;
use crate::typing::function::ParamMode;
use crate::typing::function::TyFunction;
use crate::typing::Ty;
use crate::typing::TyName;
use crate::typing::TypingAttr;
use crate::typing::TypingBinOp;
use crate::typing::TypingOracle;

#[derive(Debug, thiserror::Error)]
enum TypingOracleCtxError {
    #[error("Expected type `{require}` but got `{got}`")]
    IncompatibleType { got: String, require: String },
    #[error("Call to a non-callable type `{ty}`")]
    CallToNonCallable { ty: String },
    #[error("Missing required parameter `{name}`")]
    MissingRequiredParameter { name: String },
    #[error("Unexpected parameter named `{name}`")]
    UnexpectedNamedArgument { name: String },
    #[error("Too many positional arguments")]
    TooManyPositionalArguments,
    #[error("Call arguments incompatible")]
    CallArgumentsIncompatible,
}

/// Oracle reference with utility methods.
///
/// This type is stateless.
#[derive(Clone, Copy, Dupe)]
pub struct TypingOracleCtx<'a> {
    pub(crate) oracle: &'a dyn TypingOracle,
    pub(crate) codemap: &'a CodeMap,
}

impl<'a> TypingOracle for TypingOracleCtx<'a> {
    fn attribute(&self, ty: &Ty, attr: TypingAttr) -> Option<Result<Ty, ()>> {
        Some(Ok(match ty {
            ty if ty == &Ty::none() => return Some(Err(())),
            Ty::Tuple(tys) => match attr {
                TypingAttr::BinOp(TypingBinOp::In) => {
                    Ty::function(vec![Param::pos_only(Ty::unions(tys.clone()))], Ty::bool())
                }
                TypingAttr::Iter => Ty::unions(tys.clone()),
                TypingAttr::Index => {
                    Ty::function(vec![Param::pos_only(Ty::int())], Ty::unions(tys.clone()))
                }
                _ => return Some(Err(())),
            },
            Ty::Name(x) if x == "tuple" => match attr {
                TypingAttr::Iter => Ty::Any,
                TypingAttr::BinOp(TypingBinOp::In) => {
                    Ty::function(vec![Param::pos_only(Ty::Any)], Ty::bool())
                }
                TypingAttr::Index => Ty::function(vec![Param::pos_only(Ty::int())], Ty::Any),
                _ => return Some(Err(())),
            },
            Ty::Custom(c) => return c.0.attribute_dyn(attr),
            ty => return self.oracle.attribute(ty, attr),
        }))
    }

    fn as_function(&self, ty: &TyName) -> Option<Result<TyFunction, ()>> {
        self.oracle.as_function(ty)
    }

    fn subtype(&self, require: &TyName, got: &TyName) -> bool {
        self.oracle.subtype(require, got)
    }
}

impl<'a> TypingOracleCtx<'a> {
    pub(crate) fn mk_error(&self, span: Span, err: impl Into<anyhow::Error>) -> TypingError {
        TypingError::new(err.into(), span, self.codemap)
    }

    pub(crate) fn msg_error(&self, span: Span, msg: impl Display) -> TypingError {
        TypingError::msg(msg, span, self.codemap)
    }

    pub(crate) fn validate_type(
        &self,
        got: &Ty,
        require: &Ty,
        span: Span,
    ) -> Result<(), TypingError> {
        if !got.intersects(require, *self) {
            Err(self.mk_error(
                span,
                TypingOracleCtxError::IncompatibleType {
                    got: got.to_string(),
                    require: require.to_string(),
                },
            ))
        } else {
            Ok(())
        }
    }

    fn validate_args(
        &self,
        params: &[Param],
        args: &[Spanned<Arg>],
        span: Span,
    ) -> Result<(), TypingError> {
        // Want to figure out which arguments go in which positions
        let mut param_args: Vec<Vec<Spanned<&Ty>>> = vec![vec![]; params.len()];
        // The next index a positional parameter might fill
        let mut param_pos = 0;
        let mut seen_vargs = false;

        for arg in args {
            match &arg.node {
                Arg::Pos(ty) => loop {
                    match params.get(param_pos) {
                        None => {
                            return Err(self.mk_error(
                                arg.span,
                                TypingOracleCtxError::TooManyPositionalArguments,
                            ));
                        }
                        Some(param) => {
                            let found_index = param_pos;
                            if param.mode != ParamMode::Args {
                                param_pos += 1;
                            }
                            if param.allows_pos() {
                                param_args[found_index].push(Spanned {
                                    span: arg.span,
                                    node: ty,
                                });
                                break;
                            }
                        }
                    }
                },
                Arg::Name(name, ty) => {
                    let mut success = false;
                    for (i, param) in params.iter().enumerate() {
                        if param.name() == name || param.mode == ParamMode::Kwargs {
                            param_args[i].push(Spanned {
                                span: arg.span,
                                node: ty,
                            });
                            success = true;
                            break;
                        }
                    }
                    if !success {
                        return Err(self.mk_error(
                            arg.span,
                            TypingOracleCtxError::UnexpectedNamedArgument { name: name.clone() },
                        ));
                    }
                }
                Arg::Args(_) => {
                    param_pos = params.len();
                    seen_vargs = true;
                }
                Arg::Kwargs(_) => {
                    seen_vargs = true;
                }
            }
        }

        for (param, args) in std::iter::zip(params, param_args) {
            if !param.allows_many() && args.len() > 1 {
                panic!("bad")
            }
            if args.is_empty() {
                // We assume that *args/**kwargs might have splatted things everywhere.
                if !param.optional && !seen_vargs {
                    return Err(self.mk_error(
                        span,
                        TypingOracleCtxError::MissingRequiredParameter {
                            name: param.name().to_owned(),
                        },
                    ));
                }
                continue;
            }
            match param.mode {
                ParamMode::PosOnly | ParamMode::PosOrName(_) | ParamMode::NameOnly(_) => {
                    self.validate_type(args[0].node, &param.ty, args[0].span)?;
                }
                ParamMode::Args => {
                    for ty in args {
                        // For an arg, we require the type annotation to be inner value,
                        // rather than the outer (which is always a tuple)
                        self.validate_type(ty.node, &param.ty, ty.span)?;
                    }
                }
                ParamMode::Kwargs => {
                    let val_types: Vec<_> = param
                        .ty
                        .iter_union()
                        .iter()
                        .filter_map(|x| match x {
                            Ty::Dict(k_v) => Some(k_v.1.clone()),
                            _ => None,
                        })
                        .collect();
                    if !val_types.is_empty() {
                        let require = Ty::unions(val_types);
                        for ty in args {
                            self.validate_type(ty.node, &require, ty.span)?;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub(crate) fn validate_fn_call(
        &self,
        span: Span,
        fun: &TyFunction,
        args: &[Spanned<Arg>],
    ) -> Result<Ty, TypingError> {
        self.validate_args(&fun.params, args, span)?;
        Ok((*fun.result).clone())
    }

    #[allow(clippy::collapsible_else_if)]
    pub(crate) fn validate_call(
        &self,
        span: Span,
        fun: &Ty,
        args: &[Spanned<Arg>],
    ) -> Result<Ty, TypingError> {
        match fun {
            Ty::Never => Ok(Ty::Never),
            Ty::Any => Ok(Ty::Any),
            Ty::Name(n) => match self.oracle.as_function(n) {
                None => {
                    // Unknown type, may be callable.
                    Ok(Ty::Any)
                }
                Some(Ok(f)) => self.validate_fn_call(span, &f, args),
                Some(Err(())) => Err(self.mk_error(
                    span,
                    TypingOracleCtxError::CallToNonCallable {
                        ty: fun.to_string(),
                    },
                )),
            },
            Ty::List(_) | Ty::Dict(_) | Ty::Tuple(_) => Err(self.mk_error(
                span,
                TypingOracleCtxError::CallToNonCallable {
                    ty: fun.to_string(),
                },
            )),
            Ty::Iter(_) => {
                // Unknown type, may be callable.
                Ok(Ty::Any)
            }
            Ty::Custom(t) => t.0.validate_call_dyn(span, args, *self),
            Ty::Union(variants) => {
                let mut successful = Vec::new();
                let mut errors = Vec::new();
                for variant in variants.alternatives() {
                    match self.validate_call(span, variant, args) {
                        Ok(ty) => successful.push(ty),
                        Err(e) => errors.push(e),
                    }
                }
                if !successful.is_empty() {
                    Ok(Ty::unions(successful))
                } else {
                    if errors.len() == 1 {
                        Err(errors.pop().unwrap())
                    } else {
                        Err(self.mk_error(span, TypingOracleCtxError::CallArgumentsIncompatible))
                    }
                }
            }
        }
    }
}
