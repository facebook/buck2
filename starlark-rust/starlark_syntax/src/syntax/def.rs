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

use std::collections::HashSet;

use crate::codemap::CodeMap;
use crate::codemap::Spanned;
use crate::eval_exception::EvalException;
use crate::syntax::ast::AstAssignIdentP;
use crate::syntax::ast::AstExprP;
use crate::syntax::ast::AstParameterP;
use crate::syntax::ast::AstPayload;
use crate::syntax::ast::AstTypeExprP;
use crate::syntax::ast::ParameterP;

pub enum DefParamKind<'a, P: AstPayload> {
    Regular(
        /// Default value.
        Option<&'a AstExprP<P>>,
    ),
    Args,
    Kwargs,
}

pub struct DefParam<'a, P: AstPayload> {
    pub ident: &'a AstAssignIdentP<P>,
    pub kind: DefParamKind<'a, P>,
    pub ty: Option<&'a AstTypeExprP<P>>,
}

/// Post-processed AST for function parameters.
///
/// * Validated
/// * `*` parameter replaced with `num_positional` field
pub struct DefParams<'a, P: AstPayload> {
    pub params: Vec<Spanned<DefParam<'a, P>>>,
    /// Number of parameters which can be filled positionally.
    /// That is, number of parameters before first `*`, `*args` or `**kwargs`.
    pub num_positional: u32,
}

fn check_param_name<'a, P: AstPayload, T>(
    argset: &mut HashSet<&'a str>,
    n: &'a AstAssignIdentP<P>,
    arg: &Spanned<T>,
    codemap: &CodeMap,
) -> Result<(), EvalException> {
    if !argset.insert(n.node.ident.as_str()) {
        return Err(EvalException::parser_error(
            "duplicated parameter name",
            arg.span,
            codemap,
        ));
    }
    Ok(())
}

impl<'a, P: AstPayload> DefParams<'a, P> {
    pub fn unpack(
        ast_params: &'a [AstParameterP<P>],
        codemap: &CodeMap,
    ) -> Result<DefParams<'a, P>, EvalException> {
        // you can't repeat argument names
        let mut argset = HashSet::new();
        // You can't have more than one *args/*, **kwargs
        // **kwargs must be last
        // You can't have a required `x` after an optional `y=1`
        let mut seen_args = false;
        let mut seen_kwargs = false;
        let mut seen_optional = false;

        let mut params = Vec::with_capacity(ast_params.len());
        let mut num_positional = None;

        for (i, param) in ast_params.iter().enumerate() {
            let span = param.span;
            match &param.node {
                ParameterP::Normal(n, ty) => {
                    if seen_kwargs || (seen_optional && !seen_args) {
                        return Err(EvalException::parser_error(
                            "positional parameter after non positional",
                            param.span,
                            codemap,
                        ));
                    }
                    check_param_name(&mut argset, n, param, codemap)?;
                    params.push(Spanned {
                        span,
                        node: DefParam {
                            ident: n,
                            kind: DefParamKind::Regular(None),
                            ty: ty.as_deref(),
                        },
                    });
                }
                ParameterP::WithDefaultValue(n, ty, default_value) => {
                    if seen_kwargs {
                        return Err(EvalException::parser_error(
                            "Default parameter after args array or kwargs dictionary",
                            param.span,
                            codemap,
                        ));
                    }
                    seen_optional = true;
                    check_param_name(&mut argset, n, param, codemap)?;
                    params.push(Spanned {
                        span,
                        node: DefParam {
                            ident: n,
                            kind: DefParamKind::Regular(Some(default_value)),
                            ty: ty.as_deref(),
                        },
                    });
                }
                ParameterP::NoArgs => {
                    if seen_args || seen_kwargs {
                        return Err(EvalException::parser_error(
                            "Args parameter after another args or kwargs parameter",
                            param.span,
                            codemap,
                        ));
                    }
                    seen_args = true;
                }
                ParameterP::Args(n, ty) => {
                    if seen_args || seen_kwargs {
                        return Err(EvalException::parser_error(
                            "Args parameter after another args or kwargs parameter",
                            param.span,
                            codemap,
                        ));
                    }
                    seen_args = true;
                    check_param_name(&mut argset, n, param, codemap)?;
                    params.push(Spanned {
                        span,
                        node: DefParam {
                            ident: n,
                            kind: DefParamKind::Args,
                            ty: ty.as_deref(),
                        },
                    });
                }
                ParameterP::KwArgs(n, ty) => {
                    if seen_kwargs {
                        return Err(EvalException::parser_error(
                            "Multiple kwargs dictionary in parameters",
                            param.span,
                            codemap,
                        ));
                    }
                    seen_kwargs = true;
                    check_param_name(&mut argset, n, param, codemap)?;
                    params.push(Spanned {
                        span,
                        node: DefParam {
                            ident: n,
                            kind: DefParamKind::Kwargs,
                            ty: ty.as_deref(),
                        },
                    });
                }
            }

            if matches!(
                param.node,
                ParameterP::Args(..) | ParameterP::KwArgs(..) | ParameterP::NoArgs
            ) && num_positional.is_none()
            {
                num_positional = Some(i);
            }
        }
        Ok(DefParams {
            num_positional: u32::try_from(num_positional.unwrap_or(params.len())).unwrap(),
            params,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::ast::AssignIdentP;
    use crate::syntax::ast::AstNoPayload;
    use crate::syntax::ast::ExprP;

    fn fails(params: &[AstParameterP<AstNoPayload>], expected_error: &str) {
        let codemap = CodeMap::default();
        let x = DefParams::unpack(params, &codemap);
        match x {
            Ok(_) => panic!("Expected error from {params:?}"),
            Err(e) => assert!(
                e.to_string().contains(expected_error),
                "Unexpected error:\n{e}\n\nExpected: {expected_error}"
            ),
        }
    }

    fn passes(params: &[AstParameterP<AstNoPayload>]) {
        let codemap = CodeMap::default();
        assert!(DefParams::unpack(params, &codemap).is_ok());
    }

    fn spanned<T>(node: T) -> Spanned<T> {
        Spanned {
            span: Default::default(),
            node,
        }
    }

    fn ident(i: usize) -> AstAssignIdentP<AstNoPayload> {
        spanned(AssignIdentP {
            ident: format!("x{i}"),
            payload: Default::default(),
        })
    }

    fn param(i: usize) -> AstParameterP<AstNoPayload> {
        spanned(ParameterP::Normal(ident(i), None))
    }

    fn args(i: usize) -> AstParameterP<AstNoPayload> {
        spanned(ParameterP::Args(ident(i), None))
    }

    fn kwargs(i: usize) -> AstParameterP<AstNoPayload> {
        spanned(ParameterP::KwArgs(ident(i), None))
    }

    fn noargs() -> AstParameterP<AstNoPayload> {
        spanned(ParameterP::NoArgs)
    }

    fn default(i: usize) -> AstParameterP<AstNoPayload> {
        let default = spanned(ExprP::Tuple(Vec::new()));
        spanned(ParameterP::WithDefaultValue(
            ident(i),
            None,
            Box::new(default),
        ))
    }

    #[test]
    fn test_params_unpack() {
        fails(&[param(0), param(1), param(0)], "duplicated parameter name");
        fails(
            &[default(0), param(1)],
            "positional parameter after non positional",
        );
        fails(
            &[kwargs(0), default(1)],
            "Default parameter after args array or kwargs dictionary",
        );
        fails(
            &[args(0), args(1)],
            "Args parameter after another args or kwargs parameter",
        );
        fails(
            &[kwargs(0), args(1)],
            "Args parameter after another args or kwargs parameter",
        );
        fails(
            &[kwargs(0), kwargs(1)],
            "Multiple kwargs dictionary in parameters",
        );

        passes(&[param(0), param(1), default(2), args(3), kwargs(4)]);
    }

    #[test]
    fn test_params_noargs() {
        fails(
            &[noargs(), noargs()],
            "Args parameter after another args or kwargs parameter",
        );
        fails(
            &[param(0), default(1), param(2)],
            "positional parameter after non positional",
        );
        passes(&[args(0), param(1)]);
        passes(&[args(0), default(1)]);
        passes(&[args(0), param(1), default(2)]);
        passes(&[default(0), args(1), param(2)]);
        passes(&[args(0), param(1), default(2), param(3)]);
        passes(&[noargs(), param(0), default(1), param(2)]);
    }
}
