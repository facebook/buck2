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

use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;
use std::iter;

use allocative::Allocative;
use dupe::Dupe;
use starlark_map::small_set::SmallSet;
use starlark_syntax::internal_error;
use starlark_syntax::other_error;
use starlark_syntax::syntax::def::DefParamIndices;

use crate::eval::runtime::params::display::fmt_param_spec;
use crate::eval::runtime::params::display::ParamFmt;
use crate::eval::runtime::params::display::PARAM_FMT_OPTIONAL;
use crate::typing::small_arc_vec_or_static::SmallArcVec1OrStatic;
use crate::typing::Ty;
use crate::util::arc_str::ArcStr;

#[derive(
    Debug, Clone, Dupe, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Allocative
)]
pub(crate) enum ParamIsRequired {
    Yes,
    No,
}

/// The type of a parameter - can be positional, by name, `*args` or `**kwargs`.
#[derive(Debug, Clone, Dupe, PartialEq, Eq, Hash, PartialOrd, Ord, Allocative)]
pub(crate) enum ParamMode {
    /// Parameter can only be passed by position.
    PosOnly(ParamIsRequired),
    /// Parameter can be passed by position or name.
    PosOrName(ArcStr, ParamIsRequired),
    /// Parameter can only be passed by name.
    NameOnly(ArcStr, ParamIsRequired),
    /// Parameter is `*args`.
    Args,
    /// Parameter is `**kwargs`.
    Kwargs,
}

/// A parameter argument to a function
#[derive(Debug, Clone, Dupe, PartialEq, Eq, Hash, PartialOrd, Ord, Allocative)]
pub struct Param {
    /// The type of parameter
    pub(crate) mode: ParamMode,
    /// The type of the parameter.
    /// For `*args` it is the type of the tuple elements.
    /// For `**kwargs` it is the type of the dict values.
    pub(crate) ty: Ty,
}

impl Param {
    /// Create a positional only parameter.
    pub fn pos_only(ty: Ty) -> Self {
        Self {
            mode: ParamMode::PosOnly(ParamIsRequired::Yes),
            ty,
        }
    }

    /// Create a named only parameter.
    pub fn name_only(name: &str, ty: Ty) -> Self {
        Self {
            mode: ParamMode::NameOnly(ArcStr::from(name), ParamIsRequired::Yes),
            ty,
        }
    }

    /// Create a positional or named parameter.
    pub fn pos_or_name(name: &str, ty: Ty) -> Self {
        Self {
            mode: ParamMode::PosOrName(ArcStr::from(name), ParamIsRequired::Yes),
            ty,
        }
    }

    /// Make a parameter optional.
    pub fn optional(self) -> Self {
        Param {
            mode: match self.mode {
                ParamMode::PosOnly(_x) => ParamMode::PosOnly(ParamIsRequired::No),
                ParamMode::PosOrName(x, _y) => ParamMode::PosOrName(x, ParamIsRequired::No),
                ParamMode::NameOnly(x, _y) => ParamMode::NameOnly(x, ParamIsRequired::No),
                ParamMode::Args => ParamMode::Args,
                ParamMode::Kwargs => ParamMode::Kwargs,
            },
            ty: self.ty,
        }
    }

    /// Create a `*args` parameter.
    ///
    /// `ty` is a tuple item type.
    pub const fn args(ty: Ty) -> Self {
        Self {
            mode: ParamMode::Args,
            ty,
        }
    }

    /// Create a `**kwargs` parameter.
    ///
    /// `ty` is a dict value type.
    pub const fn kwargs(ty: Ty) -> Self {
        Self {
            mode: ParamMode::Kwargs,
            ty,
        }
    }

    pub(crate) fn allows_pos(&self) -> bool {
        match self.mode {
            ParamMode::PosOnly(_) | ParamMode::PosOrName(_, _) | ParamMode::Args => true,
            ParamMode::NameOnly(_, _) | ParamMode::Kwargs => false,
        }
    }

    pub(crate) fn name(&self) -> Option<&str> {
        match &self.mode {
            ParamMode::PosOnly(_) => None,
            ParamMode::PosOrName(x, _) => Some(x.as_str()),
            ParamMode::NameOnly(x, _) => Some(x.as_str()),
            ParamMode::Args => None,
            ParamMode::Kwargs => None,
        }
    }

    /// Get a display name for this parameter.
    pub(crate) fn name_display(&self) -> &str {
        match &self.mode {
            ParamMode::PosOnly(_) => "_",
            ParamMode::PosOrName(x, _) => x,
            ParamMode::NameOnly(x, _) => x,
            ParamMode::Args => "*args",
            ParamMode::Kwargs => "**kwargs",
        }
    }
}

struct ParamSpecSplit<'a> {
    pos_only: &'a [Param],
    pos_or_named: &'a [Param],
    args: Option<&'a Param>,
    named_only: &'a [Param],
    kwargs: Option<&'a Param>,
}

/// Callable parameter specification (e.g. positional only followed by `**kwargs`).
#[derive(Debug, Eq, PartialEq, Clone, Dupe, Hash, PartialOrd, Ord, Allocative)]
pub struct ParamSpec {
    params: SmallArcVec1OrStatic<Param>,
    indices: DefParamIndices,
}

impl Display for ParamSpec {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fn pf(p: &'_ Param) -> ParamFmt<'_, &'_ Ty, &'static str> {
            ParamFmt {
                name: match &p.mode {
                    ParamMode::PosOrName(name, _) | ParamMode::NameOnly(name, _) => name.as_str(),
                    ParamMode::PosOnly(_) => "_",
                    ParamMode::Args => "args",
                    ParamMode::Kwargs => "kwargs",
                },
                ty: Some(&p.ty),
                default: match p.mode {
                    ParamMode::PosOnly(ParamIsRequired::Yes)
                    | ParamMode::PosOrName(_, ParamIsRequired::Yes)
                    | ParamMode::NameOnly(_, ParamIsRequired::Yes) => None,
                    ParamMode::PosOnly(ParamIsRequired::No)
                    | ParamMode::PosOrName(_, ParamIsRequired::No)
                    | ParamMode::NameOnly(_, ParamIsRequired::No) => Some(PARAM_FMT_OPTIONAL),
                    ParamMode::Args | ParamMode::Kwargs => None,
                },
            }
        }

        let ParamSpecSplit {
            pos_only,
            pos_or_named,
            args,
            named_only,
            kwargs,
        } = self.split();

        fmt_param_spec(
            f,
            pos_only.iter().map(pf),
            pos_or_named.iter().map(pf),
            args.map(pf),
            named_only.iter().map(pf),
            kwargs.map(pf),
        )
    }
}

impl ParamSpec {
    pub(crate) fn params(&self) -> &[Param] {
        &self.params
    }

    /// Constructor.
    /// Return an error if the sequence of parameters is incorrect,
    /// for example, if positional-only parameters follow named-only.
    pub fn new(params: Vec<Param>) -> crate::Result<ParamSpec> {
        if params.as_slice() == Self::any().params() {
            Ok(ParamSpec::any())
        } else {
            #[derive(Ord, PartialOrd, Eq, PartialEq)]
            enum State {
                PosOnly,
                PosOrName,
                Args,
                NameOnly,
                Kwargs,
            }

            let mut names = SmallSet::new();

            let mut state = State::PosOnly;

            let mut seen_optional = false;

            let mut num_positional_only = 0;
            let mut num_positional = 0;
            let mut args = None;
            let mut kwargs = None;

            for (i, param) in params.iter().enumerate() {
                if let Some(name) = param.name() {
                    if !names.insert(name) {
                        return Err(other_error!("duplicate parameter name: `{}`", name));
                    }
                }

                match param.mode {
                    ParamMode::PosOnly(req) => {
                        if state > State::PosOnly {
                            return Err(other_error!(
                                "positional only parameters may only be in the beginning"
                            ));
                        }
                        if req == ParamIsRequired::Yes && seen_optional {
                            return Err(other_error!(
                                "required positional only parameter after optional"
                            ));
                        }
                        if req == ParamIsRequired::No {
                            seen_optional = true;
                        }
                        state = State::PosOnly;
                    }
                    ParamMode::PosOrName(_, req) => {
                        if state > State::PosOrName {
                            return Err(other_error!(
                                "positional or named parameters may only follow positional only"
                            ));
                        }
                        if req == ParamIsRequired::Yes && seen_optional {
                            return Err(other_error!(
                                "required positional or named parameter after optional"
                            ));
                        }
                        if req == ParamIsRequired::No {
                            seen_optional = true;
                        }
                        state = State::PosOrName;
                    }
                    ParamMode::Args => {
                        if state >= State::Args {
                            return Err(other_error!("*args must not come after *-args"));
                        }
                        if args.is_some() {
                            return Err(internal_error!("args must not be already set"));
                        }
                        args = Some(i.try_into().unwrap());
                        state = State::Args;
                    }
                    ParamMode::NameOnly(_, req) => {
                        if state > State::NameOnly {
                            return Err(other_error!("named only parameters may only follow star"));
                        }
                        if req == ParamIsRequired::No {
                            // This is no-op, but update for consistency.
                            seen_optional = true;
                        }
                        state = State::NameOnly;
                    }
                    ParamMode::Kwargs => {
                        if state >= State::Kwargs {
                            return Err(other_error!("**kwargs must be the last parameter"));
                        }
                        if kwargs.is_some() {
                            return Err(internal_error!("kwargs must not be already set"));
                        }
                        kwargs = Some(i.try_into().unwrap());
                        state = State::Kwargs;
                    }
                }

                if state <= State::PosOnly {
                    num_positional_only += 1;
                }
                if state <= State::PosOrName {
                    num_positional += 1;
                }
            }

            Ok(ParamSpec {
                params: SmallArcVec1OrStatic::clone_from_slice(&params),
                indices: DefParamIndices {
                    num_positional_only,
                    num_positional,
                    args,
                    kwargs,
                },
            })
        }
    }

    pub(crate) fn new_parts(
        pos_only: impl IntoIterator<Item = (ParamIsRequired, Ty)>,
        pos_or_name: impl IntoIterator<Item = (ArcStr, ParamIsRequired, Ty)>,
        args: Option<Ty>,
        named_only: impl IntoIterator<Item = (ArcStr, ParamIsRequired, Ty)>,
        kwargs: Option<Ty>,
    ) -> crate::Result<ParamSpec> {
        Self::new(
            iter::empty()
                .chain(pos_only.into_iter().map(|(req, ty)| Param {
                    mode: ParamMode::PosOnly(req),
                    ty,
                }))
                .chain(pos_or_name.into_iter().map(|(name, req, ty)| Param {
                    mode: ParamMode::PosOrName(name, req),
                    ty,
                }))
                .chain(args.map(|ty| Param {
                    mode: ParamMode::Args,
                    ty,
                }))
                .chain(named_only.into_iter().map(|(name, req, ty)| Param {
                    mode: ParamMode::NameOnly(name, req),
                    ty,
                }))
                .chain(kwargs.map(|ty| Param {
                    mode: ParamMode::Kwargs,
                    ty,
                }))
                .collect(),
        )
    }

    /// `*args`.
    pub(crate) fn args(ty: Ty) -> ParamSpec {
        ParamSpec::new(vec![Param::args(ty)]).unwrap()
    }

    /// `**kwargs`.
    pub fn kwargs(ty: Ty) -> ParamSpec {
        ParamSpec::new(vec![Param::kwargs(ty)]).unwrap()
    }

    /// `/, arg=, arg=, ..., arg, arg, ...`.
    pub(crate) fn pos_only(
        required: impl IntoIterator<Item = Ty>,
        optional: impl IntoIterator<Item = Ty>,
    ) -> ParamSpec {
        ParamSpec::new(
            required
                .into_iter()
                .map(Param::pos_only)
                .chain(
                    optional
                        .into_iter()
                        .map(|ty| Param::pos_only(ty).optional()),
                )
                .collect(),
        )
        .unwrap()
    }

    /// No parameters.
    pub fn empty() -> ParamSpec {
        ParamSpec::pos_only([], [])
    }

    pub(crate) fn any() -> ParamSpec {
        static ANY_PARAMS: [Param; 2] = [Param::args(Ty::any()), Param::kwargs(Ty::any())];
        ParamSpec {
            params: SmallArcVec1OrStatic::new_static(&ANY_PARAMS),
            indices: DefParamIndices {
                num_positional: 0,
                num_positional_only: 0,
                args: Some(0),
                kwargs: Some(1),
            },
        }
    }

    /// Is `*args, **kwargs`.
    pub(crate) fn is_any(&self) -> bool {
        self == &Self::any()
    }

    fn split(&self) -> ParamSpecSplit<'_> {
        ParamSpecSplit {
            pos_only: &self.params[self.indices.pos_only()],
            pos_or_named: &self.params[self.indices.pos_or_named()],
            args: self.indices.args.map(|a| &self.params[a as usize]),
            named_only: &self.params[self.indices.named_only(self.params.len())],
            kwargs: self.indices.kwargs.map(|a| &self.params[a as usize]),
        }
    }

    /// All parameters are required and positional only.
    pub(crate) fn all_required_pos_only(&self) -> Option<Vec<Ty>> {
        match self.split() {
            ParamSpecSplit {
                pos_only,
                pos_or_named: [],
                args: None,
                named_only: [],
                kwargs: None,
            } => pos_only
                .iter()
                .map(|p| match p.mode {
                    ParamMode::PosOnly(ParamIsRequired::Yes) => Some(p.ty.dupe()),
                    _ => None,
                })
                .collect(),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::fmt::Write;

    use starlark_syntax::dialect::Dialect;
    use starlark_syntax::golden_test_template::golden_test_template;
    use starlark_syntax::syntax::AstModule;

    use crate::environment::Globals;
    use crate::typing::AstModuleTypecheck;

    #[test]
    fn test_param_spec_display() {
        let functions = r#"
def simple(x, y, z): pass
def default_value(x, y=1, z=2): pass
def param_type(x: int, y: str, z: int, w: list): pass
def named_only_a(x, *, y): pass
def named_only_b(*, y): pass
def pos_only_a(x, /, y): pass
def pos_only_b(x, /, *, y): pass
def pos_only_c(x, /, *args): pass
def pos_only_d(x, /, *args, **kwargs): pass
"#;
        let mut out = String::new();
        let mut first = true;

        for test in functions.lines() {
            let test = test.trim();
            if test.is_empty() {
                continue;
            }

            let ast = AstModule::parse(
                "test_param_spec_display.star",
                test.to_owned(),
                &Dialect::AllOptionsInternal,
            )
            .unwrap();
            let (errors, typemap, _interface, approximations) =
                ast.typecheck(&Globals::standard(), &HashMap::new());
            if let Some(error) = errors.into_iter().next() {
                panic!("Error: {:?}", error);
            }
            assert!(approximations.is_empty());
            let def = typemap.find_first_binding().unwrap();

            if first {
                first = false;
            } else {
                writeln!(out).unwrap();
            }
            write!(out, "{test}\n{def}\n").unwrap();
        }

        golden_test_template(
            "src/typing/callable_param_test_param_spec_display.golden",
            &out,
        );
    }
}
