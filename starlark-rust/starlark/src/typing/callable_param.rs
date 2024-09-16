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

use allocative::Allocative;
use dupe::Dupe;

use crate::typing::small_arc_vec_or_static::SmallArcVec1OrStatic;
use crate::typing::Ty;
use crate::values::layout::heap::profile::arc_str::ArcStr;

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
    /// The type of the parameter
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

    pub(crate) fn is_optional_or_stars(&self) -> bool {
        match &self.mode {
            ParamMode::PosOnly(req)
            | ParamMode::PosOrName(_, req)
            | ParamMode::NameOnly(_, req) => match req {
                ParamIsRequired::Yes => false,
                ParamIsRequired::No => true,
            },
            ParamMode::Args | ParamMode::Kwargs => true,
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

    pub(crate) fn allows_many(&self) -> bool {
        match self.mode {
            ParamMode::Args | ParamMode::Kwargs => true,
            _ => false,
        }
    }

    /// Get a display name for this parameter.
    pub fn name(&self) -> &str {
        match &self.mode {
            ParamMode::PosOnly(_) => "_",
            ParamMode::PosOrName(x, _) => x,
            ParamMode::NameOnly(x, _) => x,
            ParamMode::Args => "*args",
            ParamMode::Kwargs => "**kwargs",
        }
    }
}

/// Callable parameter specification (e.g. positional only followed by `**kwargs`).
#[derive(Debug, Eq, PartialEq, Clone, Dupe, Hash, PartialOrd, Ord, Allocative)]
pub struct ParamSpec {
    params: SmallArcVec1OrStatic<Param>,
}

impl Display for ParamSpec {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (i, param) in self.params.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            fn optional(req: &ParamIsRequired) -> &'static str {
                match req {
                    ParamIsRequired::Yes => "",
                    ParamIsRequired::No => "=..",
                }
            }
            match &param.mode {
                ParamMode::PosOnly(opt) => write!(f, "#: {}{}", param.ty, optional(opt))?,
                ParamMode::PosOrName(name, opt) => {
                    write!(f, "#{}: {}{}", name, param.ty, optional(opt))?
                }
                ParamMode::NameOnly(name, opt) => {
                    write!(f, "{}: {}{}", name, param.ty, optional(opt))?
                }
                ParamMode::Args => write!(f, "*args: {}", param.ty)?,
                ParamMode::Kwargs => write!(f, "**kwargs: {}", param.ty)?,
            }
        }
        Ok(())
    }
}

impl ParamSpec {
    pub(crate) fn params(&self) -> &[Param] {
        &self.params
    }

    /// Constructor.
    pub fn new(params: Vec<Param>) -> ParamSpec {
        if params.as_slice() == Self::any().params() {
            ParamSpec::any()
        } else {
            ParamSpec {
                params: SmallArcVec1OrStatic::clone_from_slice(&params),
            }
        }
    }

    /// `*args`, `**kwargs` parameters.
    fn any_params() -> &'static [Param] {
        static ANY_PARAMS: [Param; 2] = [Param::args(Ty::any()), Param::kwargs(Ty::any())];
        &ANY_PARAMS
    }

    /// `*args`.
    pub(crate) fn args(ty: Ty) -> ParamSpec {
        ParamSpec::new(vec![Param::args(ty)])
    }

    /// `**kwargs`.
    pub fn kwargs(ty: Ty) -> ParamSpec {
        ParamSpec::new(vec![Param::kwargs(ty)])
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
    }

    /// No parameters.
    pub fn empty() -> ParamSpec {
        ParamSpec::pos_only([], [])
    }

    pub(crate) fn any() -> ParamSpec {
        ParamSpec {
            params: SmallArcVec1OrStatic::new_static(Self::any_params()),
        }
    }

    /// Is `*args, **kwargs`.
    pub(crate) fn is_any(&self) -> bool {
        self == &Self::any()
    }

    /// All parameters are required and positional only.
    pub(crate) fn all_required_pos_only(&self) -> Option<Vec<Ty>> {
        self.params
            .iter()
            .map(|p| match &p.mode {
                ParamMode::PosOnly(ParamIsRequired::Yes) => Some(p.ty.clone()),
                _ => None,
            })
            .collect()
    }
}
