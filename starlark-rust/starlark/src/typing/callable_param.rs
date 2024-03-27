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

/// The type of a parameter - can be positional, by name, `*args` or `**kwargs`.
#[derive(Debug, Clone, Dupe, PartialEq, Eq, Hash, PartialOrd, Ord, Allocative)]
pub(crate) enum ParamMode {
    /// Parameter can only be passed by position.
    PosOnly,
    /// Parameter can be passed by position or name.
    PosOrName(ArcStr),
    /// Parameter can only be passed by name.
    NameOnly(ArcStr),
    /// Parameter is `*args`.
    Args,
    /// Parameter is `**kwargs`.
    Kwargs,
}

/// A parameter argument to a function
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Allocative)]
pub struct Param {
    /// The type of parameter
    pub(crate) mode: ParamMode,
    /// Whether the parameter have a default value or is otherwise optional
    pub(crate) optional: bool,
    /// The type of the parameter
    pub(crate) ty: Ty,
}

impl Param {
    /// Create a positional only parameter.
    pub fn pos_only(ty: Ty) -> Self {
        Self {
            mode: ParamMode::PosOnly,
            optional: false,
            ty,
        }
    }

    /// Create a named only parameter.
    pub fn name_only(name: &str, ty: Ty) -> Self {
        Self {
            mode: ParamMode::NameOnly(ArcStr::from(name)),
            optional: false,
            ty,
        }
    }

    /// Create a positional or named parameter.
    pub fn pos_or_name(name: &str, ty: Ty) -> Self {
        Self {
            mode: ParamMode::PosOrName(ArcStr::from(name)),
            optional: false,
            ty,
        }
    }

    /// Make a parameter optional.
    pub fn optional(self) -> Self {
        Self {
            optional: true,
            ..self
        }
    }

    /// Create a `*args` parameter.
    ///
    /// `ty` is a tuple item type.
    pub const fn args(ty: Ty) -> Self {
        Self {
            mode: ParamMode::Args,
            optional: true,
            ty,
        }
    }

    /// Create a `**kwargs` parameter.
    ///
    /// `ty` is a dict value type.
    pub const fn kwargs(ty: Ty) -> Self {
        Self {
            mode: ParamMode::Kwargs,
            optional: true,
            ty,
        }
    }

    pub(crate) fn allows_pos(&self) -> bool {
        match self.mode {
            ParamMode::PosOnly | ParamMode::PosOrName(_) | ParamMode::Args => true,
            ParamMode::NameOnly(_) | ParamMode::Kwargs => false,
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
            ParamMode::PosOnly => "_",
            ParamMode::PosOrName(x) => x,
            ParamMode::NameOnly(x) => x,
            ParamMode::Args => "*args",
            ParamMode::Kwargs => "**kwargs",
        }
    }
}

/// Callable parameter specification (e.g. positional only followed by `**kwargs`).
#[derive(Debug, Eq, PartialEq, Clone, Hash, PartialOrd, Ord, Allocative)]
pub struct ParamSpec {
    params: SmallArcVec1OrStatic<Param>,
}

impl Display for ParamSpec {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (i, param) in self.params.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            let opt = if param.optional { "=.." } else { "" };
            match &param.mode {
                ParamMode::PosOnly => write!(f, "#: {}{}", param.ty, opt)?,
                ParamMode::PosOrName(name) => write!(f, "#{}: {}{}", name, param.ty, opt)?,
                ParamMode::NameOnly(name) => write!(f, "{}: {}{}", name, param.ty, opt)?,
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

    pub(crate) fn new(params: Vec<Param>) -> ParamSpec {
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

    fn any() -> ParamSpec {
        ParamSpec {
            params: SmallArcVec1OrStatic::new_static(Self::any_params()),
        }
    }
}
