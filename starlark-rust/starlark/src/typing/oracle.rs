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

// This makes for a better API.
#![allow(clippy::result_unit_err)]

use crate::typing::ty::Arg;
use crate::typing::ty::Ty;
use crate::typing::ty::TyName;

/// Callbacks which provide types when typechecking a module.
pub trait TypingOracle {
    /// Given a type and a `.` attribute, what is its type.
    /// Return [`Err`] to indicate we _know_ this isn't a valid attribute.
    fn attribute(&self, ty: &Ty, attr: &str) -> Result<Ty, ()>;

    /// Given a symbol in the global environment, what is its type.
    /// Return [`Err`] if we _know_ this isn't a valid builtin.
    fn builtin(&self, name: &str) -> Result<Ty, ()>;

    /// A call to a builtin function. Only used for functions where the return type
    /// is dependent on the arguments in a way that can't be expressed by [`TypingOracle::builtin`].
    /// Return [`None`] to defer to [`TypingOracle::builtin`].
    /// Return [`Err`] with a message if these arguments are type-incorrect.
    fn builtin_call(&self, name: &str, args: &[Arg]) -> Option<Result<Ty, String>>;

    /// If we require the first type, but got the second type, is that OK?
    /// Usually its OK if the requirement is a subtype of the one we got.
    fn subtype(&self, require: &TyName, got: &TyName) -> bool;
}

/// Returns `Any` for everything.
pub struct OracleNone;

impl TypingOracle for OracleNone {
    fn attribute(&self, _ty: &Ty, _attr: &str) -> Result<Ty, ()> {
        Ok(Ty::Any)
    }

    fn builtin(&self, _name: &str) -> Result<Ty, ()> {
        Ok(Ty::Any)
    }

    fn builtin_call(&self, _name: &str, _args: &[Arg]) -> Option<Result<Ty, String>> {
        None
    }

    fn subtype(&self, _require: &TyName, _got: &TyName) -> bool {
        false
    }
}
