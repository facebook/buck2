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

use crate::environment::Globals;
use crate::stdlib::LibraryExtension;
use crate::typing::oracle::docs::OracleDocs;
use crate::typing::oracle::traits::TypingOracle;
use crate::typing::ty::Ty;
use crate::typing::ty::TyName;

/// A [`TypingOracle`] based on information from documentation.
pub struct OracleStandard {
    /// The things we don't have special code for, but just use whatever docs tells us
    fallback: OracleDocs,
}

impl OracleStandard {
    /// Create a new [`OracleDocs`], following the Starlark standard, but with the given extension you intend to enable.
    pub fn new(extensions: &[LibraryExtension]) -> Self {
        let mut fallback = OracleDocs::new();
        fallback.add_module(&Globals::extended_by(extensions).documentation());

        Self { fallback }
    }
}

impl TypingOracle for OracleStandard {
    fn attribute(&self, ty: &TyName, attr: &str) -> Option<Result<Ty, ()>> {
        let res = self.fallback.attribute(ty, attr);
        if res.is_none() && self.fallback.known_object(ty.as_str()) {
            return Some(Err(()));
        } else {
            return res;
        }
    }

    fn subtype(&self, _require: &TyName, _got: &TyName) -> bool {
        false
    }
}
