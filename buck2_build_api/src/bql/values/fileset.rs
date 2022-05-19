/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ops::Deref;

use anyhow::anyhow;
use buck2_query::query::syntax::simple::eval::file_set::FileSet;
use derive_more::Display;
use gazebo::{any::AnyLifetime, cell::ARef};
use starlark::{
    eval::Evaluator,
    starlark_type,
    values::{NoSerialize, StarlarkValue, UnpackValue, Value},
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum BqlFilesetError {
    #[error("fileset literals aren't currently supported in buck2 bql")]
    FileSetLiteral,
}

/// FileSetExpr is just a simple type that can be used in starlark_module
/// functions for arguments that should be file sets. It will accept either a
/// literal (like `//path/to/some/file.txt`) or a FileSet Value (from one of the
/// bql functions that return them).
pub enum FileSetExpr<'a> {
    Literal(&'a str),
    FileSet(ARef<'a, FileSet>),
}

impl<'a> FileSetExpr<'a> {
    pub fn get(self, _eval: &Evaluator) -> anyhow::Result<ARef<'a, FileSet>> {
        match self {
            FileSetExpr::Literal(_val) => Err(anyhow!(BqlFilesetError::FileSetLiteral)),
            FileSetExpr::FileSet(val) => Ok(val),
        }
    }
}

impl<'v> UnpackValue<'v> for FileSetExpr<'v> {
    fn expected() -> String {
        "unimplemented".to_owned()
    }

    fn unpack_value(_value: Value<'v>) -> Option<Self> {
        unimplemented!()
    }
}

#[derive(Debug, Display, AnyLifetime)]
#[derive(NoSerialize)] // TODO maybe this should be
pub struct StarlarkFileSet(pub FileSet);

starlark_simple_value!(StarlarkFileSet);

impl StarlarkValue<'_> for StarlarkFileSet {
    starlark_type!("file_set");
}

impl From<FileSet> for StarlarkFileSet {
    fn from(v: FileSet) -> Self {
        Self(v)
    }
}

impl Deref for StarlarkFileSet {
    type Target = FileSet;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
