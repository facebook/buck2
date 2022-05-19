/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{borrow::Cow, ops::Deref};

use buck2_query::query::{environment::QueryEnvironment, syntax::simple::eval::file_set::FileSet};
use derive_more::Display;
use gazebo::any::AnyLifetime;
use starlark::{
    starlark_type,
    values::{NoSerialize, StarlarkValue, UnpackValue, Value, ValueLike},
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
    FileSet(&'a FileSet),
}

impl<'a> FileSetExpr<'a> {
    pub async fn get<QueryEnv: QueryEnvironment>(
        self,
        env: &QueryEnv,
    ) -> anyhow::Result<Cow<'a, FileSet>> {
        let set = match self {
            FileSetExpr::Literal(val) => Cow::Owned(env.eval_file_literal(val).await?),
            FileSetExpr::FileSet(val) => Cow::Borrowed(val),
        };
        Ok(set)
    }

    // This will unpack a Value to a FileSetExpr, but doesn't accept as single string literal,
    // only a FileSetExpr or a list of string literals.
    fn unpack_set(value: Value<'a>) -> Option<Self> {
        value
            .downcast_ref::<StarlarkFileSet>()
            .map(|s| FileSetExpr::FileSet(s))
    }
}

impl<'v> UnpackValue<'v> for FileSetExpr<'v> {
    fn expected() -> String {
        "literal or set of file names".to_owned()
    }

    fn unpack_value(value: Value<'v>) -> Option<Self> {
        match value.unpack_str() {
            Some(s) => Some(FileSetExpr::Literal(s)),
            None => FileSetExpr::unpack_set(value),
        }
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
