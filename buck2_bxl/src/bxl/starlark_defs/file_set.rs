/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::fmt;
use std::ops::Deref;
use std::sync::Arc;

use buck2_common::file_ops::SimpleDirEntry;
use buck2_core::cells::cell_path::CellPath;
use buck2_query::query::environment::QueryEnvironment;
use buck2_query::query::syntax::simple::eval::file_set::FileSet;
use derive_more::Display;
use either::Either;
use gazebo::any::ProvidesStaticType;
use gazebo::display::display_container;
use gazebo::prelude::*;
use indexmap::IndexSet;
use starlark::starlark_simple_value;
use starlark::starlark_type;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueError;
use starlark::values::ValueLike;

/// FileSetExpr is just a simple type that can be used in starlark_module
/// functions for arguments that should be file sets. It will accept either a
/// literal (like `//path/to/some/file.txt`) or a FileSet Value (from one of the
/// bxl functions that return them).
pub enum FileSetExpr<'a> {
    Literal(&'a str),
    Literals(Vec<&'a str>),
    FileSet(&'a FileSet),
}

impl<'a> FileSetExpr<'a> {
    pub async fn get<QueryEnv: QueryEnvironment>(
        self,
        env: &QueryEnv,
    ) -> anyhow::Result<Cow<'a, FileSet>> {
        let set = match self {
            FileSetExpr::Literal(val) => Cow::Owned(env.eval_file_literal(val).await?),
            FileSetExpr::Literals(val) => {
                let mut file_set = FileSet::new(IndexSet::new());
                for arg in val.iter() {
                    file_set.insert_all(&env.eval_file_literal(arg).await?);
                }
                Cow::Owned(file_set)
            }
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

impl<'v> StarlarkTypeRepr for FileSetExpr<'v> {
    fn starlark_type_repr() -> String {
        Either::<String, StarlarkFileSet>::starlark_type_repr()
    }
}

impl<'v> UnpackValue<'v> for FileSetExpr<'v> {
    fn expected() -> String {
        "literal or set of file names".to_owned()
    }

    fn unpack_value(value: Value<'v>) -> Option<Self> {
        if let Some(s) = value.unpack_str() {
            Some(FileSetExpr::Literal(s))
        } else if let Some(s) = <Vec<&'v str>>::unpack_value(value) {
            Some(FileSetExpr::Literals(s))
        } else {
            FileSetExpr::unpack_set(value)
        }
    }
}

#[derive(Debug, Display, ProvidesStaticType)]
#[derive(NoSerialize)] // TODO maybe this should be
pub struct StarlarkFileSet(pub FileSet);

starlark_simple_value!(StarlarkFileSet);

impl<'v> StarlarkValue<'v> for StarlarkFileSet {
    starlark_type!("file_set");

    fn iterate<'a>(
        &'a self,
        heap: &'v Heap,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Value<'v>> + 'a>>
    where
        'v: 'a,
    {
        Ok(box self
            .0
            .iter()
            .map(|cell_path| heap.alloc(StarlarkFileNode(cell_path.clone()))))
    }

    fn at(&self, index: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let i = index.unpack_int().ok_or_else(|| {
            ValueError::IncorrectParameterTypeWithExpected(
                "int".to_owned(),
                index.get_type().to_owned(),
            )
        })?;
        if i < 0 {
            return Err(anyhow::anyhow!(ValueError::IndexOutOfBound(i)));
        }
        self.0
            .get_index(i as usize)
            .ok_or_else(|| anyhow::anyhow!(ValueError::IndexOutOfBound(i)))
            .map(|cell_path| heap.alloc(StarlarkFileNode(cell_path.clone())))
    }
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

#[derive(Debug, Display, ProvidesStaticType, Clone)]
#[derive(NoSerialize)]
pub struct StarlarkFileNode(pub CellPath);

starlark_simple_value!(StarlarkFileNode);

impl StarlarkValue<'_> for StarlarkFileNode {
    starlark_type!("file_node");
}

#[derive(Debug, ProvidesStaticType, Clone)]
#[derive(NoSerialize)]
pub struct StarlarkReadDirSet {
    pub cell_path: CellPath,
    pub included: Arc<Vec<SimpleDirEntry>>,
    pub ignored: Option<Arc<Vec<SimpleDirEntry>>>,
}

starlark_simple_value!(StarlarkReadDirSet);

impl StarlarkReadDirSet {
    fn iter(&self) -> impl Iterator<Item = CellPath> + '_ {
        itertools::merge(
            self.included.iter().map(|e| &e.file_name),
            self.ignored
                .as_ref()
                .map(|i| i.map(|e| &e.file_name))
                .into_iter()
                .flatten(),
        )
        .map(|file_name| self.cell_path.join(&file_name))
    }
}

impl fmt::Display for StarlarkReadDirSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_container(f, "[", "]", self.iter())
    }
}

impl<'v> StarlarkValue<'v> for StarlarkReadDirSet {
    starlark_type!("read_dir_set");

    fn iterate<'a>(
        &'a self,
        heap: &'v Heap,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Value<'v>> + 'a>>
    where
        'v: 'a,
    {
        let iter = self
            .iter()
            .map(|cell_path| heap.alloc(StarlarkFileNode(cell_path)));

        Ok(box iter)
    }
}
