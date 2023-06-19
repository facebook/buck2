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

use allocative::Allocative;
use buck2_common::file_ops::SimpleDirEntry;
use buck2_core::cells::cell_path::CellPath;
use buck2_query::query::syntax::simple::eval::file_set::FileNode;
use buck2_query::query::syntax::simple::eval::file_set::FileSet;
use derive_more::Display;
use display_container::fmt_container;
use either::Either;
use gazebo::prelude::VecExt;
use indexmap::IndexSet;
use starlark::any::ProvidesStaticType;
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
use starlark::StarlarkDocs;

use crate::bxl::starlark_defs::context::BxlContext;

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
    pub async fn get(self, bxl: &BxlContext<'_>) -> anyhow::Result<Cow<'a, FileSet>> {
        let set = match self {
            FileSetExpr::Literal(val) => Cow::Owned(FileSet::from_iter([FileNode(
                bxl.parse_query_file_literal(val)?,
            )])),
            FileSetExpr::Literals(val) => {
                let mut file_set = FileSet::new(IndexSet::new());
                for arg in val.iter() {
                    file_set.insert(FileNode(bxl.parse_query_file_literal(arg)?));
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

#[derive(Debug, Display, ProvidesStaticType, Allocative, StarlarkDocs)]
#[derive(NoSerialize)] // TODO maybe this should be
#[starlark_docs(directory = "bxl")]
pub struct StarlarkFileSet(
    /// Set of files or directories.
    pub FileSet,
);

starlark_simple_value!(StarlarkFileSet);

impl<'v> StarlarkValue<'v> for StarlarkFileSet {
    starlark_type!("file_set");

    fn iterate_collect(&self, heap: &'v Heap) -> anyhow::Result<Vec<Value<'v>>> {
        Ok(self
            .0
            .iter()
            .map(|cell_path| heap.alloc(StarlarkFileNode(cell_path.clone())))
            .collect())
    }

    fn at(&self, index: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let i = index.unpack_i32().ok_or_else(|| {
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

    fn length(&self) -> anyhow::Result<i32> {
        match i32::try_from(self.0.len()) {
            Ok(l) => Ok(l),
            Err(e) => Err(e.into()),
        }
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

#[derive(Debug, Display, ProvidesStaticType, Clone, Allocative, StarlarkDocs)]
#[derive(NoSerialize)]
#[starlark_docs(directory = "bxl")]
pub struct StarlarkFileNode(
    /// Cell path to the file or directory.
    pub CellPath,
);

starlark_simple_value!(StarlarkFileNode);

impl StarlarkValue<'_> for StarlarkFileNode {
    starlark_type!("file_node");
}

#[derive(Debug, ProvidesStaticType, Clone, Allocative, StarlarkDocs)]
#[derive(NoSerialize)]
#[starlark_docs(directory = "bxl")]
pub struct StarlarkReadDirSet {
    /// Cell path to the directory/files.
    pub cell_path: CellPath,
    /// Files that are not ignored within the buckconfig.
    /// Sorted.
    pub included: Arc<[SimpleDirEntry]>,
    /// Only return directories when iterating or printing.
    pub dirs_only: bool,
}

starlark_simple_value!(StarlarkReadDirSet);

impl StarlarkReadDirSet {
    fn children(&self) -> anyhow::Result<Vec<CellPath>> {
        let mut result: Vec<CellPath> = Vec::with_capacity(self.included.len());
        result.extend(self.included.iter().filter_map(|e| {
            if !self.dirs_only || e.file_type.is_dir() {
                Some(self.cell_path.join(&e.file_name))
            } else {
                None
            }
        }));
        result.sort();
        Ok(result)
    }
}

impl fmt::Display for StarlarkReadDirSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.children() {
            Ok(children) => fmt_container(f, "[", "]", children),
            Err(e) => write!(f, "<Error: {}>", e),
        }
    }
}

impl<'v> StarlarkValue<'v> for StarlarkReadDirSet {
    starlark_type!("read_dir_set");

    fn iterate_collect(&self, heap: &'v Heap) -> anyhow::Result<Vec<Value<'v>>> {
        Ok(self
            .children()?
            .into_map(|cell_path| heap.alloc(StarlarkFileNode(cell_path))))
    }
}
