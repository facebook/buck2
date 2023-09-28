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
use starlark::typing::Ty;
use starlark::values::starlark_value;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueError;
use starlark::StarlarkDocs;

use crate::bxl::starlark_defs::context::BxlContextNoDice;

/// FileSetExpr is just a simple type that can be used in starlark_module
/// functions for arguments that should be file sets. It will accept either a
/// literal (like `//path/to/some/file.txt`) or a FileSet Value (from one of the
/// bxl functions that return them).
pub(crate) enum FileSetExpr<'a> {
    Literal(&'a str),
    Literals(Vec<&'a str>),
    FileSet(&'a StarlarkFileSet),
}

impl<'a> FileSetExpr<'a> {
    pub(crate) async fn get(self, bxl: &BxlContextNoDice<'_>) -> anyhow::Result<Cow<'a, FileSet>> {
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
            FileSetExpr::FileSet(val) => Cow::Borrowed(&val.0),
        };
        Ok(set)
    }
}

impl<'v> StarlarkTypeRepr for FileSetExpr<'v> {
    fn starlark_type_repr() -> Ty {
        Either::<String, StarlarkFileSet>::starlark_type_repr()
    }
}

impl<'v> UnpackValue<'v> for FileSetExpr<'v> {
    fn expected() -> String {
        "literal or set of file names".to_owned()
    }

    #[allow(clippy::manual_map)]
    fn unpack_value(value: Value<'v>) -> Option<Self> {
        if let Some(s) = value.unpack_str() {
            Some(FileSetExpr::Literal(s))
        } else if let Some(s) = <Vec<&'v str>>::unpack_value(value) {
            Some(FileSetExpr::Literals(s))
        } else if let Some(s) = UnpackValue::unpack_value(value) {
            Some(FileSetExpr::FileSet(s))
        } else {
            None
        }
    }
}

#[derive(Debug, Display, ProvidesStaticType, Allocative, StarlarkDocs)]
#[derive(NoSerialize)] // TODO maybe this should be
#[starlark_docs(directory = "bxl")]
pub(crate) struct StarlarkFileSet(
    /// Set of files or directories.
    pub(crate) FileSet,
);

starlark_simple_value!(StarlarkFileSet);

#[starlark_value(type = "file_set")]
impl<'v> StarlarkValue<'v> for StarlarkFileSet {
    fn iterate_collect(&self, heap: &'v Heap) -> anyhow::Result<Vec<Value<'v>>> {
        Ok(self
            .0
            .iter()
            .map(|cell_path| heap.alloc(StarlarkFileNode(cell_path.clone())))
            .collect())
    }

    fn at(&self, index: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let i = i32::unpack_value_err(index)?;
        if let Ok(i) = usize::try_from(i) {
            if let Some(cell_path) = self.0.get_index(i) {
                return Ok(heap.alloc(StarlarkFileNode(cell_path.clone())));
            }
        }
        Err(anyhow::anyhow!(ValueError::IndexOutOfBound(i)))
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
pub(crate) struct StarlarkFileNode(
    /// Cell path to the file or directory.
    pub(crate) CellPath,
);

starlark_simple_value!(StarlarkFileNode);

#[starlark_value(type = "file_node")]
impl<'v> StarlarkValue<'v> for StarlarkFileNode {}

#[derive(Debug, ProvidesStaticType, Clone, Allocative, StarlarkDocs)]
#[derive(NoSerialize)]
#[starlark_docs(directory = "bxl")]
pub(crate) struct StarlarkReadDirSet {
    /// Cell path to the directory/files.
    pub(crate) cell_path: CellPath,
    /// Files that are not ignored within the buckconfig.
    /// Sorted.
    pub(crate) included: Arc<[SimpleDirEntry]>,
    /// Only return directories when iterating or printing.
    pub(crate) dirs_only: bool,
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

#[starlark_value(type = "read_dir_set")]
impl<'v> StarlarkValue<'v> for StarlarkReadDirSet {
    fn iterate_collect(&self, heap: &'v Heap) -> anyhow::Result<Vec<Value<'v>>> {
        Ok(self
            .children()?
            .into_map(|cell_path| heap.alloc(StarlarkFileNode(cell_path))))
    }
}
