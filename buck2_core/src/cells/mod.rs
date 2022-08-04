/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//!
//! # Cell
//! A 'Cell' is sub-project within the main project for Buck. All files
//! reachable by Buck is belongs to a single Cell.
//! Cells can be sub-directories of other cells, but that makes that
//! sub-directory part of the sub-cell and no longer part of the parent cell.
//! For example, let's say there's cells 'parent-cell' and 'sub-cell' declared
//! in folders of the same names.
//! ```text
//!  parent-cell
//! +-- folder1
//! +-- folder2
//! +-- sub-cell
//! |   +-- folder3
//! ```
//! All files part of `folder1` and `folder2` will be part of 'parent-cell'.
//! Anything part of `sub-cell`, including `folder3`, are only part of the
//! 'sub-cell'.
//!
//! For users, each Cell is identified by 'CellAlias's. A 'CellAlias' is a
//! human-readable name that contains alphanumeric characters and underscores.
//! (i.e. shouldn't contain any special characters like `/`). Something like `1`
//! is a valid identifier, though not we do not suggest such naming as it's not
//! very descriptive.
//!
//! It's possible that in certain cell contexts, some Cells are not reachable by
//! any 'CellAlias'. However, in the global context, every Cell will be
//! reachable by at least one 'CellAlias'.
//!
//! ## Cell Alias
//! The cell alias appears within a fully qualified target with the syntax
//! `<cell alias>//<target label>`. For example, in `foo//some:target`, `foo` is
//! the cell alias. Examples like `foo/bar//some:target` has an invalid cell
//! alias of `foo/bar` since special characters are forbidden.
//!
//! The 'CellAlias' is specified via configuration files per cell. A
//! configuration specifies these with the syntax `<cell alias>=<relative path
//! to cell>`. We allow a many to one mapping from 'CellAlias' to Cell.
//!
//! Each Cell may give different aliases to the same cell. The 'CellAlias' will
//! be resolved based on the contextual cell that the alias appears in.
//! e.g. `fbcode//foo:bar` build file will have any aliases that appears within
//! it be resolved using the aliases defined in `fbcode` cell.
//!
//! Cells may omit declaring aliases for cells that exists globally. This means
//! that there will be no alias for those cells, and hence render those cells
//! inaccessible from the cell context that doesn't declare them.
//!
//! ### The Empty Cell Alias
//! The empty cell alias is a special alias injected by Buck to represent the
//! current contextual cell. That means, inside `fbcode` cell, references to the
//! 'CellAlias' `""` will resolve to the `fbcode` cell.
//!
//! ## Cell Name
//! Each Cell is uniquely identifier globally via a one to one mapping to a
//! 'CellName'. A 'CellName' is a canonicalized, human-readable name that
//! corresponds to a 'CellInstance'. The cell name is inferred from the global
//! list of 'CellAlias's available, by picking the first alias for each cell
//! path based on lexicogrpahic ordering of the aliases. The 'CellName' is
//! subject to the same character restrictions as 'CellAlias'.
//!
//! # Resolving Cells
//! Cells are represented by 'CellInstance'. The 'CellResolver' is able to
//! resolve 'CellNames' to 'CellInstance's. It is also able to find the
//! containing Cell given a path. 'CellAlias' can be resolved with an
//! 'CellAliasResolver'. Each 'CellInstance' contains a 'CellAliasResolver' for
//! the cell alias mapping for that particular cell.
//!
//! e.g.
//! ```
//! use buck2_core::fs::project::{ProjectRelativePath, ProjectRelativePathBuf};
//! use buck2_core::fs::paths::ForwardRelativePathBuf;
//! use buck2_core::cells::{CellResolver, CellName, CellAlias};
//! use std::convert::TryFrom;
//! use maplit::hashmap;
//! use buck2_core::cells::cell_root_path::CellRootPathBuf;
//! use buck2_core::cells::testing::CellResolverExt;
//!
//! let cell_config = ForwardRelativePathBuf::try_from(".buckconfig".to_owned())?;
//! let fbsource = ProjectRelativePath::new("")?;
//! let fbcode = ProjectRelativePath::new("fbcode")?;
//!
//! let cells = CellResolver::with_names_and_paths_with_alias(&[
//!     (CellName::unchecked_new("fbsource".to_owned()), CellRootPathBuf::new(fbsource.to_buf()), hashmap![
//!         CellAlias::new("fbsource".to_owned()) => CellName::unchecked_new("fbsource".to_owned()),
//!         CellAlias::new("".to_owned()) => CellName::unchecked_new("fbsource".to_owned()),
//!         CellAlias::new("fbcode".to_owned()) => CellName::unchecked_new("fbcode".to_owned()),
//!     ]),
//!     (CellName::unchecked_new("fbcode".to_owned()), CellRootPathBuf::new(fbcode.to_buf()), hashmap![
//!         CellAlias::new("fbcode".to_owned()) => CellName::unchecked_new("fbcode".to_owned()),
//!         CellAlias::new("".to_owned()) => CellName::unchecked_new("fbcode".to_owned()),
//!         CellAlias::new("fbsource".to_owned()) => CellName::unchecked_new("fbsource".to_owned()),
//!     ])
//! ]);
//!
//! let fbsource_cell_name = cells.find(ProjectRelativePath::new("something/in/fbsource")?)?;
//! assert_eq!(fbsource_cell_name, &CellName::unchecked_new("fbsource".into()));
//!
//! let fbcode_cell_name = cells.find(ProjectRelativePath::new("fbcode/something/in/fbcode")?)?;
//! assert_eq!(fbcode_cell_name, &CellName::unchecked_new("fbcode".into()));
//!
//! let fbsource_cell = cells.get(fbsource_cell_name)?;
//! assert_eq!(fbsource_cell.name(), &CellName::unchecked_new("fbsource".into()));
//! let fbcode_cell = cells.get(fbcode_cell_name)?;
//! assert_eq!(fbcode_cell.name(), &CellName::unchecked_new("fbcode".into()));
//!
//! let fbsource_aliases = fbsource_cell.cell_alias_resolver();
//! assert_eq!(fbsource_aliases.resolve("")?, &CellName::unchecked_new("fbsource".into()));
//! assert_eq!(fbsource_aliases.resolve("fbsource")?, &CellName::unchecked_new("fbsource".into()));
//! assert_eq!(fbsource_aliases.resolve("fbcode")?, &CellName::unchecked_new("fbcode".into()));
//!
//! let fbcode_aliases = fbcode_cell.cell_alias_resolver();
//! assert_eq!(fbcode_aliases.resolve("")?, &CellName::unchecked_new("fbcode".into()));
//! assert_eq!(fbcode_aliases.resolve("fbsource")?, &CellName::unchecked_new("fbsource".into()));
//! assert_eq!(fbcode_aliases.resolve("fbcode")?, &CellName::unchecked_new("fbcode".into()));
//!
//! # anyhow::Ok(())
//! ```
//!

pub mod build_file_cell;
pub mod cell_path;
pub mod cell_root_path;
pub mod paths;

use std::borrow::Borrow;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::path::Path;
use std::sync::Arc;

use anyhow::anyhow;
use anyhow::Context;
use derivative::Derivative;
use derive_more::Display;
use gazebo::prelude::*;
use itertools::Itertools;
use sequence_trie::SequenceTrie;
use thiserror::Error;

use crate::cells::cell_path::CellPath;
use crate::cells::cell_root_path::CellRootPath;
use crate::cells::cell_root_path::CellRootPathBuf;
use crate::fs::paths::AbsPath;
use crate::fs::paths::AbsPathBuf;
use crate::fs::paths::FileNameBuf;
use crate::fs::paths::RelativePath;
use crate::fs::project::ProjectFilesystem;
use crate::fs::project::ProjectRelativePath;
use crate::fs::project::ProjectRelativePathBuf;

/// Errors from cell creation
#[derive(Error, Debug)]
pub enum CellError {
    #[error("Cell paths `{1}` and `{2}` had the same alias `{0}`.")]
    DuplicateAliases(CellAlias, CellRootPathBuf, CellRootPathBuf),
    #[error("Cell paths `{1}` and `{2}` had the same cell name `{0}`.")]
    DuplicateNames(CellName, CellRootPathBuf, CellRootPathBuf),
    #[error("cannot find the cell at current path `{0}`. Known roots are `<{}>`", .1.join(", "))]
    UnknownCellPath(ProjectRelativePathBuf, Vec<String>),
    #[error("unknown cell alias: `{0}`. known aliases are: `{}`", .1.iter().join(", "))]
    UnknownCellAlias(CellAlias, Vec<CellAlias>),
    #[error("unknown cell name: `{0}`. known cell names are `{}`", .1.iter().join(", "))]
    UnknownCellName(CellName, Vec<CellName>),
    #[error(
        "cells should be specified as `cellname`=`relative path` on each line, but found line: `{0}`."
    )]
    ParsingError(String),
}

/// A 'CellInstance', contains a 'CellName' and a path for that cell.
#[derive(Clone, Debug, Display, Dupe, PartialEq, Eq)]
#[display(fmt = "{}", "_0.name")]
pub struct CellInstance(Arc<CellData>);

/// A 'CellAlias' is a user-provided string name that maps to a 'CellName'.
/// The mapping of 'CellAlias' to 'CellName' is specific to the current cell so
/// that the same 'CellAlias' may map to different 'CellName's depending on what
/// the current 'CellInstance' is that references the 'CellAlias'.
#[derive(Clone, Debug, Display, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct CellAlias(String);

impl CellAlias {
    pub fn new(alias: String) -> CellAlias {
        CellAlias(alias)
    }
}

impl Borrow<str> for CellAlias {
    fn borrow(&self) -> &str {
        &self.0
    }
}

/// A 'CellAliasResolver' is unique to a 'CellInstance'.
/// It is responsible for resolving all 'CellAlias' encountered within the
/// 'CellInstance' into the global canonical 'CellName's
#[derive(Clone, Dupe, Debug, PartialEq, Eq)]
pub struct CellAliasResolver(Arc<HashMap<CellAlias, CellName>>);

impl CellAliasResolver {
    /// Create an instance of `CellAliasResolver`. The special alias `""` must be present, or
    /// this will fail
    pub fn new(
        alias_mapping: Arc<HashMap<CellAlias, CellName>>,
    ) -> anyhow::Result<CellAliasResolver> {
        if alias_mapping.contains_key("") {
            Ok(CellAliasResolver(alias_mapping))
        } else {
            Err(anyhow::Error::new(CellError::UnknownCellAlias(
                CellAlias::new("".to_owned()),
                alias_mapping.keys().cloned().collect(),
            )))
        }
    }

    /// resolves a 'CellAlias' into its corresponding 'CellName'
    pub fn resolve<T: ?Sized>(&self, alias: &T) -> anyhow::Result<&CellName>
    where
        CellAlias: Borrow<T>,
        T: Hash + Eq + Display,
    {
        self.0.get(alias).ok_or_else(|| {
            anyhow::Error::new(CellError::UnknownCellAlias(
                CellAlias::new(alias.to_string()),
                self.0.keys().cloned().collect(),
            ))
        })
    }

    /// finds the 'CellName' for the current cell (with the alias `""`. See module docs)
    pub fn resolve_self(&self) -> &CellName {
        self.resolve("").expect("The alias \"\" to be valid")
    }

    pub fn mappings(&self) -> impl Iterator<Item = (&CellAlias, &CellName)> {
        self.0.iter()
    }
}

/// A 'CellName' is a canonicalized, human-readable name that corresponds to a
/// 'CellInstance'. There should be a one to one mapping between a 'CellName'
/// and a 'CellInstance'.
///
/// The cell within a fully qualified target like `foo//some:target` is `foo`.
/// The cell name is also restricted to alphabet characters (i.e. shouldn't
/// contain any special characters like `/`), so `foo/bar//some:target` has an
/// invalid cell name of `foo/bar`.
// TODO consider if we need to intern the string
#[derive(Clone, Debug, Display, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct CellName(String);

impl CellName {
    pub fn unchecked_new(name: String) -> CellName {
        CellName(name)
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[derive(Derivative, PartialEq, Eq)]
#[derivative(Debug)]
struct CellData {
    /// the fully canonicalized 'CellName'
    name: CellName,
    /// the project relative path to this 'CellInstance'
    path: CellRootPathBuf,
    /// a list of potential buildfile names for this cell (e.g. 'BUCK', 'TARGETS',
    /// 'TARGET.v2'). The candidates are listed in priority order, buck will use
    /// the first one it encounters in a directory.
    buildfiles: Vec<FileNameBuf>,
    #[derivative(Debug = "ignore")]
    /// the aliases of this specific cell
    aliases: CellAliasResolver,
}

impl CellInstance {
    fn new(
        name: CellName,
        path: CellRootPathBuf,
        buildfiles: Vec<FileNameBuf>,
        aliases: CellAliasResolver,
    ) -> CellInstance {
        CellInstance(Arc::new(CellData {
            name,
            path,
            buildfiles,
            aliases,
        }))
    }

    /// Get the name of the cell, as supplied in `cell_name//foo:bar`.
    pub fn name(&self) -> &CellName {
        &self.0.name
    }

    /// Get the path of the cell, where it is routed.
    pub fn path(&self) -> &CellRootPath {
        &self.0.path
    }

    // Get the name of build files for the cell.
    pub fn buildfiles(&self) -> &[FileNameBuf] {
        &self.0.buildfiles
    }

    pub fn cell_alias_resolver(&self) -> &CellAliasResolver {
        &self.0.aliases
    }
}

/// Resolves 'CellName's into 'CellInstance's.
// TODO(bobyf) we need to check if cells changed
#[derive(Clone, Dupe, PartialEq, Eq, Debug)]
pub struct CellResolver(Arc<CellResolverInternals>);

#[derive(PartialEq, Eq, Debug)]
struct CellResolverInternals {
    cells: HashMap<CellName, CellInstance>,
    path_mappings: SequenceTrie<FileNameBuf, CellName>,
}

impl CellResolver {
    // Make this public till we start parsing config files from cells
    pub fn new(
        cells: HashMap<CellName, CellInstance>,
        path_mappings: SequenceTrie<FileNameBuf, CellName>,
    ) -> CellResolver {
        CellResolver(Arc::new(CellResolverInternals {
            cells,
            path_mappings,
        }))
    }

    pub fn contains(&self, cell: &CellName) -> bool {
        self.0.cells.contains_key(cell)
    }

    /// Get a `Cell` from the `CellMap`
    pub fn get(&self, cell: &CellName) -> anyhow::Result<&CellInstance> {
        self.0.cells.get(cell).ok_or_else(|| {
            anyhow::Error::new(CellError::UnknownCellName(
                cell.clone(),
                self.0.cells.keys().cloned().collect(),
            ))
        })
    }

    pub fn root_cell(&self) -> &CellName {
        self.find(ProjectRelativePath::new("").unwrap())
            .expect("Should have had a cell at the project root.")
    }

    pub fn root_cell_instance(&self) -> &CellInstance {
        self.get(self.root_cell())
            .expect("Should have had a root cell")
    }

    pub fn root_cell_cell_alias_resolver(&self) -> &CellAliasResolver {
        self.root_cell_instance().cell_alias_resolver()
    }

    /// Get a `CellName` from a path by finding the best matching cell path that
    /// is a prefix of the current path relative to the project root. e.g. `fbcode/foo/bar` matches
    /// cell path `fbcode`.
    pub fn find<P: AsRef<ProjectRelativePath> + ?Sized>(
        &self,
        path: &P,
    ) -> anyhow::Result<&CellName> {
        self.0
            .path_mappings
            .get_ancestor(path.as_ref().iter())
            .ok_or_else(|| {
                anyhow::Error::new(CellError::UnknownCellPath(
                    path.as_ref().to_buf(),
                    self.0
                        .path_mappings
                        .keys()
                        .map(|p| p.iter().join("/"))
                        .collect(),
                ))
            })
    }

    pub fn get_cell_path<P: AsRef<ProjectRelativePath> + ?Sized>(
        &self,
        path: &P,
    ) -> anyhow::Result<CellPath> {
        let path = path.as_ref();
        let cell = self.find(path)?;
        let instance = self.get(cell)?;
        let relative = path.strip_prefix(instance.path().as_project_relative_path())?;
        Ok(CellPath::new(cell.clone(), relative.to_owned().into()))
    }

    pub fn get_cell_path_from_abs_or_rel_path(
        &self,
        path: &Path,
        fs: &ProjectFilesystem,
        cwd: &ProjectRelativePath,
    ) -> anyhow::Result<CellPath> {
        if path.is_absolute() {
            let abs_path = AbsPath::new(path)?;
            self.get_cell_path(&fs.relativize(abs_path)?)
        } else {
            let rel_path = RelativePath::from_path(path)?;
            let project_path = cwd.join_normalized(rel_path)?;
            self.get_cell_path(&project_path)
        }
    }

    pub fn cells(&self) -> impl Iterator<Item = (&CellName, &CellInstance)> {
        self.0.cells.iter()
    }

    /// Resolves a cell alias and a cell relative path into an absolute path.
    /// `cwd` is used to perform contextual resolution and figure out which
    /// cell mapping to use (i.e., map from alias to cell name).
    pub fn resolve_cell_relative_path(
        &self,
        cell_alias: &str,
        cell_relative_path: &str,
        project_filesystem: &ProjectFilesystem,
        cwd: &AbsPath,
    ) -> anyhow::Result<AbsPathBuf> {
        // We expect this to always succeed as long as the client connects to the
        // appropriate daemon.
        let proj_relative_path = project_filesystem
            .relativize(cwd)
            .with_context(|| format!("Error relativizing cwd (`{}`)", cwd))?;
        let context_cell_name = self.find(&proj_relative_path)?;
        let context_cell = self.get(context_cell_name)?;

        let resolved_cell_name = context_cell.cell_alias_resolver().resolve(cell_alias)?;
        let cell = self.get(resolved_cell_name)?;
        let cell_absolute_path = project_filesystem.resolve(cell.path().as_project_relative_path());
        cell_absolute_path.join_normalized(cell_relative_path)
    }

    /// Resolves a given 'Package' to the 'ProjectRelativePath' that points to
    /// the 'Package'
    ///
    /// ```
    /// use buck2_core::cells::{CellResolver, CellName};
    /// use buck2_core::fs::project::{ProjectRelativePath, ProjectRelativePathBuf};
    /// use buck2_core::fs::paths::{ForwardRelativePathBuf, ForwardRelativePath};
    /// use std::convert::TryFrom;
    /// use buck2_core::cells::cell_path::CellPath;
    /// use buck2_core::cells::cell_root_path::CellRootPathBuf;
    /// use buck2_core::cells::paths::CellRelativePathBuf;
    /// use buck2_core::cells::testing::CellResolverExt;
    ///
    /// let cell_path = ProjectRelativePath::new("my/cell")?;
    /// let cells = CellResolver::of_names_and_paths(&[
    ///     (CellName::unchecked_new("mycell".to_owned()), CellRootPathBuf::new(cell_path.to_buf()))
    /// ]);
    ///
    /// let cell_path = CellPath::new(
    ///     CellName::unchecked_new("mycell".into()),
    ///     CellRelativePathBuf::unchecked_new("some/path".to_owned()));
    ///
    /// assert_eq!(
    ///     cells.resolve_path(&cell_path)?,
    ///     ProjectRelativePathBuf::unchecked_new("my/cell/some/path".into()),
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn resolve_path(&self, cell_path: &CellPath) -> anyhow::Result<ProjectRelativePathBuf> {
        Ok(self.get(cell_path.cell())?.path().join(cell_path.path()))
    }
}

/// Aggregates cell information as we parse cell configs and keeps state to
/// generate a final 'CellResolver'
#[derive(Debug)]
pub struct CellsAggregator {
    cell_infos: HashMap<CellRootPathBuf, CellAggregatorInfo>,
}

fn default_buildfiles() -> Vec<FileNameBuf> {
    (&["BUCK.v2", "BUCK"][..]).map(|&n| FileNameBuf::try_from(n.to_owned()).unwrap())
}

#[derive(Default, Debug)]
struct CellAggregatorInfo {
    all_aliases: BTreeSet<String>,
    alias_mapping: HashMap<CellAlias, CellRootPathBuf>,
    /// The build file name in this if it's been set. If it hasn't we'll use the
    /// default `["BUCK.v2", "BUCK"]` when building the resolver.
    buildfiles: Option<Vec<FileNameBuf>>,
}

impl CellsAggregator {
    pub fn new() -> Self {
        Self {
            cell_infos: HashMap::new(),
        }
    }

    fn cell_info(&mut self, cell_path: CellRootPathBuf) -> &mut CellAggregatorInfo {
        self.cell_infos
            .entry(cell_path)
            .or_insert_with(CellAggregatorInfo::default)
    }

    /// Adds a cell alias configuration entry
    pub fn add_cell_alias_entry(
        &mut self,
        cell_root: CellRootPathBuf,
        parsed_alias: CellAlias,
        alias_path: CellRootPathBuf,
    ) -> anyhow::Result<()> {
        self.cell_info(alias_path.clone())
            .all_aliases
            .insert(parsed_alias.0.clone());

        let root_info = self.cell_info(cell_root);

        let old = root_info
            .alias_mapping
            .insert(parsed_alias.clone(), alias_path.clone());
        if let Some(old) = old {
            if old != alias_path {
                return Err(anyhow!(CellError::DuplicateAliases(
                    parsed_alias,
                    old,
                    alias_path
                )));
            }
        }
        Ok(())
    }

    pub fn set_buildfiles(&mut self, cell_root: CellRootPathBuf, buildfiles: Vec<FileNameBuf>) {
        let mut cell_info = self.cell_info(cell_root);
        cell_info.buildfiles = Some(buildfiles);
    }

    /// for now, the global cell-name is the first alias in lexicographic sorted
    /// order of all the aliases for a particular cell path
    fn get_cell_name_from_path(&self, path: &CellRootPath) -> anyhow::Result<CellName> {
        self.cell_infos
            .get(path)
            .and_then(|info| info.all_aliases.first())
            .map(|alias| CellName::unchecked_new(alias.clone()))
            .ok_or_else(|| {
                anyhow!(CellError::UnknownCellPath(
                    path.as_project_relative_path().to_buf(),
                    self.cell_infos
                        .keys()
                        .map(|p| p.as_str().to_owned())
                        .collect()
                ))
            })
    }

    /// Creates the 'CellResolver' from all the entries that were aggregated
    pub fn make_cell_resolver(&self) -> anyhow::Result<CellResolver> {
        let mut cell_mappings = HashMap::new();
        let mut cell_path_mappings = SequenceTrie::new();

        for (cell_path, cell_info) in &self.cell_infos {
            let mut aliases_for_cell = HashMap::new();
            let cell_name = self.get_cell_name_from_path(cell_path)?;
            aliases_for_cell.insert(CellAlias::new("".into()), cell_name.clone());

            for (alias, path_for_alias) in &cell_info.alias_mapping {
                aliases_for_cell
                    .insert(alias.clone(), self.get_cell_name_from_path(path_for_alias)?);
            }
            aliases_for_cell.insert(CellAlias::new("".into()), cell_name.clone());

            let old = cell_mappings.insert(
                cell_name.clone(),
                CellInstance::new(
                    cell_name.clone(),
                    cell_path.clone(),
                    cell_info
                        .buildfiles
                        .clone()
                        .unwrap_or_else(default_buildfiles),
                    CellAliasResolver::new(Arc::new(aliases_for_cell))?,
                ),
            );
            if let Some(old) = old {
                return Err(anyhow!(CellError::DuplicateNames(
                    old.name().clone(),
                    old.path().to_buf(),
                    cell_path.clone()
                )));
            }

            cell_path_mappings.insert(cell_path.iter(), cell_name);
        }

        Ok(CellResolver::new(cell_mappings, cell_path_mappings))
    }
}

// test helpers
pub mod testing {
    use std::collections::HashMap;
    use std::sync::Arc;

    use sequence_trie::SequenceTrie;

    use super::default_buildfiles;
    use crate::cells::cell_root_path::CellRootPathBuf;
    use crate::cells::CellAlias;
    use crate::cells::CellAliasResolver;
    use crate::cells::CellInstance;
    use crate::cells::CellName;
    use crate::cells::CellResolver;
    pub trait CellResolverExt {
        /// Creates a new 'CellResolver' based on the given iterator of (cell
        /// name, cell path). The 'CellAliasResolver' of each cell is
        /// empty. i.e. no aliases are defined for any of the cells.
        fn of_names_and_paths(cells: &[(CellName, CellRootPathBuf)]) -> CellResolver;

        fn with_names_and_paths_with_alias(
            cells: &[(CellName, CellRootPathBuf, HashMap<CellAlias, CellName>)],
        ) -> CellResolver;
    }

    impl CellResolverExt for CellResolver {
        fn of_names_and_paths(cells: &[(CellName, CellRootPathBuf)]) -> CellResolver {
            let mut cell_mappings = HashMap::new();
            let mut path_mappings = SequenceTrie::new();

            for (name, path) in cells {
                cell_mappings.insert(
                    name.clone(),
                    CellInstance::new(
                        name.clone(),
                        path.clone(),
                        default_buildfiles(),
                        CellAliasResolver(Arc::new(Default::default())),
                    ),
                );

                path_mappings.insert(path.iter(), name.clone());
            }

            Self::new(cell_mappings, path_mappings)
        }

        fn with_names_and_paths_with_alias(
            cells: &[(CellName, CellRootPathBuf, HashMap<CellAlias, CellName>)],
        ) -> CellResolver {
            let mut cell_mappings = HashMap::new();
            let mut path_mappings = SequenceTrie::new();

            for (name, path, alias) in cells {
                let prev = cell_mappings.insert(
                    name.clone(),
                    CellInstance::new(
                        name.clone(),
                        path.clone(),
                        default_buildfiles(),
                        CellAliasResolver(Arc::new(alias.clone())),
                    ),
                );
                assert!(prev.is_none());

                let prev = path_mappings.insert(path.iter(), name.clone());
                assert!(prev.is_none());
            }

            Self::new(cell_mappings, path_mappings)
        }
    }

    #[cfg(test)]
    #[test]
    fn test_of_names_and_paths() -> anyhow::Result<()> {
        use crate::fs::project::ProjectRelativePathBuf;

        let cell_resolver = CellResolver::of_names_and_paths(&[(
            CellName::unchecked_new("foo".into()),
            CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("bar".into())),
        )]);

        let cell = cell_resolver.get(&CellName::unchecked_new("foo".into()))?;
        assert_eq!(&CellName::unchecked_new("foo".into()), cell.name());
        assert_eq!("bar", cell.path().as_str());

        Ok(())
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::cells::testing::CellResolverExt;
    use crate::fs::paths::ForwardRelativePath;
    use crate::fs::paths::ForwardRelativePathBuf;

    #[test]
    fn test_cells() -> anyhow::Result<()> {
        let cell1_path = CellRootPath::new(ProjectRelativePath::new("my/cell1")?);
        let cell2_path = CellRootPath::new(ProjectRelativePath::new("cell2")?);
        let cell3_path = CellRootPath::new(ProjectRelativePath::new("my/cell3")?);

        let cells = CellResolver::with_names_and_paths_with_alias(&[
            (
                CellName::unchecked_new("cell1".to_owned()),
                cell1_path.to_buf(),
                hashmap![
                    CellAlias::new("cell1".to_owned()) => CellName::unchecked_new("cell1".to_owned()),
                    CellAlias::new("".to_owned()) => CellName::unchecked_new("cell1".to_owned()),
                    CellAlias::new("cell2".to_owned()) => CellName::unchecked_new("cell2".to_owned()),
                    CellAlias::new("cell3".to_owned()) => CellName::unchecked_new("cell3".to_owned()),
                ],
            ),
            (
                CellName::unchecked_new("cell2".to_owned()),
                cell2_path.to_buf(),
                hashmap![
                    CellAlias::new("cell2".to_owned()) => CellName::unchecked_new("cell2".to_owned()),
                    CellAlias::new("".to_owned()) => CellName::unchecked_new("cell2".to_owned()),
                    CellAlias::new("cell1".to_owned()) => CellName::unchecked_new("cell1".to_owned()),
                    CellAlias::new("cell3".to_owned()) => CellName::unchecked_new("cell3".to_owned()),
                ],
            ),
            (
                CellName::unchecked_new("cell3".to_owned()),
                cell3_path.to_buf(),
                hashmap![
                    CellAlias::new("z_cell3".to_owned()) => CellName::unchecked_new("cell3".to_owned()),
                    CellAlias::new("".to_owned()) => CellName::unchecked_new("cell3".to_owned()),
                    CellAlias::new("z_cell1".to_owned()) => CellName::unchecked_new("cell1".to_owned()),
                    CellAlias::new("z_cell2".to_owned()) => CellName::unchecked_new("cell2".to_owned()),
                ],
            ),
        ]);

        {
            let cell1 = cells.get(&CellName::unchecked_new("cell1".into())).unwrap();
            assert_eq!(cell1.path(), cell1_path);

            let aliases = cell1.cell_alias_resolver();
            assert_eq!(
                aliases.resolve("").unwrap(),
                &CellName::unchecked_new("cell1".into())
            );
            assert_eq!(
                aliases.resolve("cell1").unwrap(),
                &CellName::unchecked_new("cell1".into())
            );
            assert_eq!(
                aliases.resolve("cell2").unwrap(),
                &CellName::unchecked_new("cell2".into())
            );
            assert_eq!(
                aliases.resolve("cell3").unwrap(),
                &CellName::unchecked_new("cell3".into())
            );
        }

        {
            let cell2 = cells.get(&CellName::unchecked_new("cell2".into())).unwrap();
            assert_eq!(cell2.path(), cell2_path);

            let aliases = cell2.cell_alias_resolver();
            assert_eq!(
                aliases.resolve("").unwrap(),
                &CellName::unchecked_new("cell2".into())
            );
            assert_eq!(
                aliases.resolve("cell1").unwrap(),
                &CellName::unchecked_new("cell1".into())
            );
            assert_eq!(
                aliases.resolve("cell2").unwrap(),
                &CellName::unchecked_new("cell2".into())
            );
            assert_eq!(
                aliases.resolve("cell3").unwrap(),
                &CellName::unchecked_new("cell3".into())
            );
        }

        {
            let cell3 = cells.get(&CellName::unchecked_new("cell3".into())).unwrap();
            assert_eq!(cell3.path(), cell3_path);

            let aliases = cell3.cell_alias_resolver();
            assert_eq!(
                aliases.resolve("").unwrap(),
                &CellName::unchecked_new("cell3".into())
            );
            assert_eq!(
                aliases.resolve("z_cell1").unwrap(),
                &CellName::unchecked_new("cell1".into())
            );
            assert_eq!(
                aliases.resolve("z_cell2").unwrap(),
                &CellName::unchecked_new("cell2".into())
            );
            assert_eq!(
                aliases.resolve("z_cell3").unwrap(),
                &CellName::unchecked_new("cell3".into())
            );
        }

        assert_eq!(
            cells.find(cell1_path)?,
            &CellName::unchecked_new("cell1".into())
        );
        assert_eq!(
            cells.find(cell2_path)?,
            &CellName::unchecked_new("cell2".into())
        );
        assert_eq!(
            cells.find(cell3_path)?,
            &CellName::unchecked_new("cell3".into())
        );
        assert_eq!(
            cells.find(
                &cell2_path
                    .as_project_relative_path()
                    .join(ForwardRelativePath::new("fake/cell3")?)
            )?,
            &CellName::unchecked_new("cell2".into())
        );
        assert_eq!(
            cells.find(
                &cell3_path
                    .as_project_relative_path()
                    .join(ForwardRelativePath::new("more/foo")?)
            )?,
            &CellName::unchecked_new("cell3".into())
        );
        assert_eq!(cells.find(ProjectRelativePath::new("blah")?).is_err(), true);

        assert_eq!(
            cells.get_cell_path(cell1_path)?,
            CellPath::new(
                CellName::unchecked_new("cell1".to_owned()),
                ForwardRelativePathBuf::unchecked_new("".to_owned()).into()
            )
        );

        assert_eq!(
            cells.get_cell_path(cell2_path)?,
            CellPath::new(
                CellName::unchecked_new("cell2".to_owned()),
                ForwardRelativePathBuf::unchecked_new("".to_owned()).into()
            )
        );

        assert_eq!(
            cells.get_cell_path(
                &cell2_path
                    .as_project_relative_path()
                    .join(ForwardRelativePath::new("fake/cell3")?)
            )?,
            CellPath::new(
                CellName::unchecked_new("cell2".to_owned()),
                ForwardRelativePathBuf::unchecked_new("fake/cell3".to_owned()).into()
            )
        );

        Ok(())
    }
}
