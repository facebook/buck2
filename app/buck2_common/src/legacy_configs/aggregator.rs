/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::fmt::Debug;

use buck2_core::cells::alias::NonEmptyCellAlias;
use buck2_core::cells::cell_root_path::CellRootPath;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::cells::external::ExternalCellOrigin;
use buck2_core::cells::instance;
use buck2_core::cells::name::CellName;
use buck2_core::cells::nested::NestedCells;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellResolver;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use dupe::Dupe;
use instance::CellInstance;

/// Errors from cell creation
#[derive(buck2_error::Error, Debug)]
#[buck2(input)]
enum CellError {
    #[error("Cell paths `{1}` and `{2}` had the same alias `{0}`.")]
    DuplicateAliases(NonEmptyCellAlias, CellRootPathBuf, CellRootPathBuf),
    #[error("cannot find the cell at current path `{0}`. Known roots are `<{}>`", .1.join(", "))]
    UnknownCellPath(ProjectRelativePathBuf, Vec<String>),
    #[error(
        "Cell name `{0}` should be an alias for an existing cell, but `{1}` isn't a known alias"
    )]
    AliasOnlyCell(NonEmptyCellAlias, NonEmptyCellAlias),
    #[error("No cell name for the root path, add an entry for `.`")]
    NoRootCell,
    #[error("Cell `{0}` was marked as external twice")]
    DuplicateExternalCell(CellName),
}

/// Aggregates cell information as we parse cell configs and keeps state to
/// generate a final 'CellResolver'
#[derive(Debug)]
pub(crate) struct CellsAggregator {
    cell_infos: HashMap<CellRootPathBuf, CellAggregatorInfo>,
    root_aliases: HashMap<NonEmptyCellAlias, CellRootPathBuf>,
}

#[derive(Debug, Default)]
struct CellAggregatorInfo {
    /// The name to use for this alias.
    /// So that it is predictable, we always use the first name we encounter,
    /// so the root file can choose what the alias is called.
    name: Option<CellName>,
    external: Option<ExternalCellOrigin>,
}

impl CellsAggregator {
    pub(crate) fn new() -> Self {
        Self {
            cell_infos: HashMap::new(),
            root_aliases: HashMap::new(),
        }
    }

    fn cell_info(&mut self, cell_path: CellRootPathBuf) -> &mut CellAggregatorInfo {
        self.cell_infos
            .entry(cell_path)
            .or_insert_with(CellAggregatorInfo::default)
    }

    pub(crate) fn get_name(&self, cell_path: &CellRootPath) -> Option<CellName> {
        match self.cell_infos.get(cell_path) {
            None => None,
            Some(info) => info.name,
        }
    }

    /// Adds a cell configuration entry
    pub(crate) fn add_cell_entry(
        &mut self,
        cell_root: CellRootPathBuf,
        parsed_alias: NonEmptyCellAlias,
        alias_path: CellRootPathBuf,
    ) -> anyhow::Result<()> {
        let name = &mut self.cell_info(alias_path.clone()).name;
        if name.is_none() {
            *name = Some(CellName::unchecked_new(parsed_alias.as_str())?);
        }

        if cell_root.is_repo_root() {
            self.add_cell_alias_for_root_cell_inner(parsed_alias, alias_path)?;
        }
        Ok(())
    }

    /// Adds a cell alias configuration entry
    pub(crate) fn add_cell_alias_for_root_cell(
        &mut self,
        parsed_alias: NonEmptyCellAlias,
        alias_destination: NonEmptyCellAlias,
    ) -> anyhow::Result<CellRootPathBuf> {
        let alias_path = match self.root_aliases.get(&alias_destination) {
            None => return Err(CellError::AliasOnlyCell(parsed_alias, alias_destination).into()),
            Some(alias_path) => alias_path.clone(),
        };
        self.add_cell_alias_for_root_cell_inner(parsed_alias, alias_path.clone())?;
        Ok(alias_path)
    }

    fn add_cell_alias_for_root_cell_inner(
        &mut self,
        from: NonEmptyCellAlias,
        to: CellRootPathBuf,
    ) -> anyhow::Result<()> {
        let old: Option<CellRootPathBuf> = self.root_aliases.insert(from.clone(), to.clone());
        if let Some(old) = old {
            if old != to {
                return Err(CellError::DuplicateAliases(from, old, to).into());
            }
        }
        Ok(())
    }

    fn get_cell_name_from_path(&self, path: &CellRootPath) -> anyhow::Result<CellName> {
        self.cell_infos
            .get(path)
            .and_then(|info| info.name)
            .ok_or_else(|| {
                anyhow::anyhow!(CellError::UnknownCellPath(
                    path.as_project_relative_path().to_buf(),
                    self.cell_infos
                        .keys()
                        .map(|p| p.as_str().to_owned())
                        .collect()
                ))
            })
    }

    /// Creates the 'CellResolver' from all the entries that were aggregated
    pub(crate) fn make_cell_resolver(mut self) -> anyhow::Result<CellResolver> {
        let mut cell_mappings = Vec::new();

        let all_cell_roots_for_nested_cells: Vec<_> = self
            .cell_infos
            .keys()
            .map(|path| Ok((self.get_cell_name_from_path(path)?, path.as_path())))
            .collect::<anyhow::Result<_>>()?;

        let mut root_cell_name = None;

        for (cell_path, cell_info) in &self.cell_infos {
            let nested_cells =
                NestedCells::from_cell_roots(&all_cell_roots_for_nested_cells, cell_path);

            let cell_name = self.get_cell_name_from_path(cell_path)?;

            if cell_path.is_repo_root() {
                root_cell_name = Some(cell_name);
            }

            cell_mappings.push(CellInstance::new(
                cell_name,
                cell_path.clone(),
                cell_info.external.dupe(),
                nested_cells,
            )?);
        }

        let root_cell_name = root_cell_name.ok_or(CellError::NoRootCell)?;
        let mut root_aliases = HashMap::new();
        for (alias, path) in std::mem::take(&mut self.root_aliases) {
            root_aliases.insert(alias, self.get_cell_name_from_path(&path)?);
        }

        let root_cell_alias_resolver = CellAliasResolver::new(root_cell_name, root_aliases)?;

        CellResolver::new(cell_mappings, root_cell_alias_resolver)
    }

    pub(crate) fn mark_external_cell(
        &mut self,
        cell_root: CellRootPathBuf,
        origin: ExternalCellOrigin,
    ) -> anyhow::Result<()> {
        let info = self.cell_info(cell_root.clone());
        if info.external.is_some() {
            return Err(CellError::DuplicateExternalCell(
                self.get_cell_name_from_path(&cell_root)?,
            )
            .into());
        }
        info.external = Some(origin);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_duplicate_aliases() -> anyhow::Result<()> {
        let mut agg = CellsAggregator::new();

        let cell_root = CellRootPathBuf::new(ProjectRelativePathBuf::try_from("".to_owned())?);
        let alias_path =
            CellRootPathBuf::new(ProjectRelativePathBuf::try_from("random/path".to_owned())?);

        agg.add_cell_entry(
            cell_root.clone(),
            NonEmptyCellAlias::new("root".to_owned()).unwrap(),
            cell_root.clone(),
        )?;
        agg.add_cell_entry(
            cell_root.clone(),
            NonEmptyCellAlias::new("hello".to_owned()).unwrap(),
            alias_path.clone(),
        )?;
        agg.add_cell_entry(
            cell_root.clone(),
            NonEmptyCellAlias::new("cruel".to_owned()).unwrap(),
            alias_path.clone(),
        )?;
        agg.add_cell_entry(
            cell_root,
            NonEmptyCellAlias::new("world".to_owned()).unwrap(),
            alias_path,
        )?;

        // We want the first alias to win (hello), rather than the lexiographically first (cruel)
        let cell_resolver = agg.make_cell_resolver()?;
        assert!(cell_resolver.get(CellName::testing_new("hello")).is_ok());
        assert!(cell_resolver.get(CellName::testing_new("cruel")).is_err());
        Ok(())
    }

    #[test]
    fn test_alias_only_error() -> anyhow::Result<()> {
        let mut agg = CellsAggregator::new();

        assert!(
            agg.add_cell_alias_for_root_cell(
                NonEmptyCellAlias::new("root".to_owned()).unwrap(),
                NonEmptyCellAlias::new("does_not_exist".to_owned()).unwrap(),
            )
            .is_err()
        );
        Ok(())
    }
}
