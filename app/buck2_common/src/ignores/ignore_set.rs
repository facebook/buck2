/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;
use std::sync::LazyLock;

use allocative::Allocative;
use buck2_core::cells::CellResolver;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::internal_error;
use buck2_hash::BuckMutMap;
use dice::DiceData;
use dice::DiceDataBuilder;
use dupe::Dupe;
use globset::GlobSetBuilder;
use pagable::PagableDeserialize;
use pagable::PagableDeserializer;
use pagable::PagableSerialize;
use pagable::PagableSerializer;
use pagable::PagableUnsupported;
use regex::Regex;

use crate::legacy_configs::configs::LegacyBuckConfig;
use crate::legacy_configs::key::BuckconfigKeyRef;

pub fn compute_ignore_set(
    cells: CellResolver,
    configs: &BuckMutMap<CellName, LegacyBuckConfig>,
) -> buck2_error::Result<IgnoreSet> {
    let by_cell = configs
        .iter()
        .map(|(cell_name, config)| {
            buck2_error::Ok((
                *cell_name,
                PerCellIgnoreSet::from_config(config, cells.is_root_cell(*cell_name))?,
            ))
        })
        .collect::<buck2_error::Result<_>>()?;

    Ok(IgnoreSet { by_cell, cells })
}

#[derive(Debug, Allocative, PagableUnsupported, PartialEq, Eq)]
pub struct IgnoreSet {
    by_cell: BuckMutMap<CellName, PerCellIgnoreSet>,
    cells: CellResolver,
}

impl IgnoreSet {
    /// Returns a pattern that matches the candidate if there is one.
    pub(crate) fn matches_candidate<'a>(&'a self, path: &ProjectRelativePath) -> Option<&'a str> {
        let (cell, path) = self.cells.split_cell_path(path);
        self.by_cell
            .get(&cell)
            .and_then(|ignore_set| ignore_set.matches_candidate(path))
    }

    /// Returns whether any pattern matches.
    pub fn is_match(&self, path: &ProjectRelativePath) -> bool {
        let (cell, path) = self.cells.split_cell_path(path);
        self.by_cell
            .get(&cell)
            .is_some_and(|ignore_set| ignore_set.is_match(path))
    }

    /// Whether the `project.ignore` of `config` is the one this set was built from for `cell`.
    pub fn built_from(
        &self,
        cell: CellName,
        config: &LegacyBuckConfig,
        root_cell: bool,
    ) -> buck2_error::Result<bool> {
        Ok(self.by_cell.get(&cell) == Some(&PerCellIgnoreSet::from_config(config, root_cell)?))
    }

    /// Ignores nothing (beyond `buck-out`), whatever the cells are.
    pub fn testing_empty() -> Self {
        let root = CellName::testing_new("root");
        let cells = CellResolver::testing_with_name_and_path(
            root,
            CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new(String::new())),
        );
        Self::testing_new_single_cell(cells, root, "").unwrap()
    }

    pub fn testing_new_single_cell(
        cells: CellResolver,
        cell: CellName,
        spec: &str,
    ) -> buck2_error::Result<Self> {
        let root_cell = cells.is_root_cell(cell);
        let mut by_cell = BuckMutMap::default();
        by_cell.insert(cell, PerCellIgnoreSet::from_ignore_spec(spec, root_cell)?);
        Ok(Self { by_cell, cells })
    }
}

pub trait HasIgnoreSet {
    fn get_ignore_set(&self) -> Arc<IgnoreSet>;
}

pub trait SetIgnoreSet {
    fn set_ignore_set(&mut self, ignore_set: Arc<IgnoreSet>);
}

impl HasIgnoreSet for DiceData {
    fn get_ignore_set(&self) -> Arc<IgnoreSet> {
        self.get::<Arc<IgnoreSet>>()
            .expect("ignore set should be set")
            .dupe()
    }
}

impl SetIgnoreSet for DiceDataBuilder {
    fn set_ignore_set(&mut self, ignore_set: Arc<IgnoreSet>) {
        self.set(ignore_set)
    }
}

impl SetIgnoreSet for DiceData {
    fn set_ignore_set(&mut self, ignore_set: Arc<IgnoreSet>) {
        self.set(ignore_set)
    }
}

#[derive(Debug, Clone, Allocative)]
struct PerCellIgnoreSet {
    #[allocative(skip)]
    globset: globset::GlobSet,
    // Patterns that were added to the globset. Storing this seprately to support ser/de and
    // so that error messages can refer to the specific pattern that was matched.
    // This should be in the same order as the strings were added to the GlobSet to match the indices returned from it.
    patterns: Vec<String>,
}

impl PagableSerialize for PerCellIgnoreSet {
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> pagable::Result<()> {
        self.patterns.pagable_serialize(serializer)
    }
}

impl<'de> PagableDeserialize<'de> for PerCellIgnoreSet {
    fn pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> pagable::Result<Self> {
        let patterns = Vec::<String>::pagable_deserialize(deserializer)?;
        let globset = Self::build_globset(&patterns)
            .map_err(|e| pagable::Error::new(e).context("rebuilding IgnoreSet globset"))?;
        Ok(Self { globset, patterns })
    }
}

impl PartialEq for PerCellIgnoreSet {
    fn eq(&self, other: &Self) -> bool {
        // Only compare patterns because globset is derived from patterns.
        self.patterns == other.patterns
    }
}

impl Eq for PerCellIgnoreSet {}

impl PerCellIgnoreSet {
    fn from_config(config: &LegacyBuckConfig, root_cell: bool) -> buck2_error::Result<Self> {
        let spec = config
            .get(BuckconfigKeyRef {
                section: "project",
                property: "ignore",
            })
            .unwrap_or_default();
        Self::from_ignore_spec(spec, root_cell)
    }

    /// Creates an IgnoreSet from an "ignore spec".
    ///
    /// This is modeled after buck1's parsing of project.ignores.
    ///
    /// An ignore spec is a comma-separated list of ignore patterns. If an ignore pattern
    /// contains a glob character, then it uses java.nio.file.FileSystem.getPathMatcher,
    /// otherwise it creates a com.facebook.buck.io.filesystem.RecursivePathMatcher
    ///
    /// Java's path matcher does not allow  '*' to cross directory boundaries. We get
    /// the RecursivePathMatcher behavior by identifying non-globby things and appending
    /// a '/**'.
    ///
    /// Always ignores `buck-out` if it is a `root_cell`.
    fn from_ignore_spec(spec: &str, root_cell: bool) -> buck2_error::Result<Self> {
        // TODO(cjhopman): There's opportunity to greatly improve the performance of IgnoreSet by
        // constructing special cases for a couple of common patterns we see in ignore specs. We
        // know that these can get large wins in some places where we've done this same ignore (watchman, buck1's ignores).
        // `**/filename`: a filename filter. These can all be merged into one hashset lookup.
        // `**/*.ext`: an extension filter. These can all be merged into one hashset lookup.
        // `**/*x*x*`: just some general glob on the filename alone, can merge these into one GlobSet that just needs to check against the filename.
        // `some/prefix/**`: a directory prefix. These can all be merged into one trie lookup.
        let mut patterns = Vec::new();
        let buck_out = if root_cell { Some("buck-out") } else { None };
        for val in buck_out.into_iter().chain(spec.split(',')) {
            let val = val.trim();
            if val.is_empty() {
                continue;
            }

            let val = val.trim_end_matches('/');
            patterns.push(val.to_owned());
        }

        let globset = Self::build_globset(&patterns).map_err(|e| internal_error!("{}", e))?;

        Ok(Self { globset, patterns })
    }

    /// Build a `GlobSet` from the given patterns.
    ///
    /// Glob-containing patterns use `literal_separator(true)`, while plain
    /// directory names are turned into `{name,name/**}` matchers.
    fn build_globset(patterns: &[String]) -> Result<globset::GlobSet, globset::Error> {
        static GLOB_CHARS: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"[*?{\[]").unwrap());

        let mut builder = GlobSetBuilder::new();
        for val in patterns {
            if GLOB_CHARS.is_match(val) {
                builder.add(
                    globset::GlobBuilder::new(val)
                        .literal_separator(true)
                        .build()?,
                );
            } else {
                builder.add(globset::Glob::new(&format!("{{{val},{val}/**}}"))?);
            }
        }
        builder.build()
    }

    /// Returns a pattern that matches the candidate if there is one.
    pub(crate) fn matches_candidate(&self, candidate: &CellRelativePath) -> Option<&str> {
        match self
            .globset
            .matches_candidate(&globset::Candidate::new(candidate.as_str()))
            .as_slice()
        {
            [] => None,
            [v, ..] => Some(&self.patterns[*v]),
        }
    }

    /// Returns whether any pattern matches.
    fn is_match(&self, path: &CellRelativePath) -> bool {
        self.globset.is_match(path.as_str())
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::cells::cell_root_path::CellRootPathBuf;

    use super::*;

    #[test]
    fn test_root_cell_defaults_include_buck_out() {
        let root = CellName::testing_new("root");
        let cells =
            CellResolver::testing_with_name_and_path(root, CellRootPathBuf::testing_new(""));
        let set = IgnoreSet::testing_new_single_cell(cells, root, "").unwrap();
        assert!(set.is_match(ProjectRelativePath::new("buck-out/gen/src/file.txt").unwrap()));
        assert!(set.is_match(ProjectRelativePath::new("buck-out/art/src/file.txt").unwrap()));
        assert!(!set.is_match(ProjectRelativePath::new("src/file.txt").unwrap()));
    }

    #[test]
    fn test_non_root_cell_prefixes_patterns() {
        let root = CellName::testing_new("root");
        let cells =
            CellResolver::testing_with_name_and_path(root, CellRootPathBuf::testing_new(""));
        let set = IgnoreSet::testing_new_single_cell(cells, root, "recursive, some/*").unwrap();
        assert!(set.is_match(ProjectRelativePath::new("recursive").unwrap()));
        assert!(set.is_match(ProjectRelativePath::new("recursive/sub").unwrap()));
        assert!(set.is_match(ProjectRelativePath::new("some/x").unwrap()));
        assert!(!set.is_match(ProjectRelativePath::new("x/recursive").unwrap()));
        assert!(!set.is_match(ProjectRelativePath::new("foo/bar/some/x/y").unwrap()));
    }
}
