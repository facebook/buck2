/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_core::cells::cell_root_path::CellRootPath;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use globset::Candidate;
use globset::GlobSetBuilder;
use once_cell::sync::Lazy;
use regex::Regex;

#[derive(Debug, Allocative)]
pub struct IgnoreSet {
    #[allocative(skip)]
    globset: globset::GlobSet,
    // We keep patterns so that error messages can refer to the specific pattern that was matched.
    // This should be in the same order as the strings were added to the GlobSet to match the indices returned from it.
    patterns: Vec<String>,
}

impl PartialEq for IgnoreSet {
    fn eq(&self, other: &Self) -> bool {
        // Only compare patterns because globset is derived from patterns.
        self.patterns == other.patterns
    }
}

impl Eq for IgnoreSet {}

impl IgnoreSet {
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
    pub fn from_ignore_spec(spec: &str) -> anyhow::Result<Self> {
        // TODO(cjhopman): There's opportunity to greatly improve the performance of IgnoreSet by
        // constructing special cases for a couple of common patterns we see in ignore specs. We
        // know that these can get large wins in some places where we've done this same ignore (watchman, buck1's ignores).
        // `**/filename`: a filename filter. These can all be merged into one hashset lookup.
        // `**/*.ext`: an extension filter. These can all be merged into one hashset lookup.
        // `**/*x*x*`: just some general glob on the filename alone, can merge these into one GlobSet that just needs to check against the filename.
        // `some/prefix/**`: a directory prefix. These can all be merged into one trie lookup.
        let mut patterns_builder = GlobSetBuilder::new();
        let mut patterns = Vec::new();
        for val in spec.split(',') {
            let val = val.trim();
            if val.is_empty() {
                continue;
            }

            let val = val.trim_end_matches('/');

            static GLOB_CHARS: Lazy<Regex> = Lazy::new(|| Regex::new(r"[*?{\[]").unwrap());

            if GLOB_CHARS.is_match(val) {
                patterns_builder.add(
                    globset::GlobBuilder::new(val)
                        .literal_separator(true)
                        .build()?,
                );
            } else {
                patterns_builder.add(globset::Glob::new(&format!("{{{},{}/**}}", val, val))?);
            }
            patterns.push(val.to_owned());
        }

        Ok(Self {
            globset: patterns_builder.build()?,
            patterns,
        })
    }

    /// Constructs an IgnoreSet that will ignore anything contained in a deeper cell.
    ///
    /// Ex. if this cell's path is `some/cell` and other cells are at `.`, `other`,
    /// `some/cell/deeper`, this would construct an IgnoreSet to ignore `deeper/**`
    /// (note that these ignores are expected to receive cell-relative paths.)
    pub(crate) fn from_cell_roots(
        all_cells: &[(CellName, &CellRootPath)],
        this_cell: &CellRootPath,
    ) -> anyhow::Result<Self> {
        let mut cells_builder = GlobSetBuilder::new();
        let mut cell_names = Vec::new();
        for (name, path) in all_cells {
            if *path == this_cell {
                continue;
            }

            if !path.starts_with(this_cell) {
                continue;
            }

            let relative = path.strip_prefix(this_cell).unwrap();
            cells_builder.add(globset::Glob::new(&format!(
                "{{{},{}/**}}",
                relative, relative
            ))?);
            cell_names.push(name.as_str().to_owned());
        }

        Ok(Self {
            globset: cells_builder.build()?,
            patterns: cell_names,
        })
    }

    /// Returns a pattern that matches the candidate if there is one.
    pub(crate) fn matches_candidate(&self, candidate: &Candidate) -> Option<&str> {
        match self.globset.matches_candidate(candidate).as_slice() {
            [] => None,
            [v, ..] => Some(&self.patterns[*v]),
        }
    }

    /// Returns whether any pattern matches.
    pub fn is_match(&self, path: &CellRelativePath) -> bool {
        self.globset.is_match(path.as_str())
    }
}
