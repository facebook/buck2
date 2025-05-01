/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::sync::Arc;

use buck2_common::buildfiles::parse_buildfile_name;
use buck2_common::invocation_roots::InvocationRoots;
use buck2_common::legacy_configs::cells::BuckConfigBasedCells;
use buck2_core::cells::name::CellName;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::file_name::FileNameBuf;

use super::path_sanitizer::SanitizedPath;

pub(crate) struct CompletionResults<'a> {
    roots: &'a InvocationRoots,
    cell_configs: Arc<BuckConfigBasedCells>,
    buildfiles: HashMap<CellName, Vec<FileNameBuf>>,
    results: BTreeSet<String>,
}

impl<'a> CompletionResults<'a> {
    pub(crate) fn new(roots: &'a InvocationRoots, cell_configs: Arc<BuckConfigBasedCells>) -> Self {
        Self {
            roots,
            cell_configs,
            buildfiles: HashMap::new(),
            results: BTreeSet::<String>::new(),
        }
    }

    pub(crate) fn insert(&mut self, target: &str) -> &mut Self {
        self.results.insert(target.to_owned());
        self
    }

    pub(crate) async fn insert_path(&mut self, path: &SanitizedPath) -> &mut Self {
        self.insert_dir(&path.abs_path(), path.given()).await
    }

    pub(crate) async fn insert_dir(&mut self, abs_dir: &AbsNormPath, nickname: &str) -> &mut Self {
        if nickname.ends_with("//") {
            self.insert(nickname);
            self.insert_package_colon_if_buildfile_exists(abs_dir, nickname)
                .await;
        } else if nickname.ends_with('/') {
            self.insert(nickname);
        } else {
            self.insert(&format!("{}/", nickname));
            self.insert_package_colon_if_buildfile_exists(abs_dir, nickname)
                .await;
        }
        self
    }

    pub(crate) async fn insert_package_colon_if_buildfile_exists(
        &mut self,
        abs_dir: &AbsNormPath,
        nickname: &str,
    ) -> &mut Self {
        for f in self.buildfile_names(abs_dir).await.unwrap() {
            if abs_dir.join(f).exists() {
                self.insert(&format!("{}:", nickname));
                break;
            }
        }
        self
    }

    async fn buildfile_names(
        &mut self,
        abs_dir: &AbsNormPath,
    ) -> buck2_error::Result<&Vec<FileNameBuf>> {
        let relative_to_project = self.roots.project_root.relativize(abs_dir)?;
        let cell_configs = &self.cell_configs;
        let cell_name = cell_configs.cell_resolver.find(&relative_to_project)?;
        if let Entry::Vacant(e) = self.buildfiles.entry(cell_name) {
            let cell_config = cell_configs
                .parse_single_cell(cell_name, &self.roots.project_root)
                .await?;
            e.insert(parse_buildfile_name(&cell_config)?);
        }
        Ok(self.buildfiles.get(&cell_name).unwrap())
    }
}

impl<'a> From<CompletionResults<'a>> for Vec<String> {
    fn from(pr: CompletionResults) -> Self {
        pr.results.iter().map(String::from).collect()
    }
}
