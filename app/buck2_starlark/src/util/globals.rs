/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;

use buck2_common::result::SharedError;
use buck2_common::result::SharedResult;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::name::CellName;
use buck2_interpreter::dice::HasCalculationDelegate;
use buck2_interpreter::file_loader::LoadedModule;
use buck2_interpreter::global_interpreter_state::HasGlobalInterpreterState;
use buck2_interpreter::import_paths::HasImportPaths;
use buck2_interpreter::path::StarlarkModulePath;
use buck2_interpreter::path::StarlarkPath;
use dice::DiceTransaction;
use dupe::Dupe;

#[derive(Copy, Dupe, Clone, Debug, Hash, PartialEq, Eq)]
enum PathType {
    Build,
    Load,
    Bxl,
}

impl PathType {
    fn from_path(path: &StarlarkPath<'_>) -> Self {
        match path {
            StarlarkPath::BuildFile(_) => Self::Build,
            StarlarkPath::LoadFile(_) => Self::Load,
            StarlarkPath::BxlFile(_) => Self::Bxl,
        }
    }
}

/// The "globals" for a path are defined by its CellName and its path type.
///
/// To compute the globals we need the Rust-level globals, the prelude, and
/// any pre-imported paths. Figuring out the names in those requires evaluating
/// Starlark code, which might fail.
pub(crate) struct CachedGlobals<'a> {
    dice: &'a DiceTransaction,
    cached: HashMap<(CellName, PathType), SharedResult<Arc<HashSet<String>>>>,
}

impl<'a> CachedGlobals<'a> {
    pub(crate) fn new(dice: &'a DiceTransaction) -> CachedGlobals<'a> {
        Self {
            dice,
            cached: HashMap::new(),
        }
    }

    async fn load_module(&self, path: &ImportPath) -> anyhow::Result<LoadedModule> {
        // This is mostly correct - in theory a .bzl file can evaluate differently based on which cell it is in.
        // In practice, for the stuff we are doing, its close enough just to use the cell it lives in.
        let cell = path.cell();
        let calc = self
            .dice
            .get_interpreter_calculator(cell, BuildFileCell::new(cell))
            .await?;
        calc.eval_module(StarlarkModulePath::LoadFile(path)).await
    }

    async fn compute_names(
        &self,
        cell: CellName,
        path: PathType,
    ) -> anyhow::Result<HashSet<String>> {
        let mut res = HashSet::new();

        // First lets get some interesting state
        // We could cache this in GlobalCache, or compute it in `new`, but its all cached on DICE anyway, so keep it simple
        let global_state = self.dice.get_global_interpreter_state().await?;
        let config = global_state.configuror();

        // Find the information from the globals
        let globals = match path {
            PathType::Build => config.build_file_globals(),
            PathType::Load => config.extension_file_globals(),
            PathType::Bxl => config.bxl_file_globals(),
        };
        for x in globals.names() {
            res.insert(x.as_str().to_owned());
        }

        // Next grab the prelude, unless we are in the prelude cell and not a build file
        if let Some(prelude) = config.prelude_import() {
            if path == PathType::Build || prelude.cell() != cell {
                let env = self.load_module(prelude).await?;
                for x in env.env().names() {
                    res.insert(x.as_str().to_owned());
                }
                if path == PathType::Build {
                    if let Some(native) = env.env().get_option("native")? {
                        let native = native.value();
                        for attr in native.dir_attr() {
                            res.insert(attr.to_owned());
                        }
                    }
                }
            }
        }

        // Now grab the pre-load things
        let import_paths = self
            .dice
            .import_paths_for_cell(BuildFileCell::new(cell))
            .await?;
        if let Some(root) = import_paths.root_import() {
            let env = self.load_module(root).await?;
            for x in env.env().names() {
                res.insert(x.as_str().to_owned());
            }
        }

        Ok(res)
    }

    pub(crate) async fn get_names(
        &mut self,
        path: &StarlarkPath<'_>,
    ) -> SharedResult<Arc<HashSet<String>>> {
        let path_type = PathType::from_path(path);
        let cell = path.cell();
        if let Some(res) = self.cached.get(&(cell, path_type)) {
            return res.dupe();
        }
        let res = match self.compute_names(cell, path_type).await {
            Ok(v) => Ok(Arc::new(v)),
            Err(e) => Err(SharedError::new(e)),
        };
        self.cached.insert((cell, path_type), res.dupe());
        res
    }
}
