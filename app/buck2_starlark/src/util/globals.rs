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
use buck2_interpreter::file_type::StarlarkFileType;
use buck2_interpreter::import_paths::HasImportPaths;
use buck2_interpreter::load_module::InterpreterCalculation;
use buck2_interpreter::load_module::INTERPRETER_CALCULATION_IMPL;
use buck2_interpreter::path::StarlarkPath;
use dice::DiceTransaction;
use dupe::Dupe;
use starlark::environment::Globals;

/// The environment in which a Starlark file is evaluated.
struct Environment {
    /// The globals that are driven from Rust.
    globals: Globals,
    /// The path to the prelude, if the prelude is loaded in this file.
    /// Note that in a BUCK file the `native` value is also exploded into the top-level.
    prelude: Option<ImportPath>,
    /// A path that is implicitly loaded as additional globals.
    preload: Option<ImportPath>,
}

impl Environment {
    async fn new(
        cell: CellName,
        path_type: StarlarkFileType,
        dice: &DiceTransaction,
    ) -> anyhow::Result<Environment> {
        // Find the information from the globals
        let globals = INTERPRETER_CALCULATION_IMPL
            .get()?
            .global_env_for_file_type(dice, path_type)
            .await?;

        // Next grab the prelude, unless we are in the prelude cell and not a build file
        let prelude = match INTERPRETER_CALCULATION_IMPL
            .get()?
            .prelude_import(dice)
            .await?
        {
            Some(prelude) if path_type == StarlarkFileType::Buck || prelude.cell() != cell => {
                Some(prelude)
            }
            _ => None,
        };

        // Now grab the pre-load things
        let preload = dice
            .import_paths_for_cell(BuildFileCell::new(cell))
            .await?
            .root_import()
            .cloned();

        Ok(Environment {
            globals,
            prelude,
            preload,
        })
    }

    async fn get_names(
        &self,
        path_type: StarlarkFileType,
        dice: &DiceTransaction,
    ) -> anyhow::Result<HashSet<String>> {
        let mut names = HashSet::new();

        for x in self.globals.names() {
            names.insert(x.as_str().to_owned());
        }

        if let Some(prelude) = &self.prelude {
            let m = dice.get_loaded_module_from_import_path(prelude).await?;
            for x in m.env().names() {
                names.insert(x.as_str().to_owned());
            }
            if path_type == StarlarkFileType::Buck {
                if let Some(native) = m.env().get_option("native")? {
                    let native = native.value();
                    for attr in native.dir_attr() {
                        names.insert(attr.to_owned());
                    }
                }
            }
        }

        if let Some(preload) = &self.preload {
            let m = dice.get_loaded_module_from_import_path(preload).await?;
            for x in m.env().names() {
                names.insert(x.as_str().to_owned());
            }
        }

        Ok(names)
    }
}

/// The "globals" for a path are defined by its CellName and its path type.
///
/// To compute the globals we need the Rust-level globals, the prelude, and
/// any pre-imported paths. Figuring out the names in those requires evaluating
/// Starlark code, which might fail.
pub(crate) struct CachedGlobals<'a> {
    dice: &'a DiceTransaction,
    cached: HashMap<(CellName, StarlarkFileType), SharedResult<Arc<HashSet<String>>>>,
}

impl<'a> CachedGlobals<'a> {
    pub(crate) fn new(dice: &'a DiceTransaction) -> CachedGlobals<'a> {
        Self {
            dice,
            cached: HashMap::new(),
        }
    }

    async fn compute_names(
        &self,
        cell: CellName,
        path: StarlarkFileType,
    ) -> anyhow::Result<HashSet<String>> {
        let env = Environment::new(cell, path, self.dice).await?;
        env.get_names(path, self.dice).await
    }

    pub(crate) async fn get_names(
        &mut self,
        path: &StarlarkPath<'_>,
    ) -> SharedResult<Arc<HashSet<String>>> {
        let path_type = path.file_type();
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
