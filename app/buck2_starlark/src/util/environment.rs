/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;

use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::name::CellName;
use buck2_interpreter::file_type::StarlarkFileType;
use buck2_interpreter::import_paths::HasImportPaths;
use buck2_interpreter::load_module::InterpreterCalculation;
use buck2_interpreter::load_module::INTERPRETER_CALCULATION_IMPL;
use buck2_interpreter::prelude_path::PreludePath;
use dice::DiceTransaction;
use starlark::environment::Globals;

/// The environment in which a Starlark file is evaluated.
pub(crate) struct Environment {
    /// The globals that are driven from Rust.
    pub(crate) globals: Globals,
    /// The path to the prelude, if the prelude is loaded in this file.
    /// Note that in a BUCK file the `native` value is also exploded into the top-level.
    prelude: Option<PreludePath>,
    /// A path that is implicitly loaded as additional globals.
    preload: Option<ImportPath>,
}

impl Environment {
    pub(crate) async fn new(
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
            Some(prelude)
                if path_type == StarlarkFileType::Buck || prelude.import_path().cell() != cell =>
            {
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

    pub(crate) async fn get_names(
        &self,
        path_type: StarlarkFileType,
        dice: &DiceTransaction,
    ) -> anyhow::Result<HashSet<String>> {
        let mut names = HashSet::new();

        for x in self.globals.names() {
            names.insert(x.as_str().to_owned());
        }

        if let Some(prelude) = &self.prelude {
            let m = dice
                .get_loaded_module_from_import_path(prelude.import_path())
                .await?;
            for x in m.env().names() {
                names.insert(x.as_str().to_owned());
            }
            if path_type == StarlarkFileType::Buck {
                for (name, _value) in m.extra_globals_from_prelude_for_buck_files()? {
                    names.insert(name.to_owned());
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
