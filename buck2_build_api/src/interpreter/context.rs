/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_core::cells::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::facebook_only;
use starlark::environment::GlobalsBuilder;

use crate::interpreter::build_defs::register_natives;
use crate::interpreter::rule_defs::cmd_args::register_args_function;
use crate::interpreter::rule_defs::command_executor_config::register_command_executor_config;
use crate::interpreter::rule_defs::register_rule_defs;
use crate::interpreter::rule_defs::transition::starlark::register_transition_defs;

// TODO(cjhopman): We need to figure out some other way to deal with this.
pub fn prelude_path(cells: &CellResolver) -> ImportPath {
    let prelude_cell = CellName::unchecked_new("prelude".to_owned());
    if cells.contains(&prelude_cell) {
        let prelude_file = CellRelativePathBuf::unchecked_new("prelude.bzl".to_owned());
        ImportPath::new(
            CellPath::new(prelude_cell.clone(), prelude_file),
            BuildFileCell::new(prelude_cell),
        )
        .unwrap()
    } else {
        // DEPRECATED: We want to require the prelude is in a prelude cell, or otherwise have no prelude.
        //             We plan to eliminate the fbcode located prelude cell.
        facebook_only();
        let prelude_cell = CellName::unchecked_new("fbcode".to_owned());
        let prelude_file =
            CellRelativePathBuf::unchecked_new("buck2/prelude/prelude.bzl".to_owned());

        ImportPath::new(
            CellPath::new(prelude_cell.clone(), prelude_file),
            BuildFileCell::new(prelude_cell),
        )
        .unwrap()
    }
}

pub fn configure_build_file_globals(globals_builder: &mut GlobalsBuilder) {
    // TODO(cjhopman): This unconditionally adds the native symbols to the global
    // env, but that needs to be a cell-based config.
    register_natives(globals_builder);
    register_args_function(globals_builder);
}

pub fn configure_extension_file_globals(globals_builder: &mut GlobalsBuilder) {
    // TODO(cjhopman): This unconditionally adds the native symbols to the global
    // env, but that needs to be a cell-based config.
    register_natives(globals_builder);
    register_args_function(globals_builder);
    register_rule_defs(globals_builder);
    register_transition_defs(globals_builder);
    register_command_executor_config(globals_builder);
}
