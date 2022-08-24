/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::ptr;
use std::sync::Arc;

use buck2_common::package_listing::listing::PackageListing;
use buck2_common::result::SharedResult;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_core::cells::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::facebook_only;
use buck2_interpreter::extra::cell_info::InterpreterCellInfo;
use buck2_interpreter::extra::ExtraContextDyn;
use buck2_interpreter::extra::InterpreterConfiguror;
use buck2_interpreter::extra::InterpreterHostArchitecture;
use buck2_interpreter::extra::InterpreterHostPlatform;
use buck2_interpreter::file_loader::LoadedModules;
use buck2_interpreter::interpreter::configure_base_globals;
use buck2_interpreter::package_imports::ImplicitImport;
use buck2_interpreter_for_build::attrs::coerce::ctx::BuildAttrCoercionContext;
use buck2_interpreter_for_build::interpreter::module_internals::ModuleInternals;
use buck2_interpreter_for_build::interpreter::module_internals::PackageImplicits;
use gazebo::cmp::PartialEqAny;
use gazebo::prelude::*;
use starlark::environment::Globals;
use starlark::environment::GlobalsBuilder;

use crate::interpreter::build_defs::register_natives;
use crate::interpreter::rule_defs::cmd_args::register_args_function;
use crate::interpreter::rule_defs::command_executor_config::register_command_executor_config;
use crate::interpreter::rule_defs::register_rule_defs;
use crate::interpreter::rule_defs::transition::starlark::register_transition_defs;
use crate::query::analysis::environment::ConfiguredGraphQueryEnvironment;

#[derive(Clone)]
struct ConfigureGlobalsFn(fn(&mut GlobalsBuilder));

impl Debug for ConfigureGlobalsFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ConfiguredGlobalsFn")
    }
}

impl PartialEq for ConfigureGlobalsFn {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0 as *const (), other.0 as *const ())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BuildInterpreterConfiguror {
    prelude_import: Option<ImportPath>,
    host_platform: InterpreterHostPlatform,
    host_architecture: InterpreterHostArchitecture,
    record_target_call_stack: bool,
    configure_build_file_globals: ConfigureGlobalsFn,
    configure_extension_file_globals: ConfigureGlobalsFn,
    configure_bxl_file_globals: ConfigureGlobalsFn,
}

impl BuildInterpreterConfiguror {
    pub fn new(
        prelude_import: Option<ImportPath>,
        host_platform: InterpreterHostPlatform,
        host_architecture: InterpreterHostArchitecture,
        record_target_call_stack: bool,
        configure_build_file_globals: fn(&mut GlobalsBuilder),
        configure_extension_file_globals: fn(&mut GlobalsBuilder),
        configure_bxl_file_globals: fn(&mut GlobalsBuilder),
    ) -> Arc<Self> {
        Arc::new(Self {
            prelude_import,
            host_platform,
            host_architecture,
            record_target_call_stack,
            configure_build_file_globals: ConfigureGlobalsFn(configure_build_file_globals),
            configure_extension_file_globals: ConfigureGlobalsFn(configure_extension_file_globals),
            configure_bxl_file_globals: ConfigureGlobalsFn(configure_bxl_file_globals),
        })
    }
}

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

impl InterpreterConfiguror for BuildInterpreterConfiguror {
    fn build_file_globals(&self) -> Globals {
        // We want the `native` module to contain most things, so match what is in extension files
        configure_base_globals(self.configure_extension_file_globals.0)
            .with(self.configure_build_file_globals.0)
            .build()
    }

    fn extension_file_globals(&self) -> Globals {
        configure_base_globals(self.configure_extension_file_globals.0)
            .with(self.configure_extension_file_globals.0)
            .build()
    }

    fn bxl_file_globals(&self) -> Globals {
        configure_base_globals(self.configure_extension_file_globals.0)
            .with(self.configure_bxl_file_globals.0)
            .build()
    }

    fn host_platform(&self) -> InterpreterHostPlatform {
        self.host_platform
    }

    fn host_architecture(&self) -> InterpreterHostArchitecture {
        self.host_architecture
    }

    fn new_extra_context(
        &self,
        cell_info: &InterpreterCellInfo,
        buildfile_path: BuildFilePath,
        package_listing: PackageListing,
        package_boundary_exception: bool,
        loaded_modules: &LoadedModules,
        implicit_import: Option<&Arc<ImplicitImport>>,
    ) -> SharedResult<Box<dyn ExtraContextDyn>> {
        let package_implicits = implicit_import.map(|spec| {
            PackageImplicits::new(
                spec.dupe(),
                loaded_modules
                    .map
                    .get(spec.import().id().as_str())
                    .unwrap_or_else(|| {
                        panic!(
                            "Should've had an env for the package implicit import (`{}`).",
                            spec.import(),
                        )
                    })
                    .env()
                    .dupe(),
            )
        });
        let attr_coercer = BuildAttrCoercionContext::new_with_package(
            cell_info.cell_alias_resolver().dupe(),
            (buildfile_path.package().dupe(), package_listing),
            package_boundary_exception,
            Arc::new(ConfiguredGraphQueryEnvironment::functions()),
        );

        let imports = loaded_modules.imports().cloned().collect();

        Ok(box ModuleInternals::new(
            attr_coercer,
            Arc::new(buildfile_path),
            imports,
            package_implicits,
            cell_info.default_visibility_to_public(),
            self.record_target_call_stack,
        ))
    }

    fn prelude_import(&self) -> Option<&ImportPath> {
        self.prelude_import.as_ref()
    }

    fn eq_token(&self) -> PartialEqAny {
        PartialEqAny::new(self)
    }
}
