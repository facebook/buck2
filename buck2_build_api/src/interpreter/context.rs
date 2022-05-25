/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{borrow::Borrow, fmt::Debug, ptr, sync::Arc};

use buck2_core::{
    cells::{
        paths::{CellPath, CellRelativePath},
        CellName,
    },
    facebook_only,
    fs::paths::FileName,
    result::SharedResult,
};
use buck2_interpreter::{
    common::{BuildFileCell, BuildFilePath, ImportPath, StarlarkPath},
    extra::{
        cell_info::InterpreterCellInfo, ExtraContextDyn, InterpreterConfiguror,
        InterpreterHostPlatform,
    },
    file_loader::{LoadedModule, LoadedModules},
    package_imports::ImplicitImport,
    package_listing::listing::PackageListing,
};
use gazebo::{cmp::PartialEqAny, prelude::*};
use starlark::environment::{GlobalsBuilder, LibraryExtension};

use crate::interpreter::{
    build_defs::register_natives,
    module_internals::{ModuleInternals, PackageImplicits},
    rule_defs::{
        attr::BuildAttrCoercionContext, cmd_args::register_args_function,
        command_executor_config::register_command_executor_config, register_rule_defs,
        transition::starlark::register_transition_defs,
    },
};

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
    record_target_call_stack: bool,
    configure_build_file_globals: ConfigureGlobalsFn,
    configure_extension_file_globals: ConfigureGlobalsFn,
    configure_bxl_file_globals: ConfigureGlobalsFn,
}

impl BuildInterpreterConfiguror {
    pub fn new(
        prelude_import: Option<ImportPath>,
        host_platform: InterpreterHostPlatform,
        record_target_call_stack: bool,
        configure_build_file_globals: fn(&mut GlobalsBuilder),
        configure_extension_file_globals: fn(&mut GlobalsBuilder),
        configure_bxl_file_globals: fn(&mut GlobalsBuilder),
    ) -> Arc<Self> {
        Arc::new(Self {
            prelude_import,
            host_platform,
            record_target_call_stack,
            configure_build_file_globals: ConfigureGlobalsFn(configure_build_file_globals),
            configure_extension_file_globals: ConfigureGlobalsFn(configure_extension_file_globals),
            configure_bxl_file_globals: ConfigureGlobalsFn(configure_bxl_file_globals),
        })
    }
}

// TODO(cjhopman): We need to figure out some other way to deal with this.
pub fn fbcode_prelude() -> ImportPath {
    facebook_only();
    let prelude_cell = CellName::unchecked_new("fbcode".to_owned());
    let prelude_dir = CellRelativePath::unchecked_new("buck2/prelude");

    ImportPath::new(
        CellPath::new(
            prelude_cell.clone(),
            prelude_dir.join_unnormalized(FileName::unchecked_new("prelude.bzl")),
        ),
        BuildFileCell::new(prelude_cell),
    )
    .unwrap()
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
    fn configure_build_file_globals(&self, globals_builder: &mut GlobalsBuilder) {
        (self.configure_build_file_globals.0)(globals_builder);
    }

    fn configure_extension_file_globals(&self, globals_builder: &mut GlobalsBuilder) {
        (self.configure_extension_file_globals.0)(globals_builder);
    }

    fn configure_bxl_file_globals(&self, globals_builder: &mut GlobalsBuilder) {
        (self.configure_bxl_file_globals.0)(globals_builder);
    }

    fn configure_native_struct(&self, native_module: &mut GlobalsBuilder) {
        register_natives(native_module);
        register_args_function(native_module);
        // FIXME(ndmitchell): Not sure what set of things should be on native here
        LibraryExtension::Map.add(native_module);
        LibraryExtension::Filter.add(native_module);
        LibraryExtension::Partial.add(native_module);
    }

    fn host_platform(&self) -> InterpreterHostPlatform {
        self.host_platform
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
        );

        let imports = LoadedModule::imports_from_loaded_modules(loaded_modules);

        Ok(box ModuleInternals::new(
            attr_coercer,
            Arc::new(buildfile_path),
            imports,
            package_implicits,
            cell_info.default_visibility_to_public(),
            self.record_target_call_stack,
        ))
    }

    fn get_prelude_import(
        &self,
        import: StarlarkPath,
    ) -> Option<&buck2_interpreter::common::ImportPath> {
        if let Some(prelude_import) = &self.prelude_import {
            let import_path = import.path();
            let prelude_path = prelude_import.path();

            // Only return the prelude for things outside the prelude directory.
            if import.unpack_build_file().is_some()
                || !is_prelude_path(import_path.borrow(), prelude_path)
            {
                return Some(prelude_import);
            }
        }

        None
    }

    fn is_prelude_path(&self, path: &CellPath) -> bool {
        if let Some(prelude_import) = &self.prelude_import {
            let prelude_path = prelude_import.path();
            return is_prelude_path(path, prelude_path);
        }

        false
    }

    fn eq_token(&self) -> PartialEqAny {
        PartialEqAny::new(self)
    }
}

fn is_prelude_path(import_path: &CellPath, prelude_path: &CellPath) -> bool {
    import_path.cell() == prelude_path.cell()
        && import_path.path().starts_with(
            prelude_path
                .path()
                .parent()
                .expect("prelude should have a dir"),
        )
}
