/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::sync::Arc;

use allocative::Allocative;
use buck2_common::package_listing::listing::PackageListing;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::target::label::interner::ConcurrentTargetLabelInterner;
use buck2_interpreter::extra::xcode::XcodeVersionInfo;
use buck2_interpreter::extra::InterpreterHostArchitecture;
use buck2_interpreter::extra::InterpreterHostPlatform;
use buck2_interpreter::file_loader::LoadedModules;
use buck2_interpreter::package_imports::ImplicitImport;
use buck2_interpreter::paths::module::StarlarkModulePath;
use buck2_interpreter::prelude_path::PreludePath;
use buck2_node::super_package::SuperPackage;
use dupe::Dupe;
use starlark::environment::Globals;
use starlark::environment::GlobalsBuilder;

use crate::attrs::coerce::ctx::BuildAttrCoercionContext;
use crate::interpreter::cell_info::InterpreterCellInfo;
use crate::interpreter::functions::host_info::HostInfo;
use crate::interpreter::globals::base_globals;
use crate::interpreter::module_internals::ModuleInternals;
use crate::interpreter::module_internals::PackageImplicits;

#[derive(Clone, Dupe, Allocative)]
pub struct AdditionalGlobalsFn(
    #[allocative(skip)] pub Arc<dyn Fn(&mut GlobalsBuilder) + Sync + Send>,
);

impl Debug for AdditionalGlobalsFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AdditionalGlobalsFn").finish()
    }
}

impl PartialEq for AdditionalGlobalsFn {
    fn eq(&self, other: &Self) -> bool {
        // https://rust-lang.github.io/rust-clippy/master/index.html#vtable_address_comparisons
        // `ptr_eq` compares both data addresses and vtables.
        // And if compiler merges or splits vtables, we don't care,
        // because we behavior will be correct either way.
        // Anyway, this code is used only in tests.
        #[allow(ambiguous_wide_pointer_comparisons)]
        Arc::ptr_eq(&self.0, &other.0)
    }
}

#[derive(Clone, Debug, PartialEq, Allocative)]
pub struct BuildInterpreterConfiguror {
    /// Path to prelude import (typically `prelude//:prelude.bzl`).
    ///
    /// It serves two purposes:
    /// * It defines symbols imported into each file (e.g. rule definitions)
    /// * Parent directory of prelude import (e.g. `prelude//`) is considered special:
    ///   imports from that directory are evaluated with prelude cell context,
    ///   not with caller cell context (see the comments in `resolve_load`)
    prelude_import: Option<PreludePath>,
    host_info: HostInfo,
    record_target_call_stack: bool,
    skip_targets_with_duplicate_names: bool,
    global_target_interner: Arc<ConcurrentTargetLabelInterner>,
    /// For test.
    additional_globals: Option<AdditionalGlobalsFn>,
}

impl BuildInterpreterConfiguror {
    pub fn new(
        prelude_import: Option<PreludePath>,
        host_platform: InterpreterHostPlatform,
        host_architecture: InterpreterHostArchitecture,
        host_xcode_version: Option<XcodeVersionInfo>,
        record_target_call_stack: bool,
        skip_targets_with_duplicate_names: bool,
        additional_globals: Option<AdditionalGlobalsFn>,
        global_target_interner: Arc<ConcurrentTargetLabelInterner>,
    ) -> buck2_error::Result<Arc<Self>> {
        Ok(Arc::new(Self {
            prelude_import,
            host_info: HostInfo::new(host_platform, host_architecture, host_xcode_version),
            record_target_call_stack,
            skip_targets_with_duplicate_names,
            additional_globals,
            global_target_interner,
        }))
    }

    pub(crate) fn globals(&self) -> Globals {
        base_globals()
            .with(|g| {
                if let Some(additional_globals) = &self.additional_globals {
                    (additional_globals.0)(g);
                }
            })
            .build()
    }

    pub fn host_info(&self) -> &HostInfo {
        &self.host_info
    }

    pub(crate) fn new_extra_context(
        &self,
        cell_info: &InterpreterCellInfo,
        buildfile_path: BuildFilePath,
        package_listing: PackageListing,
        super_package: SuperPackage,
        package_boundary_exception: bool,
        loaded_modules: &LoadedModules,
        implicit_import: Option<&Arc<ImplicitImport>>,
    ) -> buck2_error::Result<ModuleInternals> {
        let record_target_call_stack = self.record_target_call_stack;
        let skip_targets_with_duplicate_names = self.skip_targets_with_duplicate_names;
        let package_implicits = implicit_import.map(|spec| {
            PackageImplicits::new(
                spec.dupe(),
                loaded_modules
                    .map
                    .get(&StarlarkModulePath::LoadFile(spec.import()))
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
            cell_info.cell_resolver().dupe(),
            cell_info.cell_alias_resolver().dupe(),
            (buildfile_path.package().dupe(), package_listing.dupe()),
            package_boundary_exception,
            self.global_target_interner.dupe(),
        );

        let imports = loaded_modules.imports().cloned().collect();

        Ok(ModuleInternals::new(
            attr_coercer,
            Arc::new(buildfile_path),
            imports,
            package_implicits,
            record_target_call_stack,
            skip_targets_with_duplicate_names,
            package_listing,
            super_package,
        ))
    }

    pub fn prelude_import(&self) -> Option<&PreludePath> {
        self.prelude_import.as_ref()
    }
}
