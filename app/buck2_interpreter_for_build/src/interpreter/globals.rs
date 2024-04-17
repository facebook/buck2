/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter::downstream_crate_starlark_defs::REGISTER_BUCK2_ANON_TARGETS_GLOBALS;
use buck2_interpreter::downstream_crate_starlark_defs::REGISTER_BUCK2_BUILD_API_GLOBALS;
use buck2_interpreter::downstream_crate_starlark_defs::REGISTER_BUCK2_BXL_GLOBALS;
use buck2_interpreter::downstream_crate_starlark_defs::REGISTER_BUCK2_CFG_CONSTRUCTOR_GLOBALS;
use buck2_interpreter::downstream_crate_starlark_defs::REGISTER_BUCK2_TRANSITION_GLOBALS;
use buck2_interpreter::starlark_promise::register_promise;
use buck2_interpreter::types::cell_path::register_cell_path;
use buck2_interpreter::types::cell_root::register_cell_root;
use buck2_interpreter::types::configured_providers_label::register_providers_label;
use buck2_interpreter::types::regex::register_buck_regex;
use buck2_interpreter::types::target_label::register_target_label;
use buck2_util::late_binding::LateBinding;
use starlark::environment::GlobalsBuilder;
use starlark::environment::LibraryExtension;

use crate::attrs::attrs_global::register_attrs;
use crate::interpreter::functions::dedupe::register_dedupe;
use crate::interpreter::functions::host_info::register_host_info;
use crate::interpreter::functions::internals::register_internals;
use crate::interpreter::functions::load_symbols::register_load_symbols;
use crate::interpreter::functions::path::register_path;
use crate::interpreter::functions::read_config::register_read_config;
use crate::interpreter::functions::regex::register_regex;
use crate::interpreter::functions::sha256::register_sha256;
use crate::interpreter::functions::soft_error::register_soft_error;
use crate::interpreter::functions::starlark::register_set_starlark_peak_allocated_byte_limit;
use crate::interpreter::functions::warning::register_warning;
use crate::interpreter::natives::register_module_natives;
use crate::interpreter::selector::register_select;
use crate::plugins::register_plugins;
use crate::rule::register_rule_function;
use crate::super_package::defs::register_package_natives;
use crate::super_package::package_value::register_read_package_value;

fn from_late_binding(l: &LateBinding<fn(&mut GlobalsBuilder)>, builder: &mut GlobalsBuilder) {
    if let Ok(v) = l.get() {
        v(builder);
    }
}

fn register_universal_natives(builder: &mut GlobalsBuilder) {
    from_late_binding(&REGISTER_BUCK2_BUILD_API_GLOBALS, builder);
    from_late_binding(&REGISTER_BUCK2_TRANSITION_GLOBALS, builder);
    from_late_binding(&REGISTER_BUCK2_BXL_GLOBALS, builder);
    from_late_binding(&REGISTER_BUCK2_CFG_CONSTRUCTOR_GLOBALS, builder);
    register_module_natives(builder);
    register_host_info(builder);
    register_read_config(builder);
    register_read_package_value(builder);
    register_soft_error(builder);
    register_package_natives(builder);
    register_warning(builder);
    register_regex(builder);
    register_buck_regex(builder);
    register_load_symbols(builder);
    register_rule_function(builder);
    register_attrs(builder);
    register_plugins(builder);
    register_providers_label(builder);
    register_cell_path(builder);
    register_cell_root(builder);
    register_target_label(builder);
    register_path(builder);
    register_select(builder);
    register_promise(builder);
    register_sha256(builder);
    register_dedupe(builder);
    register_set_starlark_peak_allocated_byte_limit(builder);
    from_late_binding(&REGISTER_BUCK2_ANON_TARGETS_GLOBALS, builder);
}

pub fn starlark_library_extensions_for_buck2() -> &'static [LibraryExtension] {
    &[
        LibraryExtension::Breakpoint,
        LibraryExtension::Debug,
        LibraryExtension::EnumType,
        LibraryExtension::Filter,
        LibraryExtension::Json,
        LibraryExtension::Map,
        LibraryExtension::Partial,
        LibraryExtension::Pprint,
        LibraryExtension::Pstr,
        LibraryExtension::Prepr,
        LibraryExtension::Print,
        LibraryExtension::RecordType,
        LibraryExtension::StructType,
        LibraryExtension::Typing,
        LibraryExtension::Internal,
        LibraryExtension::CallStack,
    ]
}

/// The standard set of globals that is available in all files.
///
/// This does not include the implicit prelude and cell imports which are only available in `BUCK`
/// files, but does include everything else.
pub fn base_globals() -> GlobalsBuilder {
    let starlark_extensions = starlark_library_extensions_for_buck2();
    let mut global_env =
        GlobalsBuilder::extended_by(starlark_extensions).with(register_universal_natives);
    global_env.struct_("__internal__", |x| {
        register_internals(x);
        for ext in starlark_extensions {
            ext.add(x)
        }
        register_universal_natives(x);
    });
    global_env
}
