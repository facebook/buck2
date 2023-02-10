/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter::functions::dedupe::dedupe;
use buck2_interpreter::functions::sha256::register_sha256;
use buck2_interpreter::globspec::GlobSpec;
use buck2_interpreter::selector::register_select;
use starlark::environment::GlobalsBuilder;
use starlark::environment::LibraryExtension;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::list::AllocList;
use starlark::values::Value;

use crate::interpreter::build_context::BuildContext;

#[starlark_module]
pub fn native_module(builder: &mut GlobalsBuilder) {
    fn glob<'v>(
        include: Vec<String>,
        #[starlark(require = named)] exclude: Option<Vec<String>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let extra = BuildContext::from_context(eval)?;
        let excludes = exclude.unwrap_or_default();
        let spec = GlobSpec::new(&include, &excludes)?;
        let res = extra.resolve_glob(&spec)?.map(|path| path.as_str());
        Ok(eval.heap().alloc(AllocList(res)))
    }

    fn package(eval: &mut Evaluator) -> anyhow::Result<String> {
        // TODO(cjhopman): Is this used much? Can we change it to return a thin wrapper
        // over the Package itself that exposes things like the cell name or fully specified name?
        Ok(BuildContext::from_context(eval)?
            .require_package()?
            .to_string())
    }

    fn package_name(eval: &mut Evaluator) -> anyhow::Result<String> {
        // An (IMO) unfortunate choice in the skylark api is that this just gives the cell-relative
        //  path of the package (which isn't a unique "name" for the package)
        Ok(BuildContext::from_context(eval)?
            .require_package()?
            .cell_relative_path()
            .to_string())
    }

    fn get_base_path(eval: &mut Evaluator) -> anyhow::Result<String> {
        Ok(BuildContext::from_context(eval)?
            .require_package()?
            .cell_relative_path()
            .to_string())
    }

    fn repository_name(eval: &mut Evaluator) -> anyhow::Result<String> {
        // In Buck v1 the repository name has a leading `@` on it, so match that with v2.
        // In practice, most users do `repository_name()[1:]` to drop it.
        Ok(format!(
            "@{}",
            BuildContext::from_context(eval)?.cell_info().name()
        ))
    }

    fn get_cell_name(eval: &mut Evaluator) -> anyhow::Result<String> {
        Ok(BuildContext::from_context(eval)?
            .cell_info()
            .name()
            .to_string())
    }
}

/// Native functions included in all contexts (`BUCK`, `bzl`, `bxl`).
pub fn register_base_natives(registry: &mut GlobalsBuilder) {
    native_module(registry);
    register_select(registry);
    register_sha256(registry);
}

/// Configure globals for all three possible environments: `BUCK`, `bzl` and `bxl`.
pub fn configure_base_globals(
    configure_native_struct: impl FnOnce(&mut GlobalsBuilder),
) -> GlobalsBuilder {
    let starlark_extensions = [
        LibraryExtension::Abs,
        LibraryExtension::Breakpoint,
        LibraryExtension::Debug,
        LibraryExtension::EnumType,
        LibraryExtension::Filter,
        LibraryExtension::Json,
        LibraryExtension::Map,
        LibraryExtension::Partial,
        LibraryExtension::Pprint,
        LibraryExtension::Print,
        LibraryExtension::RecordType,
        LibraryExtension::ExperimentalRegex,
        LibraryExtension::StructType,
    ];
    let mut global_env = GlobalsBuilder::extended_by(&starlark_extensions)
        .with(register_base_natives)
        .with(dedupe);
    global_env.struct_("__internal__", |x| {
        register_base_natives(x);
        // If `native.` symbols need to be added to the global env, they should be done
        // in `configure_build_file_globals()` or
        // `configure_extension_file_globals()`
        for ext in starlark_extensions {
            ext.add(x)
        }
        configure_native_struct(x);
    });
    global_env
}
