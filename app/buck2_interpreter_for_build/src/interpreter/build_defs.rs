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
use crate::interpreter::module_internals::ModuleInternals;

#[starlark_module]
pub fn native_module(builder: &mut GlobalsBuilder) {
    /// The `glob()` function specifies a set of files using patterns.
    /// Only available from `BUCK` files.
    ///
    /// A typical `glob` call looks like:
    ///
    /// ```python
    /// glob(["foo/**/*.h"])
    /// ```
    ///
    /// This call will match all header files in the `foo` directory, recursively.
    ///
    /// You can also pass a named `exclude` parameter to remove files matching a pattern:
    ///
    /// ```python
    /// glob(["foo/**/*.h"], exclude = ["**/config.h"])
    /// ```
    ///
    /// This call will remove all `config.h` files from the initial match.
    ///
    /// The `glob()` call is evaluated against the list of files owned by this `BUCK` file.
    /// A file is owned by whichever `BUCK` file is closest above it - so given `foo/BUCK` and
    /// `foo/bar/BUCK` the file `foo/file.txt` would be owned by `foo/BUCK` (and available from
    /// its `glob` results) but the file `foo/bar/file.txt` would be owned by `foo/bar/BUCk`
    /// and _not_ appear in the glob result of `foo/BUCK`, even if you write `glob(["bar/file.txt"])`.
    /// As a consequence of this rule, `glob(["../foo.txt"])` will always return an empty list of files.
    ///
    /// Currently `glob` is evaluated case-insensitively on all file systems, but we expect
    /// that to change to case sensitive in the near future.
    #[starlark(return_type = "[str.type]")]
    fn glob<'v>(
        include: Vec<String>,
        #[starlark(require = named, default=Vec::new())] exclude: Vec<String>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let extra = ModuleInternals::from_context(eval, "glob")?;
        let spec = GlobSpec::new(&include, &exclude)?;
        let res = extra.resolve_glob(&spec).map(|path| path.as_str());
        Ok(eval.heap().alloc(AllocList(res)))
    }

    /// `package_name()` can only be called in `BUCK` files, and returns the name of the package.
    /// E.g. inside `foo//bar/baz/BUCK` the output will be `bar/baz`.
    fn package_name(eval: &mut Evaluator) -> anyhow::Result<String> {
        // An (IMO) unfortunate choice in the skylark api is that this just gives the cell-relative
        //  path of the package (which isn't a unique "name" for the package)
        Ok(BuildContext::from_context(eval)?
            .require_package()?
            .cell_relative_path()
            .to_string())
    }

    /// `get_base_path()` can only be called in `BUCK` files, and returns the name of the package.
    /// E.g. inside `foo//bar/baz/BUCK` the output will be `bar/baz`.
    ///
    /// This function is identical to `package_name`.
    fn get_base_path(eval: &mut Evaluator) -> anyhow::Result<String> {
        Ok(BuildContext::from_context(eval)?
            .require_package()?
            .cell_relative_path()
            .to_string())
    }

    /// Like `get_cell_name()` but prepends a leading `@` for compatibility with Buck1.
    /// You should call `get_cell_name()` instead, and if you really want the `@`,
    /// prepend it yourself.
    fn repository_name(eval: &mut Evaluator) -> anyhow::Result<String> {
        // In Buck v1 the repository name has a leading `@` on it, so match that with v2.
        // In practice, most users do `repository_name()[1:]` to drop it.
        Ok(format!(
            "@{}",
            BuildContext::from_context(eval)?.cell_info().name()
        ))
    }

    /// `get_cell_name()` can be called from either a `BUCK` file or a `.bzl` file,
    /// and returns the name of the cell where the `BUCK` file that started the call
    /// lives.
    ///
    /// For example, inside `foo//bar/baz/BUCK` the output will be `foo`.
    /// If that `BUCK` file does a `load("hello//world.bzl", "something")` then
    /// the result in that `.bzl` file will also be `foo`.
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
