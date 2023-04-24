/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::none::NoneOr;
use starlark::values::StringValue;
use starlark::values::StringValueLike;
use starlark::values::Value;

use crate::interpreter::build_context::BuildContext;

#[starlark_module]
pub fn register_read_config(globals: &mut GlobalsBuilder) {
    /// Read a configuration from the nearest enclosing `.buckconfig`
    /// of the `BUCK` file that started evaluation of this code.
    ///
    /// As an example, if you have a `.buckconfig` of:
    ///
    /// ```toml
    /// [package_options]
    /// compile = super_fast
    /// ```
    ///
    /// Then you would get the following results:
    ///
    /// ```python
    /// read_config("package_options", "compile") == "super_fast"
    /// read_config("package_options", "linker") == None
    /// read_config("package_options", "linker", "a_default") == "a_default"
    /// ```
    ///
    /// In general the use of `.buckconfig` is discouraged in favour of `select`,
    /// but it can still be useful.
    #[starlark(speculative_exec_safe)]
    fn read_config<'v>(
        section: StringValue,
        key: StringValue,
        default: Option<Value<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let buckconfig = &BuildContext::from_context(eval)?.buckconfig;
        match buckconfig.get(section, key)? {
            Some(v) => Ok(v.to_value()),
            None => Ok(default.unwrap_or_else(Value::new_none)),
        }
    }

    /// Like `read_root_config` but the project root `.buckconfig` is always consulted,
    /// regardless of the cell of the originating `BUCK` file.
    #[starlark(speculative_exec_safe)]
    fn read_root_config<'v>(
        #[starlark(require = pos)] section: StringValue,
        #[starlark(require = pos)] key: StringValue,
        // Unlike `read_config` we only allow string or `None` as default.
        #[starlark(require = pos, default = NoneOr::None)] default: NoneOr<StringValue<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<NoneOr<StringValue<'v>>> {
        let buckconfig = &BuildContext::from_context(eval)?.root_buckconfig;
        match buckconfig.get(section, key)? {
            Some(v) => Ok(NoneOr::Other(v.to_string_value())),
            None => Ok(default),
        }
    }
}
