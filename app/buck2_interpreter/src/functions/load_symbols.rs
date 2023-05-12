/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use starlark::collections::SmallMap;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::values::none::NoneType;
use starlark::values::Value;

#[starlark_module]
pub fn register_load_symbols(builder: &mut GlobalsBuilder) {
    /// Used in a `.bzl` file to set exported symbols. In most cases just defining
    /// the symbol as a top-level binding is sufficient, but sometimes the names
    /// might be programatically generated.
    ///
    /// It is undefined behaviour if you try and use any of the symbols exported
    /// here later in the same module, or if they overlap with existing definitions.
    /// This function should be used rarely.
    fn load_symbols<'v>(
        symbols: SmallMap<&'v str, Value<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<NoneType> {
        for (k, v) in symbols.into_iter() {
            eval.set_module_variable_at_some_point(k, v)?;
        }
        Ok(NoneType)
    }
}
