/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use gazebo::prelude::*;
use starlark::environment::GlobalsBuilder;
use starlark::values::StringValue;
use starlark::values::Value;

use crate::interpreter::rule_defs::cmd_args::options::QuoteStyle;

mod builder;
mod options;
mod traits;
mod typ;
pub(crate) mod value;
pub mod value_as;

pub use builder::*;
pub use traits::*;
pub use typ::*;

#[starlark_module]
pub fn register_cmd_args(builder: &mut GlobalsBuilder) {
    #[starlark(type = "cmd_args")]
    /// The `cmd_args` type is created by this function and is consumed by `ctx.actions.run`.
    /// The type is a mutable collection of strings and artifact values.
    /// In general, command lines, artifacts, strings, `RunInfo` and lists thereof can be added to or used to construct a `cmd_args` value.
    ///
    /// The arguments are:
    ///
    /// * `*args` - a list of things to add to the command line, each of which must be coercible to a command line. Further items can be added with `cmd.add`.
    /// * `format` - a string that provides a format to apply to the argument. for example, `cmd_args(x, format="--args={}")` would prepend `--args=` before `x`, or if `x` was a list, before each element in `x`.
    /// * `delimiter` - added between arguments to join them together. For example, `cmd_args(["--args=",x], delimiter="")` would produce a single argument to the underlying tool.
    /// * `prepend` - added as a separate argument before each argument.
    /// * `quote` - indicates whether quoting is to be applied to each argument. The only current valid value is `"shell"`.
    fn cmd_args<'v>(
        #[starlark(args)] args: Vec<Value<'v>>,
        delimiter: Option<StringValue<'v>>,
        format: Option<StringValue<'v>>,
        prepend: Option<StringValue<'v>>,
        quote: Option<&str>,
    ) -> anyhow::Result<StarlarkCommandLine<'v>> {
        StarlarkCommandLine::try_from_values_with_options(
            &args,
            delimiter,
            format,
            prepend,
            quote.try_map(QuoteStyle::parse)?,
        )
    }
}
