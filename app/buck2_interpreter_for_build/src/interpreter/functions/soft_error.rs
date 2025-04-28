/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::soft_error;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::none::NoneType;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum SoftErrorError {
    #[error("Error produced by Starlark: {category}: {message}\n{call_stack}")]
    StarlarkSoftError {
        category: String,
        message: String,
        call_stack: String,
    },
    #[error("Error produced by Starlark: {category}: {message}")]
    StarlarkSoftErrorNoStack { category: String, message: String },
    #[error(
        "soft_error originated from starlark should have category starting with `starlark_`, got: `{0}`"
    )]
    InvalidCategory(String),
}

#[starlark_module]
pub(crate) fn register_soft_error(builder: &mut GlobalsBuilder) {
    /// Produce an error that will become a hard error at some point in the future, but
    /// for now is a warning which is logged to the server.
    /// In the open source version of Buck2 this function always results in an error.
    ///
    /// Called passing a stable key (must be `snake_case` and start with `starlark_`,
    /// used for consistent reporting) and an arbitrary message (used for debugging).
    ///
    /// As an example:
    ///
    /// ```python
    /// soft_error(
    ///     "starlark_rule_is_too_long",
    ///     "Length of property exceeds 100 characters in " + repr(ctx.label),
    /// )
    /// ```
    fn soft_error<'v>(
        #[starlark(require = pos)] category: &str,
        #[starlark(require = pos)] message: String,
        #[starlark(require = named)] quiet: Option<bool>,
        #[starlark(require = named)] stack: Option<bool>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<NoneType> {
        if !category.starts_with("starlark_") {
            return Err(buck2_error::Error::from(SoftErrorError::InvalidCategory(
                category.to_owned(),
            ))
            .into());
        }

        let err = if stack.unwrap_or(true) {
            SoftErrorError::StarlarkSoftError {
                category: category.to_owned(),
                message,
                call_stack: eval.call_stack().to_string(),
            }
            .into()
        } else {
            SoftErrorError::StarlarkSoftErrorNoStack {
                category: category.to_owned(),
                message,
            }
            .into()
        };

        soft_error!(category, err, quiet: quiet.unwrap_or_default(),)?;
        Ok(NoneType)
    }
}
