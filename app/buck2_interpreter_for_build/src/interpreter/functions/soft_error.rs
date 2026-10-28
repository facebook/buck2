/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_core::error::SoftErrorKind;
use buck2_core::error::StructuredErrorOptions;
use buck2_core::error::handle_soft_error;
use buck2_core::error::validate_logview_category;
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

fn validate_category(category: &str) -> starlark::Result<()> {
    if !category.starts_with("starlark_") {
        return Err(
            buck2_error::Error::from(SoftErrorError::InvalidCategory(category.to_owned())).into(),
        );
    }

    validate_logview_category(category).map_err(starlark::Error::from)?;

    Ok(())
}

#[starlark_module]
pub(crate) fn register_soft_error(builder: &mut GlobalsBuilder) {
    /// Records a use of something that is being removed, so that whoever is removing it can see
    /// what is left. Nothing is shown to the user. Once nothing triggers it any more, the call
    /// site should `fail` instead.
    ///
    /// The category must be `snake_case`, start with `starlark_`, and stay stable, since it is
    /// what the uses are counted under; the message is free text for whoever reads them. `stack`
    /// includes the Starlark call stack in the message.
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
        #[starlark(require = named)] stack: Option<bool>,
        #[starlark(require = named)] quiet: Option<bool>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<NoneType> {
        validate_category(category)?;
        // A deprecation is never shown, so `quiet` has nothing to say; it stays accepted until
        // the callers in the repo have dropped it, since they and buck2 do not release together.
        let _ = quiet;

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

        handle_soft_error(
            category,
            err,
            (file!(), line!(), column!()),
            StructuredErrorOptions {
                kind: SoftErrorKind::Deprecation {
                    owner: None,
                    started: None,
                },
                ..Default::default()
            },
        )?;
        Ok(NoneType)
    }

    /// Validate that a soft error category is valid for use from Starlark.
    /// The category must start with `starlark_` and be `lower_snake_case`
    /// (only `a-z` and `_`, no consecutive underscores, must start and end with a letter).
    /// Raises an error if the category is invalid, returns `None` on success.
    fn validate_soft_error_category(
        #[starlark(require = pos)] category: &str,
    ) -> starlark::Result<NoneType> {
        validate_category(category)?;
        Ok(NoneType)
    }
}

#[cfg(test)]
mod tests {
    use starlark::assert::Assert;

    use crate::interpreter::functions::soft_error::register_soft_error;

    #[test]
    fn test_validate_soft_error_category_valid() {
        let mut a = Assert::new();
        a.globals_add(register_soft_error);
        a.is_true("validate_soft_error_category('starlark_valid_category') == None");
    }

    #[test]
    fn test_validate_soft_error_category_missing_prefix() {
        let mut a = Assert::new();
        a.globals_add(register_soft_error);
        a.fail(
            "validate_soft_error_category('no_starlark_prefix')",
            "should have category starting with `starlark_`",
        );
    }

    #[test]
    fn test_validate_soft_error_category_ends_with_underscore() {
        let mut a = Assert::new();
        a.globals_add(register_soft_error);
        a.fail(
            "validate_soft_error_category('starlark_')",
            "must be lower_snake_case",
        );
    }

    #[test]
    fn test_validate_soft_error_category_consecutive_underscores() {
        let mut a = Assert::new();
        a.globals_add(register_soft_error);
        a.fail(
            "validate_soft_error_category('starlark_has__double')",
            "must be lower_snake_case",
        );
    }

    #[test]
    fn test_validate_soft_error_category_contains_digit() {
        let mut a = Assert::new();
        a.globals_add(register_soft_error);
        a.fail(
            "validate_soft_error_category('starlark_has_1number')",
            "must be lower_snake_case",
        );
    }

    #[test]
    fn test_validate_soft_error_category_empty() {
        let mut a = Assert::new();
        a.globals_add(register_soft_error);
        a.fail(
            "validate_soft_error_category('')",
            "should have category starting with `starlark_`",
        );
    }
}
