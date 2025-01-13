/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::env;
use std::env::VarError;
use std::sync::OnceLock;

use buck2_error::BuckErrorContext;

pub struct EnvHelper<T> {
    convert: fn(&str) -> buck2_error::Result<T>,
    var: &'static str,
    cell: OnceLock<Option<T>>,
}

impl<T> EnvHelper<T> {
    pub const fn with_converter_from_macro(
        var: &'static str,
        convert: fn(&str) -> buck2_error::Result<T>,
    ) -> Self {
        Self {
            convert,
            var,
            cell: OnceLock::new(),
        }
    }

    // This code does not really require `'static` lifetime.
    // `EnvHelper` caches computed value. When it is used like
    // `EnvHelper::new(...).get(...)`, it performs unnecessary work.
    // To avoid it, we require `'static` lifetime, to force placing `EnvHelper` in static variable.
    pub fn get(&'static self) -> buck2_error::Result<Option<&'static T>> {
        let var = self.var;
        let convert = self.convert;

        self.cell
            .get_or_try_init(move || match env::var(var) {
                Ok(v) => {
                    tracing::info!("Env override found: ${} = {}", var, v);
                    Ok(Some((convert)(&v)?))
                }
                Err(VarError::NotPresent) => Ok(None),
                Err(VarError::NotUnicode(..)) => Err(buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Tier0,
                    "Variable is not unicode"
                )),
            })
            .map(Option::as_ref)
            .with_buck_error_context(|| format!("Invalid value for ${}", var))
    }
}
