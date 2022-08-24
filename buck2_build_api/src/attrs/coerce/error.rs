/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::provider::label::ProvidersLabel;
use gazebo::prelude::*;
use starlark::values::Value;

#[derive(thiserror::Error, Debug)]
pub(crate) enum CoercionError {
    #[error("Expected value of type `{0}`, got value with type `{1}` (value was `{2}`)")]
    TypeError(String, String, String),
    #[error("Used one_of with an empty list.")]
    OneOfEmpty,
    #[error("one_of fails, the errors against each alternative in turn were:\n{}", .0.map(|x| format!("{:#}", x)).join("\n"))]
    OneOfMany(Vec<anyhow::Error>),
    #[error("default_only is not allowed to be specified, but got `{0}`")]
    DefaultOnly(String),
    #[error("enum called with `{0}`, only allowed: {}", .1.map(|x| format!("`{}`", x)).join(", "))]
    InvalidEnumVariant(String, Vec<String>),
    #[error("attrs.configuration_dep() attributes shouldn't have any subtargets, but got `{0}`")]
    UnexpectedSubTarget(ProvidersLabel),
}

impl CoercionError {
    pub(crate) fn type_error(expected_type: &str, value: Value) -> CoercionError {
        CoercionError::TypeError(
            expected_type.to_owned(),
            value.get_type().to_owned(),
            value.to_repr(),
        )
    }

    pub(crate) fn one_of_many(mut errs: Vec<anyhow::Error>) -> anyhow::Error {
        if errs.is_empty() {
            CoercionError::OneOfEmpty.into()
        } else if errs.len() == 1 {
            errs.pop().unwrap()
        } else {
            CoercionError::OneOfMany(errs).into()
        }
    }
}
