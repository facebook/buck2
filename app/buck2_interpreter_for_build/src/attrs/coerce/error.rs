/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use gazebo::prelude::*;

#[derive(buck2_error::Error, Debug)]
#[buck2(input)]
pub(crate) enum CoercionError {
    #[error("Used one_of with an empty list.")]
    OneOfEmpty,
    #[error("one_of fails, the errors against each alternative in turn were:\n{}", .0.map(|x| format!("{:#}", x)).join("\n"))]
    OneOfMany(Vec<buck2_error::Error>),
    #[error("default_only is not allowed to be specified, but got `{0}`")]
    DefaultOnly(String),
    #[error("enum called with `{0}`, only allowed: {}", .1.map(|x| format!("`{}`", x)).join(", "))]
    InvalidEnumVariant(String, Vec<String>),
}

impl CoercionError {
    pub fn one_of_many(mut errs: Vec<buck2_error::Error>) -> buck2_error::Error {
        if errs.is_empty() {
            CoercionError::OneOfEmpty.into()
        } else if errs.len() == 1 {
            errs.pop().unwrap()
        } else {
            CoercionError::OneOfMany(errs).into()
        }
    }
}
