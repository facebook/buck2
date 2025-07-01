/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

pub mod aquery;
pub mod cquery;
pub mod printer;
pub(crate) mod query_target_ext;
pub(crate) mod starlark_profile;
pub mod uquery;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum QueryCommandError {
    #[error(
        "query result was a set of files and one or more --output-attribute was requested, but files have not attributes"
    )]
    FileSetHasNoAttributes,
}
