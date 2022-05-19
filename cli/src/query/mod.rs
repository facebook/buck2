/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use thiserror::Error;

pub mod printer;

#[derive(Debug, Error)]
enum QueryCommandError {
    #[error(
        "query result was a set of files and one or more --output-attribute was requested, but files have not attributes"
    )]
    FileSetHasNoAttributes,
}
