/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("State {0} not found")]
    MissingState(String),
    #[error("Word {0} contains non-space whitespace")]
    InvalidWhitespace(String),
}
