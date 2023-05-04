/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[derive(thiserror::Error, Debug)]
pub(crate) enum ResolveError {
    #[error("Attribute cannot be converted to Starlark value: `{0}`")]
    AttrCannotBeConvertedToValue(String),
}
