/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

/// When calling a command like `buck2 build dramatic`,
/// this trait is used to resolve the string `dramatic` to a fully qualified target name.
pub trait TargetAliasResolver {
    fn get<'a>(&'a self, name: &str) -> buck2_error::Result<Option<&'a str>>;
}
