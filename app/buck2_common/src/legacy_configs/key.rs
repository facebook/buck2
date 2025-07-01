/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use dupe::Dupe;

// TODO(nga): implement `buck2 help-buckconfig`
//   https://www.internalfb.com/tasks/?t=183528129
#[derive(derive_more::Display, Debug, Copy, Clone, Dupe, Eq, PartialEq)]
#[display("{}.{}", section, property)]
pub struct BuckconfigKeyRef<'a> {
    pub section: &'a str,
    pub property: &'a str,
}
