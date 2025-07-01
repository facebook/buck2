/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_util::late_binding::LateBinding;

pub static CLIENT_ONLY_VAL: LateBinding<bool> = LateBinding::new("client_only_val");

pub fn is_client_only() -> buck2_error::Result<bool> {
    CLIENT_ONLY_VAL.get().copied()
}
