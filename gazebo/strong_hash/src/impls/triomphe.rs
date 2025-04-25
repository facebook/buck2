/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg(feature = "triomphe")]

use crate::StrongHash;

impl<T: StrongHash + ?Sized> StrongHash for triomphe::Arc<T> {
    fn strong_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_ref().strong_hash(state);
    }
}
