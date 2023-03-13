/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use once_cell::sync::OnceCell;

/// Value (typically a function pointer) that is initialized at program start.
pub struct LateBinding<T> {
    /// Name for diagnostic.
    name: &'static str,
    symbol: OnceCell<T>,
}

impl<T> LateBinding<T> {
    pub const fn new(name: &'static str) -> LateBinding<T> {
        LateBinding {
            name,
            symbol: OnceCell::new(),
        }
    }

    pub fn init(&self, symbol: T) {
        if self.symbol.set(symbol).is_err() {
            panic!("{} already set", self.name);
        }
    }

    #[inline]
    pub fn get(&self) -> anyhow::Result<&T> {
        self.symbol
            .get()
            .with_context(|| format!("{} not set (internal error)", self.name))
    }
}
