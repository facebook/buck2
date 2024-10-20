/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;

use ref_cast::ref_cast_custom;
use ref_cast::RefCastCustom;

use crate::impls::key::DiceKeyErased;
use crate::Key;

/// A type erased Key. Dice APIs that return key references will pass them as DynKey (unless they can be
/// passed as the specific Key type).
#[derive(RefCastCustom)]
#[repr(transparent)]
pub struct DynKey {
    pub(crate) erased: DiceKeyErased,
}

impl DynKey {
    pub fn key_type_name(&self) -> &'static str {
        self.erased.key_type_name()
    }

    pub fn downcast_ref<K: Key>(&self) -> Option<&K> {
        self.erased.as_any().downcast_ref()
    }

    pub fn request_value<T: 'static>(&self) -> Option<T> {
        self.erased.request_value()
    }

    pub fn request_ref<T: ?Sized + 'static>(&self) -> Option<&T> {
        self.erased.request_ref()
    }

    pub fn from_key(k: impl Key) -> Self {
        Self {
            erased: DiceKeyErased::key(k),
        }
    }

    #[ref_cast_custom]
    pub(crate) const fn ref_cast(erased: &DiceKeyErased) -> &Self;
}

impl Display for DynKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.erased, f)
    }
}
