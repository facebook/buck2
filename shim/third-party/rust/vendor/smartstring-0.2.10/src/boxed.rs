// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use alloc::string::String;
use core::cmp::Ordering;

#[allow(unreachable_pub)]
pub trait BoxedString {
    fn string(&self) -> &String;
    fn string_mut(&mut self) -> &mut String;
    fn into_string(self) -> String;

    fn cmp_with_str(&self, other: &str) -> Ordering;
    fn cmp_with_self(&self, other: &Self) -> Ordering;
    fn eq_with_str(&self, other: &str) -> bool;
    fn eq_with_self(&self, other: &Self) -> bool;

    fn len(&self) -> usize {
        self.string().len()
    }
}

impl BoxedString for String {
    #[inline(always)]
    fn string(&self) -> &String {
        self
    }

    #[inline(always)]
    fn string_mut(&mut self) -> &mut String {
        self
    }

    fn into_string(self) -> String {
        self
    }

    #[inline(always)]
    fn cmp_with_str(&self, other: &str) -> Ordering {
        self.as_str().cmp(other)
    }

    #[inline(always)]
    fn cmp_with_self(&self, other: &Self) -> Ordering {
        self.cmp(other)
    }

    #[inline(always)]
    fn eq_with_str(&self, other: &str) -> bool {
        self == other
    }

    #[inline(always)]
    fn eq_with_self(&self, other: &Self) -> bool {
        self == other
    }
}
