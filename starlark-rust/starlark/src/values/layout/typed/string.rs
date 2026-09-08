/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use std::borrow::Borrow;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;

use allocative::Allocative;
use dupe::Dupe;
use serde::Serialize;

use crate::coerce::CoerceKey;
use crate::collections::Hashed;
use crate::sealed::Sealed;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::Trace;
use crate::values::Value;
use crate::values::ValueTyped;
use crate::values::layout::static_string::VALUE_EMPTY_STRING;
use crate::values::string::str_type::StarlarkStr;

/// Convenient type alias.
///
/// We use `ValueTyped<StarlarkStr>` often, but also we define more operations
/// on `ValueTyped<StarlarkStr>` than on generic `ValueTyped<T>`.
///
/// Note there's a macro `const_frozen_string!` to statically allocate a `StringValue<'static>`:
///
/// ```
/// use starlark::const_frozen_string;
/// use starlark::values::StringValue;
///
/// let s: StringValue<'static> = const_frozen_string!("magic");
/// assert_eq!("magic", s.as_str());
/// ```
pub type StringValue<'v> = ValueTyped<'v, StarlarkStr>;

impl<'v> Borrow<str> for StringValue<'v> {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl<'v> Default for StringValue<'v> {
    fn default() -> Self {
        VALUE_EMPTY_STRING.erase().at()
    }
}

impl<'v> StringValue<'v> {
    /// Freeze the string into the [`Freezer`]'s heap.
    pub fn freeze<'fv>(self, freezer: &Freezer<'fv>) -> FreezeResult<StringValue<'fv>> {
        // SAFETY: Freezing a string yields a string.
        Ok(unsafe { StringValue::new_unchecked(freezer.freeze(self.to_value())?) })
    }

    /// Get self along with the hash.
    pub fn get_hashed(self) -> Hashed<Self> {
        Hashed::new_unchecked(self.get_hash(), self)
    }

    /// Get the string reference along with the hash.
    pub fn get_hashed_str(self) -> Hashed<&'v str> {
        Hashed::new_unchecked(self.get_hash(), self.as_str())
    }

    /// Get the [`Value`] along with the hash.
    pub fn get_hashed_value(self) -> Hashed<Value<'v>> {
        Hashed::new_unchecked(self.get_hash(), self.to_value())
    }
}

/// What [`StringValue`] is generic code written against; it is the only implementation.
pub trait StringValueLike<'v>:
    Trace<'v>
    + CoerceKey<StringValue<'v>>
    + Borrow<str>
    + Display
    + Debug
    + Default
    + Eq
    + Ord
    + Copy
    + Clone
    + Dupe
    + Serialize
    + Allocative
    + Sealed
    + 'v
{
    /// Convert to a [`StringValue`].
    fn to_string_value(self) -> StringValue<'v>;

    /// Convert to a [`str`].
    fn as_str(self) -> &'v str {
        self.to_string_value().as_str()
    }
}

impl<'v> Sealed for StringValue<'v> {}

impl<'v> StringValueLike<'v> for StringValue<'v> {
    fn to_string_value(self) -> StringValue<'v> {
        self
    }
}

impl<'v> Hash for StringValue<'v> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state)
    }
}

impl<'v> PartialOrd for StringValue<'v> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'v> Ord for StringValue<'v> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_ref().cmp(other.as_ref())
    }
}

#[cfg(test)]
mod tests {
    use crate::collections::Hashed;
    use crate::values::FrozenHeap;
    use crate::values::Heap;
    use crate::values::StringValue;
    use crate::values::Value;

    #[test]
    fn test_string_hashes() {
        let expected = Hashed::new("xyz").hash();

        Heap::temp(|heap| {
            let s: StringValue = heap.alloc_str("xyz");
            assert_eq!(expected, Hashed::new(s).hash());
            assert_eq!(s.get_hashed().hash(), s.hashed().unwrap().hash());
            let v: Value = heap.alloc_str("xyz").to_value();
            assert_eq!(expected, v.get_hashed().unwrap().hash());
        });

        FrozenHeap::temp(|heap| {
            let fs: StringValue = heap.alloc_str("xyz");
            assert_eq!(expected, Hashed::new(fs).hash());
            let fv: Value = heap.alloc_str("xyz").to_value();
            assert_eq!(expected, fv.get_hashed().unwrap().hash());
        });
    }
}
