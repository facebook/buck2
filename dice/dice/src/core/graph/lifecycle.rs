/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use dupe::Dupe;
use pagable::DataKey;

use crate::value::DiceValidValue;
use crate::value::MaybeResident;

/// A retained value together with where it sits in the page-out lifecycle. Every state except
/// `PagedOut` keeps the value resident; `PagedOut` keeps only the on-disk `DataKey`.
#[derive(Allocative, Debug)]
pub(crate) enum PagableValue {
    /// Resident, no on-disk copy: a candidate for the next page-out.
    NeverPagedOut(DiceValidValue),
    /// Serialized to disk and evicted from memory.
    PagedOut(DataKey),
    /// Resident but not serializable (e.g. `NoValueSerialize`, or a serialization error); never
    /// a page-out candidate.
    NonPageable(DiceValidValue),
    /// Paged out once and resident again, recomputed or read back. A value is paged out at most
    /// once, so this is not a candidate either.
    Recomputed(DiceValidValue),
}

mini_vec::size_assert::words_of_type!(PagableValue, 3);

impl PagableValue {
    /// The resident value, if any.
    pub(crate) fn as_hydrated(&self) -> Option<&DiceValidValue> {
        match self {
            PagableValue::NeverPagedOut(value)
            | PagableValue::NonPageable(value)
            | PagableValue::Recomputed(value) => Some(value),
            PagableValue::PagedOut(_) => None,
        }
    }

    /// The on-disk key of a paged-out value; `None` for resident values.
    pub(crate) fn data_key(&self) -> Option<DataKey> {
        match self {
            PagableValue::PagedOut(key) => Some(*key),
            _ => None,
        }
    }

    pub(crate) fn is_page_out_candidate(&self) -> bool {
        matches!(self, PagableValue::NeverPagedOut(_))
    }

    /// The value as a lookup hands it out.
    pub(crate) fn as_maybe_resident(&self) -> MaybeResident<DiceValidValue> {
        match self {
            PagableValue::PagedOut(key) => MaybeResident::PagedOut(*key),
            resident => MaybeResident::Resident(
                resident
                    .as_hydrated()
                    .expect("every state but PagedOut is resident")
                    .dupe(),
            ),
        }
    }

    /// The state a value newly retained for a key takes, given the state of the key's most
    /// recently retained value: a key whose value has been paged out once stays resident from
    /// then on.
    pub(crate) fn lifecycle_after(previous: Option<&PagableValue>) -> fn(DiceValidValue) -> Self {
        match previous {
            None | Some(PagableValue::NeverPagedOut(_)) => PagableValue::NeverPagedOut,
            Some(PagableValue::NonPageable(_)) => PagableValue::NonPageable,
            Some(PagableValue::Recomputed(_) | PagableValue::PagedOut(_)) => {
                PagableValue::Recomputed
            }
        }
    }

    /// Makes a paged-out value resident again with `value`, which was read back from its
    /// `DataKey` or has just been proven equal to it.
    pub(crate) fn make_resident(&mut self, value: DiceValidValue) {
        if matches!(self, PagableValue::PagedOut(_)) {
            *self = PagableValue::Recomputed(value);
        }
    }
}

mini_vec::size_assert::words_of_type!(PagableValue, 3);
