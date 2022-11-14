/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::num::NonZeroU64;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;

use allocative::Allocative;
use gazebo::dupe::Dupe;
use serde::Serialize;

/// A SpanId is a unique identifier for a span, which is a pair of events that represent a conceptual start and stop
/// of a particular operation.
#[derive(
    Debug,
    Copy,
    Clone,
    Dupe,
    Serialize,
    PartialEq,
    Eq,
    Hash,
    derive_more::Display,
    Allocative
)]
pub struct SpanId(pub(crate) NonZeroU64);

impl SpanId {
    /// Generates a new SpanId, suitable for identifying a particular span within the context of a trace. Span IDs are
    /// increasing nonzero 64-bit integers.
    pub fn new() -> SpanId {
        static NEXT_ID: AtomicU64 = AtomicU64::new(1);
        loop {
            let next_id = NEXT_ID.fetch_add(1, Ordering::AcqRel);
            match NonZeroU64::new(next_id) {
                Some(id) => return SpanId(id),
                // 64-bit wrap around; continue the loop to generate the next non-zero ID.
                None => continue,
            }
        }
    }
}
