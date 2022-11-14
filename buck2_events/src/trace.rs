/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;
use std::str::FromStr;

use allocative::Allocative;
use byteorder::ByteOrder;
use byteorder::NetworkEndian;
use gazebo::dupe::Dupe;
use uuid::Uuid;

/// A TraceId is a unique identifier for a trace. Trace IDs are globally unique; their textual form is a v4 UUID.
///
/// TraceIds generally correspond to commands, but they do not have to, e.g. in the case of a Buck daemon producing
/// events even when a command is not running.
#[derive(Debug, Clone, PartialEq, Eq, Allocative)]
pub struct TraceId(
    #[allocative(skip)] // TODO: should count.
    pub(crate)  Uuid,
);

// UUID is 16 bytes.
impl Dupe for TraceId {}

impl serde::ser::Serialize for TraceId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl Display for TraceId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut buf = Uuid::encode_buffer();
        f.write_str(self.0.to_hyphenated().encode_lower(&mut buf))
    }
}

impl FromStr for TraceId {
    type Err = uuid::Error;
    fn from_str(s: &str) -> Result<TraceId, Self::Err> {
        Ok(TraceId(Uuid::parse_str(s)?))
    }
}

impl TraceId {
    /// Generates a new TraceId, suitable for identifying a particular trace.
    pub fn new() -> TraceId {
        TraceId(Uuid::new_v4())
    }

    /// Retrieves the cached hash of this TraceId.
    pub fn hash(&self) -> i64 {
        NetworkEndian::read_i64(&self.0.as_bytes()[8..16])
    }
}

#[allow(clippy::derive_hash_xor_eq)] // The derived PartialEq is still correct.
impl Hash for TraceId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash().hash(state);
    }
}
