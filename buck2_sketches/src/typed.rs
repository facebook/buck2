/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Type-safe wrappers around [`SetSketch`] that distinguish between different
//! semantic uses of sketches.
//!
//! Two sketch types are currently defined:
//!
//! - [`DependencyGraphSketch`] — the configured dependency graph of a target;
//!   cardinality estimates the number of configured targets.
//! - [`MemoryUsageSketch`] — retained analysis memory of a target; cardinality
//!   estimates the number of bytes.
//!
//! Both share the same underlying [`SetSketch`] data structure and serialization
//! format, but expose domain-specific accessor names so callers don't have to
//! guess what `cardinality()` means in context.
//!
//! The shared behavior is provided by the [`TypedSketch`] trait; each concrete
//! type only needs to implement the four required accessors.

use setsketch::SetSketch;
use setsketch::SetSketchDecodeError;

// ---------------------------------------------------------------------------
// Trait
// ---------------------------------------------------------------------------

/// Common interface for newtype wrappers around [`SetSketch`].
///
/// Implementors provide access to the inner sketch; the trait supplies
/// constructors, serialization, merge/union, and overlap queries as defaults.
pub trait TypedSketch: Sized + Clone {
    fn from_inner(sketch: SetSketch) -> Self;
    fn inner(&self) -> &SetSketch;
    fn inner_mut(&mut self) -> &mut SetSketch;
    fn into_inner(self) -> SetSketch;

    /// Decode from a base64 string that may or may not have a version prefix.
    fn from_base64(encoded: &str) -> Result<Self, SetSketchDecodeError> {
        SetSketch::from_base64_maybe_versioned(encoded).map(Self::from_inner)
    }

    /// Create from raw register bytes (8192 bytes = 4096 × u16).
    fn from_bytes(bytes: &[u8]) -> Result<Self, String> {
        sketch_from_bytes(bytes).map(Self::from_inner)
    }

    /// Serialize to raw register bytes.
    fn to_bytes(&self) -> Vec<u8> {
        sketch_to_bytes(self.inner())
    }

    /// Merge another sketch into this one in place.
    fn merge(&mut self, other: &Self) {
        self.inner_mut().merge(other.inner());
    }

    /// Return a new sketch that is the union of `self` and `other`, consuming `self`.
    fn union(self, other: &Self) -> Self {
        Self::from_inner(self.into_inner().union(other.inner()))
    }

    /// Estimated overlap between two sketches.
    fn absolute_overlap(&self, other: &Self) -> f64 {
        self.inner().absolute_overlap(other.inner())
    }

    /// Confidence interval for the proportion of elements in this sketch
    /// not found in `other`.
    fn approx_proportion_not_included(&self, other: &Self) -> (f64, f64) {
        self.inner().approx_proportion_not_included(other.inner())
    }

    /// Returns the cardinality estimate as an `i64`, truncating toward zero.
    fn get_size_approx(&self) -> i64 {
        self.inner().get_size_approx()
    }

    /// Construct from raw u16 registers using recommended parameters.
    fn from_recommended_registers(registers: Vec<u16>) -> Self {
        Self::from_inner(SetSketch::from_recommended_registers(registers))
    }

    /// Decode from a base64 string with a required version prefix (e.g. `"V1:..."`).
    fn decode_base64_versioned(encoded: &str) -> Result<Self, SetSketchDecodeError> {
        SetSketch::decode_base64_versioned(encoded).map(Self::from_inner)
    }
}

// ---------------------------------------------------------------------------
// DependencyGraphSketch
// ---------------------------------------------------------------------------

/// A sketch representing the configured dependency graph of a target.
///
/// Cardinality estimates the number of configured targets in the graph.
#[derive(Clone, Debug, Default)]
pub struct DependencyGraphSketch(SetSketch);

impl TypedSketch for DependencyGraphSketch {
    fn from_inner(sketch: SetSketch) -> Self {
        Self(sketch)
    }
    fn inner(&self) -> &SetSketch {
        &self.0
    }
    fn inner_mut(&mut self) -> &mut SetSketch {
        &mut self.0
    }
    fn into_inner(self) -> SetSketch {
        self.0
    }
}

impl DependencyGraphSketch {
    /// Estimate the number of configured targets in the dependency graph.
    pub fn estimated_target_count(&self) -> f64 {
        self.0.cardinality()
    }
}

// ---------------------------------------------------------------------------
// MemoryUsageSketch
// ---------------------------------------------------------------------------

/// A sketch representing the retained analysis memory of a target.
///
/// Cardinality estimates the number of bytes of retained analysis memory.
#[derive(Clone, Debug, Default)]
pub struct MemoryUsageSketch(SetSketch);

impl TypedSketch for MemoryUsageSketch {
    fn from_inner(sketch: SetSketch) -> Self {
        Self(sketch)
    }
    fn inner(&self) -> &SetSketch {
        &self.0
    }
    fn inner_mut(&mut self) -> &mut SetSketch {
        &mut self.0
    }
    fn into_inner(self) -> SetSketch {
        self.0
    }
}

impl MemoryUsageSketch {
    /// Estimate the number of bytes of retained analysis memory.
    pub fn estimated_bytes(&self) -> f64 {
        self.0.cardinality()
    }

    /// Estimate the megabytes of retained analysis memory.
    pub fn estimated_megabytes(&self) -> f64 {
        self.0.cardinality() / 1_000_000.0
    }
}

// ---------------------------------------------------------------------------
// Shared helpers
// ---------------------------------------------------------------------------

/// Decode a sketch from raw u16 register bytes.
fn sketch_from_bytes(bytes: &[u8]) -> Result<SetSketch, String> {
    if bytes.len() != 8192 {
        return Err(format!("Expected 8192 bytes, got {}", bytes.len()));
    }
    let registers: Vec<u16> = bytes
        .chunks_exact(2)
        .map(|c| u16::from_ne_bytes([c[0], c[1]]))
        .collect();
    Ok(SetSketch::from_recommended_registers(registers))
}

/// Serialize a sketch to raw u16 register bytes.
fn sketch_to_bytes(sketch: &SetSketch) -> Vec<u8> {
    sketch
        .get_registers()
        .iter()
        .copied()
        .flat_map(|v| v.to_ne_bytes())
        .collect()
}
