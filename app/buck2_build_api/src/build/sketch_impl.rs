/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::hash::BuildHasherDefault;
use std::sync::Arc;

use allocative::Allocative;
use base64::write::EncoderWriter;
use buck2_util::strong_hasher::Blake3StrongHasher;
use derivative::Derivative;
use dupe::Dupe;
use ref_cast::RefCast;
use setsketch::SetSketchParams;
use setsketch::SetSketcher;
use strong_hash::StrongHash;
use strong_hash::UseStrongHashing;

/// Trait for sketch operations, allowing for mocking in tests.
pub(crate) trait Sketcher<T> {
    fn sketch(&mut self, t: &T);
    fn sketch_weighted(&mut self, t: &T, weight: u64);
}

/// This is a struct representing graph sketches returned from DICE call to compute sketches.
/// It satisfies 2 properties.
/// (1) It can be merged with other sketches via VersionedSketcher's `merge` method. It does
/// so by holding directly onto the `SetSketcher` type.
/// (2) It implements Dupe, Hash, and Eq. Hash and Eq are implemented by precomputing and holding
/// onto a signature of the sketch.
#[derive(Clone, Dupe, Derivative, Allocative)]
#[derivative(Debug)]
pub struct MergeableGraphSketch<T: StrongHash> {
    version: SketchVersion,
    #[derivative(Debug = "ignore", PartialEq = "ignore", Hash = "ignore")]
    #[allocative(skip)] // TODO(scottcao): Figure out how to implement allocative properly
    sketcher: Arc<SetSketcher<UseStrongHashing<T>, Blake3StrongHasher>>,
}

impl<T: StrongHash> PartialEq for MergeableGraphSketch<T> {
    fn eq(&self, other: &Self) -> bool {
        self.version == other.version
            && self.sketcher.get_registers() == other.sketcher.get_registers()
    }
}

impl<T: StrongHash> Eq for MergeableGraphSketch<T> {}

impl<T: StrongHash> MergeableGraphSketch<T> {
    pub(crate) fn new(
        version: SketchVersion,
        sketcher: SetSketcher<UseStrongHashing<T>, Blake3StrongHasher>,
    ) -> Self {
        let sketcher = Arc::new(sketcher);
        Self { version, sketcher }
    }

    pub fn serialize(&self) -> String {
        let mut res = format!("{}:", self.version).into_bytes();
        let mut enc =
            EncoderWriter::new(&mut res, &base64::engine::general_purpose::STANDARD_NO_PAD);
        for v in self.sketcher.get_registers() {
            use std::io::Write;
            enc.write_all(&v.to_ne_bytes()).unwrap();
        }
        enc.finish().unwrap();
        drop(enc);
        String::from_utf8(res).unwrap()
    }
}

#[derive(
    Copy,
    Clone,
    Dupe,
    Debug,
    Eq,
    Hash,
    PartialEq,
    Allocative,
    derive_more::Display
)]
pub(crate) enum SketchVersion {
    V1,
}

pub(crate) static DEFAULT_SKETCH_VERSION: SketchVersion = SketchVersion::V1;

impl SketchVersion {
    pub(crate) fn create_sketcher<T: StrongHash>(self) -> VersionedSketcher<T> {
        let sketcher = match self {
            Self::V1 => SetSketcher::new(
                SetSketchParams::recommended(),
                BuildHasherDefault::<Blake3StrongHasher>::new(), // We want a predictable hash here.
            ),
        };

        VersionedSketcher {
            version: self,
            sketcher,
        }
    }
}

pub(crate) struct VersionedSketcher<T: StrongHash> {
    version: SketchVersion,
    sketcher: SetSketcher<UseStrongHashing<T>, Blake3StrongHasher>,
}

impl<T: StrongHash> Sketcher<T> for VersionedSketcher<T> {
    fn sketch(&mut self, t: &T) {
        self.sketcher.sketch(UseStrongHashing::ref_cast(t));
    }

    fn sketch_weighted(&mut self, t: &T, weight: u64) {
        self.sketcher
            .sketch_weighted_locality_unstable(UseStrongHashing::ref_cast(t), weight);
    }
}

impl<T, S: Sketcher<T>> Sketcher<T> for Option<S> {
    fn sketch(&mut self, t: &T) {
        if let Some(s) = self {
            s.sketch(t);
        }
    }

    fn sketch_weighted(&mut self, t: &T, weight: u64) {
        if let Some(s) = self {
            s.sketch_weighted(t, weight);
        }
    }
}

impl<T: StrongHash> VersionedSketcher<T> {
    pub(crate) fn into_mergeable_graph_sketch(self) -> MergeableGraphSketch<T> {
        MergeableGraphSketch::new(self.version, self.sketcher)
    }

    pub(crate) fn merge(&mut self, other: &MergeableGraphSketch<T>) -> buck2_error::Result<()> {
        if self.version == other.version {
            self.sketcher.merge(&other.sketcher);
            Ok(())
        } else {
            Err(buck2_error::internal_error!(
                // This is currently an internal error because users cannot specify sketch version to use.
                "Set sketch version mismatch between {} and {}. Cannot merge.",
                self.version,
                other.version
            ))
        }
    }
}
