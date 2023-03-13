/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! An 'Artifact' represents a File to be used as part of the build. It can be either a file in the
//! source tree, or a file to be generated as part of the build.
//!
//! The existence of an Artifact does not mean the actual file exists. Artifacts needs to be
//! 'made available' before its existence on the filesystem is guaranteed.
//!
//! An 'Artifact' is first "declared" by rule implementation as a 'DeclaredArtifact'. The artifact
//! will need to be "bound" to an 'Action' through being used as an 'OutputArtifact'. Once bound,
//! it becomes a 'BuildArtifact' that can be available.
//!

pub mod build_artifact;

pub mod artifact_type;
pub mod materializer;
pub(crate) mod projected_artifact;
pub mod source_artifact;
