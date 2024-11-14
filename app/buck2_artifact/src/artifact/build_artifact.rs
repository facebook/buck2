/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_core::fs::buck_out_path::BuildArtifactPath;
use buck2_data::ToProtoMessage;
use buck2_error::internal_error;
use buck2_execute::execute::request::OutputType;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;
use static_assertions::assert_eq_size;

use crate::actions::key::ActionKey;

/// An artifact that is built by the build system
#[allow(clippy::derived_hash_with_manual_eq)] // The Eq is equivalent to what would have been generated
#[derive(Clone, Debug, Dupe, Display, Derivative, Allocative)]
#[derivative(PartialEq, Eq, Hash)]
#[display("`{}`, action: {}", path, key)]
pub struct BuildArtifact {
    path: BuildArtifactPath,
    key: ActionKey,
    output_type: OutputType,
}

assert_eq_size!(BuildArtifact, [usize; 6]);

impl BuildArtifact {
    pub fn new(
        path: BuildArtifactPath,
        key: ActionKey,
        output_type: OutputType,
    ) -> buck2_error::Result<Self> {
        if key.holder_key().owner() != path.owner().owner() {
            return Err(internal_error!("BaseDeferredKey mismatch"));
        }
        Ok(BuildArtifact {
            path,
            key,
            output_type,
        })
    }

    pub fn get_path(&self) -> &BuildArtifactPath {
        &self.path
    }

    pub fn key(&self) -> &ActionKey {
        &self.key
    }

    pub fn output_type(&self) -> OutputType {
        self.output_type
    }
}

impl ToProtoMessage for BuildArtifact {
    type Message = buck2_data::BuildArtifact;

    fn as_proto(&self) -> Self::Message {
        buck2_data::BuildArtifact {
            key: Some(self.key().as_proto()),
            path: self.get_path().path().to_string(),
        }
    }
}
