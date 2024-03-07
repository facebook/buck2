/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_core::fs::buck_out_path::BuckOutPath;
use buck2_data::ToProtoMessage;
use buck2_execute::execute::request::OutputType;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;

use crate::actions::key::ActionKey;

/// An artifact that is built by the build system
#[allow(clippy::derived_hash_with_manual_eq)] // The Eq is equivalent to what would have been generated
#[derive(Clone, Debug, Dupe, Display, Derivative, Allocative)]
#[derivative(PartialEq, Eq, Hash)]
#[display(fmt = "`{}`, action: {}", path, key)]
pub struct BuildArtifact {
    path: BuckOutPath,
    key: ActionKey,
    output_type: OutputType,
}

impl BuildArtifact {
    pub fn new(path: BuckOutPath, key: ActionKey, output_type: OutputType) -> Self {
        BuildArtifact {
            path,
            key,
            output_type,
        }
    }

    pub fn get_path(&self) -> &BuckOutPath {
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
