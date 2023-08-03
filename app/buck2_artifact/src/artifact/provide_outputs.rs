/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::actions::key::ActionKey;
use crate::artifact::build_artifact::BuildArtifact;

/// Dynamic data requested from deferreds.
///
/// Newtype to grep easier and to provide some type safety.
pub struct ProvideOutputs(pub anyhow::Result<Vec<BuildArtifact>>);

/// Dynamic data requested from deferreds, allows access to the action key for registered actions.
pub struct ProvideActionKey(pub ActionKey);
