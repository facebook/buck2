/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use starlark::environment::GlobalsBuilder;

use super::ArtifactTag;

#[starlark_module]
pub fn artifact_tag_factory(builder: &mut GlobalsBuilder) {
    fn make_tag() -> anyhow::Result<ArtifactTag> {
        Ok(ArtifactTag::new())
    }
}
