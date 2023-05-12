/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::rule_defs::artifact_tagging::ArtifactTag;
use starlark::environment::GlobalsBuilder;
use starlark::starlark_module;

#[starlark_module]
pub(crate) fn artifact_tag_factory(builder: &mut GlobalsBuilder) {
    fn make_tag() -> anyhow::Result<ArtifactTag> {
        Ok(ArtifactTag::new())
    }
}
