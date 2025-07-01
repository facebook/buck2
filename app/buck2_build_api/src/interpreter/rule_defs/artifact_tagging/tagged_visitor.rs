/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::artifact_type::OutputArtifact;

use super::ArtifactTag;
use crate::artifact_groups::ArtifactGroup;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;

/// Wrap an existing CommandLineArtifactVisitor into one that adds an ArtifactTag.
pub struct TaggedVisitor<'a, 'b, 'v> {
    inner: &'b mut dyn CommandLineArtifactVisitor<'v>,
    tag: &'a ArtifactTag,
    inputs_only: bool,
}

impl<'a, 'b, 'v> TaggedVisitor<'a, 'b, 'v> {
    pub fn wrap(
        tag: &'a ArtifactTag,
        inputs_only: bool,
        inner: &'b mut dyn CommandLineArtifactVisitor<'v>,
    ) -> Self {
        Self {
            inner,
            tag,
            inputs_only,
        }
    }
}

impl<'a, 'b, 'v> CommandLineArtifactVisitor<'v> for TaggedVisitor<'a, 'b, 'v> {
    /// Ignore the inner tag, set our own. Nesting input groups generally isn't a great idea, but
    /// we can't statically prevent it.
    fn visit_input(&mut self, input: ArtifactGroup, _tag: Option<&ArtifactTag>) {
        self.inner.visit_input(input, Some(self.tag))
    }

    fn visit_declared_output(&mut self, artifact: OutputArtifact<'v>, _tag: Option<&ArtifactTag>) {
        let tag = if self.inputs_only {
            None
        } else {
            Some(self.tag)
        };
        self.inner.visit_declared_output(artifact, tag)
    }

    fn visit_frozen_output(&mut self, artifact: Artifact, _tag: Option<&ArtifactTag>) {
        let tag = if self.inputs_only {
            None
        } else {
            Some(self.tag)
        };
        self.inner.visit_frozen_output(artifact, tag)
    }
}
