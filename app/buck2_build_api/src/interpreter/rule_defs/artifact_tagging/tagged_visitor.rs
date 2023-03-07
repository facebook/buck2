/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use super::ArtifactTag;
use crate::actions::artifact::OutputArtifact;
use crate::artifact_groups::ArtifactGroup;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;

/// Wrap an existing CommandLineArtifactVisitor into one that adds an ArtifactTag.
pub struct TaggedVisitor<'a, 'b> {
    inner: &'b mut dyn CommandLineArtifactVisitor,
    tag: &'a ArtifactTag,
    inputs_only: bool,
}

impl<'a, 'b> TaggedVisitor<'a, 'b> {
    pub fn wrap(
        tag: &'a ArtifactTag,
        inputs_only: bool,
        inner: &'b mut dyn CommandLineArtifactVisitor,
    ) -> Self {
        Self {
            tag,
            inputs_only,
            inner,
        }
    }
}

impl<'a, 'b> CommandLineArtifactVisitor for TaggedVisitor<'a, 'b> {
    /// Ignore the inner tag, set our own. Nesting input groups generally isn't a great idea, but
    /// we can't statically prevent it.
    fn visit_input(&mut self, input: ArtifactGroup, _tag: Option<&ArtifactTag>) {
        self.inner.visit_input(input, Some(self.tag))
    }

    /// Same as above, no nesting here.
    fn visit_output(&mut self, artifact: OutputArtifact, _tag: Option<&ArtifactTag>) {
        let tag = if self.inputs_only {
            None
        } else {
            Some(self.tag)
        };
        self.inner.visit_output(artifact, tag)
    }
}
