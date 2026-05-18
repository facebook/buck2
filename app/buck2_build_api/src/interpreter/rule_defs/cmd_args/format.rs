/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Cow;

use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;

use crate::interpreter::rule_defs::cmd_args::traits::ArtifactPathMapper;
use crate::interpreter::rule_defs::cmd_args::traits::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::traits::CommandLineContext;

pub struct CommandLineFormatter<'a> {
    pub(crate) cli: &'a mut dyn CommandLineBuilder,
    pub(crate) context: &'a mut dyn CommandLineContext,
    pub(crate) artifact_path_mapping: &'a dyn ArtifactPathMapper,
}

impl<'a> CommandLineFormatter<'a> {
    pub fn new(
        cli: &'a mut dyn CommandLineBuilder,
        context: &'a mut dyn CommandLineContext,
        artifact_path_mapping: &'a dyn ArtifactPathMapper,
    ) -> Self {
        Self {
            cli,
            context,
            artifact_path_mapping,
        }
    }

    pub fn push_str(&mut self, s: &str) {
        self.cli.push_arg(Cow::Borrowed(s));
    }

    pub fn push_string(&mut self, s: String) {
        self.cli.push_arg(Cow::Owned(s));
    }

    pub fn push_artifact(&mut self, artifact: &Artifact) -> buck2_error::Result<()> {
        let location = self
            .context
            .resolve_artifact(artifact, self.artifact_path_mapping)?;
        self.cli.push_location(location);
        Ok(())
    }

    pub fn push_output_artifact(&mut self, artifact: &Artifact) -> buck2_error::Result<()> {
        let location = self.context.resolve_output_artifact(artifact)?;
        self.cli.push_location(location);
        Ok(())
    }

    pub fn push_cell_path(&mut self, path: CellPathRef) -> buck2_error::Result<()> {
        let location = self.context.resolve_cell_path(path)?;
        self.cli.push_location(location);
        Ok(())
    }

    pub fn push_project_path(&mut self, path: ProjectRelativePathBuf) -> buck2_error::Result<()> {
        let location = self.context.resolve_project_path(path)?;
        self.cli.push_location(location);
        Ok(())
    }
}
