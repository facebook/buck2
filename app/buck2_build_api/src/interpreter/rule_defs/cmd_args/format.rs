/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

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
}
