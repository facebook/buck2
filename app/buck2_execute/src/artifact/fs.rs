/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_core::execution_types::executor_config::PathSeparatorKind;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;

pub struct ExecutorFs<'a> {
    fs: &'a ArtifactFs,
    path_separator: PathSeparatorKind,
}

impl<'a> ExecutorFs<'a> {
    pub fn new(fs: &'a ArtifactFs, path_separator: PathSeparatorKind) -> Self {
        Self { fs, path_separator }
    }

    pub fn fs(&self) -> &ArtifactFs {
        self.fs
    }

    pub fn path_separator(&self) -> PathSeparatorKind {
        self.path_separator
    }
}
