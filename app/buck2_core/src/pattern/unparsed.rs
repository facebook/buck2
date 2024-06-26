/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::marker::PhantomData;

use allocative::Allocative;

use crate::fs::project_rel_path::ProjectRelativePath;
use crate::fs::project_rel_path::ProjectRelativePathBuf;
use crate::pattern::pattern_type::PatternType;

#[derive(Allocative, Debug, Clone, PartialEq, Eq)]
pub struct UnparsedPatterns<T: PatternType> {
    /// Patterns, e.g. `[":foo", "//bar/..."]`.
    patterns: Vec<String>,
    /// Patterns are to be resolved relative to this directory.
    working_dir: ProjectRelativePathBuf,
    _marker: PhantomData<T>,
}

impl<T: PatternType> UnparsedPatterns<T> {
    pub fn new(patterns: Vec<String>, working_dir: ProjectRelativePathBuf) -> Self {
        UnparsedPatterns {
            patterns,
            working_dir,
            _marker: PhantomData,
        }
    }

    pub fn patterns(&self) -> &[String] {
        &self.patterns
    }

    pub fn working_dir(&self) -> &ProjectRelativePath {
        &self.working_dir
    }
}

#[derive(Allocative, Debug, Clone, PartialEq, Eq)]
pub enum UnparsedPatternPredicate<T: PatternType> {
    Any,
    AnyOf(UnparsedPatterns<T>),
}
