/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

mod check;
mod develop;
mod new;
mod target_or_file;

#[derive(Debug, Clone)]
pub(crate) enum Input {
    Targets(Vec<Target>),
    Files(Vec<PathBuf>),
    Buildfile(Vec<PathBuf>),
}

use std::path::PathBuf;

pub(crate) use check::Check;
pub(crate) use develop::Develop;
pub(crate) use develop::develop_with_sysroot;
pub(crate) use new::New;
pub(crate) use new::ProjectKind;
pub(crate) use target_or_file::TargetOrFile;

use crate::target::Target;
