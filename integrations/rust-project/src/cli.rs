/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod check;
mod develop;
mod new;

#[derive(Debug, Clone)]
pub(crate) enum Input {
    Targets(Vec<Target>),
    Files(Vec<PathBuf>),
    Buildfile(Vec<PathBuf>),
}

use std::path::PathBuf;

pub(crate) use check::Check;
pub(crate) use develop::develop_with_sysroot;
pub(crate) use develop::Develop;
pub(crate) use new::New;
pub(crate) use new::ProjectKind;

use crate::target::Target;
