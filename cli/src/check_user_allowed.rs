/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[cfg(windows)]
pub(crate) fn check_user_allowed() -> anyhow::Result<()> {
    Ok(())
}

#[cfg(not(windows))]
pub(crate) fn check_user_allowed() -> anyhow::Result<()> {
    use std::os::unix::fs::MetadataExt;

    use anyhow::Context;
    use buck2_core::fs::fs_util;
    use buck2_core::soft_error;

    #[derive(Debug, thiserror::Error)]
    #[error("buck2 is not allowed to run as root (unless home dir is owned by root)")]
    struct RootError;

    if nix::unistd::geteuid().is_root() {
        let home_dir = dirs::home_dir().context("home dir not found")?;
        let home_dir_metadata = fs_util::metadata(&home_dir)?;
        if home_dir_metadata.uid() != 0 {
            soft_error!("root_not_allowed", RootError.into())?;
        }
    }
    Ok(())
}
