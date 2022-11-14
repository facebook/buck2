/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub(crate) enum BuckConfigFile {
    // Buckconfig file in the cell relative to project root, such as .buckconfig or .buckconfig.local
    ProjectRelativeFile(&'static str),

    // Buckconfig folder in the cell, assuming all files in this folder are buckconfig
    ProjectRelativeFolder(&'static str),

    // Buckconfig file in the user's home directory
    UserFile(&'static str),

    // Buckconfig folder in the user's home directory, assuming all files in this folder are buckconfig
    UserFolder(&'static str),

    // Global buckconfig file. Repo related config is not allowed
    GlobalFile(&'static str),

    // Global buckconfig folder, assuming all files in this folder are buckconfig. Repo related config is not allowed
    GlobalFolder(&'static str),
}

/// The override order of buck config, from highest priority to lowest
/// 1. .buckconfig.local in repo
/// 2. .buckconfig in repo
/// 3. files in .buckconfig.d folder in repo
/// 4. .buckconfig.local in user's home directory
/// 5. files in .buckconfig.d folder in user's home directory
/// 6. global file /etc/buckconfig
/// 7. files in global directory /etc/buckconfig.d
pub(crate) static DEFAULT_BUCK_CONFIG_FILES: &[BuckConfigFile] = &[
    #[cfg(not(windows))]
    BuckConfigFile::GlobalFolder("/etc/buckconfig.d"),
    #[cfg(not(windows))]
    BuckConfigFile::GlobalFile("/etc/buckconfig"),
    // TODO: use %PROGRAMDATA% on Windows
    #[cfg(windows)]
    BuckConfigFile::GlobalFolder("C:/ProgramData/buckconfig.d"),
    #[cfg(windows)]
    BuckConfigFile::GlobalFile("C:/ProgramData/buckconfig"),
    BuckConfigFile::UserFolder(".buckconfig.d"),
    BuckConfigFile::UserFile(".buckconfig.local"),
    BuckConfigFile::ProjectRelativeFolder(".buckconfig.d"),
    BuckConfigFile::ProjectRelativeFile(".buckconfig"),
    BuckConfigFile::ProjectRelativeFile(".buckconfig.local"),
];
