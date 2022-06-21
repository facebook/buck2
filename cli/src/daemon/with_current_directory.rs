/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg_attr(windows, allow(dead_code))] // Used only on Unix.

use std::env;
use std::path::Path;
use std::path::PathBuf;

use anyhow::Context;

pub(crate) struct WithCurrentDirectory {
    current_dir: Box<PathBuf>,
}

impl WithCurrentDirectory {
    pub(crate) fn new(new_dir: &Path) -> anyhow::Result<Self> {
        let dir = Self {
            current_dir: box (env::current_dir()?),
        };
        match env::set_current_dir(&new_dir) {
            Ok(_e) => Ok(dir),
            Err(e) => Err(anyhow::anyhow!(e))
                .with_context(|| format!("Failed to change directory to `{}`", new_dir.display())),
        }
    }
}

impl Drop for WithCurrentDirectory {
    fn drop(&mut self) {
        match env::set_current_dir(&*self.current_dir) {
            Ok(_e) => {}
            Err(e) => panic!("Failed to change directory back {}", e),
        }
    }
}
