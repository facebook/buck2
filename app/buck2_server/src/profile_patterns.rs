/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashMap;
use std::sync::Mutex;

use buck2_error::internal_error;
use buck2_fs::fs_util;
use buck2_fs::paths::abs_path::AbsPathBuf;
use buck2_fs::paths::file_name::FileName;
use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_interpreter::factory::ProfileEventListener;
use dupe::Dupe;
use itertools::Itertools;

pub(crate) struct FileWritingProfileEventListener {
    base_path: AbsPathBuf,
    state: Mutex<State>,
}

struct State {
    written: HashMap<ForwardRelativePathBuf, usize>,
    evaluations: Vec<String>,
    errors: Vec<buck2_error::Error>,
}

impl FileWritingProfileEventListener {
    pub(crate) fn new(base_path: AbsPathBuf) -> Self {
        Self {
            base_path,
            state: Mutex::new(State {
                written: HashMap::new(),
                evaluations: Vec::new(),
                errors: Vec::new(),
            }),
        }
    }
}

impl FileWritingProfileEventListener {
    /// Writes the all_keys.list file and returns an error if any occurred while writing the profile files.
    pub fn finalize(&self) -> buck2_error::Result<()> {
        let lock = self.state.lock().unwrap();
        fs_util::create_dir_all(&self.base_path)?;
        fs_util::write(
            self.base_path.join("all_keys.list"),
            lock.evaluations.iter().join("\n"),
        )?;
        if let Some(e) = lock.errors.first() {
            return Err(e.dupe());
        }
        Ok(())
    }

    fn handle_profile_collected(
        &self,
        eval_kind: buck2_interpreter::dice::starlark_provider::StarlarkEvalKind,
        profile_data: &std::sync::Arc<
            buck2_interpreter::starlark_profiler::data::StarlarkProfileDataAndStats,
        >,
    ) -> buck2_error::Result<()> {
        let subpath = eval_kind.as_path()?;

        let suffix = {
            let mut lock = self.state.lock().unwrap();
            lock.evaluations.push(eval_kind.to_string());
            match lock.written.entry(subpath.clone()) {
                std::collections::hash_map::Entry::Occupied(mut occupied_entry) => {
                    *occupied_entry.get_mut() += 1;
                    format!("-{}", *occupied_entry.get())
                }
                std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(1);
                    "".to_owned()
                }
            }
        };

        let subpath = subpath
            .parent()
            .ok_or_else(|| internal_error!("profiling path has no parent"))?
            .join(FileName::new(&format!(
                "{}{}.profile",
                subpath
                    .file_name()
                    .ok_or_else(|| internal_error!("profiling path has no filename"))?,
                suffix,
            ))?);

        let output_path = self.base_path.join(subpath.as_path());
        fs_util::create_dir_all(output_path.parent().unwrap())?;
        fs_util::write(output_path, profile_data.profile_data.gen_csv()?)?;
        Ok(())
    }
}

impl ProfileEventListener for FileWritingProfileEventListener {
    fn profile_collected(
        &self,
        eval_kind: buck2_interpreter::dice::starlark_provider::StarlarkEvalKind,
        profile_data: &std::sync::Arc<
            buck2_interpreter::starlark_profiler::data::StarlarkProfileDataAndStats,
        >,
    ) {
        if let Err(e) = self.handle_profile_collected(eval_kind, profile_data) {
            let mut lock = self.state.lock().unwrap();
            lock.errors.push(e);
        }
    }
}
