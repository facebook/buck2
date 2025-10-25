/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fs;
use std::io::Write;
use std::ops::Deref;
use std::os::fd::OwnedFd;
use std::os::unix::process::CommandExt;
use std::path::PathBuf;
use std::process::Command;
use std::sync::Arc;

use buck2_core::fs::paths::file_name::FileName;
use dupe::Dupe;
use nix::fcntl::OFlag;
use nix::sys::stat::Mode;

use crate::cgroup_files::CgroupFile;
use crate::cgroup_files::MemoryStat;
use crate::path::CgroupPath;
use crate::path::CgroupPathBuf;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Environment)]
enum CgroupError {
    #[error("{msg} IO error: {io_err}")]
    Io { msg: String, io_err: std::io::Error },
    #[error("Failed to create cgroup: {cgroup_path}, because of error {io_err}")]
    CreationFailed {
        cgroup_path: String,
        io_err: std::io::Error,
    },
    #[error("Failed to configure cgroup at {path}, because of error {io_err}")]
    ConfigurationFailed {
        path: String,
        io_err: std::io::Error,
    },
}

/// Like `Cgroup`, but more bare-bones.
///
/// Specifically, these kinds of cgroups do not have access to any of the memory files; however,
/// that means they're a little bit cheaper to open, and also they can be used in cases where the
/// memory controller has not been enabled on the cgroup
pub struct CgroupMinimal {
    /// Store the dirfd, not the more standard `DIR*`, because this one is thread safe
    dir: OwnedFd,
    /// `cgroup.procs`
    procs: Arc<CgroupFile>,
    path: CgroupPathBuf,
}

impl CgroupMinimal {
    pub fn new(root_path: &CgroupPath, name: &FileName) -> buck2_error::Result<Self> {
        let path = root_path.join(name.into());
        fs::create_dir_all(path.as_path()).map_err(|e| CgroupError::CreationFailed {
            cgroup_path: path.to_string_lossy().to_string(),
            io_err: e,
        })?;

        Self::try_from_path(path)
    }

    pub fn try_from_path(path: CgroupPathBuf) -> buck2_error::Result<Self> {
        let dir = nix::fcntl::open(
            path.as_path(),
            OFlag::O_CLOEXEC | OFlag::O_DIRECTORY,
            Mode::empty(),
        )
        .map_err(|e| CgroupError::Io {
            msg: format!("Failed to open cgroup directory: {path:?}"),
            io_err: e.into(),
        })?;

        let cgroup = Self {
            procs: Arc::new(CgroupFile::open(
                &dir,
                FileName::unchecked_new("cgroup.procs"),
                true,
            )?),
            path,
            dir,
        };

        Ok(cgroup)
    }

    pub fn path(&self) -> &CgroupPath {
        &self.path
    }

    fn subtree_control_path(&self) -> PathBuf {
        self.path().as_path().join("cgroup.subtree_control")
    }

    /// confgure cgroup.subtree_control to enable controllers for sub cgroups
    ///
    /// This enables resource memory controller on the current cgroup, allowing
    /// child cgroups to use those controllers.
    ///
    /// Note: Due to the "no internal processes" rule in cgroups v2, once you enable any
    /// controller in `cgroup.subtree_control`, this cgroup must not directly contain any
    /// processes (i.e., `cgroup.procs` must be empty). All processes must be placed in
    /// child(leaf) cgroups instead. Otherwise, writing to `cgroup.subtree_control` will fail with
    /// a "Device or resource busy" error.
    ///
    /// For more info, see:
    /// <https://www.kernel.org/doc/html/latest/admin-guide/cgroup-v2.html#no-internal-processes>
    ///
    /// Maybe need two types of cgroup to distinguish.
    pub fn config_subtree_control(&self) -> buck2_error::Result<()> {
        // We only need to configure this once, so no need to hold the fd.
        let sub_tree_control_file_path = self.subtree_control_path();

        fs::write(&sub_tree_control_file_path, "+memory").map_err(|e| {
            CgroupError::ConfigurationFailed {
                path: sub_tree_control_file_path.to_string_lossy().to_string(),
                io_err: e,
            }
        })?;

        Ok(())
    }

    /// Configures a Command to run in this cgroup using pre_exec.
    ///
    /// Uses pre_exec hook which runs in the child process after fork() but before exec().
    /// This ensures the process is migrated to the cgroup before it starts executing.
    ///
    /// Parent process: Command new() -> ... -> spwan()
    ///                  ↓
    /// System: fork() creates child process
    ///                  ↓
    /// Child process: pre_exec() closure runs (cgroup migration)
    ///                  ↓
    /// Child process: exec() run the command
    pub fn setup_command(&self, command: &mut Command) -> buck2_error::Result<()> {
        // NOTE: We need to make sure that this fd is kept alive until the point where the command
        // is spawned/forked. The lifetimes on this type don't prevent the user from dropping the
        // `Cgroup` before that happens.
        //
        // This is accomplished by capturing the `Arc<CgroupFile>` into the `pre_exec` closure. The
        // closure is not dropped until the surrounding command is dropped, at which point the spawn
        // must have happened.
        //
        // Some testing shows that the closure is not dropped in the child process at all; that's
        // good, as we don't actually want to do the `free` that may be implicated by dropping the
        // `Arc`
        let procs = self.procs.dupe();

        let pre_exec = move || {
            let pid = std::process::id();

            // Write PID to stack-allocated buffer instead of heap allocating by `pid.to_string().as_bytes()`
            let mut buf = [0u8; 16]; // u32::MAX is 10 digits, so 16 bytes is plenty
            let mut cursor = std::io::Cursor::new(&mut buf[..]);
            write!(cursor, "{pid}").map_err(std::io::Error::from)?;
            let pos = cursor.position() as usize;
            let pid_bytes = &buf[..pos];

            procs.write(&pid_bytes)
        };
        // Safety: The unsafe block is required for pre_exec which is inherently unsafe due to fork/exec restrictions.
        // However, it's safe here because:
        // 1. We only call async-signal-safe functions (write to file)
        // 2. No memory allocation or complex operations that could deadlock
        unsafe {
            command.pre_exec(pre_exec);
        }
        Ok(())
    }

    pub fn add_process(&self, pid: u32) -> buck2_error::Result<()> {
        let pid = pid.to_string();
        Ok(self.procs.write(pid.as_bytes())?)
    }
}

pub struct Cgroup {
    inner: CgroupMinimal,
    memory_stat: CgroupFile,
}

impl Cgroup {
    pub fn new(root_path: &CgroupPath, name: &FileName) -> buck2_error::Result<Self> {
        Self::from_minimal(CgroupMinimal::new(root_path, name)?)
    }

    pub fn try_from_path(path: CgroupPathBuf) -> buck2_error::Result<Self> {
        Self::from_minimal(CgroupMinimal::try_from_path(path)?)
    }

    pub fn from_minimal(m: CgroupMinimal) -> buck2_error::Result<Self> {
        Ok(Cgroup {
            memory_stat: CgroupFile::open(&m.dir, FileName::unchecked_new("memory.stat"), false)?,
            inner: m,
        })
    }

    #[allow(dead_code)]
    fn memory_peak_path(&self) -> PathBuf {
        self.path().as_path().join("memory.peak")
    }

    fn memory_high_path(&self) -> PathBuf {
        self.path().as_path().join("memory.high")
    }

    /// Set the memory.high limit for this cgroup
    pub fn set_memory_high(&self, memory_high: &str) -> buck2_error::Result<()> {
        let memory_high_file_path = self.memory_high_path();
        fs::write(&memory_high_file_path, memory_high).map_err(|e| {
            CgroupError::ConfigurationFailed {
                path: memory_high_file_path.to_string_lossy().to_string(),
                io_err: e,
            }
        })?;
        Ok(())
    }

    pub fn read_memory_stat(&self) -> buck2_error::Result<MemoryStat> {
        self.memory_stat.read_memory_stat()
    }
}

impl Deref for Cgroup {
    type Target = CgroupMinimal;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[cfg(test)]
impl Cgroup {
    pub(crate) fn create_for_test() -> Option<Self> {
        use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
        use buck2_util::process::background_command;

        if !cfg!(buck_build) || !cfg!(target_os = "linux") {
            return None;
        }

        let prep_script = std::env::var("PREP_CGROUP_SCRIPT").unwrap();
        let path = background_command(&prep_script)
            .stdout(std::process::Stdio::piped())
            .output()
            .unwrap()
            .stdout;
        let path = String::from_utf8(path).unwrap();
        let path = CgroupPath::new(AbsNormPath::new(path.trim()).unwrap());
        Some(Self::try_from_path(path.to_buf()).unwrap())
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::fs::fs_util;
    use buck2_util::process::background_command;

    use crate::cgroup::Cgroup;

    #[test]
    fn self_test_harness() {
        let Some(cgroup) = Cgroup::create_for_test() else {
            return;
        };

        assert_eq!(
            fs_util::try_exists(cgroup.path().as_abs_path()).unwrap(),
            true
        );
        assert_eq!(cgroup.procs.read_to_buf().unwrap().len(), 0);
    }

    #[test]
    fn repro_drop_cgroup_before_command_spawn() {
        let Some(cgroup) = Cgroup::create_for_test() else {
            return;
        };

        let mut cmd = background_command("true");
        cgroup.setup_command(&mut cmd).unwrap();

        drop(cgroup);

        assert!(cmd.status().unwrap().success());
    }
}
