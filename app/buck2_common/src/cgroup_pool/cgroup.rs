/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::fs;
use std::hash::Hash;
use std::io::Write;
use std::os::fd::AsFd;
use std::os::fd::AsRawFd;
use std::os::fd::BorrowedFd;
use std::os::fd::OwnedFd;
use std::os::unix::process::CommandExt;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

use dupe::Dupe;
use nix::dir::Dir;
use nix::fcntl::OFlag;
use nix::fcntl::openat;
use nix::sys::stat::Mode;
use nix::unistd;

/// A unique identifier for a cgroup
#[derive(Debug, Clone, PartialEq, Eq, Hash, Dupe)]
pub struct CgroupID(usize);

impl CgroupID {
    pub(super) fn new(id: usize) -> Self {
        Self(id)
    }
}

impl fmt::Display for CgroupID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "CgroupID {}", self.0)
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Environment)]
pub enum CgroupError {
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
    #[error("Cgroup v2 is not available.")]
    CgroupV2NotAvailable,
    #[error(
        "Incomplete write to cgroup.procs: wrote {bytes_written} bytes, expected {expected_bytes}"
    )]
    IncompleteProcsWrite {
        bytes_written: usize,
        expected_bytes: usize,
    },
    #[error("Failed to find cgroup with id {id} in cgroup pool")]
    CgroupIDNotFound { id: CgroupID },
    #[error("Integer conversion failed: {err}")]
    IntegerConversion { err: std::num::TryFromIntError },
}

pub(super) struct Cgroup {
    id: CgroupID,
    /// FD for cgroup.procs
    procs_fd: OwnedFd,
    path: PathBuf,
}

impl Cgroup {
    pub(super) fn new(root_path: PathBuf, name: String, id: usize) -> Result<Self, CgroupError> {
        let path = root_path.join(name.clone());
        fs::create_dir_all(&path).map_err(|e| CgroupError::CreationFailed {
            cgroup_path: path.to_string_lossy().to_string(),
            io_err: e,
        })?;

        Self::try_from_path(path, id)
    }

    pub(super) fn try_from_path(path: PathBuf, id: usize) -> Result<Self, CgroupError> {
        let dir: Dir =
            nix::dir::Dir::open(&path, OFlag::O_CLOEXEC, Mode::empty()).map_err(|e| {
                CgroupError::Io {
                    msg: format!("Failed to open cgroup directory: {path:?}"),
                    io_err: e.into(),
                }
            })?;

        let procs_fd = openat(
            &dir,
            "cgroup.procs",
            OFlag::O_CLOEXEC | OFlag::O_RDWR,
            Mode::empty(),
        )
        .map_err(|e| CgroupError::Io {
            msg: format!("Failed to open cgroup.procs file: {path:?}"),
            io_err: e.into(),
        })?;

        let id = CgroupID::new(id);

        let cgroup = Self { id, procs_fd, path };

        Ok(cgroup)
    }

    /// Get the unique identifier for this cgroup
    pub(super) fn id(&self) -> &CgroupID {
        &self.id
    }

    pub(super) fn path(&self) -> &Path {
        &self.path
    }

    fn subtree_control_path(&self) -> PathBuf {
        self.path().join("cgroup.subtree_control")
    }

    #[allow(dead_code)]
    fn memory_peak_path(&self) -> PathBuf {
        self.path().join("memory.peak")
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
    /// https://www.kernel.org/doc/html/latest/admin-guide/cgroup-v2.html#no-internal-processes
    ///
    /// Maybe need two types of cgroup to distinguish.
    pub(super) fn config_subtree_control(&self) -> Result<(), CgroupError> {
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
    pub fn setup_command<'a>(
        &self,
        command: &'a mut Command,
    ) -> Result<&'a mut Command, CgroupError> {
        let procs_fd_raw = self.procs_fd.as_raw_fd();

        // Safety: The unsafe block is required for pre_exec which is inherently unsafe due to fork/exec restrictions.
        // However, it's safe here because:
        // 1. We only call async-signal-safe functions (write to file)
        // 2. No memory allocation or complex operations that could deadlock
        // 3. The raw FD remains valid post-fork since file descriptors are inherited
        Ok(unsafe {
            command.pre_exec(move || {
                let pid = std::process::id();

                // Write PID to stack-allocated buffer instead of heap allocating by `pid.to_string().as_bytes()`
                let mut buf = [0u8; 16]; // u32::MAX is 10 digits, so 16 bytes is plenty
                let mut cursor = std::io::Cursor::new(&mut buf[..]);
                write!(cursor, "{pid}").map_err(std::io::Error::from)?;
                let pos = cursor.position() as usize;
                let pid_bytes = &buf[..pos];

                // Append the process pid to cgroup.procs
                // Note: Unlike regular files, cgroup.procs writes are atomic for single PID entries.
                // So we don't need to worry too much about partial writes.
                let procs_fd = BorrowedFd::borrow_raw(procs_fd_raw);
                let bytes_written =
                    unistd::write(procs_fd, pid_bytes).map_err(std::io::Error::from)?;

                if bytes_written != pid_bytes.len() {
                    return Err(std::io::Error::other(format!(
                        "cgroup.procs write was incomplete: wrote {} bytes, expected {}",
                        bytes_written,
                        pid_bytes.len()
                    )));
                }

                Ok(())
            })
        })
    }

    pub(super) fn move_process_to(&self, cgroup: &Cgroup) -> Result<(), CgroupError> {
        // Read process IDs from current cgroup's procs_fd
        let content = read_file(&self.procs_fd)?;
        if content.is_empty() {
            return Ok(()); // No processes need to be moved
        }
        let procs_content = std::str::from_utf8(&content).map_err(|e| CgroupError::Io {
            msg: "Failed to parse cgroup.procs content as UTF-8".to_owned(),
            io_err: std::io::Error::new(std::io::ErrorKind::InvalidData, e),
        })?;

        // Parse PIDs and move them to the target cgroup
        for line in procs_content.lines() {
            let pid = line.trim();
            if !pid.is_empty() {
                // Note: Unlike regular files, cgroup.procs writes are atomic for single PID entries.
                // So we don't need to worry too much about partial writes.
                let bytes_written = unistd::write(cgroup.procs_fd.as_fd(), pid.as_bytes())
                    .map_err(|e| CgroupError::Io {
                        msg: format!("Failed to write PID {pid} to target cgroup"),
                        io_err: e.into(),
                    })?;
                if bytes_written != pid.len() {
                    return Err(CgroupError::IncompleteProcsWrite {
                        bytes_written,
                        expected_bytes: pid.len(),
                    });
                }
            }
        }

        Ok(())
    }
}

fn read_file(fd: &OwnedFd) -> Result<Vec<u8>, CgroupError> {
    let mut data = vec![0u8; 1000]; // Enough in practice
    let mut filled = 0;
    loop {
        if filled == data.len() {
            data.resize(data.len() * 2, 0);
        }

        let buf = &mut data[filled..];
        let filled_converted = filled
            .try_into()
            .map_err(|err| CgroupError::IntegerConversion { err })?;
        let read =
            nix::sys::uio::pread(fd, buf, filled_converted).map_err(|e| CgroupError::Io {
                msg: "Failed to read from target cgroup.procs file descriptor".to_owned(),
                io_err: e.into(),
            })?;
        if read == 0 {
            break;
        }
        filled += read;
    }
    data.truncate(filled);
    Ok(data)
}
