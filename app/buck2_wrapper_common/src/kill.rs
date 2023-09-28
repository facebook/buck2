/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Cross-platform process killing.

pub fn process_exists(pid: u32) -> anyhow::Result<bool> {
    os_specific::process_exists(pid)
}

/// Send `KILL` or call `TerminateProcess` on the given process.
///
/// Returns a KilledProcessHandle that can be used to observe the termination of the killed process.
pub fn kill(pid: u32) -> anyhow::Result<Box<dyn KilledProcessHandle>> {
    match os_specific::kill(pid)? {
        Some(handle) => Ok(Box::new(handle) as _),
        None => Ok(Box::new(NoProcess) as _),
    }
}

pub trait KilledProcessHandle {
    fn has_exited(&self) -> anyhow::Result<bool>;

    fn status(&self) -> Option<String>;
}

/// Get the status of a given process according to sysinfo.
pub fn get_sysinfo_status(pid: impl TryInto<u32>) -> Option<String> {
    use sysinfo::Pid;
    use sysinfo::PidExt;
    use sysinfo::ProcessExt;
    use sysinfo::ProcessRefreshKind;
    use sysinfo::System;
    use sysinfo::SystemExt;

    let pid = pid.try_into().ok()?;
    let pid = Pid::from_u32(pid);

    let mut system = System::new();
    system.refresh_process_specifics(pid, ProcessRefreshKind::new());

    let proc = system.process(pid)?;
    Some(proc.status().to_string())
}

/// Returned when os_specific::kill reports that nothing was killed because the process wasn't even
/// running.
struct NoProcess;

impl KilledProcessHandle for NoProcess {
    fn has_exited(&self) -> anyhow::Result<bool> {
        Ok(true)
    }

    fn status(&self) -> Option<String> {
        Some("NoProcess".to_owned())
    }
}

#[cfg(unix)]
mod os_specific {
    use anyhow::Context;
    use nix::sys::signal::Signal;

    use crate::kill::get_sysinfo_status;
    use crate::kill::KilledProcessHandle;

    pub(crate) fn process_exists(pid: u32) -> anyhow::Result<bool> {
        let pid = nix::unistd::Pid::from_raw(
            pid.try_into()
                .with_context(|| format!("Integer overflow converting pid {} to pid_t", pid))?,
        );
        match nix::sys::signal::kill(pid, None) {
            Ok(_) => Ok(true),
            Err(nix::errno::Errno::ESRCH) => Ok(false),
            Err(e) => Err(e)
                .with_context(|| format!("unexpected error checking if process {} exists", pid)),
        }
    }

    fn process_exists_impl(pid: nix::unistd::Pid) -> anyhow::Result<bool> {
        match nix::sys::signal::kill(pid, None) {
            Ok(_) => Ok(true),
            Err(nix::errno::Errno::ESRCH) => Ok(false),
            Err(e) => Err(e)
                .with_context(|| format!("unexpected error checking if process {} exists", pid)),
        }
    }

    pub(super) fn kill(pid: u32) -> anyhow::Result<Option<impl KilledProcessHandle>> {
        let pid = nix::unistd::Pid::from_raw(
            pid.try_into()
                .with_context(|| format!("Integer overflow converting pid {} to pid_t", pid))?,
        );

        match nix::sys::signal::kill(pid, Signal::SIGKILL) {
            Ok(()) => Ok(Some(UnixKilledProcessHandle { pid })),
            Err(nix::errno::Errno::ESRCH) => Ok(None),
            Err(e) => Err(e).with_context(|| format!("Failed to kill pid {}", pid)),
        }
    }

    struct UnixKilledProcessHandle {
        pid: nix::unistd::Pid,
    }

    impl KilledProcessHandle for UnixKilledProcessHandle {
        fn has_exited(&self) -> anyhow::Result<bool> {
            Ok(!process_exists_impl(self.pid)?)
        }

        fn status(&self) -> Option<String> {
            get_sysinfo_status(self.pid.as_raw())
        }
    }
}

#[cfg(windows)]
pub mod os_specific {
    use std::time::Duration;

    use crate::kill::get_sysinfo_status;
    use crate::kill::KilledProcessHandle;
    use crate::winapi_process::WinapiProcessHandle;

    pub(crate) fn process_exists(pid: u32) -> anyhow::Result<bool> {
        Ok(WinapiProcessHandle::open_for_info(pid).is_some())
    }

    pub(super) fn kill(pid: u32) -> anyhow::Result<Option<impl KilledProcessHandle>> {
        let handle = match WinapiProcessHandle::open_for_terminate(pid) {
            Some(proc_handle) => proc_handle,
            None => return Ok(None),
        };

        handle.terminate()?;

        Ok(Some(WindowsKilledProcessHandle { handle }))
    }

    /// Windows reuses PIDs more aggressively than UNIX, so there we add an extra guard in the form
    /// of the process creation time.
    struct WindowsKilledProcessHandle {
        handle: WinapiProcessHandle,
    }

    impl KilledProcessHandle for WindowsKilledProcessHandle {
        fn has_exited(&self) -> anyhow::Result<bool> {
            self.handle.has_exited()
        }

        fn status(&self) -> Option<String> {
            // Maybe there is a better way to get this via the handle, but for now this'll do.
            get_sysinfo_status(self.handle.pid())
        }
    }

    /// Returns process creation time with 100 ns precision.
    pub fn process_creation_time(pid: u32) -> Option<Duration> {
        let proc_handle = WinapiProcessHandle::open_for_info(pid)?;
        proc_handle.process_creation_time().ok()
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;
    use std::time::Instant;

    use buck2_util::process::background_command;

    use crate::kill::kill;
    use crate::kill::process_exists;

    #[test]
    fn test_process_exists_kill() {
        let mut command = if !cfg!(windows) {
            let mut command = background_command("sh");
            command.args(["-c", "sleep 10000"]);
            command
        } else {
            let mut command = background_command("powershell");
            command.args(["-c", "Start-Sleep -Seconds 10000"]);
            command
        };
        let mut child = command.spawn().unwrap();
        let pid = child.id();
        for _ in 0..5 {
            assert!(process_exists(pid).unwrap());
        }

        let handle = kill(pid).unwrap();

        child.wait().unwrap();
        // Drop child to ensure the Windows handle is closed.
        drop(child);

        if !cfg!(windows) {
            assert!(handle.has_exited().unwrap());
        } else {
            let start = Instant::now();
            loop {
                if handle.has_exited().unwrap() {
                    break;
                }
                assert!(
                    start.elapsed() < Duration::from_secs(20),
                    "Timed out waiting for process to die"
                );
                std::thread::sleep(Duration::from_millis(100));
            }
        }
    }
}
