/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::os::windows::io::AsRawHandle;
use std::os::windows::process::ChildExt;
use std::os::windows::process::CommandExt;
use std::process::Child;
use std::process::Command;
use std::process::ExitStatus;
use std::process::Stdio;
use std::time::Duration;

use buck2_error::BuckErrorContext;
use tokio::io;
use tokio::process::ChildStderr;
use tokio::process::ChildStdout;
use winapi::um::processthreadsapi;

use crate::win::child_process::ChildProcess;
use crate::win::job_object::JobObject;
use crate::win::utils::result_dword;

pub(crate) struct ProcessCommandImpl {
    inner: Command,
}

impl ProcessCommandImpl {
    pub(crate) fn new(mut cmd: Command) -> Self {
        // On windows we create suspended process to assign it to a job (group) and then resume.
        // This is necessary because the process might finish before we add it to a job
        cmd.creation_flags(
            winapi::um::winbase::CREATE_NO_WINDOW | winapi::um::winbase::CREATE_SUSPENDED,
        );
        Self { inner: cmd }
    }

    pub(crate) fn spawn(&mut self) -> io::Result<Child> {
        self.inner.spawn()
    }

    #[allow(dead_code)]
    pub(crate) fn stdout(&mut self, stdout: Stdio) {
        self.inner.stdout(stdout);
    }

    #[allow(dead_code)]
    pub(crate) fn stderr(&mut self, stdout: Stdio) {
        self.inner.stderr(stdout);
    }
}

/// Keeps track of the exit status of a child process without worrying about
/// polling the underlying futures even after they have completed.
enum FusedChild {
    Child(ChildProcess),
    Done(ExitStatus),
}

impl FusedChild {
    fn as_option(&self) -> Option<&ChildProcess> {
        match &self {
            FusedChild::Child(child) => Some(child),
            FusedChild::Done(_) => None,
        }
    }

    fn as_option_mut(&mut self) -> Option<&mut ChildProcess> {
        match self {
            FusedChild::Child(child) => Some(child),
            FusedChild::Done(_) => None,
        }
    }
}

pub(crate) struct ProcessGroupImpl {
    child: FusedChild,
    job: JobObject,
}

impl ProcessGroupImpl {
    pub(crate) fn new(child: Child) -> buck2_error::Result<ProcessGroupImpl> {
        let job = JobObject::new()?;
        job.assign_process(child.as_raw_handle())?;
        let process = ProcessGroupImpl {
            child: FusedChild::Child(ChildProcess::new(child)),
            job,
        };
        // We create suspended process to assign it to a job (group)
        // So we resume the process after assignment
        process.resume()?;
        Ok(process)
    }

    pub(crate) fn take_stdout(&mut self) -> Option<ChildStdout> {
        self.child
            .as_option_mut()?
            .as_std_mut()
            .stdout
            .take()
            .and_then(|s| ChildStdout::from_std(s).ok())
    }

    pub(crate) fn take_stderr(&mut self) -> Option<ChildStderr> {
        self.child
            .as_option_mut()?
            .as_std_mut()
            .stderr
            .take()
            .and_then(|s| ChildStderr::from_std(s).ok())
    }

    pub(crate) async fn wait(&mut self) -> io::Result<ExitStatus> {
        match &mut self.child {
            FusedChild::Done(exit) => Ok(*exit),
            FusedChild::Child(child) => {
                // Ensure stdin is closed so the child isn't stuck waiting on
                // input while the parent is waiting for it to exit.
                drop(child.as_std_mut().stdin.take());
                let ret = child.await;

                if let Ok(exit) = ret {
                    self.child = FusedChild::Done(exit);
                }

                ret
            }
        }
    }

    pub(crate) fn id(&self) -> Option<u32> {
        Some(self.child.as_option()?.as_std().id())
    }

    // On Windows we use JobObject API to kill the whole process tree
    pub(crate) async fn kill(
        &self,
        _graceful_shutdown_timeout_s: Option<u32>,
    ) -> buck2_error::Result<()> {
        tokio::time::timeout(Duration::from_secs(10), self.job.terminate(0))
            .await
            .map_err(|_| {
                buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Tier0,
                    "Timed out on job object termination"
                )
            })?
    }

    fn resume(&self) -> buck2_error::Result<()> {
        let handle = self
            .child
            .as_option()
            .buck_error_context("can't resume an exited process")?
            .as_std()
            .main_thread_handle()
            .as_raw_handle();
        result_dword(unsafe { processthreadsapi::ResumeThread(handle) })
    }
}
