/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::os::windows::process::CommandExt;
use std::process::Command as StdCommand;
use std::process::ExitStatus;
use std::process::Stdio;

use buck2_wrapper_common::winapi_handle::WinapiHandle;
use tokio::io;
use tokio::process::Child;
use tokio::process::ChildStderr;
use tokio::process::ChildStdout;
use tokio::process::Command;
use winapi::shared::minwindef;
use winapi::um::handleapi;
use winapi::um::processthreadsapi;
use winapi::um::tlhelp32;
use winapi::um::winnt;

use crate::win::job_object::JobObject;

pub struct ProcessCommandImpl {
    inner: Command,
}

impl ProcessCommandImpl {
    pub fn new(mut cmd: StdCommand) -> Self {
        // On windows we create suspended process to assign it to a job (group) and then resume.
        // This is necessary because the process might finish before we add it to a job
        cmd.creation_flags(
            winapi::um::winbase::CREATE_NO_WINDOW | winapi::um::winbase::CREATE_SUSPENDED,
        );
        Self { inner: cmd.into() }
    }

    pub fn spawn(&mut self) -> io::Result<Child> {
        self.inner.spawn()
    }

    pub fn stdout(&mut self, stdout: Stdio) {
        self.inner.stdout(stdout);
    }

    pub fn stderr(&mut self, stdout: Stdio) {
        self.inner.stderr(stdout);
    }
}

pub struct ProcessGroupImpl {
    inner: Child,
    job: JobObject,
}

impl ProcessGroupImpl {
    pub fn new(child: Child) -> anyhow::Result<ProcessGroupImpl> {
        let job = JobObject::new()?;
        if let Some(handle) = child.raw_handle() {
            job.assign_process(handle)?;
        }
        let process = ProcessGroupImpl { inner: child, job };
        // We create suspended process to assign it to a job (group)
        // So we resume the process after assignment
        process.resume()?;
        Ok(process)
    }

    pub fn take_stdout(&mut self) -> Option<ChildStdout> {
        self.inner.stdout.take()
    }

    pub fn take_stderr(&mut self) -> Option<ChildStderr> {
        self.inner.stderr.take()
    }

    pub async fn wait(&mut self) -> io::Result<ExitStatus> {
        self.inner.wait().await
    }

    pub fn id(&self) -> Option<u32> {
        self.inner.id()
    }

    // On Windows we use JobObject API to kill the whole process tree
    pub async fn kill(&self, _graceful_shutdown_timeout_s: Option<u32>) -> anyhow::Result<()> {
        self.job.terminate(0)
    }

    fn resume(&self) -> anyhow::Result<()> {
        let process_id = self
            .inner
            .id()
            .ok_or_else(|| anyhow::anyhow!("Failed to get the process id"))?;
        unsafe {
            let main_thread_id = get_main_thread(process_id)?;
            resume_thread(main_thread_id)
        }
    }
}

// We need main thread handle to resume process after assigning it to JobObject
// Currently there is no way to get main thread handle from tokio's process::Child
// We use CreateToolhelp32Snapshot to get a snapshot of all threads in the system
// and find the one with the given process_id.
// todo(yurysamkevich): use main_thread_handle once issue is closed
// https://github.com/tokio-rs/tokio/issues/6153
unsafe fn get_main_thread(process_id: u32) -> anyhow::Result<minwindef::DWORD> {
    let snapshot_handle = tlhelp32::CreateToolhelp32Snapshot(tlhelp32::TH32CS_SNAPTHREAD, 0);
    if snapshot_handle == handleapi::INVALID_HANDLE_VALUE {
        return Err(anyhow::anyhow!("Failed to list threads"));
    }
    let snapshot_handle = WinapiHandle::new(snapshot_handle);
    let mut thread_entry_32 = tlhelp32::THREADENTRY32 {
        dwSize: std::mem::size_of::<tlhelp32::THREADENTRY32>() as u32,
        cntUsage: 0,
        th32ThreadID: 0,
        th32OwnerProcessID: 0,
        tpBasePri: 0,
        tpDeltaPri: 0,
        dwFlags: 0,
    };
    let raw_pointer_to_thread_entry_32 = &mut thread_entry_32 as *mut tlhelp32::THREADENTRY32;

    let mut thread_result =
        tlhelp32::Thread32First(snapshot_handle.handle(), raw_pointer_to_thread_entry_32);
    while thread_result == minwindef::TRUE {
        if thread_entry_32.dwSize as usize
            >= std::mem::offset_of!(tlhelp32::THREADENTRY32, th32OwnerProcessID)
        {
            if thread_entry_32.th32OwnerProcessID == process_id {
                return Ok(thread_entry_32.th32ThreadID);
            }
        }
        thread_result = winapi::um::tlhelp32::Thread32Next(
            snapshot_handle.handle(),
            raw_pointer_to_thread_entry_32,
        );
    }
    Err(anyhow::anyhow!("Failed to find thread to resume"))
}

unsafe fn resume_thread(thread_id: minwindef::DWORD) -> anyhow::Result<()> {
    let thread_handle =
        processthreadsapi::OpenThread(winnt::THREAD_SUSPEND_RESUME, minwindef::FALSE, thread_id);
    if thread_handle.is_null() {
        return Err(anyhow::anyhow!("Failed to open thread to resume"));
    }
    let thread_handle = WinapiHandle::new(thread_handle);
    let resume_thread_result = processthreadsapi::ResumeThread(thread_handle.handle());
    if resume_thread_result == minwindef::DWORD::MAX {
        Err(anyhow::anyhow!("Failed to resume thread"))
    } else {
        Ok(())
    }
}
