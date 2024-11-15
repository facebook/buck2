/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(dead_code)]

use std::mem;
use std::ptr;
use std::sync::Arc;

use buck2_error::BuckErrorContext;
use buck2_wrapper_common::win::winapi_handle::WinapiHandle;
use dupe::Dupe;
use winapi::shared::basetsd::ULONG_PTR;
use winapi::shared::minwindef::DWORD;
use winapi::shared::minwindef::FALSE;
use winapi::shared::minwindef::LPVOID;
use winapi::um::handleapi;
use winapi::um::ioapiset;
use winapi::um::jobapi2;
use winapi::um::minwinbase::OVERLAPPED;
use winapi::um::winbase::INFINITE;
use winapi::um::winnt::JobObjectAssociateCompletionPortInformation;
use winapi::um::winnt::JobObjectExtendedLimitInformation;
use winapi::um::winnt::HANDLE;
use winapi::um::winnt::JOBOBJECT_ASSOCIATE_COMPLETION_PORT;
use winapi::um::winnt::JOBOBJECT_EXTENDED_LIMIT_INFORMATION;
use winapi::um::winnt::JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
use winapi::um::winnt::JOB_OBJECT_MSG_ACTIVE_PROCESS_ZERO;

use crate::win::utils::result_bool;

pub(crate) struct JobObject {
    job_handle: Arc<WinapiHandle>,
    completion_handle: Arc<WinapiHandle>,
}

impl JobObject {
    pub(crate) fn new() -> buck2_error::Result<Self> {
        let job_handle = unsafe {
            WinapiHandle::new_check_last_os_error(jobapi2::CreateJobObjectW(
                ptr::null_mut(),
                ptr::null_mut(),
            ))
            .buck_error_context("CreateJobObject")?
        };

        let completion_handle = unsafe {
            WinapiHandle::new_check_last_os_error(ioapiset::CreateIoCompletionPort(
                handleapi::INVALID_HANDLE_VALUE, // FileHandle
                ptr::null_mut(),                 // ExistingCompletionPort
                0,                               // CompletionKey
                1,                               // NumberOfConcurrentThreads
            ))
            .buck_error_context("CreateIoCompletionPort")?
        };

        associate_job_with_completion_port(&job_handle, &completion_handle)?;
        set_job_limits(&job_handle)?;

        Ok(Self {
            job_handle: Arc::new(job_handle),
            completion_handle: Arc::new(completion_handle),
        })
    }

    pub(crate) fn assign_process(&self, process: HANDLE) -> buck2_error::Result<()> {
        result_bool(unsafe { jobapi2::AssignProcessToJobObject(self.job_handle.handle(), process) })
    }

    pub(crate) async fn terminate(&self, exit_code: u32) -> buck2_error::Result<()> {
        result_bool(unsafe { jobapi2::TerminateJobObject(self.job_handle.handle(), exit_code) })?;
        self.wait().await
    }

    // waits until all processes in a job have exited
    // https://devblogs.microsoft.com/oldnewthing/20130405-00/?p=4743
    async fn wait(&self) -> buck2_error::Result<()> {
        const MAX_RETRY_ATTEMPT: usize = 10;
        let job = self.job_handle.dupe();
        let completion_port = self.completion_handle.dupe();

        // try to wait all the processes exit before spawn a blocking task
        for _ in 0..MAX_RETRY_ATTEMPT {
            if let Ok(false) = has_active_processes(&job, &completion_port, 0) {
                break;
            }
        }

        tokio::task::spawn_blocking(move || {
            let completion_port = completion_port;
            while has_active_processes(&job, &completion_port, INFINITE)? {}
            Ok(())
        })
        .await?
    }
}

fn has_active_processes(
    job: &WinapiHandle,
    completion_port: &WinapiHandle,
    timeout: DWORD,
) -> buck2_error::Result<bool> {
    let mut completion_code: DWORD = 0;
    let mut completion_key: ULONG_PTR = 0;
    let mut overlapped = mem::MaybeUninit::<OVERLAPPED>::uninit();
    let mut lp_overlapped = overlapped.as_mut_ptr();

    let result = unsafe {
        ioapiset::GetQueuedCompletionStatus(
            completion_port.handle(),
            &mut completion_code,
            &mut completion_key,
            &mut lp_overlapped,
            timeout,
        )
    };

    // ignore timeout errors unless the timeout was specified to INFINITE
    // https://docs.microsoft.com/en-us/windows/win32/api/ioapiset/nf-ioapiset-getqueuedcompletionstatus
    if timeout != INFINITE && result == FALSE && lp_overlapped.is_null() {
        return Ok(true);
    }

    result_bool(result)?;

    // we are interested only in the specific event from the job object
    // ignore the rest in case some other I/O gets queued to our completion port
    if completion_key != job.handle() as ULONG_PTR
        || completion_code != JOB_OBJECT_MSG_ACTIVE_PROCESS_ZERO
    {
        return Ok(true);
    }

    Ok(false)
}

fn associate_job_with_completion_port(
    job: &WinapiHandle,
    completion_port: &WinapiHandle,
) -> buck2_error::Result<()> {
    let mut associate_completion = JOBOBJECT_ASSOCIATE_COMPLETION_PORT {
        CompletionKey: job.handle(),
        CompletionPort: completion_port.handle(),
    };

    result_bool(unsafe {
        jobapi2::SetInformationJobObject(
            job.handle(),
            JobObjectAssociateCompletionPortInformation,
            &mut associate_completion as *mut _ as LPVOID,
            mem::size_of_val(&associate_completion)
                .try_into()
                .expect("cannot safely cast to DWORD"),
        )
    })
}

fn set_job_limits(job: &WinapiHandle) -> buck2_error::Result<()> {
    let mut info = JOBOBJECT_EXTENDED_LIMIT_INFORMATION::default();
    info.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;

    result_bool(unsafe {
        jobapi2::SetInformationJobObject(
            job.handle(),
            JobObjectExtendedLimitInformation,
            &mut info as *mut _ as LPVOID,
            mem::size_of_val(&info)
                .try_into()
                .expect("cannot safely cast to DWORD"),
        )
    })
}
