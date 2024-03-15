/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::mem;
use std::ptr;

use buck2_wrapper_common::win::winapi_handle::WinapiHandle;
use winapi::shared::minwindef::LPVOID;
use winapi::um::handleapi;
use winapi::um::ioapiset;
use winapi::um::jobapi2;
use winapi::um::winnt::JobObjectAssociateCompletionPortInformation;
use winapi::um::winnt::JobObjectExtendedLimitInformation;
use winapi::um::winnt::HANDLE;
use winapi::um::winnt::JOBOBJECT_ASSOCIATE_COMPLETION_PORT;
use winapi::um::winnt::JOBOBJECT_EXTENDED_LIMIT_INFORMATION;
use winapi::um::winnt::JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;

use crate::win::utils::result_bool;
use crate::win::utils::result_handle;

pub(crate) struct JobObject {
    job_handle: WinapiHandle,
    #[allow(dead_code)]
    completion_handle: WinapiHandle,
}

impl JobObject {
    pub(crate) fn new() -> anyhow::Result<Self> {
        let job_handle = result_handle(unsafe {
            WinapiHandle::new(jobapi2::CreateJobObjectW(ptr::null_mut(), ptr::null_mut()))
        })?;

        let completion_handle = result_handle(unsafe {
            WinapiHandle::new(ioapiset::CreateIoCompletionPort(
                handleapi::INVALID_HANDLE_VALUE, // FileHandle
                ptr::null_mut(),                 // ExistingCompletionPort
                0,                               // CompletionKey
                1,                               // NumberOfConcurrentThreads
            ))
        })?;

        associate_job_with_completion_port(&job_handle, &completion_handle)?;
        set_job_limits(&job_handle)?;

        Ok(Self {
            job_handle,
            completion_handle,
        })
    }

    pub(crate) fn assign_process(&self, process: HANDLE) -> anyhow::Result<()> {
        result_bool(unsafe { jobapi2::AssignProcessToJobObject(self.job_handle.handle(), process) })
    }

    pub(crate) fn terminate(&self, exit_code: u32) -> anyhow::Result<()> {
        result_bool(unsafe { jobapi2::TerminateJobObject(self.job_handle.handle(), exit_code) })
    }
}

fn associate_job_with_completion_port(
    job: &WinapiHandle,
    completion_port: &WinapiHandle,
) -> anyhow::Result<()> {
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

fn set_job_limits(job: &WinapiHandle) -> anyhow::Result<()> {
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
