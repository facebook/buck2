/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub struct WindowsJobHandle {
    job_handle_as_integer: usize,
}

impl Drop for WindowsJobHandle {
    fn drop(&mut self) {
        let job_handle = self.job_handle_as_integer as *mut winapi::ctypes::c_void;
        let close_handle_result;
        unsafe {
            close_handle_result = winapi::um::handleapi::CloseHandle(job_handle);
        }

        if close_handle_result == winapi::shared::minwindef::FALSE {
            tracing::info!("Failed to close job handle with return code {}", close_handle_result);
        }
    }
}

impl WindowsJobHandle {
    pub fn new(job_handle: *mut winapi::ctypes::c_void) -> WindowsJobHandle {
        WindowsJobHandle {
            job_handle_as_integer: job_handle as usize,
        }
    }

    pub fn get_handle(&self) -> *mut winapi::ctypes::c_void {
        self.job_handle_as_integer as *mut winapi::ctypes::c_void
    }
}

pub fn add_process_to_job(child: &tokio::process::Child) -> anyhow::Result<WindowsJobHandle> {
    let raw_process_handle = child.raw_handle().ok_or_else(|| anyhow::anyhow!("Failed to get the raw handle to the process"))?;
    unsafe {
        let raw_job_handle = winapi::um::jobapi2::CreateJobObjectW(std::ptr::null_mut(), std::ptr::null());
        if raw_job_handle.is_null() {
            return Err(anyhow::anyhow!("Failed to create job"));
        }
        let job_handle = WindowsJobHandle::new(raw_job_handle);

        let assign_process_result = winapi::um::jobapi2::AssignProcessToJobObject(raw_job_handle, raw_process_handle);

        if assign_process_result == winapi::shared::minwindef::FALSE {
            Err(anyhow::anyhow!("Failed to assign process to job"))
        }
        else {
            Ok(job_handle)
        }
    }
}

pub fn resume_process(child: &tokio::process::Child) -> anyhow::Result<()> {
    let process_id = child.id().ok_or_else(|| anyhow::anyhow!("Failed to get the process id"))?;
    let main_thread_id = get_main_thread(process_id)?;
    resume_thread(main_thread_id)
}

pub fn kill_job(job_handle: &crate::win::WindowsJobHandle) -> anyhow::Result<()> {
    unsafe {
        let terminate_job_result = winapi::um::jobapi2::TerminateJobObject(job_handle.get_handle(), 1);

        if terminate_job_result == winapi::shared::minwindef::FALSE {
            Err(anyhow::anyhow!("Failed to terminate job to kill"))
        }
        else {
            Ok(())
        }
    }
}

fn get_main_thread(process_id: u32) -> anyhow::Result<winapi::shared::minwindef::DWORD> {
    unsafe {
        let snapshot_handle = winapi::um::tlhelp32::CreateToolhelp32Snapshot(winapi::um::tlhelp32::TH32CS_SNAPTHREAD, 0);

        if snapshot_handle == winapi::um::handleapi::INVALID_HANDLE_VALUE {
            return Err(anyhow::anyhow!("Failed to list threads"))
        }

        let mut thread_entry_32 = winapi::um::tlhelp32::THREADENTRY32 {
            dwSize: std::mem::size_of::<winapi::um::tlhelp32::THREADENTRY32>() as u32,
            cntUsage: 0,
            th32ThreadID: 0,
            th32OwnerProcessID: 0,
            tpBasePri: 0,
            tpDeltaPri: 0,
            dwFlags: 0,
        };
        let raw_pointer_to_thread_entry_32 = &mut thread_entry_32  as *mut winapi::um::tlhelp32::THREADENTRY32;

        let mut main_thread_id : Option<winapi::shared::minwindef::DWORD> = None;

        let mut thread_result = winapi::um::tlhelp32::Thread32First(snapshot_handle, raw_pointer_to_thread_entry_32);
        while thread_result == winapi::shared::minwindef::TRUE {
            if thread_entry_32.dwSize as usize >= std::mem::offset_of!(winapi::um::tlhelp32::THREADENTRY32, th32OwnerProcessID) {
                if thread_entry_32.th32OwnerProcessID == process_id {
                    main_thread_id = Some(thread_entry_32.th32ThreadID);
                    break;
                }
            }
            thread_result = winapi::um::tlhelp32::Thread32Next(snapshot_handle, raw_pointer_to_thread_entry_32);
        }

        let close_handle_result = winapi::um::handleapi::CloseHandle(snapshot_handle);

        if let Some(thread_id) = main_thread_id {
            if close_handle_result == winapi::shared::minwindef::FALSE {
                Err(anyhow::anyhow!("Failed to close thread snapshot handle"))
            }
            else {
                Ok(thread_id)
            }
        }
        else {
            Err(anyhow::anyhow!("Failed to find thread to resume"))
        }
    }
}

fn resume_thread(thread_id: winapi::shared::minwindef::DWORD) -> anyhow::Result<()> {
    unsafe {
        let thread_handle = winapi::um::processthreadsapi::OpenThread(winapi::um::winnt::THREAD_SUSPEND_RESUME, winapi::shared::minwindef::FALSE, thread_id);
        if thread_handle.is_null() {
            return Err(anyhow::anyhow!("Failed to open thread to resume"));
        }
        let resume_thread_result = winapi::um::processthreadsapi::ResumeThread(thread_handle);
        let close_handle_result = winapi::um::handleapi::CloseHandle(thread_handle);
        if resume_thread_result == winapi::shared::minwindef::DWORD::MAX {
            Err(anyhow::anyhow!("Failed to resume thread"))
        }
        else if close_handle_result == winapi::shared::minwindef::FALSE {
            Err(anyhow::anyhow!("Failed to close thread handle"))
        }
        else {
            Ok(())
        }
    }
}
