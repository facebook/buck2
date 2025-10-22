/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_core::soft_error;
use buck2_error::internal_error;
use windows::Win32::Foundation::BOOL;
use windows::Win32::Foundation::CloseHandle;
use windows::Win32::Foundation::GetLastError;
use windows::Win32::System::Diagnostics::ToolHelp::CreateToolhelp32Snapshot;
use windows::Win32::System::Diagnostics::ToolHelp::PROCESSENTRY32;
use windows::Win32::System::Diagnostics::ToolHelp::Process32First;
use windows::Win32::System::Diagnostics::ToolHelp::Process32Next;
use windows::Win32::System::SystemInformation::GROUP_AFFINITY;
use windows::Win32::System::Threading::ALL_PROCESSOR_GROUPS;
use windows::Win32::System::Threading::GetActiveProcessorCount;
use windows::Win32::System::Threading::GetActiveProcessorGroupCount;
use windows::Win32::System::Threading::GetCurrentProcess;
use windows::Win32::System::Threading::GetCurrentProcessId;
use windows::Win32::System::Threading::GetCurrentProcessorNumberEx;
use windows::Win32::System::Threading::SetProcessDefaultCpuSetMasks;

fn full_mask(size: u32) -> usize {
    if size == 0 {
        return 0;
    }
    if size >= usize::BITS {
        // Handle cases where n is larger than the number of bits in usize
        // For example, return usize::MAX or panic based on your requirements.
        return usize::MAX;
    }

    (1usize.checked_shl(size).unwrap_or(0)) - 1
}

/**
Returns the thread count of the current process.
*/
pub fn get_current_thread_count() -> buck2_error::Result<u32> {
    // first determine the id of the current process
    let pid = unsafe { GetCurrentProcessId() };

    // then get a process list snapshot.
    let snapshot = unsafe {
        CreateToolhelp32Snapshot(
            windows::Win32::System::Diagnostics::ToolHelp::TH32CS_SNAPALL,
            0,
        )?
    };

    // Find the current process in the snapshot.
    let mut entry = PROCESSENTRY32 {
        ..Default::default()
    };
    entry.dwSize = std::mem::size_of::<PROCESSENTRY32>() as u32;
    unsafe {
        Process32First(snapshot, &mut entry).inspect_err(|_e| {
            let _unused = CloseHandle(snapshot);
        })?;
    };
    while entry.th32ProcessID != pid {
        unsafe {
            Process32Next(snapshot, &mut entry).inspect_err(|_e| {
                let _unused = CloseHandle(snapshot);
            })?;
        };
    }
    let _unused = unsafe { CloseHandle(snapshot) };

    Ok(entry.cntThreads)
}

pub fn windows_cpu_group_workaround() -> buck2_error::Result<()> {
    // The following block restricts (and potentially migrates) the
    // process to the windows processor group with the largest number of
    // processors. Processor count available for scheduling threads will be
    // capped at most to be 64, but it will prevent the sorts of aliasing/array
    // overruns detailed in the summary of D78203729.
    //
    // This block can and should be removed, but only after A) all deps that use
    // the old non-multi-processor-group APIs have been fixed and B) there's
    // some sort of runtime auditing in place to catch NEW usage instances of
    // the older APIs.

    // Detect if there's already been a thread created. The code below only
    // handles setting up the affinity for the current thread (and future
    // threads it creates), it may not work if additional threads have already
    // been created.

    //
    // Ideally we would verify that the process only has a single thread running
    // at this point, and error out if that's not the case. Unfortunately,
    // windows helpfully creates the "default thread pool" for you, and by the
    // time the "initial" thread gets to `mainCRTStartup`, there are already
    // three other threads running. During development we confirmed that the
    // call to this function is made in a "safe" place, and we assume that buck
    // doesn't generally make use of the default windows thread pool, so it
    // doesn't matter what core those threads run on.
    let thread_count = get_current_thread_count()?;
    const EXPECTED_THREAD_COUNT: u32 = 4;
    if thread_count != EXPECTED_THREAD_COUNT {
        soft_error!(
            "windows_initial_thread_count",
            internal_error!(
            "windows cpu group workaround expected only {} threads, but {} are running. This may impact the efficacy of the cpu group workaround.",
            EXPECTED_THREAD_COUNT,
            thread_count,
            ),
            quiet: true
        ).ok();
    }

    // Find the group with the largest CPU count. Prefer to keep the current
    // group, if they are all identical.
    let group_count = unsafe { GetActiveProcessorGroupCount() };
    let (selected_group, selected_group_core_count) = {
        let mut largest_group = 0;
        let mut largest_cpu_group_count = 0;
        let current_proc = unsafe { GetCurrentProcessorNumberEx() };
        let mut group_cpu_counts = vec![0; group_count as usize];
        for group in 0..group_count {
            let cpu_count_group = unsafe { GetActiveProcessorCount(group) };
            group_cpu_counts[group as usize] = cpu_count_group;
            if cpu_count_group > largest_cpu_group_count
                || (cpu_count_group == largest_cpu_group_count && current_proc.Group == group)
            {
                largest_group = group;
                largest_cpu_group_count = cpu_count_group;
            }
        }
        (largest_group, group_cpu_counts[largest_group as usize])
    };
    tracing::debug!(
        "The windows processor group with the most processors is group {}, procs:{}",
        selected_group,
        selected_group_core_count
    );

    let cpu_count_total = unsafe { GetActiveProcessorCount(ALL_PROCESSOR_GROUPS) };
    if cpu_count_total > selected_group_core_count {
        // Will the act of invoking the macro itself cause the error to be logged?
        soft_error!(
            "windows_cpu_count",
            internal_error!(
                "total cpu count={} is greater than largest group count={}. On windows buck will not be able to use all available cpu cores",
                cpu_count_total,
                selected_group_core_count
            ),
            quiet: true
        ).ok();
    }

    // Restrict the process to the selected group. We should see the current
    // thread migrate to the selected group, if it's not in it already.
    let mut masks: Vec<GROUP_AFFINITY> = Vec::new();
    masks.resize(1usize, unsafe { std::mem::zeroed() });
    masks[0].Group = selected_group;
    masks[0].Mask = full_mask(selected_group_core_count);
    let ret = unsafe { SetProcessDefaultCpuSetMasks(GetCurrentProcess(), Some(&masks)) };
    if ret == BOOL::from(false) {
        soft_error!(
            "windows_cpu_masks_set",
            internal_error!(
                "Failed to set process cpu sets via masks error={:#?}....",
                unsafe { GetLastError() }
            ),
            quiet: true
        )
        .ok();
    }

    // Verify the migration mentioned above.
    let current_proc = unsafe { GetCurrentProcessorNumberEx() };
    if current_proc.Group != selected_group {
        soft_error!(
            "windows_cpu_group_set",
            internal_error!(
                "Failed to migrate to the selected cpu group error={:#?}....",
                unsafe { GetLastError() }
            ),
            quiet: true
        )
        .ok();
    }

    Ok(())
}
