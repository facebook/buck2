/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;

#[cfg(not(windows))]
pub fn spawn_background_process_on_windows<'a>(
    _working_dir: &Path,
    _exe: &Path,
    _args: impl IntoIterator<Item = &'a str>,
) -> anyhow::Result<()> {
    #[derive(Debug, thiserror::Error)]
    #[error("not Windows")]
    struct NotWindows;

    Err(NotWindows.into())
}

#[cfg(windows)]
pub fn spawn_background_process_on_windows<'a>(
    working_dir: &Path,
    exe: &Path,
    args: impl IntoIterator<Item = &'a str>,
) -> anyhow::Result<()> {
    use std::ffi::OsStr;
    use std::ffi::OsString;
    use std::io;
    use std::iter;
    use std::mem;
    use std::os::windows::ffi::OsStrExt;
    use std::ptr;

    use winapi::shared::minwindef::DWORD;
    use winapi::shared::minwindef::FALSE;
    use winapi::um::handleapi::CloseHandle;
    use winapi::um::processthreadsapi::CreateProcessW;
    use winapi::um::processthreadsapi::PROCESS_INFORMATION;
    use winapi::um::processthreadsapi::STARTUPINFOW;
    use winapi::um::winbase::CREATE_NEW_PROCESS_GROUP;
    use winapi::um::winbase::CREATE_UNICODE_ENVIRONMENT;
    use winapi::um::winbase::DETACHED_PROCESS;

    // We have to call CreateProcessW manually because std::process::Command
    // doesn't allow to set 'bInheritHandles' to false. Without this waiting on
    // parent process will also wait on inherited handles of daemon process.

    fn to_nullterm(s: &OsStr) -> Vec<u16> {
        s.encode_wide().chain(iter::once(0)).collect()
    }

    // Translated from ArgvQuote at http://tinyurl.com/zmgtnls
    fn append_quoted(arg: &OsStr, cmdline: &mut Vec<u16>) {
        if !arg.is_empty()
            && !arg.encode_wide().any(|c| {
                c == ' ' as u16
                    || c == '\t' as u16
                    || c == '\n' as u16
                    || c == '\x0b' as u16
                    || c == '\"' as u16
            })
        {
            cmdline.extend(arg.encode_wide());
            return;
        }
        cmdline.push('"' as u16);

        let arg: Vec<_> = arg.encode_wide().collect();
        let mut i = 0;
        while i < arg.len() {
            let mut num_backslashes = 0;
            while i < arg.len() && arg[i] == '\\' as u16 {
                i += 1;
                num_backslashes += 1;
            }

            if i == arg.len() {
                for _ in 0..num_backslashes * 2 {
                    cmdline.push('\\' as u16);
                }
                break;
            } else if arg[i] == b'"' as u16 {
                for _ in 0..num_backslashes * 2 + 1 {
                    cmdline.push('\\' as u16);
                }
                cmdline.push(arg[i]);
            } else {
                for _ in 0..num_backslashes {
                    cmdline.push('\\' as u16);
                }
                cmdline.push(arg[i]);
            }
            i += 1;
        }
        cmdline.push('"' as u16);
    }

    #[allow(clippy::vec_init_then_push)]
    fn make_command_line<'a>(program: &OsStr, args: impl IntoIterator<Item = &'a str>) -> Vec<u16> {
        let mut cmd: Vec<u16> = Vec::new();
        cmd.push(b'"' as u16);
        cmd.extend(program.encode_wide());
        cmd.push(b'"' as u16);
        for arg in args.into_iter().map(OsString::from) {
            cmd.push(b' ' as u16);
            append_quoted(&arg, &mut cmd);
        }
        cmd.push(0); // null terminator
        cmd
    }

    let program = exe.as_os_str();
    let cwd = working_dir.as_os_str();

    let mut cmd = make_command_line(program, args);

    let mut sinfo: STARTUPINFOW = unsafe { mem::zeroed() };
    sinfo.cb = mem::size_of::<STARTUPINFOW>() as DWORD;
    let mut pinfo: PROCESS_INFORMATION = unsafe { mem::zeroed() };
    let creation_flags = CREATE_NEW_PROCESS_GROUP | DETACHED_PROCESS | CREATE_UNICODE_ENVIRONMENT;

    let status = unsafe {
        CreateProcessW(
            to_nullterm(program).as_ptr(), // lpApplicationName
            cmd.as_mut_ptr(),              // lpCommandLine
            ptr::null_mut(),               // lpProcessAttributes
            ptr::null_mut(),               // lpThreadAttributes
            FALSE,                         // bInheritHandles
            creation_flags,                // dwCreationFlags
            ptr::null_mut(),               // lpEnvironment
            to_nullterm(cwd).as_ptr(),     // lpCurrentDirectory
            &mut sinfo,
            &mut pinfo,
        )
    };
    if status == 0 {
        return Err(anyhow::anyhow!(io::Error::last_os_error()));
    }
    unsafe { CloseHandle(pinfo.hThread) };
    unsafe { CloseHandle(pinfo.hProcess) };
    Ok(())
}
