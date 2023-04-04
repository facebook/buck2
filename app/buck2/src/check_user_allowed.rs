/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[cfg(windows)]
pub(crate) fn check_user_allowed() -> anyhow::Result<()> {
    use std::io;
    use std::mem;
    use std::mem::MaybeUninit;
    use std::ptr;

    use anyhow::Context;
    use winapi::ctypes::c_void;
    use winapi::shared::minwindef::DWORD;
    use winapi::um::handleapi::CloseHandle;
    use winapi::um::processthreadsapi::GetCurrentProcess;
    use winapi::um::processthreadsapi::OpenProcessToken;
    use winapi::um::securitybaseapi::GetTokenInformation;
    use winapi::um::winnt::TokenElevation;
    use winapi::um::winnt::HANDLE;
    use winapi::um::winnt::TOKEN_ELEVATION;
    use winapi::um::winnt::TOKEN_QUERY;

    #[derive(Debug, thiserror::Error)]
    enum CheckUserAllowedError {
        #[error("OpenProcessToken returned null token handle (unreachable)")]
        NullTokenHandle,
    }

    struct Handle(HANDLE);
    impl Drop for Handle {
        fn drop(&mut self) {
            let exit = unsafe { CloseHandle(self.0) };
            if exit == 0 {
                panic!("Failed closing process token handle")
            }
        }
    }

    let mut handle = ptr::null_mut();
    let token_ok = unsafe { OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &mut handle) };
    if token_ok == 0 {
        return Err(io::Error::last_os_error()).context("OpenProcessToken failed");
    }
    if handle.is_null() {
        return Err(CheckUserAllowedError::NullTokenHandle.into());
    }

    let handle = Handle(handle);
    let size = mem::size_of::<TOKEN_ELEVATION>();
    let elevation: MaybeUninit<TOKEN_ELEVATION> = MaybeUninit::zeroed();
    let mut ret_size = 0;

    let success_get = unsafe {
        GetTokenInformation(
            handle.0,
            TokenElevation,
            elevation.as_ptr() as *mut c_void,
            size as DWORD,
            &mut ret_size,
        )
    };
    if success_get == 0 {
        return Err(io::Error::last_os_error()).context("GetTokenInformation failed");
    }

    let elevation_struct: TOKEN_ELEVATION = unsafe { elevation.assume_init() };
    if elevation_struct.TokenIsElevated == 1 {
        tracing::warn!(
            "You're running buck2 from an admin shell. Invocations from non-admin shells will likely fail going forward. To remediate, run `buck2 clean` in this admin shell, then switch to a non-admin shell."
        );
    }
    Ok(())
}

#[cfg(not(windows))]
pub(crate) fn check_user_allowed() -> anyhow::Result<()> {
    use std::os::unix::fs::MetadataExt;

    use anyhow::Context;
    use buck2_core::fs::fs_util;
    use buck2_core::soft_error;

    #[derive(Debug, thiserror::Error)]
    #[error("buck2 is not allowed to run as root (unless home dir is owned by root)")]
    struct RootError;

    if nix::unistd::geteuid().is_root() {
        let home_dir = dirs::home_dir().context("home dir not found")?;
        let home_dir_metadata = fs_util::metadata(home_dir)?;
        if home_dir_metadata.uid() != 0 {
            soft_error!("root_not_allowed", RootError.into())?;
        }
    }
    Ok(())
}
