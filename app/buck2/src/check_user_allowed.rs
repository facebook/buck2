/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#[cfg(windows)]
pub(crate) fn check_user_allowed() -> buck2_error::Result<()> {
    use std::io;
    use std::mem;
    use std::mem::MaybeUninit;
    use std::ptr;

    use buck2_core::ci::is_ci;
    use buck2_error::BuckErrorContext;
    use buck2_wrapper_common::win::winapi_handle::WinapiHandle;
    use winapi::ctypes::c_void;
    use winapi::shared::minwindef::DWORD;
    use winapi::um::processthreadsapi::GetCurrentProcess;
    use winapi::um::processthreadsapi::OpenProcessToken;
    use winapi::um::securitybaseapi::GetTokenInformation;
    use winapi::um::winnt::TOKEN_ELEVATION;
    use winapi::um::winnt::TOKEN_QUERY;
    use winapi::um::winnt::TokenElevation;

    let mut handle = ptr::null_mut();
    let token_ok = unsafe { OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &mut handle) };
    if token_ok == 0 {
        return Err(io::Error::last_os_error()).buck_error_context("OpenProcessToken failed");
    }

    let handle = unsafe {
        WinapiHandle::new_check_last_os_error(handle).buck_error_context("OpenProcessToken")?
    };
    let size = mem::size_of::<TOKEN_ELEVATION>();
    let elevation: MaybeUninit<TOKEN_ELEVATION> = MaybeUninit::zeroed();
    let mut ret_size = 0;

    let success_get = unsafe {
        GetTokenInformation(
            handle.handle(),
            TokenElevation,
            elevation.as_ptr() as *mut c_void,
            size as DWORD,
            &mut ret_size,
        )
    };
    if success_get == 0 {
        return Err(io::Error::last_os_error()).buck_error_context("GetTokenInformation failed");
    }

    let elevation_struct: TOKEN_ELEVATION = unsafe { elevation.assume_init() };
    if elevation_struct.TokenIsElevated == 1 {
        // In CI, if buck2 got run from an admin shell, we need not worry that a
        // subsequent invocation might come from a non-admin shell. It almost
        // certainly will not.
        if !is_ci()? {
            tracing::warn!(
                "You're running buck2 from an admin shell. Invocations from non-admin shells will likely fail going forward. To remediate, run `buck2 clean` in this admin shell, then switch to a non-admin shell."
            );
        }
    }
    Ok(())
}

#[cfg(not(windows))]
pub(crate) fn check_user_allowed() -> buck2_error::Result<()> {
    use std::os::unix::fs::MetadataExt;

    use buck2_core::soft_error;
    use buck2_error::BuckErrorContext;
    use buck2_fs::fs_util::uncategorized as fs_util;
    use buck2_fs::paths::abs_path::AbsPath;

    #[derive(Debug, buck2_error::Error)]
    #[error("buck2 is not allowed to run as root (unless home dir is owned by root)")]
    #[buck2(tag = Input)]
    struct RootError;

    if nix::unistd::geteuid().is_root() {
        let home_dir = dirs::home_dir().buck_error_context("home dir not found")?;
        if let Ok(home_dir) = AbsPath::new(&home_dir) {
            let home_dir_metadata = fs_util::metadata(home_dir)?;
            if home_dir_metadata.uid() != 0 {
                soft_error!("root_not_allowed", RootError.into())?;
            }
        }
    }
    Ok(())
}
