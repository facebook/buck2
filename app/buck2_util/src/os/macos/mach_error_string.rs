/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg(target_os = "macos")]

pub(crate) fn mach_error_string(err: libc::kern_return_t) -> &'static str {
    extern "C" {
        fn mach_error_string(err: libc::kern_return_t) -> *const libc::c_char;
    }

    unsafe {
        let c_str = mach_error_string(err);
        let c_str = std::ffi::CStr::from_ptr(c_str);
        c_str
            .to_str()
            .unwrap_or("mach_error_string returned invalid UTF-8")
    }
}

#[cfg(test)]
mod tests {
    use crate::os::macos::mach_error_string::mach_error_string;

    #[test]
    fn test_mach_error_string() {
        let success = mach_error_string(libc::KERN_SUCCESS);
        assert!(success.contains("success"), "{:?}", success);
        let no_space = mach_error_string(libc::KERN_NO_SPACE);
        assert!(no_space.contains("no space"), "{:?}", no_space);
    }
}
