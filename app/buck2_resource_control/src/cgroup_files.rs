/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fs::File;

use buck2_core::fs::paths::file_name::FileName;
use buck2_error::BuckErrorContext;
use nix::fcntl::OFlag;
use nix::sys::stat::Mode;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Environment)]
enum CgroupFileError {
    #[error("File size doesn't fit in off_t: {0}")]
    FileTooBig(std::num::TryFromIntError),
}

/// Represents an open handle to one of the standard kernel-supplied files in the cgroup
pub(crate) struct CgroupFile(File);

impl CgroupFile {
    pub(crate) fn open(
        d: &nix::dir::Dir,
        name: &FileName,
        write: bool,
    ) -> buck2_error::Result<Self> {
        let flags = OFlag::O_CLOEXEC
            | if write {
                OFlag::O_RDWR
            } else {
                OFlag::O_RDONLY
            };
        let file = nix::fcntl::openat(
            d,
            name.as_forward_rel_path().as_path(),
            flags,
            Mode::empty(),
        )
        .with_buck_error_context(|| format!("Failed to open cgroup file {}", name))?;
        Ok(CgroupFile(file.into()))
    }

    /// Write the given buffer to the file
    ///
    /// Generally, the way that these files work is that instead of there being delimiters or
    /// terminators for values, the kernel guarantees that writes will be atomic, and if the user
    /// wants to do more than one operation, they must issue two writes. This API expects that the
    /// file being written to does indeed work that way; incomplete writes return errors.
    ///
    /// Additionally, this code is allocation-free in the happy path, which means that it's ok to
    /// use in `pre_exec` contexts
    pub(crate) fn write(&self, data: &[u8]) -> std::io::Result<()> {
        // Because of the weird semantics, we avoid std and just use libc
        let bytes_written = nix::unistd::write(&self.0, data)?;

        if bytes_written != data.len() {
            return Err(std::io::Error::other(format!(
                "Write was incomplete: wrote {} bytes, expected {}",
                bytes_written,
                data.len()
            )));
        }

        Ok(())
    }

    pub(crate) fn read_to_buf(&self) -> buck2_error::Result<Vec<u8>> {
        let mut data = vec![0u8; 2048]; // Enough in practice
        let mut filled = 0;
        loop {
            if filled == data.len() {
                data.resize(data.len() * 2, 0);
            }

            let buf = &mut data[filled..];
            let filled_trunc: nix::libc::off_t =
                filled.try_into().map_err(CgroupFileError::FileTooBig)?;
            let read = nix::sys::uio::pread(&self.0, buf, filled_trunc)?;
            if read == 0 {
                break;
            }
            filled += read;
        }
        data.truncate(filled);
        Ok(data)
    }
}
