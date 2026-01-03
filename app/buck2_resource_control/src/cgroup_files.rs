/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::os::fd::OwnedFd;
use std::sync::Arc;

use buck2_error::BuckErrorContext;
use buck2_fs::paths::file_name::FileNameBuf;
use dupe::Dupe;
use nix::fcntl::OFlag;
use nix::sys::stat::Mode;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Environment)]
enum CgroupFileError {
    #[error("File size doesn't fit in off_t: {0}")]
    FileTooBig(std::num::TryFromIntError),
    #[error("Contents could not fit into 31 bytes: {0}")]
    FileTooLong(String),
    #[error("Unexpected format for {0}: {1}")]
    UnexpectedFormat(FileNameBuf, String),
}

/// Represents an open handle to one of the standard kernel-supplied files in the cgroup
pub(crate) struct CgroupFile(Arc<OwnedFd>, FileNameBuf);

impl CgroupFile {
    pub async fn open(
        d: Arc<OwnedFd>,
        name: FileNameBuf,
        write: bool,
    ) -> buck2_error::Result<Self> {
        tokio::task::spawn_blocking(move || Self::sync_open(&d, name, write)).await?
    }

    pub(crate) fn sync_open(
        d: &OwnedFd,
        name: FileNameBuf,
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
        Ok(CgroupFile(Arc::new(file), name.to_owned()))
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

    /// Reads files of length at most 31 bytes
    ///
    /// Semantically like `read_to_buf` but avoids a heap allocation
    async fn read_to_short_buf(&self) -> buck2_error::Result<([u8; 32], usize)> {
        let file = self.0.dupe();
        tokio::task::spawn_blocking(move || {
            let mut data = [0u8; 32];
            let mut filled = 0;
            loop {
                if filled == data.len() {
                    return Err(CgroupFileError::FileTooLong(
                        String::from_utf8_lossy(&data).to_string(),
                    )
                    .into());
                }

                let buf = &mut data[filled..];
                let filled_trunc: nix::libc::off_t =
                    filled.try_into().map_err(CgroupFileError::FileTooBig)?;
                let read = nix::sys::uio::pread(&file, buf, filled_trunc)?;
                if read == 0 {
                    break;
                }
                filled += read;
            }
            buck2_error::Ok((data, filled))
        })
        .await?
        .with_buck_error_context(|| format!("Reading cgroup file {}", self.1))
    }

    async fn read_to_buf(&self) -> buck2_error::Result<Vec<u8>> {
        let file = self.0.dupe();
        tokio::task::spawn_blocking(move || {
            let mut data = vec![0u8; 2048]; // Enough in practice
            let mut filled = 0;
            loop {
                if filled == data.len() {
                    data.resize(data.len() * 2, 0);
                }

                let buf = &mut data[filled..];
                let filled_trunc: nix::libc::off_t =
                    filled.try_into().map_err(CgroupFileError::FileTooBig)?;
                let read = nix::sys::uio::pread(&file, buf, filled_trunc)?;
                if read == 0 {
                    break;
                }
                filled += read;
            }
            data.truncate(filled);
            buck2_error::Ok(data)
        })
        .await?
        .with_buck_error_context(|| format!("Reading cgroup file {}", self.1))
    }

    pub(crate) async fn read_to_string(&self) -> buck2_error::Result<String> {
        let buf = self.read_to_buf().await?;
        Ok(String::from_utf8(buf)?)
    }

    // FIXME(JakobDegen): Ought probably to have some types to represent the files
    pub(crate) async fn read_memory_stat(&self) -> buck2_error::Result<MemoryStat> {
        MemoryStat::parse(&self.read_to_string().await?)
            .buck_error_context("Failed to parse memory.stat")
    }

    pub(crate) async fn read_max_or_int(&self) -> buck2_error::Result<Option<u64>> {
        let (data, filled) = self.read_to_short_buf().await?;
        let data = &data[..filled];
        std::str::from_utf8(data)
            .ok()
            .map(|d| d.trim_end())
            .and_then(|d| {
                if d == "max" {
                    Some(None)
                } else {
                    d.parse::<u64>().ok().map(Some)
                }
            })
            .ok_or_else(|| {
                CgroupFileError::UnexpectedFormat(
                    self.1.clone(),
                    String::from_utf8_lossy(&data).to_string(),
                )
                .into()
            })
    }
}

/// A few interesting values from memory.stat
#[derive(Default)]
pub struct MemoryStat {
    /// Anonymous memory, inclusive of swap.
    pub anon: u64,
    pub inactive_anon: u64,
    pub active_anon: u64,
    /// File-backed memory.
    pub file: u64,
    pub active_file: u64,
    pub inactive_file: u64,
    /// Kernel memory.
    pub kernel: u64,
}

impl MemoryStat {
    fn parse(content: &str) -> buck2_error::Result<Self> {
        let mut res = MemoryStat::default();

        for line in content.lines() {
            let mut parts = line.split_whitespace();
            let key = parts
                .next()
                .with_buck_error_context(|| format!("Invalid line: '{}' (no key)", line))?;
            let value = parts
                .next()
                .with_buck_error_context(|| format!("Invalid line: '{}' (no value)", line))?
                .parse::<u64>()
                .with_buck_error_context(|| format!("Invalid line: '{}' (invalid value)", line))?;
            if parts.next().is_some() {
                return Err(buck2_error::internal_error!(
                    "Invalid line: '{}' (too many parts)",
                    line
                ));
            }

            match key {
                "anon" => res.anon = value,
                "inactive_anon" => res.inactive_anon = value,
                "active_anon" => res.active_anon = value,
                "file" => res.file = value,
                "active_file" => res.active_file = value,
                "inactive_file" => res.inactive_file = value,
                "kernel" => res.kernel = value,
                // Ignore other keys
                _ => {}
            }
        }

        Ok(res)
    }
}

#[cfg(test)]
mod tests {
    use crate::cgroup::CgroupMinimal;
    use crate::cgroup_files::MemoryStat;

    #[test]
    fn test_parse_memory_stat() {
        let sample_stat = r#"anon 1048576
file 2097152
kernel 524288
kernel_stack 8192
pagetables 16384
percpu 32768
sock 4096
vmalloc 65536
shmem 131072
file_mapped 262144
file_dirty 8192
file_writeback 0
swapcached 0
anon_thp 0
file_thp 0
shmem_thp 0
inactive_anon 524288
active_anon 524288
inactive_file 1048576
active_file 1048576
unevictable 0
slab_reclaimable 196608
slab_unreclaimable 65536
slab 262144"#;

        let stat = MemoryStat::parse(sample_stat).unwrap();
        assert_eq!(stat.anon, 1048576);
        assert_eq!(stat.file, 2097152);
        assert_eq!(stat.kernel, 524288);
        assert_eq!(stat.inactive_anon, 524288);
        assert_eq!(stat.active_anon, 524288);
        assert_eq!(stat.inactive_file, 1048576);
        assert_eq!(stat.active_file, 1048576);
    }

    #[tokio::test]
    async fn test_read_memory_stat() {
        let Some(cgroup) = CgroupMinimal::create_minimal_for_test().await else {
            return;
        };
        let cgroup = cgroup.enable_memory_monitoring().await.unwrap();

        let stat = cgroup.read_memory_stat().await.unwrap();
        // Never spawned a process into it, so should be empty
        assert_eq!(stat.active_anon, 0);
        assert_eq!(stat.file, 0);
    }
}
