/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fs;
use std::path::MAIN_SEPARATOR;

use buck2_error::BuckErrorContext;

#[derive(Debug, Clone)]
pub struct CgroupMemoryInfo {
    pub memory_current: u64,
    pub memory_cached: u64,
}

const SLICE_EXT: &str = ".slice";

#[derive(Debug)]
pub struct CGroupInfo {
    pub path: String,
}

impl CGroupInfo {
    pub async fn read_async() -> buck2_error::Result<CGroupInfo> {
        Self::read_from_str(
            &tokio::fs::read_to_string("/proc/self/cgroup")
                .await
                .with_buck_error_context(|| "Failed to read /proc/self/cgroup")?,
        )
    }

    pub fn read() -> buck2_error::Result<CGroupInfo> {
        Self::read_from_str(
            &fs::read_to_string("/proc/self/cgroup")
                .with_buck_error_context(|| "Failed to read /proc/self/cgroup")?,
        )
    }

    fn read_from_str(s: &str) -> buck2_error::Result<CGroupInfo> {
        s.lines()
            .nth(0)
            .buck_error_context("Failed to read the first line of /proc/self/cgroup")
            .map(|s| CGroupInfo::parse(&s))?
    }

    pub fn parse(cgroup: &str) -> buck2_error::Result<CGroupInfo> {
        let cgroup = cgroup
            .splitn(3, ':')
            .nth(2)
            .buck_error_context("Failed to parse cgroup path")?;
        Ok(CGroupInfo {
            path: format!("/sys/fs/cgroup{cgroup}"),
        })
    }

    pub fn join_hierarchically(&self, parts: &[&str]) -> buck2_error::Result<String> {
        let mut name = self
            .get_slice_name()
            .buck_error_context("Can't find slice in cgroup path")?
            .to_owned();
        let mut path = self
            .get_slice()
            .buck_error_context("Can't get slice name in cgroup path")?
            .to_owned();
        let name_separator = '-';
        for part in parts {
            name.push(name_separator);
            name.push_str(part);
            path.push(MAIN_SEPARATOR);
            path.push_str(&name);
            path.push_str(SLICE_EXT);
        }
        Ok(path)
    }

    // Returns the path to the closest slice in a cgroup path,
    // or None if there is no slice in the path
    pub fn get_slice(&self) -> Option<&str> {
        self.path
            .rfind(SLICE_EXT)
            .map(|i| &self.path[..i + SLICE_EXT.len()])
    }

    // Returns the path to the closest slice in a cgroup path,
    // or None if there is no slice in the path
    pub fn get_slice_name(&self) -> Option<&str> {
        let slice = self.get_slice()?;
        let name_start = slice.rfind(MAIN_SEPARATOR)?;
        Some(&slice[name_start + 1..slice.len() - SLICE_EXT.len()])
    }

    pub fn read_memory_stat(&self) -> buck2_error::Result<MemoryStat> {
        let memory_stat_path = format!("{}/memory.stat", self.path);
        let content = fs::read_to_string(&memory_stat_path)
            .with_buck_error_context(|| format!("Failed to read {}", memory_stat_path))?;
        MemoryStat::parse(&content)
            .with_buck_error_context(|| format!("Failed to parse {}", memory_stat_path))
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
    use std::path::Path;

    use super::*;

    #[test]
    fn test_cgroup_info_parse() {
        let cgroup = "0::/user.slice/user-532497.slice/user@532497.service/buck2.slice/buck2-daemon.fbsource.v2.slice/buck2-daemon.fbsource.v2-forkserver.slice/buck2-daemon.fbsource.v2-forkserver-f80cd8522e809ed63d6bffd0ff21637a81c760928a3ecf01cc3ef5d9046dd6d5:145.slice";
        assert_eq!(
            "/sys/fs/cgroup/user.slice/user-532497.slice/user@532497.service/buck2.slice/buck2-daemon.fbsource.v2.slice/buck2-daemon.fbsource.v2-forkserver.slice/buck2-daemon.fbsource.v2-forkserver-f80cd8522e809ed63d6bffd0ff21637a81c760928a3ecf01cc3ef5d9046dd6d5:145.slice",
            CGroupInfo::parse(cgroup).unwrap().path
        );
    }

    #[test]
    fn test_cgroup_get_slice() {
        let cgroup = "0::/user.slice/user-532497.slice/user@532497.service/buck2.slice/buck2-daemon.fbsource.v2.slice/buck2-daemon-fbsource-v2.scope";
        let info = CGroupInfo::parse(cgroup).unwrap();
        assert_eq!(
            "/sys/fs/cgroup/user.slice/user-532497.slice/user@532497.service/buck2.slice/buck2-daemon.fbsource.v2.slice/buck2-daemon-fbsource-v2.scope",
            info.path
        );
        assert_eq!(
            Some(
                "/sys/fs/cgroup/user.slice/user-532497.slice/user@532497.service/buck2.slice/buck2-daemon.fbsource.v2.slice"
            ),
            info.get_slice()
        );
        assert_eq!(Some("buck2-daemon.fbsource.v2"), info.get_slice_name());
    }

    #[test]
    fn test_cgroup_join() {
        let cgroup = "0::/user.slice/user-532497.slice/user@532497.service/buck2.slice/buck2-daemon.fbsource.v2.slice/buck2-daemon-fbsource-v2.scope";
        let info = CGroupInfo::parse(cgroup).unwrap();
        let path = info
            .join_hierarchically(&[
                "forkserver",
                "c7f875d040777c7aa927e69c9ebbd4e14c6da4f553b56e9f81ef54467b9a6ca2:145",
            ])
            .unwrap();
        assert_eq!(
            "/sys/fs/cgroup/user.slice/user-532497.slice/user@532497.service/buck2.slice/buck2-daemon.fbsource.v2.slice/buck2-daemon.fbsource.v2-forkserver.slice/buck2-daemon.fbsource.v2-forkserver-c7f875d040777c7aa927e69c9ebbd4e14c6da4f553b56e9f81ef54467b9a6ca2:145.slice",
            path
        );
    }

    #[tokio::test]
    async fn test_cgroup_info_read_async() {
        // check if cgroups are supported
        if Path::new("/sys/fs/cgroup").exists() {
            let info = CGroupInfo::read_async().await.unwrap();
            assert!(Path::new(&info.path).exists());
        }
    }

    #[test]
    fn test_cgroup_info_read() {
        // check if cgroups are supported
        if Path::new("/sys/fs/cgroup").exists() {
            let info = CGroupInfo::read().unwrap();
            assert!(Path::new(&info.path).exists());
        }
    }

    #[test]
    fn test_cgroup_memory_read() {
        // check if cgroups are supported
        if Path::new("/sys/fs/cgroup").exists() {
            let info = CGroupInfo::read().unwrap();

            // Check if memory files exist before trying to read them
            let memory_current_path = format!("{}/memory.current", info.path);
            let memory_stat_path = format!("{}/memory.stat", info.path);

            if Path::new(&memory_current_path).exists() && Path::new(&memory_stat_path).exists() {
                let memory_info = info.read_memory_stat().unwrap();
                assert!(memory_info.anon > 0, "anon should be greater than 0");
            }
        }
    }

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
}
