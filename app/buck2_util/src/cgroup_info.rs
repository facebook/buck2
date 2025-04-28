/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs;
use std::path::MAIN_SEPARATOR;

use buck2_error::BuckErrorContext;

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
            path: format!("/sys/fs/cgroup{}", cgroup),
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
    fn get_slice(&self) -> Option<&str> {
        self.path
            .rfind(SLICE_EXT)
            .map(|i| &self.path[..i + SLICE_EXT.len()])
    }

    // Returns the path to the closest slice in a cgroup path,
    // or None if there is no slice in the path
    fn get_slice_name(&self) -> Option<&str> {
        let slice = self.get_slice()?;
        let name_start = slice.rfind(MAIN_SEPARATOR)?;
        Some(&slice[name_start + 1..slice.len() - SLICE_EXT.len()])
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
}
