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

use allocative::Allocative;
use buck2_error::BuckErrorContext;
use buck2_fs::paths::abs_norm_path::AbsNormPath;
use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;

use crate::path::CgroupPath;
use crate::path::CgroupPathBuf;

const SLICE_EXT: &str = ".slice";

#[derive(Debug, Allocative)]
pub struct CGroupInfo {
    pub path: CgroupPathBuf,
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
        // Can't use .join() since the second part is absolute too
        let path = AbsNormPathBuf::new(format!("/sys/fs/cgroup{cgroup}").into())?;
        Ok(CGroupInfo {
            path: CgroupPathBuf::new(path),
        })
    }

    // Returns the path to the closest slice in a cgroup path,
    // or None if there is no slice in the path
    pub fn get_slice(&self) -> Option<&CgroupPath> {
        self.path
            .as_path()
            .to_str()
            .and_then(|s| s.rfind(SLICE_EXT).map(|i| &s[..i + SLICE_EXT.len()]))
            .and_then(|p| AbsNormPath::new(p).ok())
            .map(CgroupPath::new)
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use buck2_fs::fs_util;

    use super::*;

    #[test]
    fn test_cgroup_info_parse() {
        let cgroup = "0::/user.slice/user-532497.slice/user@532497.service/buck2.slice/buck2-daemon.fbsource.v2.slice/buck2-daemon.fbsource.v2-forkserver.slice/buck2-daemon.fbsource.v2-forkserver-f80cd8522e809ed63d6bffd0ff21637a81c760928a3ecf01cc3ef5d9046dd6d5:145.slice";
        assert_eq!(
            "/sys/fs/cgroup/user.slice/user-532497.slice/user@532497.service/buck2.slice/buck2-daemon.fbsource.v2.slice/buck2-daemon.fbsource.v2-forkserver.slice/buck2-daemon.fbsource.v2-forkserver-f80cd8522e809ed63d6bffd0ff21637a81c760928a3ecf01cc3ef5d9046dd6d5:145.slice",
            CGroupInfo::parse(cgroup).unwrap().path.to_string()
        );
    }

    #[test]
    fn test_cgroup_get_slice() {
        let cgroup = "0::/user.slice/user-532497.slice/user@532497.service/buck2.slice/buck2-daemon.fbsource.v2.slice/buck2-daemon-fbsource-v2.scope";
        let info = CGroupInfo::parse(cgroup).unwrap();
        assert_eq!(
            "/sys/fs/cgroup/user.slice/user-532497.slice/user@532497.service/buck2.slice/buck2-daemon.fbsource.v2.slice/buck2-daemon-fbsource-v2.scope",
            info.path.to_string()
        );
        assert_eq!(
            Some(
                "/sys/fs/cgroup/user.slice/user-532497.slice/user@532497.service/buck2.slice/buck2-daemon.fbsource.v2.slice".to_owned()
            ),
            info.get_slice().map(ToString::to_string)
        );
    }

    #[tokio::test]
    async fn test_cgroup_info_read_async() {
        // check if cgroups are supported
        if Path::new("/sys/fs/cgroup").exists() {
            let info = CGroupInfo::read_async().await.unwrap();
            assert!(fs_util::try_exists(&**info.path).unwrap());
        }
    }

    #[test]
    fn test_cgroup_info_read() {
        // check if cgroups are supported
        if Path::new("/sys/fs/cgroup").exists() {
            let info = CGroupInfo::read().unwrap();
            assert!(fs_util::try_exists(&**info.path).unwrap());
        }
    }
}
