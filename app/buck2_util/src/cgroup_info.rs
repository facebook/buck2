/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;

pub struct CGroupInfo {
    pub path: String,
}

impl CGroupInfo {
    pub async fn read_async() -> anyhow::Result<CGroupInfo> {
        tokio::fs::read_to_string("/proc/self/cgroup")
            .await
            .with_context(|| "Failed to read /proc/self/cgroup")?
            .lines()
            .nth(0)
            .context("Failed to read the first line of /proc/self/cgroup")
            .map(|s| CGroupInfo::parse(&s))?
    }

    pub fn parse(cgroup: &str) -> anyhow::Result<CGroupInfo> {
        let cgroup = cgroup
            .splitn(3, ':')
            .nth(2)
            .context("Failed to parse cgroup path")?;
        Ok(CGroupInfo {
            path: format!("/sys/fs/cgroup{}", cgroup),
        })
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

    #[tokio::test]
    async fn test_cgroup_info_read() {
        // check if cgroups are supported
        if Path::new("/sys/fs/cgroup").exists() {
            let info = CGroupInfo::read_async().await.unwrap();
            assert!(Path::new(&info.path).exists());
        }
    }
}
