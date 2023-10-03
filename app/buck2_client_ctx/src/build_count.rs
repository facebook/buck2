/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::io::ErrorKind;
use std::time::Duration;

use anyhow::Context;
use buck2_common::client_utils;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileName;
use buck2_data::ParsedTargetPatterns;
use fs4::FileExt;
use serde::Deserialize;
use serde::Serialize;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncWriteExt;

#[derive(Serialize, Deserialize)]
struct BuildCount(HashMap<String, u64>);

impl BuildCount {
    pub fn increment(&mut self, patterns: &ParsedTargetPatterns) {
        for target in patterns.target_patterns.iter() {
            match self.0.get_mut(&target.value) {
                Some(counter) => {
                    *counter += 1;
                }
                None => {
                    self.0.insert(target.value.clone(), 1);
                }
            }
        }
    }

    pub fn min_count(&self, patterns: &ParsedTargetPatterns) -> u64 {
        if patterns.target_patterns.is_empty() {
            return 0;
        }

        // If the target has never been succesfully built it won't be in the map, in that case its count is 0.
        return patterns
            .target_patterns
            .iter()
            .map(|v| self.0.get(&v.value).copied().unwrap_or(0))
            .min()
            .unwrap(); // target_patterns is non-empty, so min() should return Some
    }
}

/// BuildCountManager keeps track of how many times each target has been successfully built since rebase.
/// This helps understand how much the performance differs between first and incremental builds.
pub struct BuildCountManager {
    base_dir: AbsNormPathBuf,
}

impl BuildCountManager {
    const LOCK_FILE_NAME: &'static str = "build_count.lock";
    const LOCK_TIMEOUT: Duration = Duration::from_millis(2000);

    pub fn new(base_dir: AbsNormPathBuf) -> Self {
        Self { base_dir }
    }

    async fn ensure_dir(&self) -> anyhow::Result<()> {
        tokio::fs::create_dir_all(&self.base_dir)
            .await
            .with_context(|| {
                format!("Error creating build count directory: `{}`", self.base_dir)
            })?;
        Ok(())
    }

    async fn read(&self, file_name: &FileName) -> anyhow::Result<BuildCount> {
        match tokio::fs::File::open(self.base_dir.join(file_name)).await {
            Ok(mut file) => {
                let mut buffer = String::new();
                file.read_to_string(&mut buffer).await?;
                Ok(serde_json::from_str(&buffer)?)
            }
            Err(e) => match e.kind() {
                ErrorKind::NotFound => {
                    // it is normal after rebase, clean, etc.
                    Ok(BuildCount(HashMap::new()))
                }
                _ => Err(e.into()),
            },
        }
    }

    async fn write(&self, build_count: &BuildCount, file_name: &FileName) -> anyhow::Result<()> {
        self.ensure_dir().await?;
        let mut file = tokio::fs::File::create(self.base_dir.join(file_name)).await?;
        file.write_all(&serde_json::to_vec(build_count)?).await?;
        file.sync_data().await?;
        Ok(())
    }

    async fn lock_with_timeout(&mut self, timeout: Duration) -> anyhow::Result<FileLockGuard> {
        self.ensure_dir().await?;
        let file = std::fs::File::create(self.base_dir.join(FileName::new(Self::LOCK_FILE_NAME)?))?;
        client_utils::retrying(
            Duration::from_millis(5),
            Duration::from_millis(100),
            timeout,
            async || anyhow::Ok(file.try_lock_exclusive()?),
        )
        .await?;
        Ok(FileLockGuard { file })
    }

    /// Updates the build counts for set of targets (on success) and returns the min.
    pub async fn min_build_count(
        &mut self,
        merge_base: &str,
        target_patterns: &ParsedTargetPatterns,
        is_success: bool,
    ) -> anyhow::Result<u64> {
        let file_name = FileName::new(merge_base)?;
        let _guard = self.lock_with_timeout(Self::LOCK_TIMEOUT).await?;
        let mut build_count = self.read(file_name).await?;

        if is_success {
            build_count.increment(target_patterns);
        }
        self.write(&build_count, file_name).await?;
        Ok(build_count.min_count(target_patterns))
    }
}

#[must_use]
struct FileLockGuard {
    file: std::fs::File,
}

impl Drop for FileLockGuard {
    fn drop(&mut self) {
        self.file
            .unlock()
            .expect("Unexpected failure to release a lock file for build count");
    }
}

#[cfg(test)]
mod tests {
    use gazebo::prelude::VecExt;

    use super::*;

    fn make_patterns(targets: Vec<&'static str>) -> ParsedTargetPatterns {
        ParsedTargetPatterns {
            target_patterns: targets.into_map(|v| buck2_data::TargetPattern {
                value: v.to_owned(),
            }),
        }
    }

    #[test]
    fn test_update_normal_input() -> anyhow::Result<()> {
        let mut before = HashMap::new();
        before.insert("//some:target".to_owned(), 1);
        before.insert("//some/other:target".to_owned(), 2);
        let mut bc = BuildCount(before);
        let target_patterns = make_patterns(vec!["//some/other:target", "//yet/another:target"]);
        bc.increment(&target_patterns);
        let mut expected = HashMap::new();
        expected.insert("//some:target".to_owned(), 1);
        expected.insert("//some/other:target".to_owned(), 3);
        expected.insert("//yet/another:target".to_owned(), 1);
        assert_eq!(bc.0, expected);

        Ok(())
    }

    #[test]
    fn test_update_empty_input() -> anyhow::Result<()> {
        let mut before = HashMap::new();
        before.insert("//some:target".to_owned(), 1);
        let expected = before.clone();
        let mut bc = BuildCount(before);
        let target_patterns = make_patterns(vec![]);
        bc.increment(&target_patterns);
        assert_eq!(bc.0, expected);

        Ok(())
    }

    #[test]
    fn test_min_count_some_value() -> anyhow::Result<()> {
        let mut data = HashMap::new();
        data.insert("//some:target1".to_owned(), 3);
        data.insert("//some:target2".to_owned(), 4);
        data.insert("//some:target3".to_owned(), 5);
        let bc = BuildCount(data);
        let target_patterns = make_patterns(vec!["//some:target1", "//some:target2"]);
        assert_eq!(bc.min_count(&target_patterns), 3);

        Ok(())
    }

    #[test]
    fn test_min_count_ignores_others() -> anyhow::Result<()> {
        let mut data = HashMap::new();
        data.insert("//some:target1".to_owned(), 3);
        data.insert("//some:target2".to_owned(), 4);
        data.insert("//some:target3".to_owned(), 5);
        let bc = BuildCount(data);
        let target_patterns = make_patterns(vec!["//some:target2"]);
        assert_eq!(bc.min_count(&target_patterns), 4);

        Ok(())
    }

    #[test]
    fn test_min_count_empty_data() -> anyhow::Result<()> {
        let data = HashMap::new();
        let bc = BuildCount(data);
        assert_eq!(bc.min_count(&make_patterns(vec![])), 0);

        Ok(())
    }

    #[tokio::test]
    async fn test_read_no_such_file() -> anyhow::Result<()> {
        let no_such_dir = if cfg!(windows) {
            "C:\\no\\such\\dir"
        } else {
            "/no/such/dir"
        };
        let bcm = BuildCountManager::new(AbsNormPathBuf::from(no_such_dir.to_owned())?);
        let bc = bcm.read(FileName::new("no_such_file")?).await?;
        assert_eq!(bc.0, HashMap::new());

        Ok(())
    }

    #[tokio::test]
    async fn test_read_normal_file() -> anyhow::Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let file_name = "some_file";
        tokio::fs::write(temp_dir.path().join(file_name), "{\"//some:target\":1}").await?;
        let mut expected = HashMap::new();
        expected.insert("//some:target".to_owned(), 1);
        let bcm = BuildCountManager::new(temp_dir.path().to_path_buf().try_into()?);
        let bc = bcm.read(FileName::new(file_name)?).await?;
        assert_eq!(bc.0, expected);

        Ok(())
    }

    #[tokio::test]
    async fn test_read_illegal_file_contents() -> anyhow::Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let file_name = "some_file";
        tokio::fs::write(temp_dir.path().join(file_name), "aaa").await?;
        let bcm = BuildCountManager::new(temp_dir.path().to_path_buf().try_into()?);
        assert!(bcm.read(FileName::new(file_name)?).await.is_err());

        Ok(())
    }

    #[tokio::test]
    async fn test_write_normal_input() -> anyhow::Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let file_name = "some_file";
        let bcm = BuildCountManager::new(temp_dir.path().to_path_buf().try_into()?);
        let mut data = HashMap::new();
        data.insert("//some:target".to_owned(), 1);
        bcm.write(&BuildCount(data), FileName::new(file_name)?)
            .await?;
        assert_eq!(
            &tokio::fs::read(temp_dir.path().join(file_name)).await?,
            b"{\"//some:target\":1}"
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_write_empty_input() -> anyhow::Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let file_name = "some_file";
        let bcm = BuildCountManager::new(temp_dir.path().to_path_buf().try_into()?);
        let data = HashMap::new();
        bcm.write(&BuildCount(data), FileName::new(file_name)?)
            .await?;
        assert_eq!(
            &tokio::fs::read(temp_dir.path().join(file_name)).await?,
            b"{}"
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_min_build_count_normal_input() -> anyhow::Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let file_name = "some_file";
        tokio::fs::write(temp_dir.path().join(file_name), "{\"//some:target\":1}").await?;
        let target_patterns = make_patterns(vec!["//some:target", "//some/other:target"]);
        let mut bcm = BuildCountManager::new(temp_dir.path().to_path_buf().try_into()?);
        assert_eq!(
            bcm.min_build_count(file_name, &target_patterns, true)
                .await?,
            1
        );
        assert_eq!(
            bcm.min_build_count(file_name, &target_patterns, true)
                .await?,
            2
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_min_build_count_on_failure() -> anyhow::Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let file_name = "some_file";
        tokio::fs::write(temp_dir.path().join(file_name), "{\"//some:target\":1}").await?;
        let target_patterns = make_patterns(vec!["//some:target", "//some/other:target"]);
        let mut bcm = BuildCountManager::new(temp_dir.path().to_path_buf().try_into()?);
        assert_eq!(
            bcm.min_build_count(file_name, &target_patterns, true)
                .await?,
            1
        );
        assert_eq!(
            bcm.min_build_count(file_name, &target_patterns, false)
                .await?,
            1
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_min_build_count_empty_input() -> anyhow::Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let file_name = "some_file";
        tokio::fs::write(temp_dir.path().join(file_name), "{}").await?;
        let target_patterns = make_patterns(vec![]);
        let mut bcm = BuildCountManager::new(temp_dir.path().to_path_buf().try_into()?);
        assert_eq!(
            bcm.min_build_count(file_name, &target_patterns, true)
                .await?,
            0
        );

        Ok(())
    }
}
