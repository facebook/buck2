/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::time::Duration;

use buck2_core::fs::async_fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileName;
use buck2_data::ParsedTargetPatterns;
use buck2_error::BuckErrorContext;
use serde::Deserialize;
use serde::Serialize;

use crate::client_utils;

// Version for serialized BuildCount on disk.
// Update if changing BuildCount to allow building with deployed and compiled buck on the same rev.
pub const BUILD_COUNT_VERSION: u64 = 1;

#[derive(
    Default,
    Clone,
    Copy,
    Serialize,
    Deserialize,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Debug
)]
pub struct BuildCount {
    pub successful_build_count: u64,
    pub attempted_build_count: u64,
}

impl BuildCount {
    pub fn new(successful_build_count: u64, attempted_build_count: u64) -> Self {
        Self {
            successful_build_count,
            attempted_build_count,
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct BuildCountMap(HashMap<String, BuildCount>);

impl BuildCountMap {
    pub fn increment(&mut self, patterns: &ParsedTargetPatterns, is_success: bool) {
        for target in patterns.target_patterns.iter() {
            match self.0.get_mut(&target.value) {
                Some(count) => {
                    count.attempted_build_count += 1;
                    if is_success {
                        count.successful_build_count += 1;
                    }
                }
                None => {
                    let count = BuildCount::new(if is_success { 1 } else { 0 }, 1);
                    self.0.insert(target.value.clone(), count);
                }
            }
        }
    }

    pub fn min_count(&self, patterns: &ParsedTargetPatterns) -> BuildCount {
        if patterns.target_patterns.is_empty() {
            return Default::default();
        }

        // If the target has never been successfully built it won't be in the map, in that case its count is 0.
        patterns
            .target_patterns
            .iter()
            .map(|v| self.0.get(&v.value).copied().unwrap_or(Default::default()))
            .min()
            .unwrap() // target_patterns is non-empty, so min() should return Some
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

    async fn ensure_dir(&self) -> buck2_error::Result<()> {
        async_fs_util::create_dir_all(&self.base_dir).await
    }

    async fn read(&self, file_name: &FileName) -> buck2_error::Result<BuildCountMap> {
        let path = self.base_dir.join(file_name);
        match async_fs_util::read_to_string_if_exists(&path).await? {
            Some(buffer) => Ok(serde_json::from_str(&buffer)
                .with_buck_error_context(|| format!("Parsing JSON from {}", path.display()))?),
            None => {
                // it is normal after rebase, clean, etc.
                Ok(BuildCountMap(HashMap::new()))
            }
        }
    }

    async fn write(
        &self,
        build_count: &BuildCountMap,
        file_name: &FileName,
    ) -> buck2_error::Result<()> {
        self.ensure_dir().await?;
        let path = self.base_dir.join(file_name);
        async_fs_util::write(path, &serde_json::to_vec(build_count)?).await
    }

    async fn lock_with_timeout(&self, timeout: Duration) -> buck2_error::Result<FileLockGuard> {
        self.ensure_dir().await?;
        let file = std::fs::File::create(self.base_dir.join(FileName::new(Self::LOCK_FILE_NAME)?))?;
        let fileref = &file;
        client_utils::retrying(
            Duration::from_millis(5),
            Duration::from_millis(100),
            timeout,
            || async { buck2_error::Ok(fs4::FileExt::try_lock_exclusive(fileref)?) },
        )
        .await?;
        Ok(FileLockGuard { file })
    }

    /// Updates the build counts for set of targets (on success) and returns the min.
    pub async fn increment(
        &self,
        merge_base: &str,
        target_patterns: &ParsedTargetPatterns,
        is_success: bool,
    ) -> buck2_error::Result<BuildCount> {
        self.mutate(
            merge_base,
            target_patterns,
            Some(|build_count_map: &mut BuildCountMap| {
                build_count_map.increment(target_patterns, is_success);
            }),
        )
        .await
    }

    /// Returns the existing min build count for the set of targets.
    pub async fn min_count(
        &self,
        merge_base: &str,
        target_patterns: &ParsedTargetPatterns,
    ) -> buck2_error::Result<BuildCount> {
        self.mutate(merge_base, target_patterns, None::<fn(&mut BuildCountMap)>)
            .await
    }

    async fn mutate(
        &self,
        merge_base: &str,
        target_patterns: &ParsedTargetPatterns,
        mutation: Option<impl FnOnce(&mut BuildCountMap)>,
    ) -> buck2_error::Result<BuildCount> {
        let file_name_str = format!("{}-{}", merge_base, BUILD_COUNT_VERSION);
        let file_name = FileName::new(&file_name_str)?;
        let _guard = self.lock_with_timeout(Self::LOCK_TIMEOUT).await?;
        let mut build_count_map = self.read(file_name).await?;
        if let Some(mutation) = mutation {
            mutation(&mut build_count_map);
            self.write(&build_count_map, file_name).await?;
        }
        Ok(build_count_map.min_count(target_patterns))
    }
}

#[must_use]
struct FileLockGuard {
    file: std::fs::File,
}

impl Drop for FileLockGuard {
    fn drop(&mut self) {
        fs4::FileExt::unlock(&self.file)
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
    fn test_update_normal_input() -> buck2_error::Result<()> {
        let mut before = HashMap::new();
        before.insert("//some:target".to_owned(), BuildCount::new(1, 1));
        before.insert("//some/other:target".to_owned(), BuildCount::new(2, 2));
        let mut bc = BuildCountMap(before);
        let target_patterns = make_patterns(vec!["//some/other:target", "//yet/another:target"]);
        bc.increment(&target_patterns, true);
        let mut expected = HashMap::new();
        expected.insert("//some:target".to_owned(), BuildCount::new(1, 1));
        expected.insert("//some/other:target".to_owned(), BuildCount::new(3, 3));
        expected.insert("//yet/another:target".to_owned(), BuildCount::new(1, 1));
        assert_eq!(bc.0, expected);

        Ok(())
    }

    #[test]
    fn test_update_empty_input() -> buck2_error::Result<()> {
        let mut before = HashMap::new();
        before.insert("//some:target".to_owned(), BuildCount::new(1, 1));
        let expected = before.clone();
        let mut bc = BuildCountMap(before);
        let target_patterns = make_patterns(vec![]);
        bc.increment(&target_patterns, true);
        assert_eq!(bc.0, expected);

        Ok(())
    }

    #[test]
    fn test_min_count_some_value() -> buck2_error::Result<()> {
        let mut data = HashMap::new();
        data.insert("//some:target1".to_owned(), BuildCount::new(3, 3));
        data.insert("//some:target2".to_owned(), BuildCount::new(4, 4));
        data.insert("//some:target3".to_owned(), BuildCount::new(5, 5));
        let bc = BuildCountMap(data);
        let target_patterns = make_patterns(vec!["//some:target1", "//some:target2"]);
        assert_eq!(bc.min_count(&target_patterns), BuildCount::new(3, 3));

        Ok(())
    }

    #[test]
    fn test_min_count_ignores_others() -> buck2_error::Result<()> {
        let mut data = HashMap::new();
        data.insert("//some:target1".to_owned(), BuildCount::new(3, 3));
        data.insert("//some:target2".to_owned(), BuildCount::new(4, 4));
        data.insert("//some:target3".to_owned(), BuildCount::new(5, 5));
        let bc = BuildCountMap(data);
        let target_patterns = make_patterns(vec!["//some:target2"]);
        assert_eq!(bc.min_count(&target_patterns), BuildCount::new(4, 4));

        Ok(())
    }

    #[test]
    fn test_min_count_empty_data() -> buck2_error::Result<()> {
        let data = HashMap::new();
        let bc = BuildCountMap(data);
        assert_eq!(bc.min_count(&make_patterns(vec![])), BuildCount::new(0, 0));

        Ok(())
    }

    #[tokio::test]
    async fn test_read_no_such_file() -> buck2_error::Result<()> {
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
    async fn test_read_normal_file() -> buck2_error::Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let file_name = "some_file";
        tokio::fs::write(temp_dir.path().join(file_name), "{\"//some:target\":[1,1]}").await?;
        let mut expected = HashMap::new();
        expected.insert("//some:target".to_owned(), BuildCount::new(1, 1));
        let bcm = BuildCountManager::new(temp_dir.path().to_path_buf().try_into()?);
        let bc = bcm.read(FileName::new(file_name)?).await?;
        assert_eq!(bc.0, expected);

        Ok(())
    }

    #[tokio::test]
    async fn test_read_illegal_file_contents() -> buck2_error::Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let file_name = "some_file";
        tokio::fs::write(temp_dir.path().join(file_name), "aaa").await?;
        let bcm = BuildCountManager::new(temp_dir.path().to_path_buf().try_into()?);
        assert!(bcm.read(FileName::new(file_name)?).await.is_err());

        Ok(())
    }

    #[tokio::test]
    async fn test_write_normal_input() -> buck2_error::Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let file_name = "some_file";
        let bcm = BuildCountManager::new(temp_dir.path().to_path_buf().try_into()?);
        let mut data = HashMap::new();
        data.insert("//some:target".to_owned(), BuildCount::new(1, 1));
        bcm.write(&BuildCountMap(data), FileName::new(file_name)?)
            .await?;
        assert_eq!(
            std::str::from_utf8(&tokio::fs::read(temp_dir.path().join(file_name)).await?)?,
            "{\"//some:target\":{\"successful_build_count\":1,\"attempted_build_count\":1}}"
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_write_empty_input() -> buck2_error::Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let file_name = "some_file";
        let bcm = BuildCountManager::new(temp_dir.path().to_path_buf().try_into()?);
        let data = HashMap::new();
        bcm.write(&BuildCountMap(data), FileName::new(file_name)?)
            .await?;
        assert_eq!(
            &tokio::fs::read(temp_dir.path().join(file_name)).await?,
            b"{}"
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_increment_normal_input() -> buck2_error::Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let file_name = "some_file";
        tokio::fs::write(temp_dir.path().join(file_name), "{\"//some:target\":[1,1]}").await?;
        let target_patterns = make_patterns(vec!["//some:target", "//some/other:target"]);
        let bcm = BuildCountManager::new(temp_dir.path().to_path_buf().try_into()?);
        assert_eq!(
            bcm.increment(file_name, &target_patterns, true).await?,
            BuildCount::new(1, 1),
        );
        assert_eq!(
            bcm.increment(file_name, &target_patterns, true).await?,
            BuildCount::new(2, 2),
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_increment_on_failure() -> buck2_error::Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let file_name = "some_file";
        tokio::fs::write(temp_dir.path().join(file_name), "{\"//some:target\":[1,1]}").await?;
        let target_patterns = make_patterns(vec!["//some:target", "//some/other:target"]);
        let bcm = BuildCountManager::new(temp_dir.path().to_path_buf().try_into()?);
        assert_eq!(
            bcm.increment(file_name, &target_patterns, true).await?,
            BuildCount::new(1, 1),
        );
        assert_eq!(
            bcm.increment(file_name, &target_patterns, false).await?,
            BuildCount::new(1, 2),
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_increment_empty_input() -> buck2_error::Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let file_name = "some_file";
        tokio::fs::write(temp_dir.path().join(file_name), "{}").await?;
        let target_patterns = make_patterns(vec![]);
        let bcm = BuildCountManager::new(temp_dir.path().to_path_buf().try_into()?);
        assert_eq!(
            bcm.increment(file_name, &target_patterns, true).await?,
            BuildCount::new(0, 0),
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_min_count_no_increment() -> buck2_error::Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let file_name = "some_file";
        tokio::fs::write(temp_dir.path().join(file_name), "{\"//some:target\":[1,1]}").await?;
        let target_patterns = make_patterns(vec!["//some:target", "//some/other:target"]);
        let bcm = BuildCountManager::new(temp_dir.path().to_path_buf().try_into()?);
        let _ = bcm.increment(file_name, &target_patterns, true).await?;
        assert_eq!(
            bcm.min_count(file_name, &target_patterns).await?,
            BuildCount::new(1, 1),
        );
        assert_eq!(
            bcm.min_count(file_name, &target_patterns).await?,
            BuildCount::new(1, 1),
        );

        Ok(())
    }
}
