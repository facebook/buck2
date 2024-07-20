/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;

use buck2_common::buildfiles::parse_buildfile_name;
use buck2_common::invocation_roots::InvocationRoots;
use buck2_common::legacy_configs::cells::BuckConfigBasedCells;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use relative_path::RelativePath;

/// Complete a partial pattern.
///
/// Returns a collection of possible completions, each generally including the
/// partial pattern. The partial pattern will not be returned when completion
/// logic is able to unambiguously normalize the partial pattern, such as
/// partials which cross cell boundaries. In this case, normalized
/// completion(s) are returned.
pub(crate) async fn complete(
    roots: &InvocationRoots,
    cwd: &Path,
    partial: &str,
) -> anyhow::Result<Vec<String>> {
    complete_dir(roots, cwd, partial).await
}

async fn complete_dir(
    roots: &InvocationRoots,
    cwd: &Path,
    partial: &str,
) -> anyhow::Result<Vec<String>> {
    let cwd_abs = AbsNormPath::new(cwd)?;
    let cwd_relative = roots.project_root.relativize(cwd_abs)?;
    let root_config =
        &BuckConfigBasedCells::parse_with_config_args(&roots.project_root, &[], &cwd_relative)
            .await?
            .root_config;
    let buildfile_names = parse_buildfile_name(root_config)?;

    let partial_path = Path::new(partial);
    let partial_base = partial_path.file_name().unwrap().to_str().unwrap();
    let partial_dir = partial_path.parent().unwrap();

    let mut scan_dir = cwd.to_path_buf();
    if let Some(offset_dir) = partial_path.parent() {
        scan_dir = scan_dir.join(offset_dir);
    }

    let mut ret = Vec::<String>::new();
    for entry_result in scan_dir.read_dir()? {
        let entry = entry_result?;
        if entry.path().is_dir() && file_name_string(&entry).starts_with(partial_base) {
            let safe_partial_dir = RelativePath::from_path(&partial_dir)?;
            let dir = safe_partial_dir.join(entry.file_name().to_str().unwrap());
            ret.push(format!("{}/", dir));
            for f in &buildfile_names {
                if cwd.join(dir.to_path("")).join(f).exists() {
                    ret.push(format!("{}:", dir.as_str()));
                    break;
                }
            }
        }
    }
    Ok(ret)
}

fn file_name_string(entry: &std::fs::DirEntry) -> String {
    entry.file_name().into_string().unwrap()
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use buck2_common::invocation_roots::find_invocation_roots;

    use super::*;

    fn paths_to_test_data() -> &'static [&'static str] {
        &[
            "fbcode/buck2/app/buck2_client/test_data",
            "app/buck2_client/test_data",
            "test_data",
        ]
    }

    fn in_dir(d: &str) -> anyhow::Result<(InvocationRoots, PathBuf)> {
        let cwd = std::env::current_dir().unwrap();

        let mut candidate = PathBuf::new();
        for path in paths_to_test_data() {
            candidate = cwd.join(path).join(d);
            if candidate.exists() {
                break;
            }
        }

        assert!(candidate.exists(), "test_data directory not found");
        Ok((find_invocation_roots(&candidate)?, candidate))
    }

    fn in_root() -> anyhow::Result<(InvocationRoots, PathBuf)> {
        let cwd = std::env::current_dir().unwrap();

        let mut candidate = PathBuf::new();
        for path in paths_to_test_data() {
            candidate = cwd.join(path);
            if candidate.exists() {
                break;
            }
        }

        assert!(candidate.exists(), "test_data directory not found");
        Ok((find_invocation_roots(&candidate)?, candidate))
    }

    #[tokio::test]
    async fn test_expands_top_level_directory() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let actual = complete(&roots, &cwd, "bare").await?;

        assert_eq!(actual, vec!["baredir0/"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_expands_subdirectory() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let actual = complete(&roots, &cwd, "baredir0/bare").await?;

        assert_eq!(actual, vec!["baredir0/baredir0a/"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_expands_subdirectory_with_buck_targets() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let actual = complete(&roots, &cwd, "baredir0/buck").await?;

        assert_eq!(actual, vec!["baredir0/buckdir0b/", "baredir0/buckdir0b:",]);
        Ok(())
    }

    #[tokio::test]
    async fn test_provides_subdirectory_and_target_alternatives() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let actual = complete(&roots, &cwd, "baredir0/buckdir0b").await?;

        assert_eq!(actual, vec!["baredir0/buckdir0b/", "baredir0/buckdir0b:",]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_partial_paths_and_matched_target_dirs() -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("baredir0")?;
        let actual = complete(&roots, &cwd, "b").await?;

        assert_eq!(actual, vec!["baredir0a/", "buckdir0b/", "buckdir0b:",]);
        Ok(())
    }
}
