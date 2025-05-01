/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;
use std::path;

use buck2_artifact::artifact::artifact_type::BaseArtifactKind;
use buck2_build_api::build::BuildProviderType;
use buck2_build_api::build::ProviderArtifacts;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_core::fs::project::ProjectRoot;
use buck2_error::BuckErrorContext;
use buck2_query::__derive_refs::indexmap::IndexMap;
use itertools::Itertools;
use tracing::info;

pub(crate) fn create_unhashed_outputs(
    provider_artifacts: Vec<ProviderArtifacts>,
    artifact_fs: &ArtifactFs,
    fs: &ProjectRoot,
) -> buck2_error::Result<u64> {
    let buck_out_root = fs.resolve(artifact_fs.buck_out_path_resolver().root());

    let start = std::time::Instant::now();
    // The following IndexMap will contain a key of the unhashed/symlink path and values of all the hashed locations that map to the unhashed location.
    let mut unhashed_to_hashed: IndexMap<AbsNormPathBuf, HashSet<AbsNormPathBuf>> = IndexMap::new();
    for provider_artifact in provider_artifacts {
        if !matches!(provider_artifact.provider_type, BuildProviderType::Default) {
            continue;
        }

        match provider_artifact.values.iter().exactly_one() {
            Ok((artifact, _)) => match artifact.as_parts() {
                (BaseArtifactKind::Build(build), _projected_path) => {
                    if let Some(unhashed_path) =
                        artifact_fs.retrieve_unhashed_location(build.get_path())
                    {
                        let path = artifact_fs.resolve_build(build.get_path())?;
                        let abs_unhashed_path = fs.resolve(&unhashed_path);
                        let entry = unhashed_to_hashed
                            .entry(abs_unhashed_path)
                            .or_insert_with(HashSet::new);
                        entry.insert(fs.resolve(&path));
                    }
                }
                _ => {}
            },
            Err(_) => {}
        };
    }
    // The IndexMap is used now to determine if and what conflicts exist where multiple hashed artifact locations
    // all want a symlink to the same unhashed artifact location and deal with them accordingly.
    let mut num_unhashed_links_made = 0;
    for (unhashed, hashed_set) in unhashed_to_hashed {
        if hashed_set.len() == 1 {
            create_unhashed_link(&unhashed, hashed_set.iter().next().unwrap(), &buck_out_root)?;
            num_unhashed_links_made += 1;
        } else {
            info!(
                "The following outputs have a conflicting unhashed path at {}: {:?}",
                unhashed, hashed_set
            );
        }
    }
    let duration = start.elapsed();
    info!(
        "Creating {} output compatibility symlinks in {:3}s",
        num_unhashed_links_made,
        duration.as_secs_f64()
    );
    Ok(num_unhashed_links_made)
}

fn create_unhashed_link(
    unhashed_path: &AbsNormPathBuf,
    original_path: &AbsNormPathBuf,
    buck_out_root: &AbsNormPathBuf,
) -> buck2_error::Result<()> {
    // Remove the final path separator if it exists so that the path looks like a file and not a directory or else symlink() fails.
    tracing::debug!("Creating link: `{}` -> `{}`", unhashed_path, original_path);

    let mut abs_unhashed_path = unhashed_path.to_owned();
    if let Some(path) = unhashed_path
        .to_str()
        .unwrap()
        .strip_suffix(path::is_separator)
    {
        abs_unhashed_path = AbsNormPathBuf::from(path.to_owned())?;
    }

    // We are going to need to clear the path between buck-out and the symlink we want to create.
    // To do this, we need to traverse forward out of buck_out_root and towards our symlink, and
    // delete any files or symlinks we find along the way. As soon as we find one, we can stop.

    if let Some(parent) = abs_unhashed_path.parent() {
        for prefix in iter_reverse_ancestors(parent, buck_out_root.as_ref()) {
            let meta = match fs_util::symlink_metadata_if_exists(prefix)? {
                Some(meta) => meta,
                None => continue,
            };

            if meta.is_file() || meta.is_symlink() {
                fs_util::remove_file(prefix).with_buck_error_context(
                    || "was not able to remove file while cleaning up prefixes",
                )?;
            }
        }

        fs_util::create_dir_all(parent)
            .with_buck_error_context(|| "while creating unhashed directory for symlink")?;
    }

    match fs_util::symlink_metadata(&abs_unhashed_path) {
        Ok(metadata) => {
            if metadata.is_dir() {
                fs_util::remove_dir_all(&abs_unhashed_path).with_buck_error_context(
                    || "was not able to remove absolute unhashed path (directory)",
                )?
            } else {
                fs_util::remove_file(&abs_unhashed_path).with_buck_error_context(
                    || "was not able to remove absolute unhashed path (file)",
                )?
            }
        }
        Err(_) => {}
    }
    fs_util::symlink(original_path, abs_unhashed_path).with_buck_error_context(
        || "was not able to symlink original path to absolute unhashed path",
    )?;
    Ok(())
}

/// Iterate over the path components between stop_at and path.
fn iter_reverse_ancestors<'a>(
    path: &'a AbsPath,
    stop_at: &'_ AbsPath,
) -> impl Iterator<Item = &'a AbsPath> {
    let ancestors = path
        .ancestors()
        .take_while(|a| *a != stop_at)
        .collect::<Vec<_>>();

    ancestors.into_iter().rev()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_iter_reverse_ancestors() {
        let prefix = if cfg!(windows) { "C:" } else { "" };
        let root = AbsNormPathBuf::try_from(format!("{prefix}/repo/buck-out/v2")).unwrap();
        let path =
            AbsNormPathBuf::try_from(format!("{prefix}/repo/buck-out/v2/foo/bar/some")).unwrap();

        let mut iter = iter_reverse_ancestors(&path, &root);
        assert_eq!(
            iter.next().unwrap().to_str().unwrap(),
            &format!("{prefix}/repo/buck-out/v2/foo"),
        );
        assert_eq!(
            iter.next().unwrap().to_str().unwrap(),
            &format!("{prefix}/repo/buck-out/v2/foo/bar"),
        );
        assert_eq!(
            iter.next().unwrap().to_str().unwrap(),
            &format!("{prefix}/repo/buck-out/v2/foo/bar/some"),
        );
        assert_eq!(iter.next(), None);
    }
}
