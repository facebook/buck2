/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(clippy::ref_option_ref)] // within Serialize

use std::fmt::Display;
use std::path::Path;

use buck2_common::cas_digest::CasDigest;
use buck2_common::cas_digest::DigestAlgorithmKind;
use buck2_common::file_ops::FileDigestKind;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::paths::RelativePath;
use serde::Serialize;
use serde::Serializer;

fn stringify<T, S>(value: &T, serializer: S) -> Result<S::Ok, S::Error>
where
    T: Display,
    S: Serializer,
{
    serializer.collect_str(value)
}

#[derive(Serialize)]
pub struct DirectoryInfo {}

#[derive(Serialize)]
pub struct FileInfo<'a> {
    #[serde(serialize_with = "stringify")]
    pub digest: &'a CasDigest<FileDigestKind>,
    #[serde(serialize_with = "stringify")]
    pub digest_kind: DigestAlgorithmKind,
    pub is_exec: bool,
}

#[derive(Serialize)]
pub struct SymlinkInfo<'a> {
    #[serde(serialize_with = "stringify")]
    pub symlink_rel_path: &'a RelativePath,
}

#[derive(Serialize)]
pub struct ExternalSymlinkInfo<'a> {
    pub target: &'a Path,
    pub remaining_path: Option<&'a ForwardRelativePath>,
}

#[derive(Serialize)]
#[serde(tag = "kind")]
#[serde(rename_all = "snake_case")]
pub enum ArtifactInfo<'a> {
    Directory(DirectoryInfo),
    File(FileInfo<'a>),
    Symlink(SymlinkInfo<'a>),
    ExternalSymlink(ExternalSymlinkInfo<'a>),
}

#[derive(Serialize)]
pub struct ArtifactMetadataJson<'a> {
    pub path: &'a ForwardRelativePath,
    #[serde(flatten)]
    pub info: ArtifactInfo<'a>,
}

#[cfg(test)]
mod test {
    use buck2_common::cas_digest::CasDigestConfig;

    use super::*;

    #[test]
    fn test_dir_json() {
        let path = ForwardRelativePath::unchecked_new("test");
        let metadata = ArtifactMetadataJson {
            path,
            info: ArtifactInfo::Directory(DirectoryInfo {}),
        };
        let json = serde_json::to_string(&metadata).expect("failed to serialize");
        assert_eq!(json, r#"{"path":"test","kind":"directory"}"#,);
    }

    #[test]
    fn test_file_json() {
        let path = ForwardRelativePath::unchecked_new("test.txt");
        let digest = CasDigest::parse_digest(
            "fb19d5b1546753df5f7741efbabd0d24dcaacd65:20",
            CasDigestConfig::testing_default(),
        )
        .expect("failed to create digest")
        .0;
        let metadata = ArtifactMetadataJson {
            path,
            info: ArtifactInfo::File(FileInfo {
                digest: &digest,
                digest_kind: DigestAlgorithmKind::Sha1,
                is_exec: false,
            }),
        };
        let json = serde_json::to_string(&metadata).expect("failed to serialize");
        assert_eq!(
            json,
            r#"{"path":"test.txt","kind":"file","digest":"fb19d5b1546753df5f7741efbabd0d24dcaacd65:20","digest_kind":"SHA1","is_exec":false}"#,
        );
    }

    #[test]
    fn test_symlink_json() {
        let path = ForwardRelativePath::unchecked_new("test.txt");
        let symlink_rel_path = RelativePath::new("../test.txt");
        let metadata = ArtifactMetadataJson {
            path,
            info: ArtifactInfo::Symlink(SymlinkInfo { symlink_rel_path }),
        };
        let json = serde_json::to_string(&metadata).expect("failed to serialize");
        assert_eq!(
            json,
            r#"{"path":"test.txt","kind":"symlink","symlink_rel_path":"../test.txt"}"#,
        );
    }

    #[test]
    fn test_external_symlink_json() {
        let path = ForwardRelativePath::unchecked_new("test.txt");
        let target = Path::new("/mnt/gvfs");
        let remaining =
            ForwardRelativePath::new("test.txt").expect("failed to make remaining path");
        let metadata = ArtifactMetadataJson {
            path,
            info: ArtifactInfo::ExternalSymlink(ExternalSymlinkInfo {
                target,
                remaining_path: Some(remaining),
            }),
        };
        let json = serde_json::to_string(&metadata).expect("failed to serialize");
        assert_eq!(
            json,
            r#"{"path":"test.txt","kind":"external_symlink","target":"/mnt/gvfs","remaining_path":"test.txt"}"#,
        );
    }
}
