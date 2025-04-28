/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;

use buck2_common::file_ops::FileDigest;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use serde::Serialize;
use serde::Serializer;

use crate::execute::request::ActionMetadataBlobData;

#[derive(Clone)]
pub struct PathsWithDigestBlobData(pub ActionMetadataBlobData);

fn stringify<T, S>(value: &T, serializer: S) -> Result<S::Ok, S::Error>
where
    T: Display,
    S: Serializer,
{
    serializer.collect_str(value)
}

#[derive(Serialize)]
struct PathWithDigest<'a> {
    path: ForwardRelativePathBuf,
    #[serde(serialize_with = "stringify")]
    digest: &'a FileDigest,
}

#[derive(Serialize)]
struct MetadataJson<'a> {
    version: i32,
    digests: Vec<PathWithDigest<'a>>,
}

#[derive(Default)]
pub struct PathsWithDigestBuilder<'a> {
    paths: Vec<PathWithDigest<'a>>,
}

impl<'a> PathsWithDigestBuilder<'a> {
    pub fn add(&mut self, path: ForwardRelativePathBuf, digest: &'a FileDigest) {
        self.paths.push(PathWithDigest { path, digest });
    }

    pub fn build(self) -> buck2_error::Result<PathsWithDigestBlobData> {
        let json = MetadataJson {
            digests: self.paths,
            // Increment this version if format changes
            version: 1,
        };
        let json_string = serde_json::to_string(&json)?;
        Ok(PathsWithDigestBlobData(ActionMetadataBlobData::from_json(
            json_string,
        )))
    }
}
