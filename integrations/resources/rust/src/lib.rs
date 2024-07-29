/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod manifest;

use std::collections::HashMap;
use std::env;
use std::fs;
use std::io;
use std::path::PathBuf;

use once_cell::sync::OnceCell;
use serde::de::DeserializeSeed as _;
use thiserror::Error;

use crate::manifest::ResourcesMap;

#[derive(Debug, Error)]
pub enum BuckResourcesError {
    #[error("Failed to look up our own executable path")]
    NoCurrentExe { source: io::Error },

    #[error(
        "Failed to read manifest file: `{manifest_path}`. \
        Are you maybe running `buck1`? `rust_binary` only supports `resources` under `buck2`!"
    )]
    ReadFailed {
        manifest_path: PathBuf,
        source: io::Error,
    },

    #[error("Failed to parse manifest file: `{manifest_path}`")]
    ParsingFailed {
        manifest_path: PathBuf,
        source: serde_json::Error,
    },

    #[error("No resource named `{name}` found in manifest file: `{manifest_path}`")]
    NoSuchResource {
        name: String,
        manifest_path: PathBuf,
    },

    #[error(
        "Resource `{name}` points to invalid path `{resource_path}` in manifest `{manifest_path}`"
    )]
    BadResourcePath {
        name: String,
        resource_path: PathBuf,
        manifest_path: PathBuf,
        source: io::Error,
    },
}

/// Look up a resource based on a manifest file. Built to work seamlessly
/// with `resources` defined in a `rust_binary` target, but in principle
/// it would work with any correct manifest file.
///
/// Resources follow the naming format:
///
/// ```text
/// {PATH_TO_TARGETS_FOLDER}/{TARGET_NAME}
/// ```
///
/// So for `//path/to:target`, the resource is named `path/to/target`.
///
/// Still unsure about a resource path? Inspect the JSON manifest file
/// found in the `BuckResourcesError`.
///
/// * Manifest location: `$CUR_EXE.resources.json`, where `$CUR_EXE` is
///   the absolute path of the currently executing binary.
/// * Relative paths in the manifest are resolved relative to the location
///   of the currently executing binary.
pub fn get<S>(name: S) -> Result<PathBuf, BuckResourcesError>
where
    S: AsRef<str>,
{
    static MANIFEST: OnceCell<(PathBuf, HashMap<String, PathBuf>)> = OnceCell::new();

    let (manifest_path, manifest) = MANIFEST.get_or_try_init(|| {
        let manifest_path = match env::current_exe() {
            Ok(mut value) => {
                value.as_mut_os_string().push(".resources.json");
                value
            }
            Err(source) => {
                return Err(BuckResourcesError::NoCurrentExe { source });
            }
        };

        let data = match fs::read(&manifest_path) {
            Ok(x) => x,
            Err(source) => {
                return Err(BuckResourcesError::ReadFailed {
                    manifest_path,
                    source,
                });
            }
        };

        let base_dir = manifest_path.parent().unwrap_or(&manifest_path);

        let deserializer = &mut serde_json::Deserializer::from_slice(&data);
        let manifest = match ResourcesMap::new(base_dir).deserialize(deserializer) {
            Ok(x) => x,
            Err(source) => {
                return Err(BuckResourcesError::ParsingFailed {
                    manifest_path,
                    source,
                });
            }
        };

        Ok((manifest_path, manifest))
    })?;

    if let Some(resource_path) = manifest.get(name.as_ref()) {
        dunce::canonicalize(resource_path).map_err(|source| BuckResourcesError::BadResourcePath {
            name: name.as_ref().to_owned(),
            resource_path: resource_path.clone(),
            manifest_path: manifest_path.clone(),
            source,
        })
    } else {
        Err(BuckResourcesError::NoSuchResource {
            name: name.as_ref().to_owned(),
            manifest_path: manifest_path.clone(),
        })
    }
}
