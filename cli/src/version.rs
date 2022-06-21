/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs::File;

use object::Object;
use object::ObjectSection;
use once_cell::sync::OnceCell;
use serde_json::Value;

/// Provides information about this buck version.
pub(crate) struct BuckVersion {
    version: String,
    internal_exe_hash: String,
}

impl BuckVersion {
    pub(crate) fn get() -> &'static BuckVersion {
        static VERSION: OnceCell<BuckVersion> = OnceCell::new();
        VERSION.get_or_init(Self::compute)
    }

    pub(crate) fn get_unique_id() -> &'static str {
        Self::get().unique_id()
    }

    pub(crate) fn get_version() -> &'static str {
        Self::get().version()
    }

    fn extract_revision(file: &object::File) -> Option<String> {
        if let Some(section) = file.section_by_name("fb_build_info") {
            if let Ok(data) = section.data() {
                let str_utf8_result = match std::str::from_utf8(data) {
                    Ok(section) => section.trim_matches(char::from(0)),
                    Err(err) => panic!("Failed to parse buck2 file for fb_build_info {:?}", err),
                };
                let json: Value = serde_json::from_str(str_utf8_result).unwrap();
                let revision = json["revision"].to_string().trim_matches('"').to_owned();
                if revision == String::new() {
                    return None;
                } else {
                    return Some(revision);
                }
            }
        }
        None
    }

    fn extract_unique_id(file: &object::File) -> Option<String> {
        if let Ok(Some(build_id)) = file.build_id() {
            Some(hex::encode(build_id))
        } else if let Ok(Some(uuid)) = file.mach_uuid() {
            Some(hex::encode(uuid))
        } else {
            None
        }
    }

    fn hash_binary(file: &mut File) -> String {
        let mut blake3 = blake3::Hasher::new();
        std::io::copy(file, &mut blake3).unwrap();
        let hash = blake3.finalize();
        hash.to_hex().to_string()
    }

    fn compute() -> BuckVersion {
        // TODO(cjhopman): Currently, buck is just a single executable and we don't have really stringent
        // perf requirements so we hash the binary itself for the unique id. We will need to move this to
        // be part of the build/packaging process at some point.
        let exe = std::env::current_exe().unwrap();
        let mut file = File::open(exe).unwrap();
        let file_m = match unsafe { memmap::Mmap::map(&file) } {
            Ok(mmap) => mmap,
            Err(err) => {
                panic!(
                    "Failed to map buck2 binary for version extraction: {:?}",
                    err
                );
            }
        };

        let file_object = match object::File::parse(&*file_m) {
            Ok(file) => file,
            Err(err) => {
                panic!(
                    "Failed to parse buck2 file for version extraction: {:?}",
                    err
                );
            }
        };

        let internal_exe_hash = if let Some(internal_exe_hash) =
            Self::extract_unique_id(&file_object)
        {
            internal_exe_hash
        } else {
            let _ignored = crate::eprintln!(
                "version extraction failed. This indicates an issue with the buck2 release, will fallback to binary hash"
            );
            Self::hash_binary(&mut file)
        };

        let version = if let Some(version) = Self::extract_revision(&file_object) {
            version
        } else {
            format!("{} <local>", internal_exe_hash)
        };

        BuckVersion {
            version,
            internal_exe_hash,
        }
    }

    /// Provides a globally unique identifier for this buck executable.
    pub(crate) fn unique_id(&self) -> &str {
        &self.internal_exe_hash
    }

    pub(crate) fn version(&self) -> &str {
        &self.version
    }
}
