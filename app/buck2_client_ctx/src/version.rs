/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs::File;
use std::sync::OnceLock;

use buck2_core::buck2_env;
use object::Object;

/// Provides information about this buck version.
pub struct BuckVersion {
    version: String,
    internal_exe_hash: String,
}

impl BuckVersion {
    pub fn get() -> &'static BuckVersion {
        static VERSION: OnceLock<BuckVersion> = OnceLock::new();
        VERSION.get_or_init(Self::compute)
    }

    pub fn get_unique_id() -> &'static str {
        Self::get().unique_id()
    }

    pub fn get_version() -> &'static str {
        Self::get().version()
    }

    fn extract_unique_id(file: &object::File) -> Option<String> {
        if let Ok(Some(build_id)) = file.build_id() {
            Some(hex::encode(build_id))
        } else if let Ok(Some(uuid)) = file.mach_uuid() {
            Some(hex::encode(uuid))
        } else if cfg!(windows) {
            buck2_build_info::win_internal_version().map(|s| s.to_owned())
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
        // Make sure to use the daemon exe's version, if there is one
        let exe = crate::daemon::client::connect::get_daemon_exe().unwrap();
        let mut file = File::open(exe).unwrap();
        let file_m = match unsafe { memmap2::Mmap::map(&file) } {
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

        let (internal_exe_hash, internal_exe_hash_kind) = if let Some(internal_exe_hash) =
            Self::extract_unique_id(&file_object)
        {
            (internal_exe_hash, "<build-id>")
        } else {
            if !(buck2_core::is_open_source() || buck2_env!("BUCK2_IGNORE_VERSION_EXTRACTION_FAILURE", type=bool, default=false, applicability=testing).unwrap_or(false)) {
                let _ignored = crate::eprintln!(
                    "version extraction failed. This indicates an issue with the buck2 release, will fallback to binary hash"
                );
            }
            (Self::hash_binary(&mut file), "<exe-hash>")
        };

        let version = if let Some(version) = buck2_build_info::revision() {
            version.to_owned()
        } else {
            format!("{} {}", internal_exe_hash, internal_exe_hash_kind)
        };

        BuckVersion {
            version,
            internal_exe_hash,
        }
    }

    /// Provides a globally unique identifier for this buck executable.
    pub fn unique_id(&self) -> &str {
        &self.internal_exe_hash
    }

    pub fn version(&self) -> &str {
        &self.version
    }
}
