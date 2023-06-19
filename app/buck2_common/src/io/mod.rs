/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[cfg(any(fbcode_build, cargo_internal_build))]
pub mod eden;

pub mod fs;
pub mod trace;

use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;

use crate::cas_digest::CasDigestConfig;
use crate::file_ops::RawDirEntry;
use crate::file_ops::RawPathMetadata;
use crate::io::trace::TracingIoProvider;
use crate::legacy_configs::LegacyBuckConfig;

#[async_trait]
pub trait IoProvider: Allocative + Send + Sync {
    async fn read_file_if_exists(
        &self,
        path: ProjectRelativePathBuf,
    ) -> anyhow::Result<Option<String>>;

    async fn read_dir(&self, path: ProjectRelativePathBuf) -> anyhow::Result<Vec<RawDirEntry>>;

    async fn read_path_metadata_if_exists(
        &self,
        path: ProjectRelativePathBuf,
    ) -> anyhow::Result<Option<RawPathMetadata<ProjectRelativePathBuf>>>;

    /// Request that this I/O provider be up to date with whatever I/O operations the user might
    /// have done until this point.
    async fn settle(&self) -> anyhow::Result<()>;

    fn name(&self) -> &'static str;

    /// Returns the Eden version of the underlying system of the IoProvider, if available.
    async fn eden_version(&self) -> anyhow::Result<Option<String>>;

    fn project_root(&self) -> &ProjectRoot;

    fn as_any(&self) -> &dyn std::any::Any;
}

pub async fn create_io_provider(
    fb: fbinit::FacebookInit,
    project_fs: ProjectRoot,
    root_config: Option<&LegacyBuckConfig>,
    cas_digest_config: CasDigestConfig,
    trace_io: bool,
) -> anyhow::Result<Arc<dyn IoProvider>> {
    #[cfg(any(fbcode_build, cargo_internal_build))]
    {
        use buck2_core::rollout_percentage::RolloutPercentage;

        let allow_eden_io_default =
            RolloutPercentage::from_bool(cfg!(any(target_os = "macos", target_os = "windows")));

        let allow_eden_io = root_config
            .and_then(|c| c.parse("buck2", "allow_eden_io").transpose())
            .transpose()?
            .unwrap_or(allow_eden_io_default)
            .roll();

        if allow_eden_io {
            if let Some(eden) =
                eden::EdenIoProvider::new(fb, &project_fs, cas_digest_config).await?
            {
                return if trace_io {
                    Ok(Arc::new(TracingIoProvider::new(Box::new(eden))))
                } else {
                    Ok(Arc::new(eden))
                };
            }
        }
    }

    let _allow_unused = fb;
    let _allow_unused = root_config;

    if trace_io {
        Ok(Arc::new(TracingIoProvider::new(Box::new(
            fs::FsIoProvider::new(project_fs, cas_digest_config),
        ))))
    } else {
        Ok(Arc::new(fs::FsIoProvider::new(
            project_fs,
            cas_digest_config,
        )))
    }
}
