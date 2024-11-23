/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_common::cas_digest::CasDigestConfig;
use buck2_common::io::fs::FsIoProvider;
use buck2_common::io::trace::TracingIoProvider;
use buck2_common::io::IoProvider;
use buck2_common::legacy_configs::configs::LegacyBuckConfig;
use buck2_core::fs::project::ProjectRoot;

pub async fn create_io_provider(
    fb: fbinit::FacebookInit,
    project_fs: ProjectRoot,
    root_config: &LegacyBuckConfig,
    cas_digest_config: CasDigestConfig,
    trace_io: bool,
) -> buck2_error::Result<Arc<dyn IoProvider>> {
    #[cfg(fbcode_build)]
    {
        use buck2_common::legacy_configs::key::BuckconfigKeyRef;
        use buck2_core::rollout_percentage::RolloutPercentage;

        let allow_eden_io_default =
            RolloutPercentage::from_bool(cfg!(any(target_os = "macos", target_os = "windows")));

        let allow_eden_io = root_config
            .parse(BuckconfigKeyRef {
                section: "buck2",
                property: "allow_eden_io",
            })?
            .unwrap_or(allow_eden_io_default)
            .roll();

        if allow_eden_io {
            if let Some(eden) =
                buck2_eden::io_provider::EdenIoProvider::new(fb, &project_fs, cas_digest_config)
                    .await?
            {
                return if trace_io {
                    Ok(Arc::new(TracingIoProvider::new(Box::new(eden))))
                } else {
                    Ok(Arc::new(eden))
                };
            }
        }
    }

    let _allow_unused = (fb, root_config);

    if trace_io {
        Ok(Arc::new(TracingIoProvider::new(Box::new(
            FsIoProvider::new(project_fs, cas_digest_config),
        ))))
    } else {
        Ok(Arc::new(FsIoProvider::new(project_fs, cas_digest_config)))
    }
}
