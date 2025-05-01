/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_common::init::ResourceControlConfig;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::logging::LogConfigurationReloadHandle;

#[cfg(unix)]
type RawFd = std::os::unix::io::RawFd;

#[cfg(windows)]
type RawFd = String;

#[derive(Debug, clap::Parser)]
#[clap(about = "run the forkserver")]
pub(crate) struct ForkserverCommand {
    #[clap(long)]
    fd: RawFd,

    #[clap(long)]
    state_dir: String,

    #[clap(long, value_parser = ResourceControlConfig::deserialize)]
    resource_control: ResourceControlConfig,
}

impl ForkserverCommand {
    pub(crate) fn exec(
        self,
        _matches: BuckArgMatches<'_>,
        _ctx: ClientCommandContext<'_>,
        log_reload_handle: Arc<dyn LogConfigurationReloadHandle>,
    ) -> anyhow::Result<()> {
        let state_dir = AbsNormPathBuf::from(self.state_dir)?;
        fs_util::create_dir_all(&state_dir).map_err(buck2_error::Error::from)?;

        #[cfg(unix)]
        {
            // For us to get this FD it must be non-CLOEXEC but we don't want our children to
            // inherit it.
            set_cloexec(self.fd)?;

            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()?;

            Ok(rt.block_on(buck2_forkserver::unix::run_forkserver(
                self.fd,
                log_reload_handle,
                state_dir,
                self.resource_control,
            ))?)
        }

        #[cfg(not(unix))]
        {
            let _ignored = log_reload_handle;
            Err(anyhow::anyhow!("The forkserver is only available on UNIX"))
        }
    }
}

#[cfg(unix)]
fn set_cloexec(fd: RawFd) -> std::io::Result<()> {
    unsafe {
        let flags = libc::fcntl(fd, libc::F_GETFD);
        if flags < 0 {
            return Err(std::io::Error::last_os_error());
        }

        if libc::fcntl(fd, libc::F_SETFD, flags | libc::FD_CLOEXEC) < 0 {
            return Err(std::io::Error::last_os_error());
        }
    }

    Ok(())
}
