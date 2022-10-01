/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client::client_ctx::ClientCommandContext;

#[cfg(unix)]
type RawFd = std::os::unix::io::RawFd;

#[cfg(windows)]
type RawFd = String;

#[derive(Debug, clap::Parser)]
#[clap(about = "run the forkserver")]
pub(crate) struct ForkserverCommand {
    #[clap(long)]
    fd: RawFd,
}

impl ForkserverCommand {
    pub(crate) fn exec(
        self,
        _matches: &clap::ArgMatches,
        _ctx: ClientCommandContext,
    ) -> anyhow::Result<()> {
        #[cfg(unix)]
        {
            // For us to get this FD it must be non-CLOEXEC but we don't want our children to
            // inherit it.
            set_cloexec(self.fd)?;

            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()?;

            rt.block_on(buck2_forkserver::unix::run_forkserver(self.fd))
        }

        #[cfg(not(unix))]
        {
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
