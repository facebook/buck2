/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::os::unix::io::FromRawFd;
use std::os::unix::io::RawFd;
use std::os::unix::net::UnixStream as StdUnixStream;
use std::sync::Arc;

use buck2_common::init::ResourceControlConfig;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::logging::LogConfigurationReloadHandle;
use buck2_error::conversion::from_any_with_tag;
use buck2_error::BuckErrorContext;
use buck2_forkserver_proto::forkserver_server;
use buck2_grpc::DuplexChannel;
use tokio::net::UnixStream;

use super::service::UnixForkserverService;

pub async fn run_forkserver(
    fd: RawFd,
    log_reload_handle: Arc<dyn LogConfigurationReloadHandle>,
    state_dir: AbsNormPathBuf,
    resource_control: ResourceControlConfig,
) -> buck2_error::Result<()> {
    // SAFETY: At worst, we just read (or close) the wrong FD.
    let io = UnixStream::from_std(unsafe { StdUnixStream::from_raw_fd(fd) })
        .expect("Failed to create io");

    let io = {
        let (read, write) = tokio::io::split(io);
        DuplexChannel::new(read, write)
    };

    let service = UnixForkserverService::new(log_reload_handle, &state_dir, resource_control)
        .buck_error_context("Failed to create UnixForkserverService")?;

    let router = tonic::transport::Server::builder().add_service(
        forkserver_server::ForkserverServer::new(service)
            .max_encoding_message_size(usize::MAX)
            .max_decoding_message_size(usize::MAX),
    );

    buck2_grpc::spawn_oneshot(io, router)
        .into_join_handle()
        .await?
        .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?;

    Ok(())
}
