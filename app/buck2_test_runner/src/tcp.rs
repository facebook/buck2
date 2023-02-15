/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::net::SocketAddr;

use buck2_grpc::DuplexChannel;
use clap::Parser;
use tokio::net::TcpStream;

#[derive(Debug, Parser)]
pub struct Buck2TestRunnerTcp {
    #[clap(long)]
    executor_addr: String,

    #[clap(long)]
    orchestrator_addr: String,

    args: Vec<String>,
}

impl Buck2TestRunnerTcp {
    pub async fn run(self) -> anyhow::Result<()> {
        // NOTE: We can remove this code once Tokio supports UNIX domain socket or Named pipe
        // https://github.com/tokio-rs/tokio/issues/2201

        // NOTE: We assume the parameters we received from the caller are correct here. If
        // they're not, things are probably going to go wrong but that's on our caller.
        //
        // There are a few ways in which the params could be incorrect:
        // - The TCP socket addresses aren't valid
        // - The TCP socket don't exist
        // - The TCP socket aren't alive
        let orchestrator_addr: SocketAddr = self
            .orchestrator_addr
            .parse()
            .expect("Invalid orchestrator address");
        let executor_addr: SocketAddr = self
            .executor_addr
            .parse()
            .expect("Invalid executor address");

        let orchestrator_io = TcpStream::connect(&orchestrator_addr)
            .await
            .expect("Failed to create orchestrator_io");

        let executor_io = TcpStream::connect(&executor_addr)
            .await
            .expect("Failed to create executor_io");

        let executor_io = {
            let (read, write) = tokio::io::split(executor_io);
            DuplexChannel::new(read, write)
        };

        crate::service::run(orchestrator_io, executor_io, self.args).await
    }
}
