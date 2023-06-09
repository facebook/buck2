/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::net::SocketAddr;
use std::path::Path;
use std::process::Stdio;

use anyhow::Context as _;
use buck2_util::process::async_background_command;
use futures::future::Either;
use tokio::net::TcpListener;
use tokio::net::TcpStream;

use crate::executor_launcher::ExecutorFuture;

pub(crate) async fn spawn(
    executable: &Path,
    args: Vec<String>,
    tpx_args: Vec<String>,
) -> anyhow::Result<(ExecutorFuture, TcpStream, TcpStream)> {
    // Use TCPStream via TCPListener with accept to establish a duplex connection. We set up the
    // listeners, our client connects to both, and that gets us two duplex streams.
    let (executor_addr, executor_tcp_listener) = create_tcp_listener().await?;
    let (orchestrator_addr, orchestrator_tcp_listener) = create_tcp_listener().await?;

    let mut command = async_background_command(executable);
    command
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .args(args)
        .arg("--executor-addr")
        .arg(executor_addr)
        .arg("--orchestrator-addr")
        .arg(orchestrator_addr)
        .arg("--")
        .args(tpx_args);

    let proc = command.spawn().with_context(|| {
        format!(
            "Failed to start {} for OutOfProcessTestExecutor",
            &executable.display()
        )
    })?;

    let exec = ExecutorFuture::new(proc);

    // Use join to wait executor connection in no particular order.
    let conns = async {
        let ((orchestrator_tcp_stream, _), (executor_tcp_stream, _)) = futures::future::try_join(
            orchestrator_tcp_listener.accept(),
            executor_tcp_listener.accept(),
        )
        .await
        .with_context(|| {
            format!(
                "Failed to accept TCP connection from {}",
                &executable.display()
            )
        })?;

        anyhow::Ok((orchestrator_tcp_stream, executor_tcp_stream))
    };

    futures::pin_mut!(conns);

    // Wait for our connections to come up, but also check that the child hasn't exited before we
    // get there.
    match futures::future::select(exec, conns).await {
        Either::Left((output, _)) => Err(anyhow::anyhow!(
            "Executor exited before connecting: {}",
            output?
        )),
        Either::Right((conns, exec)) => {
            let (orchestrator_tcp_stream, executor_tcp_stream) = conns?;
            Ok((exec, executor_tcp_stream, orchestrator_tcp_stream))
        }
    }
}

async fn create_tcp_listener() -> anyhow::Result<(String, TcpListener)> {
    let addr: SocketAddr = "127.0.0.1:0".parse()?;
    let tcp_listener = TcpListener::bind(addr).await?;
    let local_addr = tcp_listener.local_addr()?;
    Ok((local_addr.to_string(), tcp_listener))
}
