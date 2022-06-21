/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::net::SocketAddr;
use std::process::Stdio;

use anyhow::Context as _;
use tokio::net::TcpListener;
use tokio::net::TcpStream;
use tokio::process::Child;
use tokio::process::Command;

pub(crate) async fn spawn(
    name: &str,
    tpx_args: Vec<String>,
) -> anyhow::Result<(Child, TcpStream, TcpStream)> {
    // Use TCPStream via TCPListner with accept to simulate UnixStream.
    let (executor_addr, executor_tcp_listener) = create_tcp_listener().await?;
    let (orchestrator_addr, orchestrator_tcp_listener) = create_tcp_listener().await?;

    let mut command = Command::new(name);
    command
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .arg("--executor-addr")
        .arg(executor_addr)
        .arg("--orchestrator-addr")
        .arg(orchestrator_addr)
        .arg("--")
        .args(tpx_args);

    let proc = command
        .spawn()
        .with_context(|| format!("Failed to start {} for OutOfProcessTestExecutor", name))?;

    // Use join to wait executor connection in no particular order.
    let ((orchestrator_tcp_stream, _), (executor_tcp_stream, _)) = tokio::try_join!(
        orchestrator_tcp_listener.accept(),
        executor_tcp_listener.accept(),
    )
    .with_context(|| format!("Failed to accept TCP connection from {}", name))?;

    Ok((proc, executor_tcp_stream, orchestrator_tcp_stream))
}

async fn create_tcp_listener() -> anyhow::Result<(String, TcpListener)> {
    let addr: SocketAddr = "127.0.0.1:0".parse()?;
    let tcp_listener = TcpListener::bind(addr).await?;
    let local_addr = tcp_listener.local_addr()?;
    Ok((local_addr.to_string(), tcp_listener))
}
