/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    fs,
    fs::File,
    path::{Path, PathBuf},
    time::Duration,
};

use anyhow::Context as _;
use assert_matches::assert_matches;
use async_trait::async_trait;
use gazebo::prelude::*;
use tokio::{
    io::AsyncWriteExt,
    process::{Child, Command},
};
use watchman_client::{
    expr::Expr,
    prelude::{Connector, FileType},
};

use crate::watchman::{SyncableQuery, SyncableQueryProcessor, WatchmanEvent};

struct TestQueryProcessor;

#[derive(PartialEq, Eq, Debug)]
enum Out {
    FreshInstance,
    Files(Vec<String>),
}

#[async_trait]
impl SyncableQueryProcessor for TestQueryProcessor {
    type Output = Out;

    async fn process_events(&self, events: Vec<WatchmanEvent>) -> anyhow::Result<Self::Output> {
        Ok(Out::Files(
            events.into_map(|e| e.path.display().to_string()),
        ))
    }

    async fn on_fresh_instance(&self) -> anyhow::Result<Self::Output> {
        Ok(Out::FreshInstance)
    }
}

async fn wait_for_watchman(watchman_sock: &Path) -> anyhow::Result<()> {
    let connector = Connector::default().unix_domain_socket(&watchman_sock);

    let mut i = 0;
    loop {
        i += 1;

        match connector.connect().await {
            Ok(..) => return Ok(()),
            Err(e) if i >= 100 => return Err(anyhow::anyhow!("{}", e)),
            _ => {
                tokio::time::sleep(Duration::from_millis(50)).await;
            }
        }
    }
}

struct WatchmanInstance {
    child: Option<Child>,
    sock: PathBuf,
    log: PathBuf,
}

impl WatchmanInstance {
    pub(crate) async fn shutdown(&mut self) -> anyhow::Result<()> {
        let child = self
            .child
            .as_mut()
            .context("Watchman was already shutdown")?;

        child.kill().await.context("Failed to kill Watchman")?;

        // Everything went well. Remove the child so we don't log on Drop.
        self.child.take();

        Ok(())
    }
}

impl Drop for WatchmanInstance {
    fn drop(&mut self) {
        let child = match self.child.as_mut() {
            Some(c) => c,
            None => return,
        };

        // If we get here, something went wrong and we didn't stop Watchman properly. Log debug
        // info.
        eprintln!("WatchmanInstance did not exit cleanly!");
        let log = std::fs::read_to_string(&self.log)
            .with_context(|| format!("Failed to read log file at {}", self.log.display()));
        match log {
            Ok(log) => eprintln!("Watchman logs follow\n{}", log),
            Err(e) => eprintln!("Failed to read logs: {:#}", e),
        };

        // Try to see if Watchman had exited or not.
        match child.try_wait() {
            Ok(Some(status)) => eprintln!("Watchman had exited with status {:?}", status),
            Ok(None) => eprintln!("Watchan is still running"),
            Err(e) => eprintln!("Failed to access Watchman status: {:#}", e),
        }
    }
}

async fn spawn_watchman(watchman_dir: &Path) -> anyhow::Result<WatchmanInstance> {
    // This config might make Watchman a bit more efficient on Mac and make tests less flaky there
    let watchman_config = watchman_dir.join("config");
    let watchman_config_text = r#"{"prefer_split_fsevents_watcher": true}"#;

    {
        let mut conf = tokio::fs::File::create(&watchman_config).await?;
        conf.write_all(watchman_config_text.as_bytes()).await?;
        conf.flush().await?;
    }

    let watchman_sock = watchman_dir.join("sock");
    let watchman_log = watchman_dir.join("log");
    let watchman_pid = watchman_dir.join("pid");

    let watchman = Command::new("watchman")
        .arg("--unix-listener-path")
        .arg(&watchman_sock)
        .arg("--foreground")
        .arg("--no-site-spawner")
        .arg("--logfile")
        .arg(&watchman_log)
        .arg("--no-save-state")
        .arg("--pidfile")
        .arg(&watchman_pid)
        .env("WATCHMAN_CONFIG_FILE", watchman_config)
        .kill_on_drop(true)
        .spawn()?;

    wait_for_watchman(&watchman_sock)
        .await
        .context("Waiting for Watchman to start")?;

    Ok(WatchmanInstance {
        child: Some(watchman),
        sock: watchman_sock,
        log: watchman_log,
    })
}

#[tokio::test]
async fn test_syncable_query() -> anyhow::Result<()> {
    // This test doesn't work unless Watchman is working, so let's
    // over-approximate that as fbcode_build for now.
    if !cfg!(fbcode_build) {
        return Ok(());
    }

    let tempdir = tempfile::tempdir()?;

    // NOTE: This isn't async (and so is tempfile), but this is all tests.
    let root = tempdir.path().join("root");
    let watchman_dir = tempdir.path().join("watchman");
    fs::create_dir(&watchman_dir)?;
    fs::create_dir(&root)?;

    let mut watchman_instance = spawn_watchman(&watchman_dir).await?;

    let connector = Connector::default().unix_domain_socket(&watchman_instance.sock);

    let watchman_query = SyncableQuery::new(
        connector,
        &root,
        Expr::Any(vec![Expr::FileType(FileType::Regular)]),
        box TestQueryProcessor,
        None,
    )
    .await?;

    // Startup
    assert_eq!(watchman_query.sync().await?, Out::FreshInstance);
    assert_eq!(watchman_query.sync().await?, Out::Files(vec![]));

    // Create a file, see that we receive it.
    let test = root.join("test");
    File::create(&test)?;
    assert_eq!(
        watchman_query.sync().await?,
        Out::Files(vec!["test".into()])
    );

    // Kill Watchman, see that we're broken now
    watchman_instance.shutdown().await?;
    assert_matches!(watchman_query.sync().await, Err(..));

    // Restart Watchman, we should be fixed now, and get a fresh instance.
    let mut watchman_instance = spawn_watchman(&watchman_dir).await?;
    assert_eq!(watchman_query.sync().await?, Out::FreshInstance);
    assert_eq!(watchman_query.sync().await?, Out::Files(vec![]));

    // Clean up
    watchman_instance.shutdown().await?;

    Ok(())
}
