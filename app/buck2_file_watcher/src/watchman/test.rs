/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fs;
use std::fs::File;
use std::path::Path;
use std::path::PathBuf;
use std::time::Duration;

use assert_matches::assert_matches;
use async_trait::async_trait;
use buck2_error::BuckErrorContext;
use buck2_error::buck2_error;
use buck2_error::internal_error;
use buck2_util::process::async_background_command;
use gazebo::prelude::*;
use tokio::io::AsyncWriteExt;
use tokio::process::Child;
use watchman_client::expr::Expr;
use watchman_client::prelude::Connector;
use watchman_client::prelude::FileType;

use crate::watchman::core::SyncableQuery;
use crate::watchman::core::SyncableQueryProcessor;
use crate::watchman::core::WatchmanEvent;

struct TestQueryProcessor;

#[derive(PartialEq, Eq, Debug)]
enum Out {
    FreshInstance(Vec<String>),
    Files(Vec<String>),
}

#[async_trait]
impl SyncableQueryProcessor for TestQueryProcessor {
    type Output = Out;
    type Payload = ();

    async fn process_events(
        &mut self,
        payload: Self::Payload,
        events: Vec<WatchmanEvent>,
        _mergebase: &Option<String>,
        _watchman_version: Option<String>,
    ) -> buck2_error::Result<(Self::Output, Self::Payload)> {
        Ok((
            Out::Files(events.into_map(|e| e.path.display().to_string())),
            payload,
        ))
    }

    async fn on_fresh_instance(
        &mut self,
        payload: Self::Payload,
        events: Vec<WatchmanEvent>,
        _mergebase: &Option<String>,
        _watchman_version: Option<String>,
    ) -> buck2_error::Result<(Self::Output, Self::Payload)> {
        Ok((
            Out::FreshInstance(events.into_map(|e| e.path.display().to_string())),
            payload,
        ))
    }
}

async fn wait_for_watchman(watchman_sock: &Path) -> buck2_error::Result<()> {
    let connector = Connector::default().unix_domain_socket(watchman_sock);

    let mut i = 0;
    loop {
        i += 1;

        match connector.connect().await {
            Ok(..) => return Ok(()),
            Err(e) if i >= 100 => return Err(buck2_error!(buck2_error::ErrorTag::Tier0, "{}", e)),
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
    pub async fn shutdown(&mut self) -> buck2_error::Result<()> {
        let child = self
            .child
            .as_mut()
            .ok_or_else(|| internal_error!("Watchman was already shutdown"))?;

        child
            .kill()
            .await
            .buck_error_context("Failed to kill Watchman")?;

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
        let log = std::fs::read_to_string(&self.log).with_buck_error_context(|| {
            format!("Failed to read log file at {}", self.log.display())
        });
        match log {
            Ok(log) => eprintln!("Watchman logs follow\n{log}"),
            Err(e) => eprintln!("Failed to read logs: {e:#}"),
        };

        // Try to see if Watchman had exited or not.
        match child.try_wait() {
            Ok(Some(status)) => eprintln!("Watchman had exited with status {status:?}"),
            Ok(None) => eprintln!("Watchan is still running"),
            Err(e) => eprintln!("Failed to access Watchman status: {e:#}"),
        }
    }
}

async fn spawn_watchman(watchman_dir: &Path) -> buck2_error::Result<WatchmanInstance> {
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

    let watchman = async_background_command("watchman")
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
        .buck_error_context("Waiting for Watchman to start")?;

    Ok(WatchmanInstance {
        child: Some(watchman),
        sock: watchman_sock,
        log: watchman_log,
    })
}

#[tokio::test]
async fn test_syncable_query() -> buck2_error::Result<()> {
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
        Box::new(TestQueryProcessor),
        None,
        true,
    )?;

    // Startup
    assert_eq!(watchman_query.sync(()).await?.0, Out::FreshInstance(vec![]));
    assert_eq!(watchman_query.sync(()).await?.0, Out::Files(vec![]));

    // Create a file, see that we receive it.
    let test = root.join("test");
    File::create(&test)?;
    assert_eq!(
        watchman_query.sync(()).await?.0,
        Out::Files(vec!["test".into()])
    );

    // Kill Watchman, see that we're broken now
    watchman_instance.shutdown().await?;
    assert_matches!(watchman_query.sync(()).await, Err(..));

    // Restart Watchman, we should be fixed now, and get a fresh instance.
    let mut watchman_instance = spawn_watchman(&watchman_dir).await?;
    assert_eq!(watchman_query.sync(()).await?.0, Out::FreshInstance(vec![]));
    assert_eq!(watchman_query.sync(()).await?.0, Out::Files(vec![]));

    // Clean up
    watchman_instance.shutdown().await?;

    Ok(())
}
