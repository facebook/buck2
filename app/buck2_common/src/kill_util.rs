/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

#[allow(unused_variables)]
pub async fn try_terminate_process_gracefully(pid: i32, timeout: Duration) -> anyhow::Result<()> {
    #[cfg(unix)]
    {
        unix::try_terminate_process_gracefully(pid, timeout).await
    }

    #[cfg(not(unix))]
    {
        Err(anyhow::anyhow!(
            "Graceful process termination is not implemented for non-unix target family."
        ))
    }
}

#[cfg(unix)]
mod unix {
    use std::ops::ControlFlow;
    use std::time::Duration;
    use std::time::Instant;

    use nix::sys::signal::Signal;
    use tracing::info;
    use tracing::warn;

    pub(crate) async fn try_terminate_process_gracefully(
        pid: i32,
        timeout: Duration,
    ) -> anyhow::Result<()> {
        let pid = nix::unistd::Pid::from_raw(pid);
        match send_signal_and_wait_for_shutdown(Signal::SIGTERM, pid, Some(timeout)).await {
            ControlFlow::Continue(_) => {}
            ControlFlow::Break(StoppedWaiting::Success) => {
                return Ok(());
            }
            ControlFlow::Break(StoppedWaiting::UnexpectedError(e)) => {
                return Err(e);
            }
        }
        warn!(
            "Failed to gracefully terminate process `{}`, sending SIGKILL.",
            pid,
        );
        match send_signal_and_wait_for_shutdown(Signal::SIGKILL, pid, None).await {
            ControlFlow::Continue(_) => {
                unreachable!("Not expected when there is no timeout specified.")
            }
            ControlFlow::Break(StoppedWaiting::Success) => Ok(()),
            ControlFlow::Break(StoppedWaiting::UnexpectedError(e)) => Err(e),
        }
    }

    enum StoppedWaiting {
        Success,
        UnexpectedError(anyhow::Error),
    }

    fn check_result(pid: nix::unistd::Pid, result: nix::Result<()>) -> ControlFlow<StoppedWaiting> {
        match result {
            // Signal is sent successfully, now we need to wait for a process to terminate.
            Ok(_) => ControlFlow::Continue(()),
            // There is no such process, our desired outcome.
            Err(nix::errno::Errno::ESRCH) => ControlFlow::Break(StoppedWaiting::Success),
            Err(e) => ControlFlow::Break(StoppedWaiting::UnexpectedError(anyhow::anyhow!(
                "Unexpected error while waiting for process `{}` to terminate (`{}`)",
                pid,
                e
            ))),
        }
    }

    async fn send_signal_and_wait_for_shutdown(
        signal: nix::sys::signal::Signal,
        pid: nix::unistd::Pid,
        timeout: Option<Duration>,
    ) -> ControlFlow<StoppedWaiting> {
        let start = Instant::now();
        info!("Sending signal `{}` to process `{}`.", signal, pid,);
        // Actually send a signal.
        check_result(pid, nix::sys::signal::kill(pid, signal))?;
        loop {
            if let Some(timeout) = timeout {
                if start.elapsed() > timeout {
                    break;
                }
            }
            tokio::time::sleep(Duration::from_millis(100)).await;
            // Just check the process, signal already sent.
            check_result(pid, nix::sys::signal::kill(pid, None))?;
        }
        ControlFlow::Continue(())
    }
}

#[cfg(unix)]
#[cfg(test)]
mod tests {
    use std::process::Stdio;

    use tempfile;
    use tokio::fs::File;
    use tokio::io::AsyncBufReadExt;
    use tokio::io::AsyncWriteExt;
    use tokio::io::BufReader;

    use super::*;

    #[tokio::test]
    #[cfg(any(fbcode_build, cargo_internal_build))] // TODO(@akozhevnikov): Debug why this fails on CircleCI
    async fn test_graceful_termination() -> anyhow::Result<()> {
        let dir = tempfile::tempdir()?;

        // We cannot start child script directly else it will enter a zombie state
        // when trying to kill it and termination would be stuck.
        let mut parent_file = File::create(dir.path().join("parent.sh")).await?;
        let parent_script = r#"
            sh child.sh &
            sleep 1  # wait to give an opportunity for a child script to register a trap
            echo $!
        "#;
        parent_file.write_all(parent_script.as_bytes()).await?;

        let mut child_file = File::create(dir.path().join("child.sh")).await?;
        let child_script = r#"
            sigterm()
            {
                touch gracefully_terminated
            }
            trap sigterm SIGTERM

            sleep 100 &
            wait
        "#;
        child_file.write_all(child_script.as_bytes()).await?;

        let mut command = tokio::process::Command::new("sh");
        command
            .args(["parent.sh"])
            .current_dir(dir.path())
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::null());

        let mut parent = command.spawn()?;
        parent.wait().await?;

        let pid = {
            let stdout = parent.stdout.take().unwrap();
            let mut reader = BufReader::new(stdout).lines();
            let pid_str = reader.next_line().await?;
            pid_str.unwrap().parse().unwrap()
        };

        try_terminate_process_gracefully(pid, Duration::from_secs(1)).await?;

        assert!(tokio::fs::try_exists(dir.path().join("gracefully_terminated")).await?);

        Ok(())
    }

    #[tokio::test]
    async fn test_sigkill_when_graceful_termination_fails() -> anyhow::Result<()> {
        let dir = tempfile::tempdir()?;

        // We cannot start child script directly else it will enter a zombie state
        // when trying to kill it and termination would be stuck.
        let mut parent_file = File::create(dir.path().join("parent.sh")).await?;
        let parent_script = r#"
            sh child.sh &
            sleep 1  # wait to give an opportunity for a child script to register a trap
            echo $!
        "#;
        parent_file.write_all(parent_script.as_bytes()).await?;

        let mut child_file = File::create(dir.path().join("child.sh")).await?;
        let child_script = r#"
            sigterm()
            {
                sleep 3  # block termination
                touch gracefully_terminated
            }
            trap sigterm SIGTERM

            sleep 100 &
            wait
        "#;
        child_file.write_all(child_script.as_bytes()).await?;

        let mut command = tokio::process::Command::new("sh");
        command
            .args(["parent.sh"])
            .current_dir(dir.path())
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::null());

        let mut parent = command.spawn()?;
        parent.wait().await?;

        let pid = {
            let stdout = parent.stdout.take().unwrap();
            let mut reader = BufReader::new(stdout).lines();
            let pid_str = reader.next_line().await?;
            pid_str.unwrap().parse().unwrap()
        };

        try_terminate_process_gracefully(pid, Duration::from_secs(1)).await?;

        assert!(!tokio::fs::try_exists(dir.path().join("gracefully_terminated")).await?);

        Ok(())
    }
}
