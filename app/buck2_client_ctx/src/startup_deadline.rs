/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp;
use std::future::Future;
use std::time::Duration;
use std::time::Instant;

use anyhow::Context;
use buck2_common::client_utils::retrying;

/// Utility to time out properly with context during buck2 client startup.
///
/// The problem is this. Consider three stacked operations: `A -> B -> C`.
/// And we need to apply timeout to the whole operation.
/// If we apply timeout to `A`, we time out properly, but won't be able to provide
/// information that failure happened during `B -> C`.
///
/// So we should timeout in each of `A`, `B`, `C` separately.
/// Additionally, when going down, we should decrease timeout a bit,
/// to make sure inner operation times out before outer one.
#[derive(Debug)]
pub struct StartupDeadline {
    deadline: Instant,
}

impl StartupDeadline {
    pub fn duration_from_now(duration: Duration) -> anyhow::Result<StartupDeadline> {
        Ok(StartupDeadline {
            deadline: Instant::now().checked_add(duration).context("overflow")?,
        })
    }

    /// Deadline for a nested operation.
    ///
    /// Must be lower than outer deadline to make sure inner operation times out before outer one.
    fn down_deadline(&self) -> anyhow::Result<StartupDeadline> {
        let new_deadline = self
            .deadline
            .checked_sub(Duration::from_millis(100))
            .context("deadline underflow")?;
        Ok(StartupDeadline {
            deadline: new_deadline,
        })
    }

    /// How much time is left for the current operation.
    pub(crate) fn rem_duration(&self, op: &str) -> anyhow::Result<Duration> {
        self.deadline
            .checked_duration_since(Instant::now())
            .with_context(|| format!("timed out before {}", op))
    }

    pub(crate) async fn down<R, Fut, F>(&self, op: &str, f: F) -> anyhow::Result<R>
    where
        F: FnOnce(StartupDeadline) -> Fut,
        Fut: Future<Output = anyhow::Result<R>>,
    {
        let rem_duration = self.rem_duration(op)?;
        tokio::time::timeout_at(
            tokio::time::Instant::from_std(self.deadline),
            f(self.down_deadline()?),
        )
        .await
        .with_context(|| format!("{} timed out after {:.3}s", op, rem_duration.as_secs_f32()))?
        .with_context(|| op.to_owned())
    }

    pub(crate) async fn run<R, Fut>(&self, op: &str, f: Fut) -> anyhow::Result<R>
    where
        Fut: Future<Output = anyhow::Result<R>>,
    {
        self.down(op, |_| f).await
    }

    /// Round current deadline down to the given duration added to the current time.
    pub(crate) fn min(&self, other: Duration) -> anyhow::Result<StartupDeadline> {
        let other = Instant::now().checked_add(other).context("overflow")?;
        Ok(StartupDeadline {
            deadline: cmp::min(self.deadline, other),
        })
    }

    /// Create a deadline object for the half of remaining time.
    pub(crate) fn half(&self) -> anyhow::Result<StartupDeadline> {
        let now = Instant::now();
        let duration = self.deadline.duration_since(now) / 2;
        let deadline = now.checked_add(duration).context("duration overflow")?;
        Ok(StartupDeadline { deadline })
    }

    pub(crate) async fn retrying<R, F, Fut>(
        &self,
        op: &str,
        initial_delay: Duration,
        max_delay: Duration,
        f: F,
    ) -> anyhow::Result<R>
    where
        Fut: Future<Output = anyhow::Result<R>>,
        F: Fn() -> Fut,
    {
        retrying(initial_delay, max_delay, self.rem_duration(op)?, f)
            .await
            .with_context(|| op.to_owned())
    }
}
