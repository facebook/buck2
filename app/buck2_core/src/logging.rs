/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use anyhow::Context as _;
use tracing_subscriber::filter::Filtered;
use tracing_subscriber::fmt::MakeWriter;
use tracing_subscriber::prelude::*;
use tracing_subscriber::reload;
use tracing_subscriber::reload::Handle;
use tracing_subscriber::EnvFilter;

pub trait LogConfigurationReloadHandle: Send + Sync + 'static {
    fn update_log_filter(&self, format: &str) -> anyhow::Result<()>;
}

impl dyn LogConfigurationReloadHandle {
    pub fn noop() -> Arc<dyn LogConfigurationReloadHandle> {
        Arc::new(NoopLogConfigurationReloadHandle) as _
    }
}

struct NoopLogConfigurationReloadHandle;

impl LogConfigurationReloadHandle for NoopLogConfigurationReloadHandle {
    fn update_log_filter(&self, _filter: &str) -> anyhow::Result<()> {
        Ok(())
    }
}

impl<L, R> LogConfigurationReloadHandle for Handle<Filtered<L, EnvFilter, R>, R>
where
    L: Send + Sync + 'static,
    R: Send + Sync + 'static,
{
    fn update_log_filter(&self, raw: &str) -> anyhow::Result<()> {
        let filter = EnvFilter::try_new(raw).context("Invalid log filter")?;
        self.modify(|layer| *layer.filter_mut() = filter)
            .context("Error updating log filter")?;
        tracing::debug!("Log filter was updated to: `{}`", raw);
        Ok(())
    }
}

pub fn init_tracing_for_writer<W>(
    writer: W,
) -> anyhow::Result<Arc<dyn LogConfigurationReloadHandle>>
where
    W: for<'writer> MakeWriter<'writer> + Send + Sync + 'static,
{
    // By default, show warnings/errors.
    // If the user specifies BUCK_LOG, we want to honour that.
    const ENV_VAR: &str = "BUCK_LOG";

    let filter = match std::env::var_os(ENV_VAR) {
        Some(v) => {
            let v = v
                .into_string()
                .ok()
                .with_context(|| format!("Failed to parse ${} as utf-8", ENV_VAR))?;
            EnvFilter::try_new(v)
                .with_context(|| format!("Failed to parse ${} as a filter", ENV_VAR))?
        }
        // daemon_listener is all emitted before the client starts tailing, which is why we log
        // those by default.
        None => EnvFilter::new("warn,[daemon_listener]=info"),
    };

    let layer = tracing_subscriber::fmt::layer()
        .with_writer(writer)
        .with_filter(filter);

    let (layer, handle) = reload::Layer::new(layer);

    tracing_subscriber::registry().with(layer).init();

    Ok(Arc::new(handle) as _)
}
