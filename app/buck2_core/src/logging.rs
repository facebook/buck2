/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context as _;
use tracing_subscriber::fmt::MakeWriter;
use tracing_subscriber::prelude::*;
use tracing_subscriber::EnvFilter;

pub fn init_tracing_for_writer<W>(writer: W) -> anyhow::Result<()>
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
        None => EnvFilter::new("warn"),
    };

    let layer = tracing_subscriber::fmt::layer()
        .with_writer(writer)
        .with_filter(filter);

    tracing_subscriber::registry().with(layer).init();

    Ok(())
}
