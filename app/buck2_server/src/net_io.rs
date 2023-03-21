/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[derive(Clone, Debug)]
pub struct Counters {
    pub bytes_sent: u64,
    pub bytes_recv: u64,
}

#[cfg(unix)]
mod collector {
    use std::collections::HashMap;
    use std::sync::Arc;
    use std::sync::Mutex;

    use anyhow::Context;
    use dupe::Dupe;
    use psutil::network::NetIoCountersCollector;

    use super::*;

    /// Collects system network I/O stats (tx, rx) on a per-NIC basis. Returns delta
    /// counters in between collection periods. Most useful when done on a periodic
    /// basis (e.g. in the daemon heartbeat process, which happens every 1s).
    #[derive(Clone, Debug, Dupe)]
    pub struct SystemNetworkIoCollector {
        collector: Arc<Mutex<NetIoCountersCollector>>,
    }

    impl SystemNetworkIoCollector {
        pub fn new() -> Self {
            Self {
                collector: Arc::new(Mutex::new(NetIoCountersCollector::default())),
            }
        }

        /// Collect stats for each NIC. Some notes:
        /// * If a new NIC appears between collection periods, we'll start keeping
        ///   track of it.
        /// * If a NIC *disappears*, then we stop reporting on its stats.
        pub fn collect(&self) -> anyhow::Result<Option<HashMap<String, Counters>>> {
            let mut collector = self.collector.lock().expect("poisoned lock");
            let counters: HashMap<_, _> = collector
                .net_io_counters_pernic()
                .context("collecting old counters")?
                .into_iter()
                .map(|(nic, counters)| {
                    (
                        nic,
                        Counters {
                            bytes_sent: counters.bytes_sent(),
                            bytes_recv: counters.bytes_recv(),
                        },
                    )
                })
                .collect();

            Ok(Some(counters))
        }
    }
}

// psutil network stats aren't implemented on windows.
#[cfg(not(unix))]
mod collector {
    use std::collections::HashMap;

    use dupe::Dupe;

    use super::*;

    #[derive(Clone, Debug, Dupe)]
    pub struct SystemNetworkIoCollector;

    impl SystemNetworkIoCollector {
        pub fn new() -> Self {
            Self
        }

        pub fn collect(&self) -> anyhow::Result<Option<HashMap<String, Counters>>> {
            Ok(None)
        }
    }
}

pub use collector::SystemNetworkIoCollector;
