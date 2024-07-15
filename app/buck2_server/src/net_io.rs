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

#[cfg(any(target_os = "macos", target_os = "linux"))]
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

#[cfg(target_os = "windows")]
mod collector {
    use std::collections::HashMap;
    use std::io::Error;
    use std::slice::from_raw_parts;

    use dupe::Dupe;
    use winapi::shared::netioapi::FreeMibTable;
    use winapi::shared::netioapi::GetIfTable2;
    use winapi::shared::netioapi::MIB_IF_TABLE2;
    use winapi::shared::ntdef::FALSE;
    use winapi::shared::winerror::NO_ERROR;

    use super::*;

    #[derive(Clone, Debug, Dupe)]
    pub struct SystemNetworkIoCollector;

    struct TableGuard {
        table: *mut MIB_IF_TABLE2,
    }

    impl Drop for TableGuard {
        fn drop(&mut self) {
            unsafe { FreeMibTable(self.table as *mut _) }
        }
    }

    impl SystemNetworkIoCollector {
        pub fn new() -> Self {
            Self
        }

        pub fn collect(&self) -> anyhow::Result<Option<HashMap<String, Counters>>> {
            let mut counters = HashMap::new();
            let (_guard, entries) = unsafe {
                let mut table: *mut MIB_IF_TABLE2 = std::ptr::null_mut();

                if GetIfTable2(&mut table) != NO_ERROR {
                    return Err(anyhow::anyhow!(
                        "Failed to retrieve MIB-II interface table: {}",
                        Error::last_os_error()
                    ));
                };

                // Ensure the table gets freed
                let _guard = TableGuard { table };

                let num_entries = (*table).NumEntries;
                let table_ptr = (*table).Table.as_ptr();
                (_guard, from_raw_parts(table_ptr, num_entries as usize))
            };

            for entry in entries {
                if entry.InterfaceAndOperStatusFlags.HardwareInterface() == FALSE {
                    continue;
                }

                let name_len = entry
                    .Alias
                    .iter()
                    .position(|c| *c == 0)
                    .unwrap_or(entry.Alias.len());
                let interface_name = String::from_utf16(&entry.Alias[..name_len])
                    .unwrap_or_else(|_| String::from("<Unknown>"));
                let bytes_sent = entry.OutOctets;
                let bytes_recv = entry.InOctets;
                counters.insert(
                    interface_name,
                    Counters {
                        bytes_sent,
                        bytes_recv,
                    },
                );
            }
            Ok(Some(counters))
        }
    }
}

// psutil network stats aren't implemented other unix-likes.
#[cfg(not(any(target_os = "macos", target_os = "linux", target_os = "windows")))]
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

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_network_collector() {
        let collector = SystemNetworkIoCollector::new();
        let stat = collector.collect().unwrap().unwrap();
        assert!(!stat.is_empty());

        let (recv, sent) = stat.iter().fold((0, 0), |acc, x| {
            (acc.0 + x.1.bytes_recv, acc.0 + x.1.bytes_sent)
        });
        assert!(recv > 0);
        assert!(sent > 0);
    }
}
