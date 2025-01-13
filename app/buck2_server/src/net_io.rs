/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[derive(Copy, Clone, Debug)]
pub enum NetworkKind {
    WiFi,
    Ethernet,
    Unknown,
}

#[derive(Clone, Debug)]
pub struct NetworkStat {
    pub bytes_sent: u64,
    pub bytes_recv: u64,
    pub network_kind: NetworkKind,
}

#[cfg(any(target_os = "macos", target_os = "linux"))]
mod collector {
    use std::collections::HashMap;
    use std::sync::Arc;
    use std::sync::Mutex;

    use buck2_error::conversion::from_any_with_tag;
    use buck2_error::BuckErrorContext;
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
        pub fn collect(&self) -> buck2_error::Result<Option<HashMap<String, NetworkStat>>> {
            let mut collector = self.collector.lock().expect("poisoned lock");
            let counters: HashMap<_, _> = collector
                .net_io_counters_pernic()
                .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))
                .buck_error_context("collecting old counters")?
                .into_iter()
                .filter(|(s, _)| {
                    ["en", "eth", "wlan"]
                        .iter()
                        .any(|prefix| s.starts_with(prefix))
                })
                .map(|(nic, counters)| {
                    let network_kind = NetworkKind::from_name(&nic);
                    (
                        nic,
                        NetworkStat {
                            bytes_sent: counters.bytes_sent(),
                            bytes_recv: counters.bytes_recv(),
                            network_kind,
                        },
                    )
                })
                .collect();

            Ok(Some(counters))
        }
    }

    impl NetworkKind {
        #[cfg(target_os = "macos")]
        fn from_name(name: &str) -> NetworkKind {
            match name {
                // on macbook en0 is always WiFi
                // TODO(yurysamkevich): properly detect device type using SCNetworkInterfaceGetInterfaceType Apple API
                "en0" => NetworkKind::WiFi,
                n if n.starts_with("en") => NetworkKind::Ethernet,
                _ => NetworkKind::Unknown,
            }
        }

        #[cfg(target_os = "linux")]
        fn from_name(name: &str) -> NetworkKind {
            match name {
                n if n.starts_with("eth") => NetworkKind::Ethernet,
                n if n.starts_with("wlan") => NetworkKind::WiFi,
                _ => NetworkKind::Unknown,
            }
        }
    }
}

#[cfg(target_os = "windows")]
mod collector {
    use std::collections::HashMap;
    use std::io::Error;
    use std::slice::from_raw_parts;

    use dupe::Dupe;
    use winapi::shared::ipifcons;
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

        pub fn collect(&self) -> buck2_error::Result<Option<HashMap<String, NetworkStat>>> {
            let mut counters = HashMap::new();
            let (_guard, entries) = unsafe {
                let mut table: *mut MIB_IF_TABLE2 = std::ptr::null_mut();

                if GetIfTable2(&mut table) != NO_ERROR {
                    return Err(buck2_error::buck2_error!(
                        buck2_error::ErrorTag::Tier0,
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
                let network_kind = match entry.Type {
                    ipifcons::IF_TYPE_ETHERNET_CSMACD => NetworkKind::Ethernet,
                    ipifcons::IF_TYPE_IEEE80211 => NetworkKind::WiFi,
                    _ => NetworkKind::Unknown,
                };
                counters.insert(
                    interface_name,
                    NetworkStat {
                        bytes_sent,
                        bytes_recv,
                        network_kind,
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

        pub fn collect(&self) -> buck2_error::Result<Option<HashMap<String, NetworkStat>>> {
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
