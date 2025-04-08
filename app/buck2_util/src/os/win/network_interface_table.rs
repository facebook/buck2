/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Error;
use std::slice::from_raw_parts;

use winapi::shared::netioapi::FreeMibTable;
use winapi::shared::netioapi::GetIfTable2;
use winapi::shared::netioapi::MIB_IF_ROW2;
use winapi::shared::netioapi::MIB_IF_TABLE2;
use winapi::shared::winerror::NO_ERROR;

use crate::os::win::network_interface::NetworkInterface;

struct TableGuard {
    table: *mut MIB_IF_TABLE2,
}

impl Drop for TableGuard {
    fn drop(&mut self) {
        unsafe { FreeMibTable(self.table as *mut _) }
    }
}

/// Table of network interfaces. This is a wrapper around the Windows MIB_IF_TABLE2 struct.
pub struct NetworkInterfaceTable<'a> {
    _guard: TableGuard,
    entries_iter: std::slice::Iter<'a, MIB_IF_ROW2>,
}

impl NetworkInterfaceTable<'_> {
    pub fn new() -> buck2_error::Result<Self> {
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
        Ok(Self {
            _guard,
            entries_iter: entries.iter(),
        })
    }
}

impl Iterator for NetworkInterfaceTable<'_> {
    type Item = NetworkInterface;

    fn next(&mut self) -> Option<Self::Item> {
        self.entries_iter
            .next()
            .map(|entry| NetworkInterface::new(*entry))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_network_interface_table_creation() {
        // This test checks if the NetworkInterfaceTable can be created without crashing.
        let result = NetworkInterfaceTable::new();
        assert!(
            result.is_ok(),
            "Failed to create NetworkInterfaceTable: {:?}",
            result.err()
        );
        assert!(
            result
                .unwrap()
                .next()
                .is_some_and(|i| !i.description().is_empty())
        );
    }
}
