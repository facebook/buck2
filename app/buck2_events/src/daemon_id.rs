/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use dupe::Dupe;
use once_cell::sync::Lazy;
use uuid::Uuid;

#[derive(derive_more::Display, Clone, Dupe, allocative::Allocative)]
#[display("{}", uuid.hyphenated())]
pub struct DaemonId {
    #[allocative(skip)]
    uuid: Arc<Uuid>,
}

impl DaemonId {
    pub fn new() -> Self {
        Self {
            uuid: Arc::new(Uuid::new_v4()),
        }
    }

    pub fn null() -> DaemonId {
        DaemonId {
            uuid: Arc::new(Uuid::nil()),
        }
    }
}

pub static DAEMON_UUID: Lazy<DaemonId> = Lazy::new(DaemonId::new);

#[cfg(test)]
mod tests {
    use std::str::FromStr;
    use std::sync::Arc;

    use uuid::Uuid;

    use crate::daemon_id::DaemonId;

    #[test]
    fn test_display() {
        assert_eq!(
            "5621e477-ca64-4c68-888c-c47bdba0ff77",
            DaemonId {
                uuid: Arc::new(Uuid::from_str("5621e477-ca64-4c68-888c-c47bdba0ff77").unwrap()),
            }
            .to_string()
        );
    }
}
