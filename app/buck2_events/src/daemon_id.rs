/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use once_cell::sync::Lazy;
use uuid::Uuid;

#[derive(derive_more::Display)]
#[display(fmt = "{}", uuid.hyphenated())]
pub struct DaemonId {
    uuid: Uuid,
}

pub static DAEMON_UUID: Lazy<DaemonId> = Lazy::new(|| DaemonId {
    uuid: Uuid::new_v4(),
});

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use uuid::Uuid;

    use crate::daemon_id::DaemonId;

    #[test]
    fn test_display() {
        assert_eq!(
            "5621e477-ca64-4c68-888c-c47bdba0ff77",
            DaemonId {
                uuid: Uuid::from_str("5621e477-ca64-4c68-888c-c47bdba0ff77").unwrap()
            }
            .to_string()
        );
    }
}
