/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

/// Write out a u64 as something more readable
pub(crate) struct HumanizedBytes(pub(crate) u64);

impl fmt::Display for HumanizedBytes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut val = self.0 as f64;
        let mut label = "B";

        let factor = 1024.0;

        for next_label in &["KiB", "MiB", "GiB"] {
            if val < factor {
                break;
            }

            val /= factor;
            label = next_label;
        }

        write!(f, "{:.1} {}", val, label)
    }
}

#[cfg(test)]
mod test {
    use crate::subscribers::humanized_bytes::HumanizedBytes;

    #[test]
    fn test_humanized() {
        assert_eq!(HumanizedBytes(10).to_string(), "10.0 B");
        assert_eq!(HumanizedBytes(1536).to_string(), "1.5 KiB");
        assert_eq!(HumanizedBytes(1048575).to_string(), "1024.0 KiB");
        assert_eq!(HumanizedBytes(1048576).to_string(), "1.0 MiB");
        assert_eq!(HumanizedBytes(2168958484).to_string(), "2.0 GiB");
    }
}
