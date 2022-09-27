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
pub(crate) struct HumanizedBytes(u64);

impl HumanizedBytes {
    pub(crate) fn new(bytes: u64) -> Self {
        HumanizedBytes(bytes)
    }
}

pub(crate) struct HumanizedBytesPerSecond(u64);

impl HumanizedBytesPerSecond {
    pub(crate) fn new(bytes_per_second: u64) -> Self {
        HumanizedBytesPerSecond(bytes_per_second)
    }
}

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

        if self.0 < 1024 || f64::round(val) >= 100.0 {
            write!(f, "{:.0} {}", val, label)
        } else {
            write!(f, "{:.1} {}", val, label)
        }
    }
}

impl fmt::Display for HumanizedBytesPerSecond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/s", HumanizedBytes(self.0))
    }
}

#[cfg(test)]
mod test {
    use crate::subscribers::humanized_bytes::HumanizedBytes;
    use crate::subscribers::humanized_bytes::HumanizedBytesPerSecond;

    #[allow(clippy::identity_op)]
    #[test]
    fn test_humanized() {
        assert_eq!(HumanizedBytes::new(0).to_string(), "0 B");
        assert_eq!(HumanizedBytes::new(1).to_string(), "1 B");
        assert_eq!(HumanizedBytes::new(10).to_string(), "10 B");
        assert_eq!(HumanizedBytes::new(345).to_string(), "345 B");
        assert_eq!(HumanizedBytes::new(1023).to_string(), "1023 B");
        assert_eq!(HumanizedBytes::new(1024).to_string(), "1.0 KiB");
        assert_eq!(HumanizedBytes::new(1536).to_string(), "1.5 KiB");
        assert_eq!(HumanizedBytes::new(10 * 1024 - 1).to_string(), "10.0 KiB");
        assert_eq!(HumanizedBytes::new(10 * 1024 + 0).to_string(), "10.0 KiB");
        assert_eq!(HumanizedBytes::new(654 * 1024 + 0).to_string(), "654 KiB");
        assert_eq!(HumanizedBytes::new(1024 * 1024 - 1).to_string(), "1024 KiB");
        assert_eq!(HumanizedBytes::new(1024 * 1024 + 0).to_string(), "1.0 MiB");
        assert_eq!(
            HumanizedBytes::new(1024 * 1024 * 1024 - 1).to_string(),
            "1024 MiB"
        );
        assert_eq!(
            HumanizedBytes::new(2034 * 1024 * 1024 + 0).to_string(),
            "2.0 GiB"
        );
        assert_eq!(
            HumanizedBytes::new(100500 * 1024 * 1024 * 1024).to_string(),
            "100500 GiB"
        );
    }

    #[test]
    fn test_humanized_bytes_per_second() {
        assert_eq!(HumanizedBytesPerSecond::new(0).to_string(), "0 B/s");
        assert_eq!(
            HumanizedBytesPerSecond::new(1024 * 1024).to_string(),
            "1.0 MiB/s"
        );
    }
}
