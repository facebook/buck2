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
pub(crate) struct HumanizedBytes {
    bytes: u64,
    fixed_width: bool,
}

impl HumanizedBytes {
    pub(crate) fn new(bytes: u64) -> Self {
        HumanizedBytes {
            bytes,
            fixed_width: false,
        }
    }

    pub(crate) const FIXED_WIDTH_WIDTH: usize = 8;

    pub(crate) fn fixed_width(bytes: u64) -> Self {
        HumanizedBytes {
            bytes,
            fixed_width: true,
        }
    }
}

pub(crate) struct HumanizedBytesPerSecond {
    bytes_per_second: u64,
    fixed_width: bool,
}

impl HumanizedBytesPerSecond {
    pub(crate) fn _new(bytes_per_second: u64) -> Self {
        HumanizedBytesPerSecond {
            bytes_per_second,
            fixed_width: false,
        }
    }

    pub(crate) const FIXED_WIDTH_WIDTH: usize = HumanizedBytes::FIXED_WIDTH_WIDTH + "/s".len();

    pub(crate) fn fixed_width(bytes_per_second: u64) -> Self {
        HumanizedBytesPerSecond {
            bytes_per_second,
            fixed_width: true,
        }
    }
}

struct Preformat {
    val: f64,
    label: &'static str,
    point: bool,
}

fn preformat(value: u64, one_label: &'static str, labels: &[&'static str]) -> Preformat {
    let mut val = value as f64;
    let mut label = one_label;

    let factor = 1024.0;

    for next_label in labels {
        if val < factor {
            break;
        }

        val /= factor;
        label = next_label;
    }

    Preformat {
        val,
        label,
        point: value >= 1024 && f64::round(val) < 100.0,
    }
}

impl fmt::Display for HumanizedBytes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Preformat { val, label, point } = preformat(self.bytes, "B", &["KiB", "MiB", "GiB"]);

        match (point, self.fixed_width) {
            (false, false) => write!(f, "{:.0} {}", val, label),
            (true, false) => write!(f, "{:.1} {}", val, label),
            (false, true) => write!(f, "{:>4.0} {:<3}", val, label),
            (true, true) => write!(f, "{:>4.1} {:<3}", val, label),
        }
    }
}

impl fmt::Display for HumanizedBytesPerSecond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Preformat { val, label, point } =
            preformat(self.bytes_per_second, "B/s", &["KiB/s", "MiB/s", "GiB/s"]);

        match (point, self.fixed_width) {
            (false, false) => write!(f, "{:.0} {}", val, label),
            (true, false) => write!(f, "{:.1} {}", val, label),
            (false, true) => write!(f, "{:>4.0} {:<5}", val, label),
            (true, true) => write!(f, "{:>4.1} {:<5}", val, label),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::subscribers::humanized_bytes::HumanizedBytes;
    use crate::subscribers::humanized_bytes::HumanizedBytesPerSecond;

    #[allow(clippy::identity_op)]
    #[test]
    fn test_humanized() {
        fn t(value: u64, expected: &str, expected_fixed_width: &str) {
            assert_eq!(
                HumanizedBytes {
                    bytes: value,
                    fixed_width: false
                }
                .to_string(),
                expected
            );
            if value <= (1 << 9) {
                assert_eq!(
                    HumanizedBytes::FIXED_WIDTH_WIDTH,
                    expected_fixed_width.len()
                );
            }
            assert_eq!(
                HumanizedBytes {
                    bytes: value,
                    fixed_width: true,
                }
                .to_string(),
                expected_fixed_width
            );
        }

        t(0, "0 B", "   0 B  ");
        t(1, "1 B", "   1 B  ");
        t(10, "10 B", "  10 B  ");
        t(345, "345 B", " 345 B  ");
        t(1023, "1023 B", "1023 B  ");
        t(1024, "1.0 KiB", " 1.0 KiB");
        t(1536, "1.5 KiB", " 1.5 KiB");
        t(10 * 1024 - 1, "10.0 KiB", "10.0 KiB");
        t(10 * 1024 + 0, "10.0 KiB", "10.0 KiB");
        t(654 * 1024 + 0, "654 KiB", " 654 KiB");
        t(1024 * 1024 - 1, "1024 KiB", "1024 KiB");
        t(1024 * 1024 + 0, "1.0 MiB", " 1.0 MiB");
        t(1024 * 1024 * 1024 - 1, "1024 MiB", "1024 MiB");
        t(2034 * 1024 * 1024 + 0, "2.0 GiB", " 2.0 GiB");
        t(100500 * 1024 * 1024 * 1024, "100500 GiB", "100500 GiB");
    }

    #[test]
    fn test_humanized_bytes_per_second() {
        fn t(value: u64, expected: &str, expected_fixed_width: &str) {
            assert_eq!(
                HumanizedBytesPerSecond {
                    bytes_per_second: value,
                    fixed_width: false
                }
                .to_string(),
                expected
            );
            if value <= (1 << 9) {
                assert_eq!(
                    HumanizedBytesPerSecond::FIXED_WIDTH_WIDTH,
                    expected_fixed_width.len()
                );
            }
            assert_eq!(
                HumanizedBytesPerSecond {
                    bytes_per_second: value,
                    fixed_width: true,
                }
                .to_string(),
                expected_fixed_width
            );
        }

        t(0, "0 B/s", "   0 B/s  ");
        t(22, "22 B/s", "  22 B/s  ");
        t(1024 * 1024, "1.0 MiB/s", " 1.0 MiB/s");
    }
}
