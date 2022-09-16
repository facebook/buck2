/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

const TRUNCATION_MSG: &str = "<<omitted>>";

/// Quick and dirty function to either print the full debug message, or
/// the debug message with the middle elided if it's too long.
/// `max_length` is maximum length of truncated message.
pub fn truncate(msg: &str, max_length: usize) -> String {
    if max_length <= TRUNCATION_MSG.len() {
        TRUNCATION_MSG.to_owned()
    } else if msg.len() > max_length {
        let max_length_without_truncation_msg = max_length.saturating_sub(TRUNCATION_MSG.len());
        // Note that for Unicode strings we might end up with less than max_length characters,
        // because these functions are all in terms of bytes.
        // Not worth the hassle to do better, given how rare that is.
        format!(
            "{}{}{}",
            &msg[0..msg.ceil_char_boundary(max_length_without_truncation_msg / 2)],
            &TRUNCATION_MSG,
            &msg[msg.floor_char_boundary(msg.len() - (max_length_without_truncation_msg / 2))
                ..msg.len()]
        )
    } else {
        msg.to_owned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const MSG: &str = "rdeps(set(fbcode//buck2/... fbsource//xplat/buck2/..., fbsource//fbobjc/buck2/...), fbcode//buck2/cli:buck2)";

    #[test]
    fn test_truncate() {
        assert_eq!(&truncate(MSG, 0), "<<omitted>>");
        assert_eq!(&truncate(MSG, TRUNCATION_MSG.len()), "<<omitted>>");
        assert_eq!(&truncate(MSG, 30), "rdeps(set<<omitted>>li:buck2)");
        assert_eq!(
            &truncate(MSG, 50),
            "rdeps(set(fbcode//b<<omitted>>e//buck2/cli:buck2)"
        );
    }

    #[test]
    fn test_truncate_unicode() {
        // This will segfault if we try and split within a char boundary
        truncate(&"❤️🧡💛💚💙💜".repeat(500), 100);
        truncate(&"東京都".repeat(500), 100);
    }
}
