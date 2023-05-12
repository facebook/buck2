/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

const TRUNCATION_MSG: &str = "<<omitted>>";
const TRUNCATION_DELIM: &str = ", ";

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

/// Truncate a container of strings and return the comma-separated string representation. The end of
/// the collection is elided if the length is too long.
/// `max_length` is maximum length of truncated message excluding the truncation placeholder message.
pub fn truncate_container<'v, Iter: IntoIterator<Item = &'v str>>(
    iter: Iter,
    max_length: usize,
) -> String {
    let mut items = iter.into_iter();
    let mut result = String::new();

    match items.next() {
        None => (),
        Some(first) => {
            if first.len() > max_length {
                result.push_str(&first[0..first.floor_char_boundary(max_length)]);
                result.push_str(TRUNCATION_MSG);
                return result;
            } else {
                result.push_str(first);

                for v in items {
                    if result.len() + TRUNCATION_DELIM.len() + v.len() > max_length {
                        result.push_str(
                            &TRUNCATION_DELIM[0..std::cmp::min(
                                max_length.saturating_sub(result.len()),
                                TRUNCATION_DELIM.len(),
                            )],
                        );
                        result.push_str(&v[0..v.floor_char_boundary(max_length - result.len())]);
                        result.push_str(TRUNCATION_MSG);
                        return result;
                    }

                    result.push_str(TRUNCATION_DELIM);
                    result.push_str(v);
                }
            }
        }
    }

    result
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
    fn test_truncate_container() {
        let mut vec = Vec::new();
        for n in 1..50 {
            vec.push(n.to_string())
        }
        assert_eq!(
            &truncate_container(vec.iter().map(|e| &**e), 0),
            "<<omitted>>"
        );
        assert_eq!(
            &truncate_container(vec.iter().map(|e| &**e), 60),
            "1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 1<<omitted>>"
        );

        let vec = vec!["123", "456"];

        assert_eq!(
            &truncate_container(vec.iter().copied(), 6),
            "123, 4<<omitted>>"
        );

        let vec = vec!["1234567890", "456"];

        assert_eq!(
            &truncate_container(vec.iter().copied(), 6),
            "123456<<omitted>>"
        );

        let vec = vec!["❤️🧡💛💚💙💜", "東京都"];

        assert_eq!(
            &truncate_container(vec.iter().copied(), 35),
            "❤\u{fe0f}🧡💛💚💙💜, 東京<<omitted>>"
        );
    }

    #[test]
    fn test_truncate_unicode() {
        // This will segfault if we try and split within a char boundary
        truncate(&"❤️🧡💛💚💙💜".repeat(500), 100);
        truncate(&"東京都".repeat(500), 100);
    }
}
