/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

/// Set of ASCII characters.
pub(crate) struct AsciiCharSet {
    mask: [bool; 0x100],
}

impl AsciiCharSet {
    /// Construct a set.
    pub(crate) const fn new(chars: &str) -> AsciiCharSet {
        let mut mask = [false; 0x100];
        let mut i = 0;
        while i != chars.as_bytes().len() {
            let b = chars.as_bytes()[i];
            assert!(b & 0x80 == 0, "non-ASCII char in set");

            mask[b as usize] = true;
            i += 1;
        }
        AsciiCharSet { mask }
    }

    pub(crate) fn contains(&self, b: u8) -> bool {
        self.mask[b as usize]
    }
}

#[cfg(test)]
mod tests {
    use crate::ascii_char_set::AsciiCharSet;

    #[test]
    fn test_ascii_char_set() {
        let set = AsciiCharSet::new("abcdXYZ");

        assert!(set.contains(b'a'));
        assert!(set.contains(b'd'));
        assert!(set.contains(b'X'));
        assert!(set.contains(b'Z'));
        assert!(!set.contains(b'\n'));
        assert!(!set.contains(b'@'));
        assert!(!set.contains(0x80));
        assert!(!set.contains(0xff));
    }
}
