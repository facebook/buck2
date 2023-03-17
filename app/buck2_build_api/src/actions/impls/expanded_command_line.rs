/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use sorted_vector_map::SortedVectorMap;

/// A command line's expansion, suitable to actually run it.
pub struct ExpandedCommandLine {
    pub cli: Vec<String>,
    pub env: SortedVectorMap<String, String>,
}

/// The digest of an ExpandedCommandLine.
#[derive(Eq, PartialEq, Debug, Allocative)]
pub struct ExpandedCommandLineDigest(
    // This is OK to skip because hash is stored inline.
    #[allocative(skip)] blake3::Hash,
);

impl ExpandedCommandLine {
    /// Obtain a hash of this command line. Conceptually this is as if we serialized the command
    /// line to a length-prefixed list then hashed it, except we never actually produce the
    /// serialized representation.
    pub fn fingerprint(&self) -> ExpandedCommandLineDigest {
        let mut digest = blake3::Hasher::new();

        digest.update(self.cli.len().to_le_bytes().as_slice());
        for e in self.cli.iter() {
            let bytes = e.as_bytes();
            digest.update(bytes.len().to_le_bytes().as_slice());
            digest.update(bytes);
        }

        digest.update(self.env.len().to_le_bytes().as_slice());
        for (k, v) in self.env.iter() {
            let k_bytes = k.as_bytes();
            digest.update(k_bytes.len().to_le_bytes().as_slice());
            digest.update(k_bytes);

            let v_bytes = v.as_bytes();
            digest.update(v_bytes.len().to_le_bytes().as_slice());
            digest.update(v_bytes);
        }

        ExpandedCommandLineDigest(digest.finalize())
    }
}

#[cfg(test)]
mod test {
    use sorted_vector_map::sorted_vector_map;
    use sorted_vector_map::SortedVectorMap;

    use super::*;

    #[test]
    fn test_cli() {
        let cmd1 = ExpandedCommandLine {
            cli: vec!["foo".to_owned(), "bar".to_owned()],
            env: Default::default(),
        };

        let cmd2 = ExpandedCommandLine {
            cli: vec!["foob".to_owned(), "ar".to_owned()],
            env: Default::default(),
        };

        let cmd3 = ExpandedCommandLine {
            cli: vec!["foobar".to_owned()],
            env: Default::default(),
        };

        assert_ne!(cmd1.fingerprint(), cmd2.fingerprint());
        assert_ne!(cmd1.fingerprint(), cmd3.fingerprint());
        assert_ne!(cmd2.fingerprint(), cmd3.fingerprint());
    }

    #[test]
    fn test_env() {
        let cmd1 = ExpandedCommandLine {
            cli: Default::default(),
            env: sorted_vector_map! { "FOO".to_owned() => "BAR".to_owned() },
        };

        let cmd2 = ExpandedCommandLine {
            cli: Default::default(),
            env: sorted_vector_map! { "FOO2".to_owned()=> "BAR".to_owned() },
        };

        let cmd3 = ExpandedCommandLine {
            cli: Default::default(),
            env: sorted_vector_map! { "FOO".to_owned()=> "BAR2".to_owned() },
        };

        assert_ne!(cmd1.fingerprint(), cmd2.fingerprint());
        assert_ne!(cmd1.fingerprint(), cmd3.fingerprint());
        assert_ne!(cmd2.fingerprint(), cmd3.fingerprint());
    }

    #[test]
    fn test_hash_stability() {
        fn env() -> SortedVectorMap<String, String> {
            sorted_vector_map! { "FOO1".to_owned() => "BAR1".to_owned(), "FOO2".to_owned() => "BAR2".to_owned() }
        }

        let mut cmd = ExpandedCommandLine {
            cli: vec!["cmd".to_owned()],
            env: env(),
        };

        let digest = cmd.fingerprint();

        for _ in 0..10 {
            cmd.env = env();
            assert_eq!(cmd.fingerprint(), digest);
        }
    }
}
