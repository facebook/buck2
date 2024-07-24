/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

macro_rules! impl_fingerprinted_directory {
    (
        $this: ident
    ) => {
        impl<L, H> $crate::directory::fingerprinted_directory::FingerprintedDirectory<L, H> for $this<L, H>
        where
            H: DirectoryDigest,
        {
            fn fingerprinted_entries<'a>(&'a self) -> crate::directory::fingerprinted_directory::FingerprintedDirectoryEntries<'a, L, H> {
                let it = self.entries().into_iter().map(|(k, v)| {
                    let k = k.as_ref();
                    let v = v
                        .as_ref()
                        .map_dir(|v| v as &dyn $crate::directory::fingerprinted_directory::FingerprintedDirectory<L, H>);
                    (k, v)
                });
                Box::new(it)
            }

            fn fingerprint(&self) -> &H {
                $this::fingerprint(self)
            }
        }

        impl<L, H> PartialEq for $this<L, H>
        where
            H: DirectoryDigest,
        {
            fn eq(&self, other: &Self) -> bool {
                self.fingerprint() == other.fingerprint()
            }
        }

        impl<L, H> Eq for $this<L, H> where H: DirectoryDigest {}
    };
}

pub(super) use impl_fingerprinted_directory;
