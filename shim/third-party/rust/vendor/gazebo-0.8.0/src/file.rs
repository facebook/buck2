/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! File/IO operations.

use std::fs::create_dir_all;
use std::fs::write;
use std::io;
use std::path::Path;

/// A simple api for creating all the directories up to a path for a file, and
/// then writing the contents to that file.
pub fn create_dirs_and_write<P: AsRef<Path>, C: AsRef<[u8]>>(
    path: P,
    contents: C,
) -> io::Result<()> {
    // no parent means no directory component, and we can directly write to that
    // file
    path.as_ref().parent().map_or(Ok(()), create_dir_all)?;

    write(path.as_ref(), contents)
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;
    use std::time::SystemTime;

    use super::*;

    // We'd rather use something like tempdir, but keep the dependencies down
    struct RemoveDir(PathBuf);

    impl Drop for RemoveDir {
        fn drop(&mut self) {
            // Delete the path, ignore errors
            let _ignore = fs::remove_dir_all(&self.0);
        }
    }

    fn random_part() -> String {
        match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
            Ok(x) => x.as_nanos().to_string(),
            Err(_) => "no_random_part".to_owned(),
        }
    }

    #[test]
    fn test_create_all_and_write() -> io::Result<()> {
        // Make sure we create the directory with unique entropy in it (so no clashes)
        // and clean up after ourselves (so we don't leak files)
        let temp = std::env::temp_dir();
        let temp = temp.join("gazebo");
        let temp = temp.join(random_part());
        // We don't delete $TMP/gazebo because someone else might be using that
        let remove_dir = RemoveDir(temp.clone());

        let temp = temp.join("foo/bar");
        create_dirs_and_write(&temp, "contents")?;
        let contents = fs::read_to_string(temp)?;
        assert_eq!(contents, "contents");

        drop(remove_dir);
        Ok(())
    }
}
