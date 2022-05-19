/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//!
//! The paths module for buck2.
//!
//! Introduces 'ForwardRelativePath', 'ForwardRelativePathBuf', 'AbsPath', and
//! 'AbsPathBuf', which are equivalents of 'Path' and 'PathBuf'.
//!
//! ForwardRelativePaths are fully normalized relative platform agnostic paths
//! that only points forward. This means  that there is no `.` or `..` in this
//! path, and does not begin with `/`. These are resolved to the 'PathBuf' by
//! resolving them against an 'AbsPath'.
//!
//! 'AbsPath' are absolute paths, meaning they must start with a directory root
//! of either `/` or some  windows root directory like `c:`. These behave
//! roughly like 'Path'.
//!

mod abs_path;
mod cmp_impls;
mod file_name;
pub(crate) mod fmt;
mod forward_rel_path;
mod into_filename_buf_iterator;
// non public internal references

pub use abs_path::*;
pub use file_name::*;
pub use forward_rel_path::*;
pub use into_filename_buf_iterator::*;
/// 'RelativePath' and 'RelativePathBuf' types are OS platform agnostic paths
/// that always have `/` separators.
pub use relative_path::{RelativePath, RelativePathBuf};

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::fs::{
        paths::{AbsPath, AbsPathBuf, ForwardRelativePath, ForwardRelativePathBuf},
        project::ProjectRelativePath,
    };

    #[test]
    fn wrapped_paths_work_in_maps() -> anyhow::Result<()> {
        let mut map = HashMap::new();

        let p1 = ForwardRelativePath::new("foo")?;
        let p2 = ProjectRelativePath::new("bar")?;

        map.insert(p1.to_buf(), p2.to_buf());

        assert_eq!(Some(p2), map.get(p1).map(|p| p.as_ref()));

        Ok(())
    }

    #[test]
    fn path_buf_is_clonable() -> anyhow::Result<()> {
        let buf = ForwardRelativePathBuf::unchecked_new("foo".into());
        let buf_ref = &buf;

        let cloned: ForwardRelativePathBuf = buf_ref.clone();
        assert_eq!(buf, cloned);

        Ok(())
    }

    #[test]
    fn relative_path_display_is_readable() -> anyhow::Result<()> {
        let buf = ForwardRelativePathBuf::unchecked_new("foo/bar".into());
        assert_eq!("foo/bar", format!("{}", buf));
        assert_eq!("ForwardRelativePathBuf(\"foo/bar\")", format!("{:?}", buf));
        let refpath: &ForwardRelativePath = &buf;
        assert_eq!("foo/bar", format!("{}", refpath));
        assert_eq!("ForwardRelativePath(\"foo/bar\")", format!("{:?}", refpath));

        Ok(())
    }

    #[cfg(not(windows))]
    #[test]
    fn absolute_path_display_is_readable() -> anyhow::Result<()> {
        let buf = AbsPathBuf::from("/foo/bar".into())?;
        assert_eq!("/foo/bar", format!("{}", buf));
        assert_eq!("AbsPathBuf(\"/foo/bar\")", format!("{:?}", buf));
        let refpath: &AbsPath = &buf;
        assert_eq!("/foo/bar", format!("{}", refpath));
        assert_eq!("AbsPath(\"/foo/bar\")", format!("{:?}", refpath));

        Ok(())
    }

    #[cfg(windows)]
    #[test]
    fn absolute_path_display_is_readable() -> anyhow::Result<()> {
        let buf = AbsPathBuf::from("C:/foo/bar".into())?;
        assert_eq!("C:/foo/bar", format!("{}", buf));
        assert_eq!("AbsPathBuf(\"C:/foo/bar\")", format!("{:?}", buf));
        let refpath: &AbsPath = &buf;
        assert_eq!("C:/foo/bar", format!("{}", refpath));
        assert_eq!("AbsPath(\"C:/foo/bar\")", format!("{:?}", refpath));

        Ok(())
    }
}
