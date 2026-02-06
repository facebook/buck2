use std::ffi::OsString;
use std::fmt;
use std::path::PathBuf;

use crate::path::safe_canonicalize;
use crate::target::Target;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TargetOrFile {
    Target(Target),
    File(PathBuf),
}

impl fmt::Display for TargetOrFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TargetOrFile::Target(target) => write!(f, "{}", target),
            TargetOrFile::File(path) => write!(f, "{}", path.display()),
        }
    }
}

impl TargetOrFile {
    pub fn from_path_buf(path_buf: PathBuf) -> Self {
        if let Some(str) = path_buf.to_str() {
            if str.contains(":") {
                return Self::Target(Target::new(str.to_owned()));
            }
        }
        Self::File(path_buf)
    }

    pub fn canonicalize(&self) -> Self {
        match self {
            TargetOrFile::Target(target) => TargetOrFile::Target(target.clone()),
            TargetOrFile::File(path) => TargetOrFile::File(safe_canonicalize(path)),
        }
    }
}

impl From<OsString> for TargetOrFile {
    fn from(os_string: OsString) -> Self {
        Self::from_path_buf(PathBuf::from(os_string))
    }
}
