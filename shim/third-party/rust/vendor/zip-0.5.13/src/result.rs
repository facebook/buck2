//! Error types that can be emitted from this library

use std::io;

use thiserror::Error;

/// Generic result type with ZipError as its error variant
pub type ZipResult<T> = Result<T, ZipError>;

/// The given password is wrong
#[derive(Error, Debug)]
#[error("invalid password for file in archive")]
pub struct InvalidPassword;

/// Error type for Zip
#[derive(Debug, Error)]
pub enum ZipError {
    /// An Error caused by I/O
    #[error(transparent)]
    Io(#[from] io::Error),

    /// This file is probably not a zip archive
    #[error("invalid Zip archive")]
    InvalidArchive(&'static str),

    /// This archive is not supported
    #[error("unsupported Zip archive")]
    UnsupportedArchive(&'static str),

    /// The requested file could not be found in the archive
    #[error("specified file not found in archive")]
    FileNotFound,
}

impl ZipError {
    /// The text used as an error when a password is required and not supplied
    ///
    /// ```rust,no_run
    /// # use zip::result::ZipError;
    /// # let mut archive = zip::ZipArchive::new(std::io::Cursor::new(&[])).unwrap();
    /// match archive.by_index(1) {
    ///     Err(ZipError::UnsupportedArchive(ZipError::PASSWORD_REQUIRED)) => eprintln!("a password is needed to unzip this file"),
    ///     _ => (),
    /// }
    /// # ()
    /// ```
    pub const PASSWORD_REQUIRED: &'static str = "Password required to decrypt file";
}

impl From<ZipError> for io::Error {
    fn from(err: ZipError) -> io::Error {
        io::Error::new(io::ErrorKind::Other, err)
    }
}
