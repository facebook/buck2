use crate::{
    fit::InsufficientWidthError,
};

/// Termimad error type
///
#[derive(thiserror::Error, Debug)]
pub enum Error {

    #[error("IO error: {0}")]
    IO(#[from] std::io::Error),

    #[error("InsufficientWidth: {0}")]
    InsufficientWidth(#[from] InsufficientWidthError),
}

pub(crate) type Result<T> = std::result::Result<T, Error>;
