use {
    std::fmt,
};

/// Error thrown when fitting isn't possible
#[derive(thiserror::Error, Debug)]
pub struct InsufficientWidthError {
    pub available_width: usize,
}

impl fmt::Display for InsufficientWidthError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Insufficient available width ({})", self.available_width)
    }
}
