/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

/// An error writing to a stdout or stderr stream
#[derive(thiserror::Error, Debug)]
pub enum OutputError {
    #[error("Error writing to output stream: {0}")]
    Write(std::io::Error),
    #[error("Error spawning thread: {0}")]
    SpawnThread(std::io::Error),
    #[error("Error interacting with terminal: {0}")]
    Terminal(std::io::Error),
}

#[derive(thiserror::Error, Debug)]
pub enum Error<D> {
    #[error(transparent)]
    Draw(D),
    #[error(transparent)]
    Output(OutputError),
}

impl<D: From<OutputError>> Error<D> {
    pub fn into_draw_error_type(self) -> D {
        match self {
            Self::Draw(d) => d,
            Self::Output(o) => o.into(),
        }
    }
}

impl<D> From<OutputError> for Error<D> {
    fn from(o: OutputError) -> Self {
        Self::Output(o)
    }
}
