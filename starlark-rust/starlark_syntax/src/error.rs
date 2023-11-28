/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use std::fmt;

/// An error produced by starlark.
///
/// This error is composed of an error kind, together with some diagnostic information indicating
/// where it occurred.
///
/// In order to prevent accidental conversions to `anyhow::Error`, this type intentionally does not
/// implement `std::error::Error`. That should probably change in the future.
#[derive(Debug)]
pub struct Error(Box<ErrorInner>);

impl Error {
    /// The kind of this error
    pub fn kind(&self) -> &ErrorKind {
        &self.0.kind
    }

    /// Convert this error into an `anyhow::Error`
    pub fn into_anyhow(self) -> anyhow::Error {
        struct Wrapped(Error);

        impl fmt::Display for Wrapped {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::Display::fmt(&self.0, f)
            }
        }

        impl fmt::Debug for Wrapped {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::Debug::fmt(&self.0, f)
            }
        }

        impl std::error::Error for Wrapped {
            fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
                self.0.kind().source()
            }
        }

        anyhow::Error::new(Wrapped(self))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO(JakobDegen): Use the diagnostic formatting here, once it's been added to the error
        fmt::Display::fmt(self.kind(), f)
    }
}

#[derive(Debug)]
struct ErrorInner {
    kind: ErrorKind,
}

/// The different kinds of errors that can be produced by starlark
#[non_exhaustive]
pub enum ErrorKind {
    /// Fallback option
    ///
    /// This is used in two cases:
    ///  1. For errors produced by starlark which have not yet been assigned their own kind
    ///  2. When a native function invoked as a part of starlark evaluation returns a
    ///     `anyhow::Error`
    Other(anyhow::Error),
}

impl ErrorKind {
    /// The source of the error, akin to `[std::error::Error::source]`
    pub fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Other(e) => e.source(),
        }
    }
}

impl fmt::Debug for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Other(e) => fmt::Debug::fmt(e, f),
        }
    }
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Other(e) => fmt::Display::fmt(e, f),
        }
    }
}

impl From<anyhow::Error> for Error {
    fn from(e: anyhow::Error) -> Self {
        Self(Box::new(ErrorInner {
            kind: ErrorKind::Other(e),
        }))
    }
}
