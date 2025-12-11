/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::marker::PhantomData;

use clap::error::ErrorKind;

use crate::conversion::from_any_with_tag;

impl From<clap::error::Error> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: clap::error::Error) -> Self {
        match value.kind() {
            // Io/Format are issues with writing to `stderr`/`stdout`, so it shouldn't be an User Error
            // Perhaps this is more accurate as Environment Error, we can change it based on data later
            ErrorKind::Io | ErrorKind::Format => from_any_with_tag(value, crate::ErrorTag::Clap),
            _ => from_any_with_tag(value, crate::ErrorTag::Input),
        }
    }
}

#[derive(Clone)]
struct BuckErrorClapParser<F, T>(F, PhantomData<T>);

impl<F, T> clap::builder::TypedValueParser for BuckErrorClapParser<F, T>
where
    F: Fn(&str) -> crate::Result<T> + Send + Sync + Clone + 'static,
    T: Send + Sync + Clone + 'static,
{
    type Value = T;

    fn parse_ref(
        &self,
        _cmd: &clap::Command,
        _arg: Option<&clap::Arg>,
        value: &std::ffi::OsStr,
    ) -> Result<Self::Value, clap::Error> {
        let value = value.to_str().ok_or_else(|| {
            clap::Error::raw(clap::error::ErrorKind::InvalidUtf8, "Utf8 expected")
        })?;
        // Note: We don't really have any shot of trying to preserve error structure here, so this
        // is the best we can do
        (self.0)(value).map_err(|e| {
            clap::Error::raw(clap::error::ErrorKind::ValueValidation, format!("{}", e))
        })
    }
}

pub fn buck_error_clap_parser<F, T>(f: F) -> impl clap::builder::TypedValueParser<Value = T>
where
    F: Fn(&str) -> crate::Result<T> + Send + Sync + Clone + 'static,
    T: Clone + Send + Sync + 'static,
{
    BuckErrorClapParser(f, PhantomData)
}
