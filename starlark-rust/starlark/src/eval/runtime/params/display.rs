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
use std::fmt::Display;

pub(crate) struct ParamFmt<'a, T: Display, D: Display> {
    /// Parameter name.
    pub(crate) name: &'a str,
    /// Parameter type. If `None`, it will be omitted.
    pub(crate) ty: Option<T>,
    pub(crate) default: Option<D>,
}

/// Utility to format function signature.
pub(crate) fn fmt_param_spec<'n, T: Display, D: Display>(
    f: &mut dyn fmt::Write,
    pos_only: impl IntoIterator<Item = ParamFmt<'n, T, D>>,
    pos_named: impl IntoIterator<Item = ParamFmt<'n, T, D>>,
    args: Option<ParamFmt<'n, T, D>>,
    named_only: impl IntoIterator<Item = ParamFmt<'n, T, D>>,
    kwargs: Option<ParamFmt<'n, T, D>>,
) -> fmt::Result {
    struct Printer<'w> {
        first: bool,
        f: &'w mut dyn fmt::Write,
    }

    impl<'w> Printer<'w> {
        fn write_comma_if_needed(&mut self) -> fmt::Result {
            if self.first {
                self.first = false;
            } else {
                write!(self.f, ", ")?;
            }
            Ok(())
        }

        fn write_param(
            &mut self,
            name: impl Display,
            ty: Option<impl Display>,
            default: Option<impl Display>,
        ) -> fmt::Result {
            self.write_comma_if_needed()?;

            write!(self.f, "{name}")?;
            if let Some(ty) = ty {
                write!(self.f, ": {ty}")?;
            }
            if let Some(default) = default {
                write!(self.f, " = {default}")?;
            }
            Ok(())
        }
    }

    let mut printer = Printer { first: true, f };

    let mut seen_pos_only = false;
    for pos_only in pos_only {
        seen_pos_only = true;
        printer.write_param(pos_only.name, pos_only.ty, pos_only.default)?;
    }
    if seen_pos_only {
        printer.write_comma_if_needed()?;
        write!(printer.f, "/")?;
    }

    for pos_named in pos_named {
        printer.write_param(pos_named.name, pos_named.ty, pos_named.default)?;
    }

    let mut named_only = named_only.into_iter().peekable();

    if let Some(args) = args {
        printer.write_param(format_args!("*{}", args.name), args.ty, args.default)?;
    } else if named_only.peek().is_some() {
        printer.write_comma_if_needed()?;
        write!(printer.f, "*")?;
    }

    for named_only in named_only {
        printer.write_param(named_only.name, named_only.ty, named_only.default)?;
    }

    if let Some(kwargs) = kwargs {
        printer.write_param(format_args!("**{}", kwargs.name), kwargs.ty, kwargs.default)?;
    }

    Ok(())
}
