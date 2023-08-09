/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use ref_cast::ref_cast_custom;
use ref_cast::RefCastCustom;

use crate::interpreter::rule_defs::cmd_args::arg_builder::ArgBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;

pub struct SpaceSeparatedCommandLineBuilder<'v> {
    builder: &'v mut dyn ArgBuilder,
    first: bool,
}

impl<'v> SpaceSeparatedCommandLineBuilder<'v> {
    // This can be used to construct a CommandLineBuilder that will append the command line
    // as a space-separated string to the arg.
    pub fn wrap(builder: &'v mut dyn ArgBuilder) -> Self {
        Self {
            builder,
            first: true,
        }
    }

    pub fn wrap_string(string: &'v mut String) -> Self {
        let builder = StringAsArgBuilder::new(string);
        Self::wrap(builder as _)
    }
}

impl CommandLineBuilder for SpaceSeparatedCommandLineBuilder<'_> {
    fn push_arg(&mut self, s: String) {
        if self.first {
            self.first = false;
        } else {
            self.builder.push_str(" ");
        }
        self.builder.push_str(&s);
    }
}

#[derive(RefCastCustom)]
#[repr(transparent)]
struct StringAsArgBuilder(String);

impl StringAsArgBuilder {
    #[ref_cast_custom]
    pub(crate) fn new(string: &mut String) -> &mut Self;
}

impl ArgBuilder for StringAsArgBuilder {
    fn push_str(&mut self, v: &str) {
        self.0.push_str(v);
    }
}

#[cfg(test)]
mod tests {
    use crate::interpreter::rule_defs::cmd_args::space_separated::SpaceSeparatedCommandLineBuilder;
    use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;

    #[test]
    fn cmdline_builder() -> anyhow::Result<()> {
        let mut s = String::new();
        SpaceSeparatedCommandLineBuilder::wrap_string(&mut s);
        assert_eq!("", &s);

        {
            let mut builder = SpaceSeparatedCommandLineBuilder::wrap_string(&mut s);
            builder.push_arg("hello".to_owned());
        }
        assert_eq!("hello", &s);

        s = String::new();
        {
            let mut builder = SpaceSeparatedCommandLineBuilder::wrap_string(&mut s);
            builder.push_arg("hello".to_owned());
            builder.push_arg("world!".to_owned());
        }
        assert_eq!("hello world!", &s);

        {
            let mut builder = SpaceSeparatedCommandLineBuilder::wrap_string(&mut s);
            builder.push_arg("hello".to_owned());
            builder.push_arg("again!".to_owned());
            builder.push_arg("and".to_owned());
            builder.push_arg("again!".to_owned());
        }
        assert_eq!("hello world!hello again! and again!", &s);

        Ok(())
    }
}
