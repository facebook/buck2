/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

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

#[cfg(test)]
mod tests {
    use crate::interpreter::rule_defs::cmd_args::arg_builder::ArgBuilder;
    use crate::interpreter::rule_defs::cmd_args::space_separated::SpaceSeparatedCommandLineBuilder;
    use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;

    #[test]
    fn cmdline_builder() -> anyhow::Result<()> {
        struct Base {
            val: String,
        }
        impl ArgBuilder for Base {
            fn push_str(&mut self, v: &str) {
                self.val.push_str(v);
            }
        }

        let mut base = Base { val: String::new() };
        SpaceSeparatedCommandLineBuilder::wrap(&mut base);
        assert_eq!("", &base.val);

        {
            let mut builder = SpaceSeparatedCommandLineBuilder::wrap(&mut base);
            builder.push_arg("hello".to_owned());
        }
        assert_eq!("hello", &base.val);

        base.val = String::new();
        {
            let mut builder = SpaceSeparatedCommandLineBuilder::wrap(&mut base);
            builder.push_arg("hello".to_owned());
            builder.push_arg("world!".to_owned());
        }
        assert_eq!("hello world!", &base.val);

        {
            let mut builder = SpaceSeparatedCommandLineBuilder::wrap(&mut base);
            builder.push_arg("hello".to_owned());
            builder.push_arg("again!".to_owned());
            builder.push_arg("and".to_owned());
            builder.push_arg("again!".to_owned());
        }
        assert_eq!("hello world!hello again! and again!", &base.val);

        Ok(())
    }
}
