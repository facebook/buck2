/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::fs::paths::RelativePathBuf;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_node::attrs::attr_type::arg::ConfiguredStringWithMacros;
use starlark::values::Value;
use value::ResolvedStringWithMacros;

use crate::actions::artifact::ExecutorFs;
use crate::attrs::resolve::ctx::AttrResolutionContext;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilderContext;
use crate::interpreter::rule_defs::cmd_args::CommandLineLocation;

pub mod query;
pub mod value;

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

impl CommandLineBuilderContext for SpaceSeparatedCommandLineBuilder<'_> {
    fn resolve_project_path(
        &self,
        path: ProjectRelativePathBuf,
    ) -> anyhow::Result<CommandLineLocation> {
        self.builder.resolve_project_path(path)
    }

    fn fs(&self) -> &ExecutorFs {
        self.builder.fs()
    }

    fn next_macro_file_path(&mut self) -> anyhow::Result<RelativePathBuf> {
        self.builder.next_macro_file_path()
    }
}

impl CommandLineBuilder for SpaceSeparatedCommandLineBuilder<'_> {
    fn add_arg_string(&mut self, s: String) {
        if self.first {
            self.first = false;
        } else {
            self.builder.push_str(" ");
        }
        self.builder.push_str(&s);
    }
}

pub(crate) trait ConfiguredStringWithMacrosExt {
    fn resolve<'v>(&self, ctx: &'v dyn AttrResolutionContext) -> anyhow::Result<Value<'v>>;
}

impl ConfiguredStringWithMacrosExt for ConfiguredStringWithMacros {
    fn resolve<'v>(&self, ctx: &'v dyn AttrResolutionContext) -> anyhow::Result<Value<'v>> {
        ResolvedStringWithMacros::resolved(self, ctx)
    }
}

/// An ArgBuilder is almost exactly a CommandLineBuilder. The difference is that while a commandline
/// builder is building a list of strings, argbuilder is appending the values to a single string.
pub trait ArgBuilder: CommandLineBuilderContext {
    /// Add the string representation to the list of command line arguments.
    fn push_str(&mut self, s: &str);
}

#[cfg(test)]
mod tests {
    use buck2_core::fs::paths::RelativePathBuf;
    use buck2_core::fs::project::ProjectRelativePathBuf;

    use crate::actions::artifact::ExecutorFs;
    use crate::attrs::resolve::attr_type::arg::ArgBuilder;
    use crate::attrs::resolve::attr_type::arg::SpaceSeparatedCommandLineBuilder;
    use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
    use crate::interpreter::rule_defs::cmd_args::CommandLineBuilderContext;
    use crate::interpreter::rule_defs::cmd_args::CommandLineLocation;

    #[test]
    fn cmdline_builder() -> anyhow::Result<()> {
        struct Base {
            val: String,
        }
        impl CommandLineBuilderContext for Base {
            fn resolve_project_path(
                &self,
                _path: ProjectRelativePathBuf,
            ) -> anyhow::Result<CommandLineLocation> {
                unimplemented!()
            }

            fn fs(&self) -> &ExecutorFs {
                unimplemented!()
            }

            fn next_macro_file_path(&mut self) -> anyhow::Result<RelativePathBuf> {
                unimplemented!()
            }
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
            builder.add_arg_string("hello".to_owned());
        }
        assert_eq!("hello", &base.val);

        base.val = String::new();
        {
            let mut builder = SpaceSeparatedCommandLineBuilder::wrap(&mut base);
            builder.add_arg_string("hello".to_owned());
            builder.add_arg_string("world!".to_owned());
        }
        assert_eq!("hello world!", &base.val);

        {
            let mut builder = SpaceSeparatedCommandLineBuilder::wrap(&mut base);
            builder.add_arg_string("hello".to_owned());
            builder.add_arg_string("again!".to_owned());
            builder.add_arg_string("and".to_owned());
            builder.add_arg_string("again!".to_owned());
        }
        assert_eq!("hello world!hello again! and again!", &base.val);

        Ok(())
    }
}
