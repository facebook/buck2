/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::TypeId;
use std::fmt::Debug;

use buck2_interpreter::types::label_relative_path::LabelRelativePath;
use gazebo::prelude::*;
use starlark::environment::GlobalsBuilder;
use starlark::values::FrozenRef;
use starlark::values::FrozenValue;
use starlark::values::StringValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use thiserror::Error;

use crate::actions::impls::write_json::FrozenWriteJsonCommandLineArg;
use crate::actions::impls::write_json::WriteJsonCommandLineArg;
use crate::attrs::resolve::attr_type::arg::value::ResolvedStringWithMacros;
use crate::interpreter::rule_defs::artifact::FrozenStarlarkOutputArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkDeclaredArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkOutputArtifact;
use crate::interpreter::rule_defs::artifact_tagging::FrozenTaggedArtifacts;
use crate::interpreter::rule_defs::artifact_tagging::TaggedArtifacts;
use crate::interpreter::rule_defs::cmd_args::options::QuoteStyle;
use crate::interpreter::rule_defs::provider::builtin::run_info::FrozenRunInfo;
use crate::interpreter::rule_defs::provider::builtin::run_info::RunInfo;
use crate::interpreter::rule_defs::transitive_set::FrozenTransitiveSetArgsProjection;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetArgsProjection;

mod builder;
mod options;
#[cfg(test)]
mod test;
mod traits;
mod typ;

pub use builder::*;
pub use traits::*;
pub use typ::*;

#[derive(Debug, Error)]
enum CommandLineArgError {
    #[error(
        "expected command line item to be a string, artifact, or label, or list thereof, not `{repr}`"
    )]
    InvalidItemType { repr: String },
}

pub(crate) trait ValueAsCommandLineLike<'v> {
    fn as_command_line(&self) -> Option<&'v dyn CommandLineArgLike>;
    fn as_command_line_err(&self) -> anyhow::Result<&'v dyn CommandLineArgLike>;
}

pub(crate) trait ValueAsFrozenCommandLineLike {
    fn as_frozen_command_line(&self) -> Option<FrozenRef<'static, dyn FrozenCommandLineArgLike>>;
}

impl<'v> ValueAsCommandLineLike<'v> for Value<'v> {
    fn as_command_line(&self) -> Option<&'v dyn CommandLineArgLike> {
        if let Some(x) = self.to_value().unpack_starlark_str() {
            return Some(x as &dyn CommandLineArgLike);
        }

        let aref = self.to_value().as_dyn_any();
        let ty = aref.static_type_of();

        macro_rules! check {
            ($t:ty) => {
                if ty == TypeId::of::<$t>() {
                    return Some(aref.downcast_ref::<$t>().unwrap() as &dyn CommandLineArgLike);
                }
            };
        }

        check!(StarlarkCommandLine);
        check!(FrozenStarlarkCommandLine);
        check!(StarlarkArtifact);
        check!(StarlarkDeclaredArtifact);
        check!(StarlarkOutputArtifact);
        check!(FrozenStarlarkOutputArtifact);
        check!(ResolvedStringWithMacros);
        check!(RunInfo);
        check!(FrozenRunInfo);
        check!(LabelRelativePath);
        check!(FrozenTransitiveSetArgsProjection);
        check!(TransitiveSetArgsProjection);
        check!(FrozenTaggedArtifacts);
        check!(TaggedArtifacts);
        check!(FrozenWriteJsonCommandLineArg);
        check!(WriteJsonCommandLineArg);
        None
    }

    fn as_command_line_err(&self) -> anyhow::Result<&'v dyn CommandLineArgLike> {
        self.as_command_line().ok_or_else(|| {
            CommandLineArgError::InvalidItemType {
                repr: self.to_value().to_repr(),
            }
            .into()
        })
    }
}

impl ValueAsFrozenCommandLineLike for FrozenValue {
    fn as_frozen_command_line(&self) -> Option<FrozenRef<'static, dyn FrozenCommandLineArgLike>> {
        if let Some(x) = self.downcast_frozen_starlark_str() {
            return Some(x.map(|s| s as &dyn FrozenCommandLineArgLike));
        }

        macro_rules! check {
            ($t:ty) => {
                if let Some(x) = self.downcast_frozen_ref::<$t>() {
                    return Some(x.map(|v| v as &dyn FrozenCommandLineArgLike));
                }
            };
        }

        check!(FrozenStarlarkCommandLine);
        check!(StarlarkArtifact);
        check!(FrozenStarlarkOutputArtifact);
        check!(ResolvedStringWithMacros);
        check!(FrozenRunInfo);
        check!(LabelRelativePath);
        check!(FrozenTransitiveSetArgsProjection);
        None
    }
}

#[starlark_module]
pub fn register_args_function(builder: &mut GlobalsBuilder) {
    #[starlark(type = "cmd_args")]
    fn cmd_args<'v>(
        #[starlark(args)] args: Vec<Value<'v>>,
        delimiter: Option<StringValue<'v>>,
        format: Option<StringValue<'v>>,
        prepend: Option<StringValue<'v>>,
        quote: Option<&str>,
    ) -> anyhow::Result<StarlarkCommandLine<'v>> {
        StarlarkCommandLine::try_from_values_with_options(
            &args,
            delimiter,
            format,
            prepend,
            quote.try_map(|q| QuoteStyle::parse(q))?,
        )
    }
}

#[cfg(test)]
pub mod tester {
    use buck2_core::fs::paths::AbsPathBuf;
    use buck2_core::fs::project::ProjectRelativePathBuf;
    use buck2_core::fs::project::ProjectRoot;
    use buck2_execute::artifact::fs::ArtifactFs;
    use buck2_execute::artifact::fs::ExecutorFs;
    use buck2_execute::path::buck_out_path::BuckOutPathResolver;
    use buck2_execute::path::buck_out_path::BuckPathResolver;
    use buck2_node::execute::config::PathSeparatorKind;
    use starlark::environment::GlobalsBuilder;
    use starlark::values::Value;

    use crate::interpreter::rule_defs::cmd_args::builder::BaseCommandLineBuilder;
    use crate::interpreter::rule_defs::cmd_args::ValueAsCommandLineLike;
    use crate::interpreter::testing::cells;

    fn artifact_fs() -> ArtifactFs {
        let cell_info = cells(None).unwrap();
        ArtifactFs::new(
            BuckPathResolver::new(cell_info.1),
            BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new(
                "buck-out/v2".to_owned(),
            )),
            ProjectRoot::new(AbsPathBuf::try_from(std::env::current_dir().unwrap()).unwrap()),
        )
    }

    fn get_command_line(value: Value) -> anyhow::Result<Vec<String>> {
        let fs = artifact_fs();
        let executor_fs = ExecutorFs::new(&fs, PathSeparatorKind::Unix);
        let mut builder = BaseCommandLineBuilder::new(&executor_fs);

        match value.as_command_line() {
            Some(v) => v.add_to_command_line(&mut builder),
            None => value
                .as_command_line_err()?
                .add_to_command_line(&mut builder),
        }?;
        Ok(builder.build())
    }

    #[starlark_module]
    pub fn command_line_stringifier(builder: &mut GlobalsBuilder) {
        fn get_args<'v>(value: Value<'v>) -> anyhow::Result<Vec<String>> {
            get_command_line(value)
        }

        fn stringify_cli_arg<'v>(value: Value<'v>) -> anyhow::Result<String> {
            let fs = artifact_fs();
            let executor_fs = ExecutorFs::new(&fs, PathSeparatorKind::Unix);
            let mut builder = BaseCommandLineBuilder::new(&executor_fs);
            value
                .as_command_line_err()?
                .add_to_command_line(&mut builder)?;
            let cli = builder.build();
            assert_eq!(1, cli.len());
            Ok(cli.get(0).unwrap().clone())
        }
    }
}
