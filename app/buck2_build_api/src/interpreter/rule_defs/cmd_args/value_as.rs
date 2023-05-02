/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter::types::label_relative_path::LabelRelativePath;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use starlark::values::FrozenRef;
use starlark::values::FrozenValue;
use starlark::values::Value;
use starlark::values::ValueLike;

use crate::attrs::resolve::attr_type::arg::value::ResolvedStringWithMacros;
use crate::interpreter::rule_defs::artifact::FrozenStarlarkOutputArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::FrozenCommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::FrozenStarlarkCommandLine;
use crate::interpreter::rule_defs::provider::builtin::run_info::FrozenRunInfo;
use crate::interpreter::rule_defs::provider::builtin::run_info::RunInfo;
use crate::interpreter::rule_defs::transitive_set::FrozenTransitiveSetArgsProjection;

#[derive(Debug, thiserror::Error)]
enum CommandLineArgError {
    #[error(
        "expected command line item to be a string, artifact, or label, or list thereof, not `{repr}`"
    )]
    InvalidItemType { repr: String },
}

pub trait ValueAsCommandLineLike<'v> {
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

        macro_rules! check {
            ($t:ty) => {
                if let Some(v) = self.to_value().downcast_ref::<$t>() {
                    return Some(v as &dyn CommandLineArgLike);
                }
            };
        }

        // Typically downcasting is provided by implementing `StarlarkValue::provide`.
        // These are exceptions:
        // * either providers, where `StarlarkValue` is generated,
        //   and plugging in `provide` is tricky
        // * or live outside of `buck2_build_api` crate,
        //   so `impl StarlarkValue` cannot provide `CommandLineArgLike`
        check!(RunInfo);
        check!(FrozenRunInfo);
        check!(LabelRelativePath);
        check!(StarlarkTargetLabel);

        self.request_value()
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
