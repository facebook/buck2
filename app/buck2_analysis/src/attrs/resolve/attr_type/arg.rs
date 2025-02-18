/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_artifact::artifact::source_artifact::SourceArtifact;
use buck2_build_api::interpreter::rule_defs::cmd_args::value::FrozenCommandLineArg;
use buck2_build_api::interpreter::rule_defs::provider::builtin::run_info::RunInfoCallable;
use buck2_build_api::interpreter::rule_defs::provider::builtin::template_placeholder_info::FrozenTemplatePlaceholderInfo;
use buck2_build_api::interpreter::rule_defs::resolved_macro::ResolvedMacro;
use buck2_build_api::interpreter::rule_defs::resolved_macro::ResolvedStringWithMacros;
use buck2_build_api::interpreter::rule_defs::resolved_macro::ResolvedStringWithMacrosPart;
use buck2_core::package::source_path::SourcePath;
use buck2_core::package::PackageLabel;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_error::BuckErrorContext;
use buck2_node::attrs::attr_type::arg::ConfiguredMacro;
use buck2_node::attrs::attr_type::arg::ConfiguredStringWithMacros;
use buck2_node::attrs::attr_type::arg::ConfiguredStringWithMacrosPart;
use buck2_node::attrs::attr_type::arg::StringWithMacros;
use buck2_node::attrs::attr_type::arg::UnrecognizedMacro;
use dupe::Dupe;
use either::Either;
use starlark::values::Value;

use crate::attrs::resolve::attr_type::arg::query::ConfiguredQueryMacroBaseExt;
use crate::attrs::resolve::ctx::AttrResolutionContext;

pub mod query;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum ResolveMacroError {
    #[error(
        "The mapping for {0} in the TemplatePlaceholderInfo for {1} was not a dictionary (required because requested arg `{2}`)."
    )]
    KeyedPlaceholderMappingNotADict(String, ConfiguredProvidersLabel, String),
    #[error(
        "The mapping for {0} in the TemplatePlaceholderInfo for {1} had no mapping for arg `{2}`."
    )]
    KeyedPlaceholderArgMissing(String, ConfiguredProvidersLabel, String),
    #[error("There was no mapping for {0} in the TemplatePlaceholderInfo for {1}.")]
    KeyedPlaceholderMappingMissing(String, ConfiguredProvidersLabel),
    #[error(
        "Macro `{0}` it not builtin, target `{1}` must provide `TemplatePlaceholderInfo` to resolve it"
    )]
    KeyedPlaceholderInfoMissing(String, ConfiguredProvidersLabel),
    #[error("There was no mapping for {0}.")]
    UnkeyedPlaceholderUnresolved(String),
    #[error("Expected a RunInfo provider from target `{0}`.")]
    ExpectedRunInfo(String),
    #[error("Can't expand unrecognized macros (`{0}`).")]
    UnrecognizedMacroUnimplemented(String),
}

pub trait ConfiguredStringWithMacrosExt {
    fn resolve<'v>(
        &self,
        ctx: &dyn AttrResolutionContext<'v>,
        pkg: PackageLabel,
    ) -> buck2_error::Result<Value<'v>>;
}

impl ConfiguredStringWithMacrosExt for ConfiguredStringWithMacros {
    fn resolve<'v>(
        &self,
        ctx: &dyn AttrResolutionContext<'v>,
        pkg: PackageLabel,
    ) -> buck2_error::Result<Value<'v>> {
        let resolved_parts = match &self.string_with_macros {
            StringWithMacros::StringPart(s) => {
                vec![ResolvedStringWithMacrosPart::String(s.dupe())]
            }
            StringWithMacros::ManyParts(ref parts) => {
                let mut resolved_parts = Vec::with_capacity(parts.len());
                for part in parts.iter() {
                    match part {
                        ConfiguredStringWithMacrosPart::String(s) => {
                            resolved_parts.push(ResolvedStringWithMacrosPart::String(s.dupe()));
                        }
                        ConfiguredStringWithMacrosPart::Macro(write_to_file, m) => {
                            resolved_parts.push(ResolvedStringWithMacrosPart::Macro(
                                *write_to_file,
                                resolve_configured_macro(m, ctx, pkg).with_buck_error_context(
                                    || format!("Error resolving `{}`.", part),
                                )?,
                            ));
                        }
                    }
                }
                resolved_parts
            }
        };

        let configured_macros = if self.anon_target_compatible {
            Some(self)
        } else {
            None
        };

        // SAFETY: FIXME(JakobDegen): This isn't quite right. We know that this is safe because we
        // know that the underlying references point into frozen heaps kept alive by this module.
        // However, it's also possible to get `'v`-lifetimed references into the non-frozen heap in
        // the current module, in which case this is unsound. Ergonomic support for this pattern
        // would require adopting an additional lifetime to represent the distinction.
        let resolved_parts = unsafe {
            std::mem::transmute::<
                Vec<ResolvedStringWithMacrosPart<'v>>,
                Vec<ResolvedStringWithMacrosPart<'static>>,
            >(resolved_parts)
        };

        Ok(ctx.heap().alloc(ResolvedStringWithMacros::new(
            resolved_parts,
            configured_macros,
        )))
    }
}

fn resolve_configured_macro<'v>(
    configured_macro: &ConfiguredMacro,
    ctx: &dyn AttrResolutionContext<'v>,
    pkg: PackageLabel,
) -> buck2_error::Result<ResolvedMacro<'v>> {
    match configured_macro {
        ConfiguredMacro::Location(target) => {
            let providers_value = ctx.get_dep(target)?;
            Ok(ResolvedMacro::Location(
                providers_value.as_ref().default_info()?,
            ))
        }
        ConfiguredMacro::Exe { label, .. } => {
            // Don't need to consider exec_dep as it already was applied when configuring the label.
            let providers = ctx.get_dep(label)?;
            let run_info = match providers.get_provider_raw(RunInfoCallable::provider_id()) {
                Some(value) => *value,
                None => {
                    return Err(ResolveMacroError::ExpectedRunInfo(label.to_string()).into());
                }
            };
            // A RunInfo is an arg-like value.
            Ok(ResolvedMacro::ArgLike(FrozenCommandLineArg::new(run_info)?))
        }
        ConfiguredMacro::Source(p) => {
            let buck_path = SourcePath::new(pkg.dupe(), p.path().dupe());
            Ok(ResolvedMacro::Source(SourceArtifact::new(buck_path).into()))
        }
        ConfiguredMacro::UserUnkeyedPlaceholder(name) => {
            let provider = ctx.resolve_unkeyed_placeholder(name)?.ok_or_else(|| {
                ResolveMacroError::UnkeyedPlaceholderUnresolved((**name).to_owned())
            })?;
            Ok(ResolvedMacro::ArgLike(provider))
        }
        ConfiguredMacro::UserKeyedPlaceholder(box (name, label, arg)) => {
            let providers = ctx.get_dep(label)?;
            let placeholder_info = providers
                .builtin_provider::<FrozenTemplatePlaceholderInfo>()
                .ok_or_else(|| {
                    ResolveMacroError::KeyedPlaceholderInfoMissing(
                        (**name).to_owned(),
                        label.dupe(),
                    )
                })?;
            let keyed_variables = placeholder_info.keyed_variables();
            let either_cmd_or_mapping = keyed_variables.get(&**name).ok_or_else(|| {
                ResolveMacroError::KeyedPlaceholderMappingMissing(
                    (**name).to_owned(),
                    label.to_owned(),
                )
            })?;

            let value: FrozenCommandLineArg = match (arg, either_cmd_or_mapping) {
                (None, Either::Left(mapping)) => *mapping,
                (Some(arg), Either::Left(_)) => {
                    return Err(ResolveMacroError::KeyedPlaceholderMappingNotADict(
                        (**name).to_owned(),
                        label.dupe(),
                        (**arg).to_owned(),
                    )
                    .into());
                }
                (arg, Either::Right(mapping)) => {
                    let arg = arg.as_deref().unwrap_or("DEFAULT");
                    mapping.get(arg).copied().ok_or_else(|| {
                        ResolveMacroError::KeyedPlaceholderArgMissing(
                            (**name).to_owned(),
                            label.dupe(),
                            arg.to_owned(),
                        )
                    })?
                }
            };

            Ok(ResolvedMacro::ArgLike(value))
        }
        ConfiguredMacro::Query(query) => Ok(ResolvedMacro::Query(query.resolve(ctx)?)),
        ConfiguredMacro::UnrecognizedMacro(box UnrecognizedMacro {
            macro_type,
            args: _,
        }) => {
            Err(ResolveMacroError::UnrecognizedMacroUnimplemented((**macro_type).to_owned()).into())
        }
    }
}
