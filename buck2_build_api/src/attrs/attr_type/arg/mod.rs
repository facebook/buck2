/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;
use std::fmt::Debug;
use std::mem;

use anyhow::anyhow;
use buck2_core::fs::paths::RelativePathBuf;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_node::attrs::attr_type::arg::parser;
use buck2_node::attrs::attr_type::arg::parser::parse_macros;
use buck2_node::attrs::attr_type::arg::parser::ParsedMacro;
use buck2_node::attrs::attr_type::arg::ArgAttrType;
use buck2_node::attrs::attr_type::arg::MacroBase;
use buck2_node::attrs::attr_type::arg::QueryExpansion;
use buck2_node::attrs::attr_type::arg::StringWithMacros;
use buck2_node::attrs::attr_type::arg::StringWithMacrosPart;
use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::attr_type::query::QueryAttrType;
use buck2_node::attrs::attr_type::query::QueryMacroBase;
use buck2_node::attrs::configuration_context::AttrConfigurationContext;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use buck2_node::attrs::configured_traversal::ConfiguredAttrTraversal;
use buck2_node::attrs::traversal::CoercedAttrTraversal;
use gazebo::prelude::*;
use once_cell::sync::Lazy;
use starlark::values::string::STRING_TYPE;
use starlark::values::Value;
use thiserror::Error;

use crate::actions::artifact::ExecutorFs;
use crate::attrs::analysis::AttrResolutionContext;
use crate::attrs::attr_type::arg::query::ConfiguredQueryMacroBaseExt;
use crate::attrs::attr_type::arg::query::UnconfiguredQueryMacroBaseExt;
use crate::attrs::attr_type::arg::value::ResolvedStringWithMacros;
use crate::attrs::attr_type::attr_literal::CoercionError;
use crate::attrs::attr_type::coerce::AttrTypeCoerce;
use crate::attrs::attr_type::query::QueryAttrTypeExt;
use crate::attrs::configurable::AttrIsConfigurable;
use crate::attrs::AttrCoercionContext;
use crate::attrs::CoercedAttr;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilderContext;
use crate::interpreter::rule_defs::cmd_args::CommandLineLocation;

pub mod query;
pub mod value;

// These are the macros we haven't yet implemented yet, we should make sure not
// to try and resolve them to user defined macros with a target parameter,
// because some of them don't take a target.
// Taken from https://buck.build/function/string_parameter_macros.html.
static UNIMPLEMENTED_MACROS: Lazy<HashSet<&'static str>> =
    Lazy::new(|| hashset!["classpath_abi", "maven_coords", "output", "query_paths",]);

#[derive(Debug, Error)]
enum MacroError {
    #[error("Expected a single target label argument. Got `[{}]`", (.0).join(", "))]
    ExpectedSingleTargetArgument(Vec<String>),
    #[error("Can't expand unrecognized macros (`{0}`).")]
    UnrecognizedMacroUnimplemented(String),
    #[error("Expected a RunInfo provider from target `{0}`.")]
    ExpectedRunInfo(String),

    #[error("{0} was not available in the analysis deps.")]
    KeyedPlaceholderDepMissing(ConfiguredProvidersLabel),
    #[error("There was no TemplatePlaceholderInfo for {0}.")]
    KeyedPlaceholderInfoMissing(ConfiguredProvidersLabel),
    #[error("There was no mapping for {0} in the TemplatePlaceholderInfo for {1}.")]
    KeyedPlaceholderMappingMissing(String, ConfiguredProvidersLabel),
    #[error(
        "The mapping for {0} in the TemplatePlaceholderInfo for {1} was not a dictionary (required because requested arg `{2}`)."
    )]
    KeyedPlaceholderMappingNotADict(String, ConfiguredProvidersLabel, String),
    #[error(
        "The mapping for {0} in the TemplatePlaceholderInfo for {1} had no mapping for arg `{2}`."
    )]
    KeyedPlaceholderArgMissing(String, ConfiguredProvidersLabel, String),

    #[error("Incorrent number of args to macro `{0}` (had {1} args)")]
    InvalidNumberOfArgs(String, usize),

    #[error("There was no mapping for {0}.")]
    UnkeyedPlaceholderUnresolved(String),
}

impl AttrTypeCoerce for ArgAttrType {
    fn coerce_item(
        &self,
        _configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        let value = value
            .unpack_str()
            .ok_or_else(|| anyhow!(CoercionError::type_error(STRING_TYPE, value)))?;

        let mut items = parse_macros(value)?.into_items();
        if let [parser::ArgItem::String(val)] = items.as_mut_slice() {
            // Specialize single-item StringWithMacros, which are in fact the common case.
            return Ok(AttrLiteral::Arg(UnconfiguredStringWithMacros::StringPart(
                mem::take(val).into_boxed_str(),
            )));
        }

        let mut parts = Vec::with_capacity(items.len());

        for item in items {
            match item {
                parser::ArgItem::String(val) => parts.push(StringWithMacrosPart::String(val)),
                parser::ArgItem::Macro(ParsedMacro {
                    write_to_file,
                    macro_type,
                    args,
                }) => {
                    let part = match macro_type.as_ref() {
                        "location" | "location-platform" if args.len() == 1 => {
                            UnconfiguredMacro::new_location(ctx, args)?
                        }
                        "exe" => UnconfiguredMacro::new_exe(ctx, args, true)?,
                        "exe_target" => UnconfiguredMacro::new_exe(ctx, args, false)?,
                        "query_outputs" | "query_targets" | "query_targets_and_outputs" => {
                            UnconfiguredMacro::new_query(ctx, &macro_type, args)?
                        }
                        _ if args.is_empty() => {
                            UnconfiguredMacro::new_user_unkeyed_placeholder(macro_type)
                        }
                        name if (args.len() == 1 || args.len() == 2)
                            && !UNIMPLEMENTED_MACROS.contains(name) =>
                        {
                            UnconfiguredMacro::new_user_keyed_placeholder(ctx, macro_type, args)?
                        }
                        _ => UnconfiguredMacro::new_unrecognized(macro_type, args),
                    };
                    parts.push(StringWithMacrosPart::Macro(write_to_file, part));
                }
            }
        }

        Ok(AttrLiteral::Arg(UnconfiguredStringWithMacros::ManyParts(
            parts.into_boxed_slice(),
        )))
    }

    fn starlark_type(&self) -> String {
        "str.type".to_owned()
    }
}

// These type aliases are just a little bit easier to use, the differentiating thing comes
// right at the beginning instead of at the end in a type param.
type UnconfiguredMacro = MacroBase<CoercedAttr>;
type ConfiguredMacro = MacroBase<ConfiguredAttr>;

type UnconfiguredStringWithMacrosPart = StringWithMacrosPart<CoercedAttr>;
type ConfiguredStringWithMacrosPart = StringWithMacrosPart<ConfiguredAttr>;

type UnconfiguredStringWithMacros = StringWithMacros<CoercedAttr>;
type ConfiguredStringWithMacros = StringWithMacros<ConfiguredAttr>;

pub(crate) trait UnconfiguredStringWithMacrosExt {
    fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
    ) -> anyhow::Result<ConfiguredStringWithMacros>;

    fn traverse<'a>(&'a self, traversal: &mut dyn CoercedAttrTraversal<'a>) -> anyhow::Result<()>;
}

impl UnconfiguredStringWithMacrosExt for UnconfiguredStringWithMacros {
    fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
    ) -> anyhow::Result<ConfiguredStringWithMacros> {
        match self {
            Self::StringPart(part) => Ok(ConfiguredStringWithMacros::StringPart(part.clone())),
            Self::ManyParts(parts) => Ok(ConfiguredStringWithMacros::ManyParts(
                parts.try_map(|p| p.configure(ctx))?.into_boxed_slice(),
            )),
        }
    }

    fn traverse<'a>(&'a self, traversal: &mut dyn CoercedAttrTraversal<'a>) -> anyhow::Result<()> {
        match self {
            Self::StringPart(..) => {}
            Self::ManyParts(ref parts) => {
                for part in parts.iter() {
                    match part {
                        StringWithMacrosPart::String(_) => {}
                        StringWithMacrosPart::Macro(_, m) => {
                            m.traverse(traversal)?;
                        }
                    }
                }
            }
        }

        Ok(())
    }
}

pub(crate) trait UnconfiguredStringWithMacrosPartExt {
    fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
    ) -> anyhow::Result<ConfiguredStringWithMacrosPart>;
}

impl UnconfiguredStringWithMacrosPartExt for UnconfiguredStringWithMacrosPart {
    fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
    ) -> anyhow::Result<ConfiguredStringWithMacrosPart> {
        match self {
            StringWithMacrosPart::String(val) => {
                Ok(ConfiguredStringWithMacrosPart::String(val.clone()))
            }
            StringWithMacrosPart::Macro(write_to_file, unconfigured) => Ok(
                ConfiguredStringWithMacrosPart::Macro(*write_to_file, unconfigured.configure(ctx)?),
            ),
        }
    }
}

pub(crate) trait ConfiguredStringWithMacrosExt {
    fn resolve<'v>(&self, ctx: &'v dyn AttrResolutionContext) -> anyhow::Result<Value<'v>>;

    fn traverse<'a>(
        &'a self,
        traversal: &mut dyn ConfiguredAttrTraversal<'a>,
    ) -> anyhow::Result<()>;
}

impl ConfiguredStringWithMacrosExt for ConfiguredStringWithMacros {
    fn resolve<'v>(&self, ctx: &'v dyn AttrResolutionContext) -> anyhow::Result<Value<'v>> {
        ResolvedStringWithMacros::resolved(self, ctx)
    }

    fn traverse<'a>(
        &'a self,
        traversal: &mut dyn ConfiguredAttrTraversal<'a>,
    ) -> anyhow::Result<()> {
        match self {
            Self::StringPart(..) => {}
            Self::ManyParts(ref parts) => {
                for part in parts.iter() {
                    match part {
                        StringWithMacrosPart::String(_) => {}
                        StringWithMacrosPart::Macro(_, m) => {
                            m.traverse(traversal)?;
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

/// A simple helper for a macro that accepts a single target argument (the most common case, by far).
fn get_single_target_arg(
    args: Vec<String>,
    ctx: &dyn AttrCoercionContext,
) -> anyhow::Result<ProvidersLabel> {
    if args.len() != 1 {
        return Err(anyhow::anyhow!(MacroError::ExpectedSingleTargetArgument(
            args
        )));
    }
    ctx.coerce_label(&args[0])
}

pub(crate) trait UnconfiguredMacroExt {
    fn new_location(
        ctx: &dyn AttrCoercionContext,
        args: Vec<String>,
    ) -> anyhow::Result<UnconfiguredMacro> {
        Ok(UnconfiguredMacro::Location(get_single_target_arg(
            args, ctx,
        )?))
    }

    fn new_exe(
        ctx: &dyn AttrCoercionContext,
        args: Vec<String>,
        exec_dep: bool,
    ) -> anyhow::Result<UnconfiguredMacro> {
        Ok(UnconfiguredMacro::Exe {
            label: get_single_target_arg(args, ctx)?,
            exec_dep,
        })
    }

    fn new_user_keyed_placeholder(
        ctx: &dyn AttrCoercionContext,
        var_name: String,
        mut args: Vec<String>,
    ) -> anyhow::Result<UnconfiguredMacro> {
        // args should've already been checked to have len == 1 or 2
        let arg = if args.len() == 2 {
            Some(args.pop().unwrap())
        } else {
            None
        };
        Ok(UnconfiguredMacro::UserKeyedPlaceholder(
            var_name,
            ctx.coerce_label(&args[0])?,
            arg,
        ))
    }

    fn new_query(
        ctx: &dyn AttrCoercionContext,
        expansion_type: &str,
        args: Vec<String>,
    ) -> anyhow::Result<UnconfiguredMacro> {
        if args.is_empty() || args.len() > 2 {
            return Err(
                MacroError::InvalidNumberOfArgs(expansion_type.to_owned(), args.len()).into(),
            );
        }
        let mut args_iter = args.into_iter();
        let (query, separator) = match (args_iter.next(), args_iter.next()) {
            (Some(query), None) => (query, None),
            (separator, Some(query)) => (query, separator),
            _ => unreachable!(),
        };

        let expansion_type = match expansion_type {
            "query_targets" => QueryExpansion::Target,
            "query_outputs" => QueryExpansion::Output,
            "query_targets_and_outputs" => QueryExpansion::TargetAndOutput(separator),
            _ => panic!("invalid expansion type {}", expansion_type),
        };

        // TODO(cjhopman): errors when args aren't the right size (too many, or separator for non-separator query)

        Ok(MacroBase::Query(box QueryMacroBase::new(
            expansion_type,
            QueryAttrType::coerce(ctx, query)?,
        )))
    }

    fn new_user_unkeyed_placeholder(var_name: String) -> UnconfiguredMacro {
        UnconfiguredMacro::UserUnkeyedPlaceholder(var_name)
    }

    fn new_unrecognized(macro_type: String, args: Vec<String>) -> UnconfiguredMacro {
        UnconfiguredMacro::UnrecognizedMacro(macro_type, args)
    }

    fn configure(&self, ctx: &dyn AttrConfigurationContext) -> anyhow::Result<ConfiguredMacro>;

    fn traverse<'a>(&'a self, traversal: &mut dyn CoercedAttrTraversal<'a>) -> anyhow::Result<()>;
}

impl UnconfiguredMacroExt for UnconfiguredMacro {
    fn configure(&self, ctx: &dyn AttrConfigurationContext) -> anyhow::Result<ConfiguredMacro> {
        Ok(match self {
            UnconfiguredMacro::Location(target) => {
                ConfiguredMacro::Location(ctx.configure_target(target))
            }
            UnconfiguredMacro::Exe { label, exec_dep } => ConfiguredMacro::Exe {
                label: if *exec_dep {
                    ctx.configure_exec_target(label)
                } else {
                    ctx.configure_target(label)
                },
                exec_dep: *exec_dep,
            },
            UnconfiguredMacro::UserUnkeyedPlaceholder(var_name) => {
                ConfiguredMacro::UserUnkeyedPlaceholder(var_name.clone())
            }
            UnconfiguredMacro::UserKeyedPlaceholder(var_name, target, arg) => {
                ConfiguredMacro::UserKeyedPlaceholder(
                    var_name.clone(),
                    ctx.configure_target(target),
                    arg.clone(),
                )
            }
            UnconfiguredMacro::Query(query) => ConfiguredMacro::Query(box query.configure(ctx)?),
            UnconfiguredMacro::UnrecognizedMacro(macro_type, args) => {
                ConfiguredMacro::UnrecognizedMacro(macro_type.clone(), args.clone())
            }
        })
    }

    fn traverse<'a>(&'a self, traversal: &mut dyn CoercedAttrTraversal<'a>) -> anyhow::Result<()> {
        match self {
            MacroBase::Location(l) | MacroBase::UserKeyedPlaceholder(_, l, _) => {
                traversal.dep(l.target())
            }
            MacroBase::Exe {
                label,
                exec_dep: true,
            } => traversal.exec_dep(label.target()),
            MacroBase::Exe {
                label,
                exec_dep: false,
            } => traversal.dep(label.target()),
            MacroBase::Query(query) => query.traverse(traversal),
            MacroBase::UserUnkeyedPlaceholder(_) | MacroBase::UnrecognizedMacro(..) => Ok(()),
        }
    }
}

/// An ArgBuilder is almost exactly a CommandLineBuilder. The difference is that while a commandline
/// builder is building a list of strings, argbuilder is appending the values to a single string.
pub trait ArgBuilder: CommandLineBuilderContext {
    /// Add the string representation to the list of command line arguments.
    fn push_str(&mut self, s: &str);
}

struct SpaceSeparatedCommandLineBuilder<'v> {
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

pub(crate) trait ConfiguredMacroExt {
    fn traverse<'a>(
        &'a self,
        traversal: &mut dyn ConfiguredAttrTraversal<'a>,
    ) -> anyhow::Result<()>;
}

impl ConfiguredMacroExt for ConfiguredMacro {
    fn traverse<'a>(
        &'a self,
        traversal: &mut dyn ConfiguredAttrTraversal<'a>,
    ) -> anyhow::Result<()> {
        // macros can't reference repo inputs (they only reference the outputs of other targets)
        match self {
            MacroBase::Location(l) | MacroBase::UserKeyedPlaceholder(_, l, _) => traversal.dep(l),
            MacroBase::Exe {
                label,
                exec_dep: true,
            } => traversal.exec_dep(label),
            MacroBase::Exe {
                label,
                exec_dep: false,
            } => traversal.dep(label),
            MacroBase::Query(query_macro) => query_macro.traverse(traversal),
            MacroBase::UserUnkeyedPlaceholder(_) | MacroBase::UnrecognizedMacro(..) => Ok(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::target::TargetLabel;
    use buck2_node::attrs::attr_type::AttrType;
    use buck2_node::attrs::configuration_context::AttrConfigurationContext;
    use starlark::environment::GlobalsBuilder;
    use starlark::environment::Module;

    use super::*;
    use crate::attrs::attr_type::attr_literal::CoercedDepsCollector;
    use crate::attrs::attr_type::attr_literal::ConfiguredAttrInfo;
    use crate::attrs::attr_type::AttrTypeExt;
    use crate::attrs::configurable::AttrIsConfigurable;
    use crate::attrs::testing::*;

    trait GetMacroDeps {
        type DepsType;
        fn get_deps(&self) -> anyhow::Result<Vec<Self::DepsType>>;
    }

    impl GetMacroDeps for UnconfiguredMacro {
        type DepsType = TargetLabel;
        fn get_deps(&self) -> anyhow::Result<Vec<Self::DepsType>> {
            let mut visitor = CoercedDepsCollector::new();
            self.traverse(&mut visitor)?;
            let CoercedDepsCollector {
                deps, exec_deps, ..
            } = visitor;
            Ok(deps.into_iter().chain(exec_deps.into_iter()).collect())
        }
    }

    #[test]
    fn test_concat() -> anyhow::Result<()> {
        let env = Module::new();
        let globals = GlobalsBuilder::extended()
            .with(buck2_interpreter::build_defs::native_module)
            .build();
        let attr = AttrType::arg();
        let value = to_value(
            &env,
            &globals,
            r#""$(exe //:foo) " + select({"DEFAULT": "$(location //:bar)"})"#,
        );

        let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)?;
        let configured = coerced.configure(&configuration_ctx())?;
        assert_eq!(
            r#""$(exe root//:foo (cfg_for//:testing_exec)) $(location root//:bar (<testing>))""#,
            configured.to_string()
        );

        Ok(())
    }

    #[test]
    fn test_location() -> anyhow::Result<()> {
        let ctx = coercion_ctx();
        let location = UnconfiguredMacro::new_location(&ctx, vec!["//some:target".to_owned()])?;
        let deps = location.get_deps()?.map(|t| t.to_string());
        assert_eq!(vec!["root//some:target".to_owned()], deps);

        let configured = location.configure(&configuration_ctx())?;

        if let MacroBase::Location(target) = &configured {
            let mut info = ConfiguredAttrInfo::new();
            configured.traverse(&mut info)?;
            assert_eq!(smallset![target.clone()], info.deps);
        } else {
            return Err(anyhow::anyhow!("Expected Location"));
        }

        Ok(())
    }

    #[test]
    fn test_exe() -> anyhow::Result<()> {
        let ctx = coercion_ctx();
        let exe = UnconfiguredMacro::new_exe(&ctx, vec!["//some:target".to_owned()], true)?;
        let deps = exe.get_deps()?.map(|t| t.to_string());
        assert_eq!(vec!["root//some:target".to_owned()], deps);
        assert_eq!("exe root//some:target", &exe.to_string());

        let config_ctx = configuration_ctx();
        let configured = exe.configure(&config_ctx)?;

        if let MacroBase::Exe { label, .. } = &configured {
            let mut info = ConfiguredAttrInfo::new();
            configured.traverse(&mut info)?;
            assert_eq!(label.cfg(), config_ctx.exec_cfg());
            assert_eq!(smallset![label.clone()], info.execution_deps);
            assert_eq!(smallset![], info.deps);
        } else {
            return Err(anyhow::anyhow!("Expected Exe"));
        }

        Ok(())
    }

    #[test]
    fn test_exe_target() -> anyhow::Result<()> {
        let ctx = coercion_ctx();
        let exe = UnconfiguredMacro::new_exe(&ctx, vec!["//some:target".to_owned()], false)?;
        let deps = exe.get_deps()?.map(|t| t.to_string());
        assert_eq!(vec!["root//some:target".to_owned()], deps);
        assert_eq!("exe_target root//some:target", &exe.to_string());

        let config_ctx = configuration_ctx();
        let configured = exe.configure(&config_ctx)?;

        if let MacroBase::Exe { label, .. } = &configured {
            let mut info = ConfiguredAttrInfo::new();
            configured.traverse(&mut info)?;
            assert_eq!(label.cfg(), config_ctx.cfg());
            assert_eq!(smallset![], info.execution_deps);
            assert_eq!(smallset![label.clone()], info.deps);
        } else {
            return Err(anyhow::anyhow!("Expected Exe"));
        }

        Ok(())
    }

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
