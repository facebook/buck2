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

use buck2_core::provider::label::ProvidersLabel;
use buck2_node::attrs::attr_type::arg::parser;
use buck2_node::attrs::attr_type::arg::parser::parse_macros;
use buck2_node::attrs::attr_type::arg::parser::ParsedMacro;
use buck2_node::attrs::attr_type::arg::ArgAttrType;
use buck2_node::attrs::attr_type::arg::MacroBase;
use buck2_node::attrs::attr_type::arg::QueryExpansion;
use buck2_node::attrs::attr_type::arg::StringWithMacrosPart;
use buck2_node::attrs::attr_type::arg::UnconfiguredMacro;
use buck2_node::attrs::attr_type::arg::UnconfiguredStringWithMacros;
use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::attr_type::query::QueryAttrType;
use buck2_node::attrs::attr_type::query::QueryMacroBase;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use maplit::hashset;
use once_cell::sync::Lazy;
use starlark::values::string::STRING_TYPE;
use starlark::values::Value;
use thiserror::Error;

use crate::attrs::coerce::attr_type::query::QueryAttrTypeExt;
use crate::attrs::coerce::error::CoercionError;
use crate::attrs::coerce::AttrTypeCoerce;

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
    #[error("Incorrent number of args to macro `{0}` (had {1} args)")]
    InvalidNumberOfArgs(String, usize),
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
            .ok_or_else(|| anyhow::anyhow!(CoercionError::type_error(STRING_TYPE, value)))?;

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

pub trait UnconfiguredMacroExt {
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

        Ok(MacroBase::Query(box QueryMacroBase {
            expansion_type,
            query: QueryAttrType::coerce(ctx, query)?,
        }))
    }

    fn new_user_unkeyed_placeholder(var_name: String) -> UnconfiguredMacro {
        UnconfiguredMacro::UserUnkeyedPlaceholder(var_name)
    }

    fn new_unrecognized(macro_type: String, args: Vec<String>) -> UnconfiguredMacro {
        UnconfiguredMacro::UnrecognizedMacro(macro_type, args)
    }
}

impl UnconfiguredMacroExt for UnconfiguredMacro {}

#[cfg(test)]
mod tests {
    use buck2_core::target::TargetLabel;
    use buck2_node::attrs::attr_type::AttrType;
    use buck2_node::attrs::coerced_deps_collector::CoercedDepsCollector;
    use buck2_node::attrs::configurable::AttrIsConfigurable;
    use buck2_node::attrs::configuration_context::AttrConfigurationContext;
    use buck2_node::attrs::configured_info::ConfiguredAttrInfo;
    use buck2_node::attrs::testing::configuration_ctx;
    use gazebo::prelude::SliceExt;
    use starlark::environment::GlobalsBuilder;
    use starlark::environment::Module;
    use starlark_map::smallset;

    use super::*;
    use crate::attrs::coerce::attr_type::AttrTypeExt;
    use crate::attrs::coerce::testing::coercion_ctx;
    use crate::attrs::coerce::testing::to_value;

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
}
