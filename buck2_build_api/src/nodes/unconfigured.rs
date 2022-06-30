/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use anyhow::Context;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::target::TargetLabel;
use buck2_core::target::TargetName;
use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coerced_deps_collector::CoercedDepsCollector;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::attrs::internal::NAME_ATTRIBUTE_FIELD;
use buck2_node::attrs::internal::VISIBILITY_ATTRIBUTE_FIELD;
use buck2_node::attrs::spec::AttributeSpec;
use buck2_node::attrs::values::AttrValues;
use buck2_node::call_stack::StarlarkCallStack;
use buck2_node::nodes::unconfigured::RuleKind;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::rule_type::RuleType;
use buck2_node::rule_type::StarlarkRuleType;
use buck2_node::visibility::VisibilityPattern;
use buck2_node::visibility::VisibilitySpecification;
use gazebo::dupe::Dupe;
use starlark::eval::CallStack;
use starlark::eval::ParametersParser;
use starlark::values::Value;

use crate::interpreter::module_internals::ModuleInternals;
use crate::nodes::attr_spec::AttributeSpecExt;

pub(crate) trait TargetNodeExt: Sized {
    fn from_params_ignore_attrs_for_profiling<'v>(
        internals: &ModuleInternals,
        cfg: Option<Arc<TransitionId>>,
        param_parser: ParametersParser<'v, '_>,
        rule_type: Arc<StarlarkRuleType>,
        buildfile_path: Arc<BuildFilePath>,
        rule_kind: RuleKind,
        attr_spec: Arc<AttributeSpec>,
    ) -> anyhow::Result<Self>;

    fn from_params<'v>(
        internals: &ModuleInternals,
        cfg: Option<Arc<TransitionId>>,
        param_parser: ParametersParser<'v, '_>,
        arg_count: usize,
        ignore_attrs_for_profiling: bool,
        rule_type: Arc<StarlarkRuleType>,
        buildfile_path: Arc<BuildFilePath>,
        rule_kind: RuleKind,
        attr_spec: Arc<AttributeSpec>,
        call_stack: Option<CallStack>,
    ) -> anyhow::Result<Self>;
}

impl TargetNodeExt for TargetNode {
    /// Extact only the name attribute from rule arguments, ignore the others.
    fn from_params_ignore_attrs_for_profiling<'v>(
        internals: &ModuleInternals,
        cfg: Option<Arc<TransitionId>>,
        mut param_parser: ParametersParser<'v, '_>,
        rule_type: Arc<StarlarkRuleType>,
        buildfile_path: Arc<BuildFilePath>,
        rule_kind: RuleKind,
        attr_spec: Arc<AttributeSpec>,
    ) -> anyhow::Result<Self> {
        for (attr_name, _attr_idx, _attr) in attr_spec.attr_specs() {
            let value: Value = param_parser.next(attr_name)?;
            if attr_name == NAME_ATTRIBUTE_FIELD {
                let label = TargetLabel::new(
                    internals.buildfile_path().package().dupe(),
                    TargetName::new(value.unpack_str().unwrap()).unwrap(),
                );
                return Ok(TargetNode::new(
                    label,
                    RuleType::Starlark(rule_type),
                    buildfile_path,
                    rule_kind,
                    cfg,
                    attr_spec.dupe(),
                    AttrValues::with_capacity(0),
                    CoercedDepsCollector::new(),
                    VisibilitySpecification::Public,
                    None,
                ));
            }
        }
        unreachable!("`name` attribute not found");
    }

    /// The body of the callable returned by `rule()`. Records the target in this package's `TargetMap`
    #[allow(clippy::box_collection)] // Parameter `call_stack`, because this is the field type.
    fn from_params<'v>(
        internals: &ModuleInternals,
        cfg: Option<Arc<TransitionId>>,
        param_parser: ParametersParser<'v, '_>,
        arg_count: usize,
        ignore_attrs_for_profiling: bool,
        rule_type: Arc<StarlarkRuleType>,
        buildfile_path: Arc<BuildFilePath>,
        rule_kind: RuleKind,
        attr_spec: Arc<AttributeSpec>,
        call_stack: Option<CallStack>,
    ) -> anyhow::Result<Self> {
        if ignore_attrs_for_profiling {
            return Self::from_params_ignore_attrs_for_profiling(
                internals,
                cfg,
                param_parser,
                rule_type,
                buildfile_path,
                rule_kind,
                attr_spec,
            );
        }

        let (target_name, attr_values) =
            attr_spec.parse_params(param_parser, arg_count, internals)?;
        let package = internals.buildfile_path().package();

        let mut visibility = match attr_spec.attr_or_none(
            &attr_values,
            VISIBILITY_ATTRIBUTE_FIELD,
            AttrInspectOptions::All,
        ) {
            Some(visibility) => parse_visibility(internals.attr_coercion_context(), visibility)
                .context("When parsing `visibility` attribute")?,
            None => VisibilitySpecification::Default,
        };

        if internals.default_visibility_to_public()
            && visibility == VisibilitySpecification::Default
        {
            visibility = VisibilitySpecification::Public;
        }

        let label = TargetLabel::new(package.dupe(), target_name);
        let mut deps_cache = CoercedDepsCollector::new();

        for (_, value) in attr_spec.attrs(&attr_values, AttrInspectOptions::All) {
            value.traverse(&mut deps_cache)?;
        }

        Ok(TargetNode::new(
            label,
            RuleType::Starlark(rule_type),
            buildfile_path,
            rule_kind,
            cfg,
            attr_spec,
            attr_values,
            deps_cache,
            visibility,
            call_stack.map(StarlarkCallStack::new),
        ))
    }
}

fn parse_visibility(
    ctx: &dyn AttrCoercionContext,
    attr: &CoercedAttr,
) -> anyhow::Result<VisibilitySpecification> {
    let visibility = match attr {
        CoercedAttr::Literal(AttrLiteral::List(values, _)) => values,
        CoercedAttr::Literal(_) => {
            unreachable!("coercion of visibility verified the type")
        }
        CoercedAttr::Selector(_) | CoercedAttr::Concat(_) => {
            unreachable!("coercion of visibility verified it's not configurable")
        }
    };

    let mut specs = Vec::new();
    for item in visibility.iter() {
        let value = match item {
            CoercedAttr::Literal(AttrLiteral::String(value)) => value,
            CoercedAttr::Literal(_) => {
                unreachable!("coercion of visibility verified the type")
            }
            CoercedAttr::Selector(_) | CoercedAttr::Concat(_) => {
                unreachable!("coercion of visibility verified it's not configurable")
            }
        };

        if value == "PUBLIC" {
            // TODO(cjhopman): We should probably enforce that this is the only entry.
            return Ok(VisibilitySpecification::Public);
        }

        specs.push(VisibilityPattern(ctx.coerce_target_pattern(value)?));
    }
    if specs.is_empty() {
        Ok(VisibilitySpecification::Default)
    } else {
        Ok(VisibilitySpecification::VisibleTo(specs))
    }
}

pub mod testing {

    use buck2_node::attrs::inspect_options::AttrInspectOptions;
    use buck2_node::nodes::unconfigured::TargetsMap;
    use serde_json::map::Map;
    use serde_json::value::Value;

    use crate::nodes::hacks::value_to_json;

    /// Take a TargetsMap and convert it to a nice json representation. Adds in a __type__ attr
    /// for each target's values to make it clear what the rule type is. That can probably go
    /// away eventually.
    pub fn targets_to_json(target: &TargetsMap, opts: AttrInspectOptions) -> anyhow::Result<Value> {
        let map: Map<String, Value> = target
            .iter()
            .map(|(target_name, values)| {
                let mut json_values: Map<String, Value> = values
                    .attrs(opts)
                    .map(|(key, value)| Ok((key.to_owned(), value_to_json(value)?)))
                    .collect::<anyhow::Result<Map<String, Value>>>()?;
                json_values.insert(
                    "__type__".to_owned(),
                    Value::String(values.rule_type().to_string()),
                );
                Ok((target_name.to_string(), Value::from(json_values)))
            })
            .collect::<anyhow::Result<Map<String, Value>>>()?;
        Ok(Value::from(map))
    }
}
