/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;
use std::fmt;
use std::fmt::Display;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::bzl::ImportPath;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_node::attrs::attr::Attribute;
use buck2_node::attrs::spec::AttributeSpec;
use buck2_node::nodes::unconfigured::RuleKind;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::rule::Rule;
use buck2_node::rule_type::RuleType;
use buck2_node::rule_type::StarlarkRuleType;
use derive_more::Display;
use dupe::Dupe;
use gazebo::prelude::*;
use itertools::Itertools;
use starlark::any::ProvidesStaticType;
use starlark::docs::DocFunction;
use starlark::docs::DocItem;
use starlark::docs::DocStringKind;
use starlark::docs::DocType;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Arguments;
use starlark::eval::Evaluator;
use starlark::eval::ParametersSpec;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::starlark_type;
use starlark::values::dict::DictOf;
use starlark::values::AllocValue;
use starlark::values::Freeze;
use starlark::values::Freezer;
use starlark::values::FrozenValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;

use crate::attrs::attribute_as_starlark_value::AttributeAsStarlarkValue;
use crate::interpreter::build_context::BuildContext;
use crate::interpreter::module_internals::ModuleInternals;
use crate::nodes::attr_spec::AttributeSpecExt;
use crate::nodes::unconfigured::TargetNodeExt;
use crate::transition::transition_id_from_value;

pub static NAME_ATTRIBUTE_FIELD: &str = "name";

/// The callable that's returned from a `rule()` call. Once frozen, and called, it adds targets'
/// parameters to the context
#[derive(Debug, ProvidesStaticType, Trace, NoSerialize, Allocative)]
struct RuleCallable<'v> {
    /// The import path that contains the rule() call; stored here so we can retrieve extra
    /// information during `export_as()`
    import_path: ImportPath,
    /// Once exported, the `import_path` and `name` of the callable. Used in DICE to retrieve rule
    /// implementations
    id: RefCell<Option<StarlarkRuleType>>,
    /// The implementation function for this rule. Must be callable and take a
    /// ctx
    implementation: Value<'v>,
    // Field Name -> Attribute
    attributes: AttributeSpec,
    /// When specified, this transition will be applied to the target before configuring it.
    cfg: Option<Arc<TransitionId>>,
    /// This kind of the rule, e.g. whether it can be used in configuration context.
    rule_kind: RuleKind,
    /// The raw docstring for this rule
    docs: Option<String>,
    /// When evaluating rule function, take only the `name` argument, ignore the others.
    ignore_attrs_for_profiling: bool,
}

impl<'v> Display for RuleCallable<'v> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.id.borrow() {
            Some(id) => write!(f, "{}()", id.name),
            None => write!(f, "<unbound rule>"),
        }
    }
}

/// Errors around rule declaration, instantiation, validation, etc
#[derive(Debug, thiserror::Error)]
enum RuleError {
    #[error("The output of rule() may only be called after the module is loaded")]
    RuleCalledBeforeFreezing,
    #[error("`{0}` is not a valid attribute name")]
    InvalidParameterName(String),
    #[error("Rule defined in `{0}` must be assigned to a variable, e.g. `my_rule = rule(...)`")]
    RuleNotAssigned(ImportPath),
    #[error(
        "Rule defined with both `is_configuration_rule` and `is_toolchain_rule`, these options are mutually exclusive"
    )]
    IsConfigurationAndToolchain,
}

impl<'v> AllocValue<'v> for RuleCallable<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex(self)
    }
}

impl<'v> StarlarkValue<'v> for RuleCallable<'v> {
    starlark_type!("rule");

    fn export_as(&self, variable_name: &str, _eval: &mut Evaluator<'v, '_>) {
        *self.id.borrow_mut() = Some(StarlarkRuleType {
            import_path: self.import_path.clone(),
            name: variable_name.to_owned(),
        });
    }

    fn invoke(
        &self,
        _me: Value<'v>,
        _args: &Arguments<'v, '_>,
        _eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        Err(RuleError::RuleCalledBeforeFreezing.into())
    }

    fn documentation(&self) -> Option<DocItem> {
        let name = self
            .id
            .borrow()
            .as_ref()
            .map_or_else(|| "unbound_rule".to_owned(), |rt| rt.name.clone());
        // TODO(nmj): These return 'None' for default values right now. It's going to take some
        //            refactoring to get that pulled out of the attributespec
        let parameters_spec = self.attributes.signature(name);

        let parameter_types = self
            .attributes
            .starlark_types()
            .into_iter()
            .enumerate()
            .map(|(i, t)| (i, DocType { raw_type: t }))
            .collect();
        let parameter_docs = self.attributes.docstrings();
        let function_docs = DocFunction::from_docstring(
            DocStringKind::Starlark,
            parameters_spec.documentation(parameter_types, parameter_docs),
            Some(DocType {
                raw_type: Value::new_none().to_string(),
            }),
            self.docs.as_deref(),
        );

        Some(DocItem::Function(function_docs))
    }
}

impl<'v> Freeze for RuleCallable<'v> {
    type Frozen = FrozenRuleCallable;
    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        let frozen_impl = self.implementation.freeze(freezer)?;
        let rule_docs = self.documentation();
        let id = match self.id.into_inner() {
            Some(x) => x,
            None => return Err(RuleError::RuleNotAssigned(self.import_path).into()),
        };
        let rule_type = Arc::new(id);
        let rule_name = rule_type.name.to_owned();
        let signature = self.attributes.signature(rule_name).freeze(freezer)?;

        Ok(FrozenRuleCallable {
            rule: Arc::new(Rule {
                attributes: self.attributes,
                rule_type: RuleType::Starlark(rule_type.dupe()),
                cfg: self.cfg,
                rule_kind: self.rule_kind,
            }),
            rule_type,
            implementation: frozen_impl,
            signature,
            rule_docs,
            ignore_attrs_for_profiling: self.ignore_attrs_for_profiling,
        })
    }
}

#[derive(Debug, Display, ProvidesStaticType, NoSerialize, Allocative)]
#[display(fmt = "{}()", "rule.rule_type.name()")]
pub struct FrozenRuleCallable {
    rule: Arc<Rule>,
    /// Identical to `rule.rule_type` but more specific type.
    rule_type: Arc<StarlarkRuleType>,
    implementation: FrozenValue,
    signature: ParametersSpec<FrozenValue>,
    rule_docs: Option<DocItem>,
    ignore_attrs_for_profiling: bool,
}
starlark_simple_value!(FrozenRuleCallable);

impl FrozenRuleCallable {
    pub fn implementation(&self) -> FrozenValue {
        self.implementation
    }

    pub fn rule_type(&self) -> &Arc<StarlarkRuleType> {
        &self.rule_type
    }

    pub fn attributes(&self) -> &AttributeSpec {
        &self.rule.attributes
    }
}

impl<'v> StarlarkValue<'v> for FrozenRuleCallable {
    starlark_type!("rule");

    fn invoke(
        &self,
        _me: Value<'v>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let record_target_call_stack =
            ModuleInternals::from_context(eval, self.rule.rule_type.name())?
                .record_target_call_stacks();
        let call_stack = if record_target_call_stack {
            Some(eval.call_stack())
        } else {
            None
        };
        let arg_count = args.len()?;
        self.signature.parser(args, eval, |param_parser, eval| {
            // The body of the callable returned by `rule()`.
            // Records the target in this package's `TargetMap`.
            let internals = ModuleInternals::from_context(eval, self.rule.rule_type.name())?;
            let target_node = TargetNode::from_params(
                self.rule.dupe(),
                internals.package(),
                internals,
                param_parser,
                arg_count,
                self.ignore_attrs_for_profiling,
                call_stack,
            )?;
            internals.record(target_node)?;
            Ok(Value::new_none())
        })
    }

    fn documentation(&self) -> Option<DocItem> {
        self.rule_docs.clone()
    }
}

#[starlark_module]
pub fn register_rule_function(builder: &mut GlobalsBuilder) {
    fn rule<'v>(
        #[starlark(require = named)] r#impl: Value<'v>,
        #[starlark(require = named)] attrs: DictOf<'v, &'v str, &'v AttributeAsStarlarkValue>,
        #[starlark(require = named)] cfg: Option<Value>,
        #[starlark(require = named, default = "")] doc: &str,
        #[starlark(require = named, default = false)] is_configuration_rule: bool,
        #[starlark(require = named, default = false)] is_toolchain_rule: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<RuleCallable<'v>> {
        // TODO(nmj): Add default attributes in here like 'name', 'visibility', etc
        // TODO(nmj): Verify that names are valid. This is technically handled by the Params
        //                 objects, but will blow up in a friendlier way here.

        let implementation = r#impl;

        let build_context = BuildContext::from_context(eval)?;
        let bzl_path = (*build_context
            .starlark_path
            .unpack_load_file()
            .ok_or_else(|| anyhow::anyhow!("`rule` can only be declared in bzl files"))?)
        .clone();
        let sorted_validated_attrs = attrs
            .to_dict()
            .into_iter()
            .sorted_by(|(k1, _), (k2, _)| Ord::cmp(k1, k2))
            .map(|(name, value)| {
                if name == NAME_ATTRIBUTE_FIELD {
                    Err(RuleError::InvalidParameterName(NAME_ATTRIBUTE_FIELD.to_owned()).into())
                } else {
                    Ok((name.to_owned(), value.clone_attribute()))
                }
            })
            .collect::<anyhow::Result<Vec<(String, Attribute)>>>()?;

        let cfg = cfg.try_map(transition_id_from_value)?;

        let rule_kind = match (is_configuration_rule, is_toolchain_rule) {
            (false, false) => RuleKind::Normal,
            (true, false) => RuleKind::Configuration,
            (false, true) => RuleKind::Toolchain,
            (true, true) => return Err(RuleError::IsConfigurationAndToolchain.into()),
        };

        Ok(RuleCallable {
            import_path: bzl_path,
            id: RefCell::new(None),
            implementation,
            attributes: AttributeSpec::from(sorted_validated_attrs)?,
            cfg,
            rule_kind,
            docs: Some(doc.to_owned()),
            ignore_attrs_for_profiling: build_context.ignore_attrs_for_profiling,
        })
    }
}
