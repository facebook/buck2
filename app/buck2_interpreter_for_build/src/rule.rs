/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::cell::RefCell;
use std::fmt;
use std::fmt::Display;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::cells::CellResolver;
use buck2_core::execution_types::execution_group::ExecutionGroup;
use buck2_core::execution_types::execution_group::NamedExecutionGroup;
use buck2_core::plugins::PluginKind;
use buck2_error::BuckErrorOptionContext;
use buck2_hash::BuckMutInsertionMap;
use buck2_interpreter::execution_group::ExecutionGroupArg;
use buck2_interpreter::late_binding_ty::AnalysisContextReprLate;
use buck2_interpreter::late_binding_ty::ProviderReprLate;
use buck2_interpreter::late_binding_ty::TransitionReprLate;
use buck2_interpreter::starlark_promise::StarlarkPromise;
use buck2_interpreter::types::rule::FROZEN_PROMISE_ARTIFACT_MAPPINGS_GET_IMPL;
use buck2_interpreter::types::rule::FROZEN_RULE_GET_IMPL;
use buck2_interpreter::types::transition::transition_id_from_value;
use buck2_node::attrs::attr::Attribute;
use buck2_node::attrs::spec::AttributeId;
use buck2_node::attrs::spec::AttributeSpec;
use buck2_node::bzl_or_bxl_path::BzlOrBxlPath;
use buck2_node::nodes::unconfigured::RuleKind;
use buck2_node::nodes::unconfigured::UnsealedTargetNode;
use buck2_node::rule::Rule;
use buck2_node::rule::RuleExecutionGroup;
use buck2_node::rule::RuleIncomingTransition;
use buck2_node::rule_type::RuleType;
use buck2_node::rule_type::StarlarkRuleType;
use derive_more::Display;
use dupe::Dupe;
use either::Either;
use itertools::Itertools;
use starlark::any::ProvidesStaticType;
use starlark::docs::DocFunction;
use starlark::docs::DocItem;
use starlark::docs::DocMember;
use starlark::docs::DocStringKind;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Arguments;
use starlark::eval::Evaluator;
use starlark::eval::ParametersSpec;
use starlark::starlark_module;
use starlark::typing::ParamSpec;
use starlark::typing::Ty;
use starlark::values::AllocValue;
use starlark::values::Freeze;
use starlark::values::FreezeError;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkPagable;
use starlark::values::StarlarkValue;
use starlark::values::StringValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTyped;
use starlark::values::dict::UnpackDictEntries;
use starlark::values::list::ListType;
use starlark::values::list::UnpackList;
use starlark::values::list_or_tuple::UnpackListOrTuple;
use starlark::values::starlark_value;
use starlark::values::typing::StarlarkCallable;
use starlark::values::typing::StarlarkCallableChecked;

use crate::attrs::coerce::ctx::MemoizedCoercionCtx;
use crate::attrs::starlark_attribute::StarlarkAttribute;
use crate::interpreter::build_context::BuildContext;
use crate::interpreter::build_context::PerFileTypeContext;
use crate::interpreter::extra_value::InterpreterExtraValue;
use crate::interpreter::module_internals::ModuleInternals;
use crate::nodes::attr_spec::AttributeSpecExt;
use crate::nodes::unconfigured::UnsealedTargetNodeExt;
use crate::plugins::PluginKindArg;

pub static NAME_ATTRIBUTE_FIELD: &str = "name";

#[derive(Debug, ProvidesStaticType, Trace, NoSerialize, Allocative, Clone, Copy)]
enum RuleImpl<'v> {
    BuildRule(StarlarkCallable<'v, (Value<'static>,), ListType<Value<'static>>>),
    BxlAnon(StarlarkCallable<'v, (Value<'static>, Value<'static>), ListType<Value<'static>>>),
}

/// The callable that's returned from a `rule()` call. Once frozen, and called, it adds targets'
/// parameters to the context
#[derive(Debug, ProvidesStaticType, Trace, NoSerialize, Allocative)]
pub struct StarlarkRuleCallable<'v> {
    /// The import path that contains the rule() call; stored here so we can retrieve extra
    /// information during `export_as()`
    rule_path: BzlOrBxlPath,
    /// Once exported, the `import_path` and `name` of the callable. Used in DICE to retrieve rule
    /// implementations
    id: RefCell<Option<StarlarkRuleType>>,
    /// The implementation function for this rule.
    /// If is a build rule or anon rule in bzl must take a ctx,
    /// If is a bxl anon rule must take a bxl context and attrs.
    implementation: RuleImpl<'v>,
    // Field Name -> Attribute
    attributes: AttributeSpec,
    /// Type for the typechecker.
    ty: Ty,
    /// When specified, this transition will be applied to the target before configuring it.
    cfg: RuleIncomingTransition,
    /// The plugins that are used by these targets
    uses_plugins: Vec<PluginKind>,
    /// The named execution groups these targets use
    execution_groups: Vec<RuleExecutionGroup>,
    /// Whether these targets list `DefaultExecGroup` or declare no named execution groups
    default_execution_group: bool,
    /// The attribute constraining the execution platform of the default execution group only
    default_exec_compatible_with_attr: Option<AttributeId>,
    /// This kind of the rule, e.g. whether it can be used in configuration context.
    rule_kind: RuleKind,
    /// The raw docstring for this rule
    docs: Option<String>,
    /// When evaluating rule function, take only the `name` argument, ignore the others.
    ignore_attrs_for_profiling: bool,
    /// Optional map of the promise artifact name to starlark function.
    /// `None` for normal rules, `Some` for anon targets.
    artifact_promise_mappings: Option<ArtifactPromiseMappings<'v>>,
}

/// Mappings of promise artifact name to the starlark function that will produce it, for anon targets.
#[derive(Debug, ProvidesStaticType, Trace, NoSerialize, Allocative)]
struct ArtifactPromiseMappings<'v> {
    mappings: BuckMutInsertionMap<StringValue<'v>, Value<'v>>,
}

/// Mappings of frozen promise artifact name to the frozen starlark function that will produce it, for anon targets.
#[derive(Debug, ProvidesStaticType, Allocative, StarlarkPagable)]
pub struct FrozenArtifactPromiseMappings<'v> {
    pub mappings: BuckMutInsertionMap<StringValue<'v>, Value<'v>>,
}

impl<'v> Display for StarlarkRuleCallable<'v> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.id.borrow() {
            Some(id) => write!(f, "{}()", id.name),
            None => write!(f, "<unbound rule>"),
        }
    }
}

/// Errors around rule declaration, instantiation, validation, etc
#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum RuleError {
    #[error("The output of rule() may only be called after the module is loaded")]
    RuleCalledBeforeFreezing,
    #[error("`{0}` is not a valid attribute name")]
    InvalidParameterName(String),
    #[error("Rule defined in `{0}` must be assigned to a variable, e.g. `my_rule = rule(...)`")]
    RuleNotAssigned(BzlOrBxlPath),
    #[error(
        "Rule defined with both `is_configuration_rule` and `is_toolchain_rule`, these options are mutually exclusive"
    )]
    IsConfigurationAndToolchain,
    #[error("`rule` can only be declared in bzl files")]
    RuleNotInBzl,
    #[error("Cannot specify `cfg` and `supports_incoming_transition` at the same time")]
    CfgAndSupportsIncomingTransition,
    #[error("{0} rules do not support incoming transitions")]
    RuleDoesNotSupportIncomingTransition(&'static str),
    #[error(
        "Execution groups of a rule must have distinct names; `{name}` is listed more than once{defined_in}"
    )]
    DuplicateExecutionGroup { name: String, defined_in: String },
    #[error(
        "Toolchain rules cannot use execution groups; their exec deps belong to the execution group of the rule depending on the toolchain"
    )]
    ExecutionGroupsOnToolchainRule,
    #[error(
        "Configuration rules cannot use execution groups; their targets are configured without an execution platform"
    )]
    ExecutionGroupsOnConfigurationRule,
    #[error(
        "Attribute `{attr}` uses execution group `{group}`, which the rule does not declare. Declared execution groups: [{declared}]{message}"
    )]
    UndeclaredExecutionGroup {
        attr: String,
        group: String,
        declared: String,
        message: String,
    },
    #[error(
        "Execution group `{group}` has more than one `attrs.exec_compatible_with()` attribute: `{first}` and `{second}`"
    )]
    MultipleExecCompatibleWith {
        group: String,
        first: String,
        second: String,
    },
}

impl<'v> AllocValue<'v> for StarlarkRuleCallable<'v> {
    fn alloc_value(self, heap: Heap<'v>) -> Value<'v> {
        heap.alloc_complex(self)
    }
}

impl<'v> StarlarkRuleCallable<'v> {
    fn new(
        implementation: RuleImpl<'v>,
        attrs: UnpackDictEntries<&'v str, &'v StarlarkAttribute>,
        cfg: Option<Value>,
        supports_incoming_transition: Option<bool>,
        doc: &str,
        is_configuration_rule: bool,
        is_toolchain_rule: bool,
        uses_plugins: Vec<PluginKind>,
        execution_groups: Vec<ExecutionGroup>,
        artifact_promise_mappings: Option<ArtifactPromiseMappings<'v>>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> buck2_error::Result<StarlarkRuleCallable<'v>> {
        let build_context = BuildContext::from_context(eval)?;

        let rule_path: BzlOrBxlPath = match (&build_context.additional, &implementation) {
            (PerFileTypeContext::Bzl(bzl_path), RuleImpl::BuildRule(_)) => {
                BzlOrBxlPath::Bzl(bzl_path.bzl_path.clone())
            }
            (PerFileTypeContext::Bxl(bxl_path), RuleImpl::BxlAnon(_)) => {
                BzlOrBxlPath::Bxl(bxl_path.clone())
            }
            (PerFileTypeContext::Bxl(_), RuleImpl::BuildRule(_)) => {
                return Err(RuleError::RuleNotInBzl.into());
            }
            // TODO(nero): add error for it
            (_, _) => unreachable!(
                "unreachable, since bxl.anon_rule is not registered for eval for bzl files"
            ),
        };

        let exec_compatible_with_attrs: Vec<(String, ExecutionGroup)> = attrs
            .entries
            .iter()
            .filter_map(|(name, attr)| {
                Some((
                    (*name).to_owned(),
                    attr.exec_compatible_with_group()?.dupe(),
                ))
            })
            .collect();

        let sorted_validated_attrs = attrs
            .entries
            .into_iter()
            .sorted_by(|(k1, _), (k2, _)| Ord::cmp(k1, k2))
            .map(|(name, value)| {
                if name == NAME_ATTRIBUTE_FIELD {
                    Err(RuleError::InvalidParameterName(NAME_ATTRIBUTE_FIELD.to_owned()).into())
                } else {
                    Ok((name.to_owned(), value.clone_attribute()))
                }
            })
            .collect::<buck2_error::Result<Vec<(String, Attribute)>>>()?;

        let cfg = match (cfg, supports_incoming_transition) {
            (Some(_), Some(_)) => return Err(RuleError::CfgAndSupportsIncomingTransition.into()),
            (Some(cfg), None) => RuleIncomingTransition::Fixed(transition_id_from_value(cfg)?),
            (None, Some(true)) => RuleIncomingTransition::FromAttribute,
            (None, Some(false) | None) => RuleIncomingTransition::None,
        };

        let rule_kind = match (is_configuration_rule, is_toolchain_rule) {
            (false, false) => RuleKind::Normal,
            (true, false) => RuleKind::Configuration,
            (false, true) => RuleKind::Toolchain,
            (true, true) => return Err(RuleError::IsConfigurationAndToolchain.into()),
        };

        if cfg != RuleIncomingTransition::None {
            let unsupported_rule_kind_str = match rule_kind {
                RuleKind::Normal => None,
                RuleKind::Configuration => Some("Configuration"),
                RuleKind::Toolchain => Some("Toolchain"),
            };
            if let Some(unsupported_rule_kind_str) = unsupported_rule_kind_str {
                return Err(RuleError::RuleDoesNotSupportIncomingTransition(
                    unsupported_rule_kind_str,
                )
                .into());
            }
        }

        let attributes = AttributeSpec::from(
            sorted_validated_attrs,
            artifact_promise_mappings.is_some(),
            &cfg,
        )?;

        let (execution_groups, default_execution_group, default_exec_compatible_with_attr) =
            validate_execution_groups(
                execution_groups,
                rule_kind,
                &attributes,
                exec_compatible_with_attrs,
                build_context.cell_resolver(),
            )?;
        let ty = Ty::ty_function(attributes.ty_function());

        Ok(StarlarkRuleCallable {
            rule_path,
            id: RefCell::new(None),
            implementation,
            attributes,
            ty,
            cfg,
            rule_kind,
            uses_plugins,
            execution_groups,
            default_execution_group,
            default_exec_compatible_with_attr,
            docs: Some(doc.to_owned()),
            ignore_attrs_for_profiling: build_context.ignore_attrs_for_profiling,
            artifact_promise_mappings,
        })
    }

    fn new_anon_impl(
        implementation: RuleImpl<'v>,
        attrs: UnpackDictEntries<&'v str, &'v StarlarkAttribute>,
        doc: &str,
        artifact_promise_mappings: BuckMutInsertionMap<
            StringValue<'v>,
            StarlarkCallable<'v, (Value<'static>,), UnpackList<Value<'static>>>,
        >,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> buck2_error::Result<Self> {
        Self::new(
            implementation,
            attrs,
            None,
            None,
            doc,
            false,
            false,
            Vec::new(),
            Vec::new(),
            Some(ArtifactPromiseMappings {
                mappings: artifact_promise_mappings
                    .iter()
                    .map(|(k, v)| (*k, v.0))
                    .collect::<BuckMutInsertionMap<_, _>>(),
            }),
            eval,
        )
    }

    fn new_anon(
        implementation: StarlarkCallable<'v, (Value<'static>,), ListType<Value<'static>>>,
        attrs: UnpackDictEntries<&'v str, &'v StarlarkAttribute>,
        doc: &str,
        artifact_promise_mappings: BuckMutInsertionMap<
            StringValue<'v>,
            StarlarkCallable<'v, (Value<'static>,), UnpackList<Value<'static>>>,
        >,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> buck2_error::Result<Self> {
        Self::new_anon_impl(
            RuleImpl::BuildRule(implementation),
            attrs,
            doc,
            artifact_promise_mappings,
            eval,
        )
    }

    pub fn new_bxl_anon(
        implementation: StarlarkCallable<
            'v,
            (Value<'static>, Value<'static>),
            ListType<Value<'static>>,
        >,
        attrs: UnpackDictEntries<&'v str, &'v StarlarkAttribute>,
        doc: &str,
        artifact_promise_mappings: BuckMutInsertionMap<
            StringValue<'v>,
            StarlarkCallable<'v, (Value<'static>,), UnpackList<Value<'static>>>,
        >,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> buck2_error::Result<Self> {
        Self::new_anon_impl(
            RuleImpl::BxlAnon(implementation),
            attrs,
            doc,
            artifact_promise_mappings,
            eval,
        )
    }

    fn documentation_impl(&self) -> DocItem {
        let name = self
            .id
            .borrow()
            .as_ref()
            .map_or_else(|| "unbound_rule".to_owned(), |rt| rt.name.clone());
        let parameters_spec = self.attributes.signature_with_default_value(name);
        let parameter_types = self.attributes.starlark_types();
        let parameter_docs = self.attributes.docstrings();
        let params = parameters_spec.documentation_with_default_value_formatter(
            parameter_types,
            parameter_docs,
            String::clone,
        );

        let function_docs = DocFunction::from_docstring(
            DocStringKind::Starlark,
            params,
            Ty::none(),
            self.docs.as_deref(),
        );

        DocItem::Member(DocMember::Function(function_docs))
    }
}

/// Checks the `exec_groups` a rule declares, and that every execution group its attributes
/// use is among them. Returns the named groups, whether the rule lists `DefaultExecGroup` or
/// declares no named groups, and the `attrs.exec_compatible_with()` attribute of the default group.
fn validate_execution_groups(
    execution_groups: Vec<ExecutionGroup>,
    rule_kind: RuleKind,
    attributes: &AttributeSpec,
    exec_compatible_with_attrs: Vec<(String, ExecutionGroup)>,
    cell_resolver: &CellResolver,
) -> buck2_error::Result<(Vec<RuleExecutionGroup>, bool, Option<AttributeId>)> {
    // The project-relative path of the file that defines `group`, or its cell path when that
    // does not resolve, so the error that names the file always surfaces.
    let defining_file = |group: &NamedExecutionGroup| {
        let file = group.defining_file();
        match cell_resolver.resolve_path(file) {
            Ok(path) => path.to_string(),
            Err(_) => file.to_string(),
        }
    };

    if !execution_groups.is_empty() {
        match rule_kind {
            RuleKind::Normal => {}
            RuleKind::Toolchain => return Err(RuleError::ExecutionGroupsOnToolchainRule.into()),
            RuleKind::Configuration => {
                return Err(RuleError::ExecutionGroupsOnConfigurationRule.into());
            }
        }
    }

    let mut declared: Vec<RuleExecutionGroup> = Vec::with_capacity(execution_groups.len());
    let mut default_listed = false;
    for group in execution_groups {
        let group = match group {
            ExecutionGroup::Default if default_listed => {
                return Err(RuleError::DuplicateExecutionGroup {
                    name: group.to_string(),
                    defined_in: String::new(),
                }
                .into());
            }
            ExecutionGroup::Default => {
                default_listed = true;
                continue;
            }
            ExecutionGroup::Named(group) => group,
        };
        // Names, not `(name, .bzl file)`, must be distinct: users see a target's groups by name.
        if let Some(first) = declared.iter().find(|g| g.group.name() == group.name()) {
            let defined_in = if first.group == group {
                String::new()
            } else {
                format!(
                    " (defined in `{}` and in `{}`)",
                    defining_file(&first.group),
                    defining_file(&group)
                )
            };
            return Err(RuleError::DuplicateExecutionGroup {
                name: group.name().to_owned(),
                defined_in,
            }
            .into());
        }
        declared.push(RuleExecutionGroup {
            group,
            exec_compatible_with_attr: None,
        });
    }

    let mut used: Vec<(&str, NamedExecutionGroup)> = Vec::new();
    for (name, _, attr) in attributes.attr_specs() {
        attr.coercer()
            .for_each_named_execution_group(&mut |group| used.push((name, group.dupe())));
    }
    used.extend(
        exec_compatible_with_attrs
            .iter()
            .filter_map(|(name, group)| match group {
                ExecutionGroup::Named(group) => Some((name.as_str(), group.dupe())),
                ExecutionGroup::Default => None,
            }),
    );
    if rule_kind == RuleKind::Toolchain
        && !(used.is_empty() && exec_compatible_with_attrs.is_empty())
    {
        return Err(RuleError::ExecutionGroupsOnToolchainRule.into());
    }
    if let Some((attr, group)) = used
        .into_iter()
        .find(|(_, g)| !declared.iter().any(|d| &d.group == g))
    {
        return Err(RuleError::UndeclaredExecutionGroup {
            attr: attr.to_owned(),
            group: group.name().to_owned(),
            declared: declared
                .iter()
                .map(|g| format!("`{}`", g.group))
                .collect::<Vec<_>>()
                .join(", "),
            message: match group.same_name_in(declared.iter().map(|d| &d.group)) {
                Some(d) => format!(
                    ". The declared `{}` is defined in `{}`, this one in `{}`",
                    group.name(),
                    defining_file(d),
                    defining_file(&group)
                ),
                None => String::new(),
            },
        }
        .into());
    }

    let mut default_exec_compatible_with_attr: Option<AttributeId> = None;
    for (name, group) in exec_compatible_with_attrs {
        let slot = match &group {
            ExecutionGroup::Default => &mut default_exec_compatible_with_attr,
            ExecutionGroup::Named(named) => {
                &mut declared
                    .iter_mut()
                    .find(|d| &d.group == named)
                    .expect("checked above that every used group is declared")
                    .exec_compatible_with_attr
            }
        };
        if let Some(first) = slot {
            return Err(RuleError::MultipleExecCompatibleWith {
                group: group.to_string(),
                first: attributes.attribute_name_by_id(*first).to_owned(),
                second: name,
            }
            .into());
        }
        *slot = Some(
            attributes
                .attribute_id_by_name(&name)
                .expect("every attribute of the rule is in its spec"),
        );
    }

    let default_execution_group = declared.is_empty() || default_listed;
    Ok((
        declared,
        default_execution_group,
        default_exec_compatible_with_attr,
    ))
}

#[starlark_value(type = "Rule")]
impl<'v> StarlarkValue<'v> for StarlarkRuleCallable<'v> {
    fn export_as(
        &self,
        variable_name: &str,
        _eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<()> {
        *self.id.borrow_mut() = Some(StarlarkRuleType {
            path: self.rule_path.clone(),
            name: variable_name.to_owned(),
        });
        Ok(())
    }

    fn invoke(
        &self,
        _me: Value<'v>,
        _args: &Arguments<'v, '_>,
        _eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<Value<'v>> {
        Err(buck2_error::Error::from(RuleError::RuleCalledBeforeFreezing).into())
    }

    fn documentation(&self) -> DocItem {
        self.documentation_impl()
    }

    fn typechecker_ty(&self) -> Option<Ty> {
        Some(self.ty.clone())
    }

    fn get_type_starlark_repr() -> Ty {
        Ty::function(ParamSpec::kwargs(Ty::any()), Ty::none())
    }
}

#[derive(Debug, ProvidesStaticType, Allocative, Clone, Dupe, StarlarkPagable)]
enum FrozenRuleImpl<'v> {
    BuildRule(StarlarkCallable<'v, (Value<'static>,), ListType<Value<'static>>>),
    BxlAnon(StarlarkCallable<'v, (Value<'static>, Value<'static>), ListType<Value<'static>>>),
}

impl<'v> FrozenRuleImpl<'v> {
    fn into_value(self) -> Value<'v> {
        match self {
            FrozenRuleImpl::BuildRule(callable) => callable.0,
            FrozenRuleImpl::BxlAnon(callable) => callable.0,
        }
    }
}

impl<'v> Freeze<'v> for RuleImpl<'v> {
    type Frozen<'fv> = FrozenRuleImpl<'fv>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Self::Frozen<'fv>> {
        match self {
            RuleImpl::BuildRule(impl_) => Ok(FrozenRuleImpl::BuildRule(impl_.freeze(freezer)?)),
            RuleImpl::BxlAnon(impl_) => Ok(FrozenRuleImpl::BxlAnon(impl_.freeze(freezer)?)),
        }
    }
}

impl<'v> Freeze<'v> for StarlarkRuleCallable<'v> {
    type Frozen<'fv> = FrozenStarlarkRuleCallable<'fv>;
    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Self::Frozen<'fv>> {
        let frozen_impl = self.implementation.freeze(freezer)?;
        let rule_docs = self.documentation_impl();
        let id = match self.id.into_inner() {
            Some(x) => x,
            None => {
                return Err(FreezeError::new(
                    RuleError::RuleNotAssigned(self.rule_path).to_string(),
                ));
            }
        };
        let rule_type = Arc::new(id);
        let rule_name = rule_type.name.to_owned();

        let signature = self.attributes.signature(rule_name);

        let artifact_promise_mappings = match self.artifact_promise_mappings {
            Some(artifacts) => {
                let mut mappings = BuckMutInsertionMap::new();
                for (name, implementation) in artifacts.mappings {
                    mappings.insert(name.freeze(freezer)?, implementation.freeze(freezer)?);
                }
                Some(FrozenArtifactPromiseMappings { mappings })
            }
            None => None,
        };

        Ok(FrozenStarlarkRuleCallable {
            rule: Arc::new(Rule {
                attributes: self.attributes,
                rule_type: RuleType::Starlark(rule_type.dupe()),
                cfg: self.cfg,
                rule_kind: self.rule_kind,
                uses_plugins: self.uses_plugins,
                execution_groups: self.execution_groups,
                default_execution_group: self.default_execution_group,
                default_exec_compatible_with_attr: self.default_exec_compatible_with_attr,
            }),
            rule_type,
            implementation: frozen_impl,
            signature,
            rule_docs,
            ty: self.ty,
            ignore_attrs_for_profiling: self.ignore_attrs_for_profiling,
            artifact_promise_mappings,
        })
    }
}

#[derive(
    Debug,
    Display,
    ProvidesStaticType,
    NoSerialize,
    Allocative,
    StarlarkPagable
)]
#[display("{}()", rule.rule_type.name())]
pub struct FrozenStarlarkRuleCallable<'v> {
    #[starlark_pagable(pagable)]
    rule: Arc<Rule>,
    /// Identical to `rule.rule_type` but more specific type.
    #[starlark_pagable(pagable)]
    rule_type: Arc<StarlarkRuleType>,
    implementation: FrozenRuleImpl<'v>,
    /// Defaults are read from `rule.attributes`, never from here.
    signature: ParametersSpec<Value<'v>>,
    #[starlark_pagable(pagable)]
    rule_docs: DocItem,
    #[starlark_pagable(pagable)]
    ty: Ty,
    ignore_attrs_for_profiling: bool,
    artifact_promise_mappings: Option<FrozenArtifactPromiseMappings<'v>>,
}

fn downcast_frozen_rule<'v>(
    rule: Value<'v>,
) -> buck2_error::Result<ValueTyped<'v, FrozenStarlarkRuleCallable<'v>>> {
    ValueTyped::new(rule).internal_error("Expecting FrozenRuleCallable")
}

/// The `AttributeSpec` of a frozen `rule()` / `anon_rule()` / `bxl.anon_rule()` value.
pub fn frozen_rule_attribute_spec<'v>(rule: Value<'v>) -> buck2_error::Result<&'v AttributeSpec> {
    Ok(downcast_frozen_rule(rule)?.as_ref().attributes())
}

pub(crate) fn init_frozen_rule_get_impl() {
    FROZEN_RULE_GET_IMPL.init(|rule| {
        let rule = downcast_frozen_rule(rule)?;
        Ok(rule.as_ref().implementation.dupe().into_value())
    })
}

pub(crate) fn init_frozen_promise_artifact_mappings_get_impl() {
    FROZEN_PROMISE_ARTIFACT_MAPPINGS_GET_IMPL.init(|rule| {
        let rule = downcast_frozen_rule(rule)?;
        Ok(rule
            .as_ref()
            .artifact_promise_mappings
            .as_ref()
            .map_or_else(BuckMutInsertionMap::new, |m| m.mappings.clone()))
    })
}

impl<'v> FrozenStarlarkRuleCallable<'v> {
    pub fn rule_type(&self) -> &Arc<StarlarkRuleType> {
        &self.rule_type
    }

    pub fn attributes(&self) -> &AttributeSpec {
        &self.rule.attributes
    }

    pub fn artifact_promise_mappings(&self) -> &Option<FrozenArtifactPromiseMappings<'v>> {
        &self.artifact_promise_mappings
    }
}

#[starlark_value(type = "Rule", frozen_vtable)]
impl<'v> StarlarkValue<'v> for FrozenStarlarkRuleCallable<'v> {
    type Canonical = StarlarkRuleCallable<'v>;

    fn invoke(
        &self,
        _me: Value<'v>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<Value<'v>> {
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
            let coercion_ctx = MemoizedCoercionCtx {
                base: internals.attr_coercion_context(),
                memo: &InterpreterExtraValue::get(eval.module())?.coerce_memo,
            };
            let target_node = UnsealedTargetNode::from_params(
                self.rule.dupe(),
                internals.package(),
                internals,
                &coercion_ctx,
                param_parser,
                arg_count,
                self.ignore_attrs_for_profiling,
                call_stack,
            )?;
            internals.record(target_node)?;
            Ok(Value::new_none())
        })
    }

    fn documentation(&self) -> DocItem {
        self.rule_docs.clone()
    }

    fn typechecker_ty(&self) -> Option<Ty> {
        Some(self.ty.clone())
    }

    fn get_type_starlark_repr() -> Ty {
        StarlarkRuleCallable::get_type_starlark_repr()
    }
}

#[starlark_module]
#[starlark_types(StarlarkRuleCallable<'_> as Rule)]
pub fn register_rule_function(builder: &mut GlobalsBuilder) {
    /// Define a rule. As a simple example:
    ///
    /// ```python
    /// def _my_rule(ctx: AnalysisContext) -> list[Provider]:
    ///     output = ctx.actions.write("hello.txt", ctx.attrs.contents, executable = ctx.attrs.exe)
    ///     return [DefaultInfo(outputs = [output])]
    ///
    /// MyRule = rule(impl = _my_rule, attrs = {
    ///     "contents": attrs.string(),
    ///     "exe": attrs.option(attrs.bool(), default = False),
    /// })
    /// ```
    fn rule<'v>(
        #[starlark(require = named)] r#impl: StarlarkCallableChecked<
            'v,
            (AnalysisContextReprLate,),
            Either<ListType<ProviderReprLate>, StarlarkPromise>,
        >,
        #[starlark(require = named)] attrs: UnpackDictEntries<&'v str, &'v StarlarkAttribute>,
        #[starlark(require = named)] cfg: Option<ValueOfUnchecked<'v, TransitionReprLate>>,
        #[starlark(require = named)] supports_incoming_transition: Option<bool>,
        #[starlark(require = named, default = "")] doc: &str,
        #[starlark(require = named, default = false)] is_configuration_rule: bool,
        #[starlark(require = named, default = false)] is_toolchain_rule: bool,
        #[starlark(require = named, default = UnpackListOrTuple::default())]
        uses_plugins: UnpackListOrTuple<PluginKindArg>,
        #[starlark(require = named, default = UnpackListOrTuple::default())]
        exec_groups: UnpackListOrTuple<ExecutionGroupArg>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<StarlarkRuleCallable<'v>> {
        Ok(StarlarkRuleCallable::new(
            RuleImpl::BuildRule(StarlarkCallable::unchecked_new(r#impl.0)),
            attrs,
            cfg.map(|v| v.get()),
            supports_incoming_transition,
            doc,
            is_configuration_rule,
            is_toolchain_rule,
            uses_plugins
                .items
                .into_iter()
                .map(|PluginKindArg { plugin_kind }| plugin_kind)
                .collect(),
            exec_groups
                .items
                .into_iter()
                .map(|ExecutionGroupArg { group }| group)
                .collect(),
            None,
            eval,
        )?)
    }

    /// Define an anon rule, similar to how a normal rule is defined, except with an extra `artifact_promise_mappings` field. This
    /// is a dict where the keys are the string name of the artifact, and the values are the callable functions that produce
    /// the artifact. This is only intended to be used with anon targets.
    fn anon_rule<'v>(
        #[starlark(require = named)] r#impl: StarlarkCallable<
            'v,
            (Value<'static>,),
            ListType<Value<'static>>,
        >,
        #[starlark(require = named)] attrs: UnpackDictEntries<&'v str, &'v StarlarkAttribute>,
        #[starlark(require = named, default = "")] doc: &str,
        #[starlark(require = named)] artifact_promise_mappings: BuckMutInsertionMap<
            StringValue<'v>,
            StarlarkCallable<'v, (Value<'static>,), UnpackList<Value<'static>>>,
        >,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<StarlarkRuleCallable<'v>> {
        StarlarkRuleCallable::new_anon(r#impl, attrs, doc, artifact_promise_mappings, eval)
            .map_err(Into::into)
    }
}
