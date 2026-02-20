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
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::plugins::PluginKind;
use buck2_error::internal_error;
use buck2_interpreter::late_binding_ty::AnalysisContextReprLate;
use buck2_interpreter::late_binding_ty::ProviderReprLate;
use buck2_interpreter::late_binding_ty::TransitionReprLate;
use buck2_interpreter::starlark_promise::StarlarkPromise;
use buck2_interpreter::types::rule::FROZEN_PROMISE_ARTIFACT_MAPPINGS_GET_IMPL;
use buck2_interpreter::types::rule::FROZEN_RULE_GET_IMPL;
use buck2_interpreter::types::transition::transition_id_from_value;
use buck2_node::attrs::attr::Attribute;
use buck2_node::attrs::display::AttrDisplayWithContextExt;
use buck2_node::attrs::spec::AttributeSpec;
use buck2_node::bzl_or_bxl_path::BzlOrBxlPath;
use buck2_node::nodes::unconfigured::RuleKind;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::rule::Rule;
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
use starlark::starlark_simple_value;
use starlark::typing::ParamSpec;
use starlark::typing::Ty;
use starlark::values::AllocValue;
use starlark::values::Freeze;
use starlark::values::FreezeError;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::FrozenRef;
use starlark::values::FrozenStringValue;
use starlark::values::FrozenValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::StringValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueOfUnchecked;
use starlark::values::dict::UnpackDictEntries;
use starlark::values::list::ListType;
use starlark::values::list::UnpackList;
use starlark::values::list_or_tuple::UnpackListOrTuple;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::typing::FrozenStarlarkCallable;
use starlark::values::typing::StarlarkCallable;
use starlark::values::typing::StarlarkCallableChecked;
use starlark_map::small_map::SmallMap;

use crate::attrs::starlark_attribute::StarlarkAttribute;
use crate::interpreter::build_context::BuildContext;
use crate::interpreter::build_context::PerFileTypeContext;
use crate::interpreter::module_internals::ModuleInternals;
use crate::nodes::attr_spec::AttributeSpecExt;
use crate::nodes::unconfigured::TargetNodeExt;
use crate::plugins::PluginKindArg;

pub static NAME_ATTRIBUTE_FIELD: &str = "name";

#[derive(Debug, ProvidesStaticType, Trace, NoSerialize, Allocative, Clone, Copy)]
enum RuleImpl<'v> {
    BuildRule(StarlarkCallable<'v, (FrozenValue,), ListType<FrozenValue>>),
    BxlAnon(StarlarkCallable<'v, (FrozenValue, FrozenValue), ListType<FrozenValue>>),
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
    mappings: SmallMap<StringValue<'v>, Value<'v>>,
}

/// Mappings of frozen promise artifact name to the frozen starlark function that will produce it, for anon targets.
#[derive(Debug, ProvidesStaticType, Trace, Allocative)]
pub struct FrozenArtifactPromiseMappings {
    pub mappings: SmallMap<FrozenStringValue, FrozenValue>,
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
    RuleNonInBzl,
    #[error("Cannot specify `cfg` and `supports_incoming_transition` at the same time")]
    CfgAndSupportsIncomingTransition,
    #[error("{0} rules do not support incoming transitions")]
    RuleDoesNotSupportIncomingTransition(&'static str),
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
                return Err(RuleError::RuleNonInBzl.into());
            }
            // TODO(nero): add error for it
            (_, _) => unreachable!(
                "unreachable, since bxl.anon_rule is not registered for eval for bzl files"
            ),
        };

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
            docs: Some(doc.to_owned()),
            ignore_attrs_for_profiling: build_context.ignore_attrs_for_profiling,
            artifact_promise_mappings,
        })
    }

    fn new_anon_impl(
        implementation: RuleImpl<'v>,
        attrs: UnpackDictEntries<&'v str, &'v StarlarkAttribute>,
        doc: &str,
        artifact_promise_mappings: SmallMap<
            StringValue<'v>,
            StarlarkCallable<'v, (FrozenValue,), UnpackList<FrozenValue>>,
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
            Some(ArtifactPromiseMappings {
                mappings: artifact_promise_mappings
                    .iter()
                    .map(|(k, v)| (*k, v.0))
                    .collect::<SmallMap<_, _>>(),
            }),
            eval,
        )
    }

    fn new_anon(
        implementation: StarlarkCallable<'v, (FrozenValue,), ListType<FrozenValue>>,
        attrs: UnpackDictEntries<&'v str, &'v StarlarkAttribute>,
        doc: &str,
        artifact_promise_mappings: SmallMap<
            StringValue<'v>,
            StarlarkCallable<'v, (FrozenValue,), UnpackList<FrozenValue>>,
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
        implementation: StarlarkCallable<'v, (FrozenValue, FrozenValue), ListType<FrozenValue>>,
        attrs: UnpackDictEntries<&'v str, &'v StarlarkAttribute>,
        doc: &str,
        artifact_promise_mappings: SmallMap<
            StringValue<'v>,
            StarlarkCallable<'v, (FrozenValue,), UnpackList<FrozenValue>>,
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
            |v| v.as_display_no_ctx().to_string(),
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

#[derive(Debug, ProvidesStaticType, Allocative, Clone, Dupe)]
enum FrozenRuleImpl {
    BuildRule(FrozenStarlarkCallable<(FrozenValue,), ListType<FrozenValue>>),
    BxlAnon(FrozenStarlarkCallable<(FrozenValue, FrozenValue), ListType<FrozenValue>>),
}

impl FrozenRuleImpl {
    fn into_frozen_value(self) -> FrozenValue {
        match self {
            FrozenRuleImpl::BuildRule(callable) => callable.0,
            FrozenRuleImpl::BxlAnon(callable) => callable.0,
        }
    }
}

impl<'v> Freeze for RuleImpl<'v> {
    type Frozen = FrozenRuleImpl;

    fn freeze(self, freezer: &Freezer) -> FreezeResult<Self::Frozen> {
        match self {
            RuleImpl::BuildRule(impl_) => Ok(FrozenRuleImpl::BuildRule(impl_.freeze(freezer)?)),
            RuleImpl::BxlAnon(impl_) => Ok(FrozenRuleImpl::BxlAnon(impl_.freeze(freezer)?)),
        }
    }
}

impl<'v> Freeze for StarlarkRuleCallable<'v> {
    type Frozen = FrozenStarlarkRuleCallable;
    fn freeze(self, freezer: &Freezer) -> FreezeResult<Self::Frozen> {
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

        // For StarlarkRuleCallable, it doesn't rely on `signature` to get the default value, instead we get the default value from `Rule.attributes`,
        // so use `signature(rule_name)` method here.
        // TODO(nero): It need to some refactor to make it more clear, e.g. add a new type `ParametersSpec<NoDefaults>` here.
        let signature = self.attributes.signature(rule_name).freeze(freezer)?;

        let artifact_promise_mappings = match self.artifact_promise_mappings {
            Some(artifacts) => {
                let mut mappings = SmallMap::new();
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

#[derive(Debug, Display, ProvidesStaticType, NoSerialize, Allocative)]
#[display("{}()", rule.rule_type.name())]
pub struct FrozenStarlarkRuleCallable {
    rule: Arc<Rule>,
    /// Identical to `rule.rule_type` but more specific type.
    rule_type: Arc<StarlarkRuleType>,
    implementation: FrozenRuleImpl,
    /// We don't need rely on `signature` to get the default value here, instead we get the default
    /// value from `Rule.attributes`. So use in the ParametersSpecNoDefaults for more clarity
    signature: ParametersSpec<FrozenValue>,
    rule_docs: DocItem,
    ty: Ty,
    ignore_attrs_for_profiling: bool,
    artifact_promise_mappings: Option<FrozenArtifactPromiseMappings>,
}
starlark_simple_value!(FrozenStarlarkRuleCallable);

fn unpack_frozen_rule(
    rule: FrozenValue,
) -> buck2_error::Result<FrozenRef<'static, FrozenStarlarkRuleCallable>> {
    rule.downcast_frozen_ref::<FrozenStarlarkRuleCallable>()
        .ok_or_else(|| internal_error!("Expecting FrozenRuleCallable"))
}

pub(crate) fn init_frozen_rule_get_impl() {
    FROZEN_RULE_GET_IMPL.init(|rule| {
        let rule = unpack_frozen_rule(rule)?;
        Ok(rule.implementation.dupe().into_frozen_value())
    })
}

pub(crate) fn init_frozen_promise_artifact_mappings_get_impl() {
    FROZEN_PROMISE_ARTIFACT_MAPPINGS_GET_IMPL.init(|rule| {
        let rule = unpack_frozen_rule(rule)?;
        Ok(rule
            .artifact_promise_mappings
            .as_ref()
            .map_or_else(SmallMap::new, |m| m.mappings.clone()))
    })
}

impl FrozenStarlarkRuleCallable {
    pub fn rule_type(&self) -> &Arc<StarlarkRuleType> {
        &self.rule_type
    }

    pub fn attributes(&self) -> &AttributeSpec {
        &self.rule.attributes
    }

    pub fn artifact_promise_mappings(&self) -> &Option<FrozenArtifactPromiseMappings> {
        &self.artifact_promise_mappings
    }
}

#[starlark_value(type = "Rule")]
impl<'v> StarlarkValue<'v> for FrozenStarlarkRuleCallable {
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
            (FrozenValue,),
            ListType<FrozenValue>,
        >,
        #[starlark(require = named)] attrs: UnpackDictEntries<&'v str, &'v StarlarkAttribute>,
        #[starlark(require = named, default = "")] doc: &str,
        #[starlark(require = named)] artifact_promise_mappings: SmallMap<
            StringValue<'v>,
            StarlarkCallable<'v, (FrozenValue,), UnpackList<FrozenValue>>,
        >,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<StarlarkRuleCallable<'v>> {
        StarlarkRuleCallable::new_anon(r#impl, attrs, doc, artifact_promise_mappings, eval)
            .map_err(Into::into)
    }

    /// Type symbol for Rule.
    const Rule: StarlarkValueAsType<StarlarkRuleCallable> = StarlarkValueAsType::new();
}
