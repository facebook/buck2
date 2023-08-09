/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;
use std::cell::RefMut;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

use allocative::Allocative;
use buck2_execute::digest_config::DigestConfig;
use buck2_interpreter::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use buck2_util::late_binding::LateBinding;
use derive_more::Display;
use dice::DiceComputations;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::typing::Ty;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::structs::StructRef;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTyped;
use starlark::values::ValueTypedComplex;

use crate::analysis::registry::AnalysisRegistry;
use crate::interpreter::rule_defs::plugins::AnalysisPlugins;

/// Functions to allow users to interact with the Actions registry.
///
/// Accessed via `ctx.actions.<function>`
#[derive(
    ProvidesStaticType,
    Debug,
    Display,
    Trace,
    NoSerialize,
    Allocative,
    StarlarkDocs
)]
#[display(fmt = "<ctx.actions>")]
pub struct AnalysisActions<'v> {
    /// Use a RefCell/Option so when we are done with it, without obtaining exclusive access,
    /// we can take the internal state without having to clone it.
    pub state: RefCell<Option<AnalysisRegistry<'v>>>,
    /// Copies from the ctx, so we can capture them for `dynamic`.
    pub attributes: ValueOfUnchecked<'v, StructRef<'v>>,
    pub plugins: ValueTypedComplex<'v, AnalysisPlugins<'v>>,
    /// Digest configuration to use when interpreting digests passed in analysis.
    pub digest_config: DigestConfig,
}

impl<'v> AnalysisActions<'v> {
    pub fn state(&self) -> RefMut<AnalysisRegistry<'v>> {
        RefMut::map(self.state.borrow_mut(), |x| {
            x.as_mut().expect("state to be present during execution")
        })
    }

    pub async fn run_promises(
        &self,
        dice: &DiceComputations,
        eval: &mut Evaluator<'v, '_>,
        description: String,
    ) -> anyhow::Result<()> {
        // We need to loop here because running the promises evaluates promise.map, which might produce more promises.
        // We keep going until there are no promises left.
        loop {
            let promises = self.state().take_promises();
            if let Some(promises) = promises {
                promises
                    .run_promises(dice, eval, description.clone())
                    .await?;
            } else {
                break;
            }
        }
        Ok(())
    }
}

#[starlark_value(type = "actions", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for AnalysisActions<'v> {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(|builder| {
            (ANALYSIS_ACTIONS_METHODS_ACTIONS.get().unwrap())(builder);
            (ANALYSIS_ACTIONS_METHODS_ANON_TARGET.get().unwrap())(builder);
        })
    }
}

impl<'v> AllocValue<'v> for AnalysisActions<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

struct RefAnalysisAction<'v>(&'v AnalysisActions<'v>);

impl<'v> StarlarkTypeRepr for RefAnalysisAction<'v> {
    fn starlark_type_repr() -> Ty {
        AnalysisActions::starlark_type_repr()
    }
}

impl<'v> UnpackValue<'v> for RefAnalysisAction<'v> {
    fn unpack_value(value: Value<'v>) -> Option<Self> {
        Some(RefAnalysisAction(
            value.downcast_ref::<AnalysisActions>().unwrap(),
        ))
    }
}

#[derive(
    ProvidesStaticType,
    Debug,
    Trace,
    NoSerialize,
    Allocative,
    StarlarkDocs
)]
pub struct AnalysisContext<'v> {
    attrs: ValueOfUnchecked<'v, StructRef<'v>>,
    pub actions: ValueTyped<'v, AnalysisActions<'v>>,
    /// Only `None` when running a `dynamic_output` action from Bxl.
    label: Option<ValueTyped<'v, StarlarkConfiguredProvidersLabel>>,
    plugins: ValueTypedComplex<'v, AnalysisPlugins<'v>>,
}

impl<'v> Display for AnalysisContext<'v> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "<ctx")?;
        if let Some(label) = &self.label {
            write!(f, " label=\"{}\"", label)?;
        }
        write!(f, " attrs=...")?;
        write!(f, " actions=...")?;
        write!(f, ">")?;
        Ok(())
    }
}

impl<'v> AnalysisContext<'v> {
    /// The context that is provided to users' UDR implementation functions. Comprised of things like attribute values, actions, etc
    pub fn new(
        heap: &'v Heap,
        attrs: Value<'v>,
        label: Option<ValueTyped<'v, StarlarkConfiguredProvidersLabel>>,
        plugins: ValueTypedComplex<'v, AnalysisPlugins<'v>>,
        registry: AnalysisRegistry<'v>,
        digest_config: DigestConfig,
    ) -> Self {
        let attrs = ValueOfUnchecked::new_checked(attrs).unwrap();

        Self {
            attrs,
            actions: heap.alloc_typed(AnalysisActions {
                state: RefCell::new(Some(registry)),
                attributes: attrs,
                plugins,
                digest_config,
            }),
            label,
            plugins,
        }
    }

    pub(crate) fn assert_no_promises(&self) -> anyhow::Result<()> {
        self.actions.state().assert_no_promises()
    }

    /// Must take an `AnalysisContext` which has never had `take_state` called on it before.
    pub fn take_state(&self) -> AnalysisRegistry<'v> {
        self.actions
            .state
            .borrow_mut()
            .take()
            .expect("nothing to have stolen state yet")
    }
}

#[starlark_value(type = "context")]
impl<'v> StarlarkValue<'v> for AnalysisContext<'v> {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(analysis_context_methods)
    }
}

impl<'v> AllocValue<'v> for AnalysisContext<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

struct RefAnalysisContext<'v>(&'v AnalysisContext<'v>);

impl<'v> StarlarkTypeRepr for RefAnalysisContext<'v> {
    fn starlark_type_repr() -> Ty {
        AnalysisContext::starlark_type_repr()
    }
}

impl<'v> UnpackValue<'v> for RefAnalysisContext<'v> {
    fn unpack_value(value: Value<'v>) -> Option<Self> {
        Some(RefAnalysisContext(
            value.downcast_ref::<AnalysisContext>().unwrap(),
        ))
    }
}

/// The type used for defining rules, usually bound as `ctx`.
/// Usually the sole argument to the `impl` argument of the `rule` function.
///
/// ```python
/// def _impl_my_rule(ctx: AnalysisContext) -> ["provider"]:
///     return [DefaultInfo()]
/// my_rule = rule(impl = _impl_my_rule, attrs = {})
/// ```
#[starlark_module]
fn analysis_context_methods(builder: &mut MethodsBuilder) {
    /// Returns the attributes of the target as a Starlark struct with a field for each attribute, which varies per rule.
    /// As an example, given a rule with the `attrs` argument of `{"foo": attrs.string()}`, this field will be
    /// a `struct` containing a field `foo` of type string.
    #[starlark(attribute)]
    fn attrs<'v>(this: RefAnalysisContext) -> anyhow::Result<ValueOfUnchecked<'v, StructRef<'v>>> {
        Ok(this.0.attrs)
    }

    /// Returns an `actions` value containing functions to define actual actions that are run.
    /// See the `actions` type for the operations that are available.
    #[starlark(attribute)]
    fn actions<'v>(
        this: RefAnalysisContext,
    ) -> anyhow::Result<ValueTyped<'v, AnalysisActions<'v>>> {
        Ok(this.0.actions)
    }

    /// Returns a `label` representing the target, or `None` if being invoked from a
    /// `dynamic_output` in Bxl.
    #[starlark(attribute)]
    fn label<'v>(
        this: RefAnalysisContext,
    ) -> anyhow::Result<Option<ValueTyped<'v, StarlarkConfiguredProvidersLabel>>> {
        Ok(this.0.label)
    }

    /// An opaque value that can be indexed with a plugin kind to get a list of the available plugin
    /// deps of that kind. The rule must set an appropriate value on `uses_plugins` in its
    /// declaration.
    #[starlark(attribute)]
    fn plugins<'v>(
        this: RefAnalysisContext,
    ) -> anyhow::Result<ValueTypedComplex<'v, AnalysisPlugins<'v>>> {
        Ok(this.0.plugins)
    }
}

#[starlark_module]
pub(crate) fn register_analysis_context(builder: &mut GlobalsBuilder) {
    const AnalysisContext: StarlarkValueAsType<AnalysisContext> = StarlarkValueAsType::new();
    const AnalysisActions: StarlarkValueAsType<AnalysisActions> = StarlarkValueAsType::new();
}

pub static ANALYSIS_ACTIONS_METHODS_ACTIONS: LateBinding<fn(&mut MethodsBuilder)> =
    LateBinding::new("ANALYSIS_ACTIONS_METHODS_ACTIONS");
pub static ANALYSIS_ACTIONS_METHODS_ANON_TARGET: LateBinding<fn(&mut MethodsBuilder)> =
    LateBinding::new("ANALYSIS_ACTIONS_METHODS_ANON_TARGET");
