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
use buck2_interpreter::types::label::Label;
use buck2_util::late_binding::LateBinding;
use derive_more::Display;
use dice::DiceComputations;
use starlark::any::ProvidesStaticType;
use starlark::docs::DocItem;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::starlark_type;
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
use starlark::values::ValueTyped;

use crate::analysis::registry::AnalysisRegistry;

/// Functions to allow users to interact with the Actions registry.
///
/// Accessed via `ctx.actions.<function>`
#[derive(ProvidesStaticType, Debug, Display, Trace, NoSerialize, Allocative)]
#[display(fmt = "<ctx.actions>")]
pub struct AnalysisActions<'v> {
    /// Use a RefCell/Option so when we are done with it, without obtaining exclusive access,
    /// we can take the internal state without having to clone it.
    pub state: RefCell<Option<AnalysisRegistry<'v>>>,
    /// Copies from the ctx, so we can capture them for `dynamic`.
    pub attributes: Value<'v>,
    /// Digest configuration to use when interpreting digests passed in analysis.
    pub digest_config: DigestConfig,
}

impl<'v> StarlarkTypeRepr for &'v AnalysisActions<'v> {
    fn starlark_type_repr() -> String {
        AnalysisActions::get_type_starlark_repr()
    }
}

impl<'v> UnpackValue<'v> for &'v AnalysisActions<'v> {
    fn unpack_value(x: Value<'v>) -> Option<&'v AnalysisActions<'v>> {
        x.downcast_ref()
    }
}

impl<'v> AnalysisActions<'v> {
    pub fn state(&self) -> RefMut<AnalysisRegistry<'v>> {
        RefMut::map(self.state.borrow_mut(), |x| {
            x.as_mut().expect("state to be present during execution")
        })
    }
}

impl<'v> StarlarkValue<'v> for AnalysisActions<'v> {
    starlark_type!("actions");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(REGISTER_CONTEXT_ACTIONS.get().unwrap())
    }
}

impl<'v> AllocValue<'v> for AnalysisActions<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

struct RefAnalysisAction<'v>(&'v AnalysisActions<'v>);

impl<'v> StarlarkTypeRepr for RefAnalysisAction<'v> {
    fn starlark_type_repr() -> String {
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

#[derive(ProvidesStaticType, Debug, Trace, NoSerialize, Allocative)]
pub struct AnalysisContext<'v> {
    attrs: Value<'v>, // A struct
    actions: ValueTyped<'v, AnalysisActions<'v>>,
    label: Option<ValueTyped<'v, Label>>,
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

/// Simple holder for documentation from AnalysisContext
pub struct ContextDocs {
    /// Docs for ctx
    pub context: Option<DocItem>,
    /// Docs for ctx.actions
    pub actions: Option<DocItem>,
}

impl<'v> AnalysisContext<'v> {
    /// The context that is provided to users' UDR implementation functions. Comprised of things like attribute values, actions, etc
    pub fn new(
        heap: &'v Heap,
        attrs: Value<'v>,
        label: Option<ValueTyped<'v, Label>>,
        registry: AnalysisRegistry<'v>,
        digest_config: DigestConfig,
    ) -> Self {
        // Check the types match what the user expects.
        assert!(StructRef::from_value(attrs).is_some());

        Self {
            attrs,
            actions: heap.alloc_typed(AnalysisActions {
                state: RefCell::new(Some(registry)),
                attributes: attrs,
                digest_config,
            }),
            label,
        }
    }

    pub(crate) async fn run_promises(
        &self,
        dice: &DiceComputations,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<()> {
        // We need to loop here because running the promises evaluates promise.map, which might produce more promises.
        // We keep going until there are no promises left.
        loop {
            let promises = self.actions.state().get_promises();
            if let Some(promises) = promises {
                promises.run_promises(dice, eval).await?;
            } else {
                break;
            }
        }
        Ok(())
    }

    pub(crate) fn assert_no_promises(&self) -> anyhow::Result<()> {
        self.actions.state().assert_no_promises()
    }

    /// Must take an `AnalysisContext` which has never had `take_state` called on it before.
    pub(crate) fn take_state(&self) -> AnalysisRegistry<'v> {
        self.actions
            .state
            .borrow_mut()
            .take()
            .expect("nothing to have stolen state yet")
    }

    /// Returns the documentation for AnalysisContext and AnalysisActions based on their get_methods() calls.
    ///
    /// That is the only reason that this function should be called.
    pub fn ctx_documentation() -> ContextDocs {
        static CTX_METHODS: MethodsStatic = MethodsStatic::new();
        static ACTIONS_METHODS: MethodsStatic = MethodsStatic::new();

        let context = CTX_METHODS
            .methods(register_context)
            .map(|methods| methods.documentation());
        let actions = ACTIONS_METHODS
            .methods(REGISTER_CONTEXT_ACTIONS.get().unwrap())
            .map(|methods| methods.documentation());

        ContextDocs { context, actions }
    }
}

impl<'v> StarlarkValue<'v> for AnalysisContext<'v> {
    starlark_type!("context");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(register_context)
    }
}

impl<'v> AllocValue<'v> for AnalysisContext<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

struct RefAnalysisContext<'v>(&'v AnalysisContext<'v>);

impl<'v> StarlarkTypeRepr for RefAnalysisContext<'v> {
    fn starlark_type_repr() -> String {
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

/// The starting type, usually bound as `ctx`
#[starlark_module]
fn register_context(builder: &mut MethodsBuilder) {
    /// Returns the attributes of the target as a Starlark struct with a field for each attribute, which varies per rule
    #[starlark(attribute)]
    fn attrs<'v>(this: RefAnalysisContext) -> anyhow::Result<Value<'v>> {
        Ok(this.0.attrs)
    }

    /// Returns `actions` allowing you to define actions
    #[starlark(attribute)]
    fn actions<'v>(
        this: RefAnalysisContext,
    ) -> anyhow::Result<ValueTyped<'v, AnalysisActions<'v>>> {
        Ok(this.0.actions)
    }

    /// Returns a `label` representing the target
    #[starlark(attribute)]
    fn label<'v>(this: RefAnalysisContext) -> anyhow::Result<Value<'v>> {
        Ok(this.0.label.map_or(Value::new_none(), |v| v.to_value()))
    }
}

pub static REGISTER_CONTEXT_ACTIONS: LateBinding<fn(&mut MethodsBuilder)> =
    LateBinding::new("REGISTER_CONTEXT_ACTIONS");
