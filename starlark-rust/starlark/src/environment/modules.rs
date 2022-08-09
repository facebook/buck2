/*
 * Copyright 2018 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//! The environment, called "Module" in [this spec](
//! https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md)
//! is the list of variable in the current scope. It can be frozen, after which
//! all values from this environment become immutable.

use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::mem;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use gazebo::prelude::*;
use itertools::Itertools;

use crate::collections::Hashed;
use crate::environment::names::FrozenNames;
use crate::environment::names::MutableNames;
use crate::environment::slots::FrozenSlots;
use crate::environment::slots::ModuleSlotId;
use crate::environment::slots::MutableSlots;
use crate::environment::EnvironmentError;
use crate::errors::did_you_mean::did_you_mean;
use crate::syntax::ast::Visibility;
use crate::values::docs;
use crate::values::docs::DocItem;
use crate::values::docs::DocString;
use crate::values::docs::DocStringKind;
use crate::values::layout::heap::heap_type::HeapKind;
use crate::values::layout::heap::profile::aggregated::AggregateHeapProfileInfo;
use crate::values::Freezer;
use crate::values::FrozenHeap;
use crate::values::FrozenHeapRef;
use crate::values::FrozenStringValue;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::OwnedFrozenValue;
use crate::values::Trace;
use crate::values::Tracer;
use crate::values::Value;

#[derive(Debug, thiserror::Error)]
enum ModuleError {
    #[error("Retained memory profiling is not enabled")]
    RetainedMemoryProfileNotEnabled,
}

/// The result of freezing a [`Module`], making it and its contained values immutable.
///
/// The values of this [`FrozenModule`] are stored on a frozen heap, a reference to which
/// can be obtained using [`frozen_heap`](FrozenModule::frozen_heap). Be careful not to use
/// these values after the [`FrozenModule`] has been released unless you obtain a reference
/// to the frozen heap.
#[derive(Debug, Clone, Dupe)]
// We store the two elements separately since the FrozenHeapRef contains
// a copy of the FrozenModuleData inside it.
// Two Arc's should still be plenty cheap enough to qualify for `Dupe`.
pub struct FrozenModule {
    heap: FrozenHeapRef,
    module: FrozenModuleRef,
    /// Module evaluation duration:
    /// * evaluation of the top-level statements
    /// * optimizations during that evaluation
    /// * freezing and optimizations during freezing
    /// * does not include parsing time
    pub(crate) eval_duration: Duration,
}

#[derive(Debug, Clone, Dupe, ProvidesStaticType, Display)]
#[display(fmt = "{:?}", self)] // Type should not be user visible
pub(crate) struct FrozenModuleRef(pub(crate) Arc<FrozenModuleData>);

impl FrozenModuleRef {
    pub(crate) fn get_module_data(&self) -> &FrozenModuleData {
        self.0.as_ref()
    }

    pub(crate) fn documentation(&self) -> Option<DocItem> {
        self.0.docstring.as_ref().map(|d| {
            DocItem::Module(docs::Module {
                docs: DocString::from_docstring(DocStringKind::Starlark, d),
            })
        })
    }
}

#[derive(Debug)]
pub(crate) struct FrozenModuleData {
    pub(crate) names: FrozenNames,
    pub(crate) slots: FrozenSlots,
    docstring: Option<String>,
    /// When heap profile enabled, this field stores retained memory info.
    heap_profile: Option<AggregateHeapProfileInfo>,
}

/// Container for the documentation for a module
#[derive(Clone, Debug, PartialEq)]
pub struct ModuleDocs {
    /// The documentation for the module itself
    pub module: Option<DocItem>,
    /// A mapping of top level symbols to their documentation, if any.
    pub members: HashMap<String, Option<DocItem>>,
}

/// A container for user values, used during execution.
///
/// A module contains both a [`FrozenHeap`] and [`Heap`] on which different values are allocated.
/// You can get references to these heaps with [`frozen_heap`](Module::frozen_heap) and
/// [`heap`](Module::heap). Be careful not to use these values after the [`Module`] has been
/// released unless you obtain a reference to the frozen heap.
#[derive(Debug)]
pub struct Module {
    heap: Heap,
    frozen_heap: FrozenHeap,
    names: MutableNames,
    // Should really be MutableSlots<'v>, where &'v self
    // Values are allocated from heap. Because of variance
    // you can inject the wrong values in, so make sure slots aren't
    // exported.
    slots: MutableSlots<'static>,
    docstring: RefCell<Option<String>>,
    /// Module evaluation duration:
    /// * evaluation of the top-level statements
    /// * optimizations during that evaluation
    /// * does not include freezing time
    /// * does not include parsing time
    eval_duration: Cell<Duration>,
    /// Field that can be used for any purpose you want.
    extra_value: Cell<Option<Value<'static>>>,
    /// When true, heap profile is collected on freeze.
    heap_profile_on_freeze: Cell<bool>,
}

impl FrozenModule {
    /// Get value, exported or private by name.
    #[doc(hidden)] // TODO(nga): Buck2 depends on this function
    pub fn get_any_visibility(&self, name: &str) -> Option<(OwnedFrozenValue, Visibility)> {
        let (slot, vis) = self.module.0.names.get_name(name)?;
        // This code is safe because we know the frozen module ref keeps the values alive
        self.module
            .0
            .slots
            .get_slot(slot)
            .map(|x| (unsafe { OwnedFrozenValue::new(self.heap.dupe(), x) }, vis))
    }

    /// Get the value of the exported variable `name`.
    /// Returns [`None`] if the variable isn't defined in the module or it is private.
    pub fn get(&self, name: &str) -> Option<OwnedFrozenValue> {
        self.get_any_visibility(name)
            .and_then(|(value, vis)| match vis {
                Visibility::Private => None,
                Visibility::Public => Some(value),
            })
    }

    /// Iterate through all the names defined in this module.
    pub fn names(&self) -> impl Iterator<Item = FrozenStringValue> + '_ {
        self.module.0.names()
    }

    /// Obtain the [`FrozenHeapRef`] which owns the storage of all values defined in this module.
    pub fn frozen_heap(&self) -> &FrozenHeapRef {
        &self.heap
    }

    /// Print out some approximation of the module definitions.
    pub fn describe(&self) -> String {
        self.module.0.describe()
    }

    pub(crate) fn all_items(&self) -> impl Iterator<Item = (FrozenStringValue, FrozenValue)> + '_ {
        self.module.0.all_items()
    }

    /// Fetch the documentation for the module.
    pub fn documentation(&self) -> Option<DocItem> {
        self.module.documentation()
    }

    /// The documentation for the module, and all of its top level values
    ///
    /// Returns (<module documentation>, { <symbol> : <that symbol's documentation> })
    pub fn module_documentation(&self) -> ModuleDocs {
        let members = self
            .names()
            .filter(|n| Module::default_visibility(n) == Visibility::Public)
            .filter_map(|n| {
                self.get(&n)
                    .map(|fv| (n.as_str().to_owned(), fv.value().get_ref().documentation()))
            })
            .collect();

        ModuleDocs {
            module: self.documentation(),
            members,
        }
    }

    /// Retained memory info, or error if not enabled.
    pub fn aggregated_heap_profile_info(&self) -> anyhow::Result<&AggregateHeapProfileInfo> {
        self.module
            .0
            .heap_profile
            .as_ref()
            .ok_or_else(|| ModuleError::RetainedMemoryProfileNotEnabled.into())
    }

    /// Write heap flame profile of retained memory if heap profiling enabled.
    pub fn gen_heap_flame_profile(&self) -> anyhow::Result<String> {
        Ok(self.aggregated_heap_profile_info()?.gen_flame_graph())
    }

    /// Write heap summary profile of retained memory if heap profiling enabled.
    pub fn gen_heap_summary_profile(&self) -> anyhow::Result<String> {
        Ok(self.aggregated_heap_profile_info()?.gen_summary_csv())
    }
}

impl FrozenModuleData {
    pub fn names(&self) -> impl Iterator<Item = FrozenStringValue> + '_ {
        self.names.symbols().map(|x| x.0)
    }

    pub fn describe(&self) -> String {
        self.items()
            .map(|(name, val)| val.to_value().describe(&name))
            .join("\n")
    }

    pub(crate) fn items(&self) -> impl Iterator<Item = (FrozenStringValue, FrozenValue)> + '_ {
        self.names
            .symbols()
            .filter_map(|(name, slot)| Some((name, self.slots.get_slot(slot)?)))
    }

    pub(crate) fn all_items(&self) -> impl Iterator<Item = (FrozenStringValue, FrozenValue)> + '_ {
        self.names
            .all_symbols()
            .filter_map(|(name, slot)| Some((name, self.slots.get_slot(slot)?)))
    }

    pub(crate) fn get_slot(&self, slot: ModuleSlotId) -> Option<FrozenValue> {
        self.slots.get_slot(slot)
    }

    /// Try and go back from a slot to a name.
    /// Inefficient - only use in error paths.
    pub(crate) fn get_slot_name(&self, slot: ModuleSlotId) -> Option<FrozenStringValue> {
        for (s, i) in self.names.symbols() {
            if i == slot {
                return Some(s);
            }
        }
        None
    }
}

impl Default for Module {
    fn default() -> Self {
        Self::new()
    }
}

impl Module {
    /// Create a new module environment with no contents.
    pub fn new() -> Self {
        Self {
            heap: Heap::new(),
            frozen_heap: FrozenHeap::new(),
            names: MutableNames::new(),
            slots: MutableSlots::new(),
            docstring: RefCell::new(None),
            eval_duration: Cell::new(Duration::ZERO),
            extra_value: Cell::new(None),
            heap_profile_on_freeze: Cell::new(false),
        }
    }

    pub(crate) fn enable_heap_profile(&self) {
        self.heap_profile_on_freeze.set(true);
    }

    /// Get the heap on which values are allocated by this module.
    pub fn heap(&self) -> &Heap {
        &self.heap
    }

    /// Get the frozen heap on which frozen values are allocated by this module.
    pub fn frozen_heap(&self) -> &FrozenHeap {
        &self.frozen_heap
    }

    pub(crate) fn names(&self) -> &MutableNames {
        &self.names
    }

    pub(crate) fn slots<'v>(&'v self) -> &'v MutableSlots<'v> {
        // Not true because of variance, but mostly true. Don't export further.
        unsafe { transmute!(&'v MutableSlots<'static>, &'v MutableSlots<'v>, &self.slots) }
    }

    /// Get value, exported or private by name.
    pub(crate) fn get_any_visibility<'v>(
        &'v self,
        name: Hashed<&str>,
    ) -> Option<(Value<'v>, Visibility)> {
        let (slot, vis) = self.names.get_name(name)?;
        let value = self.slots().get_slot(slot)?;
        Some((value, vis))
    }

    /// Get the value of the exported variable `name`.
    /// Returns [`None`] if the variable isn't defined in the module or it is private.
    pub fn get<'v>(&'v self, name: &str) -> Option<Value<'v>> {
        self.get_any_visibility(Hashed::new(name))
            .and_then(|(v, vis)| match vis {
                Visibility::Private => None,
                Visibility::Public => Some(v),
            })
    }

    /// Freeze the environment, all its value will become immutable afterwards.
    pub fn freeze(self) -> anyhow::Result<FrozenModule> {
        let Module {
            names,
            slots,
            frozen_heap,
            heap,
            docstring,
            eval_duration,
            extra_value: extra_v,
            heap_profile_on_freeze,
        } = self;
        let _ = extra_v;
        let start = Instant::now();
        // This is when we do the GC/freeze, using the module slots as roots
        // Note that we even freeze anonymous slots, since they are accessed by
        // slot-index in the code, and we don't walk into them, so don't know if
        // they are used.
        let freezer = Freezer::new(frozen_heap);
        let slots = slots.freeze(&freezer)?;
        let stacks = if heap_profile_on_freeze.get() {
            Some(AggregateHeapProfileInfo::collect(
                &heap,
                Some(HeapKind::Frozen),
            ))
        } else {
            None
        };
        let rest = FrozenModuleRef(Arc::new(FrozenModuleData {
            names: names.freeze(),
            slots,
            docstring: docstring.into_inner(),
            heap_profile: stacks,
        }));
        let frozen_module_ref = freezer.heap.alloc_any(rest.dupe());
        for frozen_def in freezer.frozen_defs.borrow().as_slice() {
            frozen_def.post_freeze(frozen_module_ref, &heap, &freezer.heap);
        }
        // The values MUST be alive up until this point (as the above line uses them),
        // but can now be dropped
        mem::drop(heap);

        Ok(FrozenModule {
            heap: freezer.into_ref(),
            module: rest,
            eval_duration: start.elapsed() + eval_duration.get(),
        })
    }

    /// Set the value of a variable in the environment.
    /// Modifying these variables while executing is ongoing can have
    /// surprising effects.
    pub fn set<'v>(&'v self, name: &str, value: Value<'v>) {
        let slot = self.names.add_name(self.frozen_heap.alloc_str_intern(name));
        let slots = self.slots();
        slots.ensure_slot(slot);
        slots.set_slot(slot, value);
    }

    /// Symbols starting with underscore are considered private.
    pub(crate) fn default_visibility(symbol: &str) -> Visibility {
        match symbol.starts_with('_') {
            true => Visibility::Private,
            false => Visibility::Public,
        }
    }

    /// Set the value of a variable in the environment. Set its visibliity to
    /// "private" to ensure that it is not re-exported
    pub(crate) fn set_private<'v>(&'v self, name: FrozenStringValue, value: Value<'v>) {
        let slot = self.names.add_name_visibility(name, Visibility::Private);
        let slots = self.slots();
        slots.ensure_slot(slot);
        slots.set_slot(slot, value);
    }

    /// Import symbols from a module, similar to what is done during `load()`.
    pub fn import_public_symbols(&self, module: &FrozenModule) {
        self.frozen_heap.add_reference(&module.heap);
        for (k, slot) in module.module.0.names.symbols() {
            if Self::default_visibility(&k) == Visibility::Public {
                if let Some(value) = module.module.0.slots.get_slot(slot) {
                    self.set_private(k, Value::new_frozen(value))
                }
            }
        }
    }

    pub(crate) fn load_symbol<'v>(
        &'v self,
        module: &FrozenModule,
        symbol: &str,
    ) -> anyhow::Result<Value<'v>> {
        if Self::default_visibility(symbol) != Visibility::Public {
            return Err(EnvironmentError::CannotImportPrivateSymbol(symbol.to_owned()).into());
        }
        match module.get_any_visibility(symbol) {
            None => Err({
                match did_you_mean(symbol, module.names().map(|s| s.as_str())) {
                    Some(better) => EnvironmentError::ModuleHasNoSymbolDidYouMean(
                        symbol.to_owned(),
                        better.to_owned(),
                    )
                    .into(),
                    None => EnvironmentError::ModuleHasNoSymbol(symbol.to_owned()).into(),
                }
            }),
            Some((v, Visibility::Public)) => Ok(v.owned_value(self.frozen_heap())),
            Some((_, Visibility::Private)) => {
                Err(EnvironmentError::ModuleSymbolIsNotExported(symbol.to_owned()).into())
            }
        }
    }

    pub(crate) fn set_docstring(&self, docstring: String) {
        self.docstring.replace(Some(docstring));
    }

    pub(crate) fn add_eval_duration(&self, duration: Duration) {
        self.eval_duration.set(self.eval_duration.get() + duration);
    }

    pub(crate) fn trace<'v>(&'v self, tracer: &Tracer<'v>) {
        self.slots().get_slots_mut().trace(tracer);

        let extra_value = self.extra_value();
        if let Some(mut extra_value) = extra_value {
            extra_value.trace(tracer);
            self.set_extra_value(extra_value);
        }
    }

    /// Field that can be used for any purpose you want.
    pub fn set_extra_value<'v>(&'v self, v: Value<'v>) {
        // Cast lifetime.
        let v = unsafe { transmute!(Value, Value, v) };
        self.extra_value.set(Some(v));
    }

    /// Field that can be used for any purpose you want.
    pub fn extra_value<'v>(&'v self) -> Option<Value<'v>> {
        // Cast lifetime.
        unsafe { transmute!(Option<Value>, Option<Value>, self.extra_value.get()) }
    }
}

#[test]
fn test_send_sync()
where
    FrozenModule: Send + Sync,
{
}

#[cfg(test)]
mod tests {
    use crate::environment::Globals;
    use crate::environment::Module;
    use crate::eval::Evaluator;
    use crate::eval::ProfileMode;
    use crate::syntax::AstModule;
    use crate::syntax::Dialect;

    #[test]
    fn test_gen_heap_summary_profile() {
        let module = Module::new();
        let mut eval = Evaluator::new(&module);
        eval.enable_profile(&ProfileMode::HeapSummaryRetained)
            .unwrap();
        eval.eval_module(
            AstModule::parse(
                "x.star",
                r"
def f(x):
    return list([x])

x = f(1)
"
                .to_owned(),
                &Dialect::Extended,
            )
            .unwrap(),
            &Globals::standard(),
        )
        .unwrap();
        let module = module.freeze().unwrap();
        let heap_summary = module.gen_heap_summary_profile().unwrap();
        // Smoke test.
        assert!(heap_summary.contains("\"x.star.f\""), "{:?}", heap_summary);
    }
}
