/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    cell::RefCell,
    fmt::{self, Display},
    sync::Arc,
};

use buck2_interpreter::common::ModuleID;
use derive_more::Display;
use gazebo::{
    any::ProvidesStaticType,
    coerce::{coerce, Coerce},
    prelude::*,
};
use serde::{Serialize, Serializer};
use starlark::{
    collections::SmallMap,
    eval::Evaluator,
    values::{
        AllocValue, Freeze, Freezer, FrozenValue, Heap, StarlarkValue, Trace, Tracer, Value,
        ValueLike,
    },
};

use crate::interpreter::rule_defs::transitive_set::TransitiveSetError;

/// A unique identity for a given [`TransitiveSetDefinition`].
#[derive(Debug, Clone, Display)]
#[display(fmt = "{}", "name")]
struct TransitiveSetId {
    module_id: ModuleID,
    name: String,
}

#[derive(Debug, ProvidesStaticType)]
pub struct TransitiveSetDefinition<'v> {
    /// The name of this transitive set. This is filed in by `export_as` when it's assigned to a
    /// top-level variable. This must be set before this is used.
    id: RefCell<Option<Arc<TransitiveSetId>>>,

    /// The module id where this `TransitiveSetDefinition` is created and assigned
    module_id: ModuleID,

    operations: TransitiveSetOperationsGen<Value<'v>>,
}

#[derive(Debug, Clone, Trace, Coerce)]
#[repr(C)]
pub struct TransitiveSetOperationsGen<V> {
    /// Callables that will project the values contained in transitive sets of this type to
    /// cmd_args. This can be used to include a transitive set into a command.
    pub(crate) args_projections: SmallMap<String, V>,

    /// Callables that will reduce the values contained in transitive sets to a single value per
    /// node. This can be used to e.g. aggregate flags throughout a transitive set;
    pub(crate) reductions: SmallMap<String, V>,
}

pub type TransitiveSetOperations<'v> = TransitiveSetOperationsGen<Value<'v>>;

impl<'v> TransitiveSetOperationsGen<Value<'v>> {
    fn freeze(self, freezer: &Freezer) -> anyhow::Result<TransitiveSetOperationsGen<FrozenValue>> {
        let Self {
            args_projections,
            reductions,
        } = self;

        let args_projections = args_projections
            .into_iter()
            .map(|(k, v)| Ok((k, v.freeze(freezer)?)))
            .collect::<anyhow::Result<_>>()?;

        let reductions = reductions
            .into_iter()
            .map(|(k, v)| Ok((k, v.freeze(freezer)?)))
            .collect::<anyhow::Result<_>>()?;

        Ok(TransitiveSetOperationsGen {
            args_projections,
            reductions,
        })
    }
}

unsafe impl<'v> Trace<'v> for TransitiveSetDefinition<'v> {
    fn trace(&mut self, tracer: &Tracer<'v>) {
        self.operations.trace(tracer)
    }
}

impl<'v> Display for TransitiveSetDefinition<'v> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.id.try_borrow() {
            Ok(val) => match val.as_deref() {
                Some(id) => write!(f, "{}", id),
                None => write!(f, "unnamed transitive set"),
            },
            Err(..) => write!(f, "borrowed transitive set"),
        }
    }
}

impl<'v> Serialize for TransitiveSetDefinition<'v> {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        s.serialize_str(&format!("{}", self))
    }
}

impl<'v> TransitiveSetDefinition<'v> {
    pub fn new(module_id: ModuleID, operations: TransitiveSetOperations<'v>) -> Self {
        Self {
            id: RefCell::new(None),
            module_id,
            operations,
        }
    }

    pub fn has_id(&self) -> bool {
        self.id.borrow().is_some()
    }
}

impl<'v> AllocValue<'v> for TransitiveSetDefinition<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex(self)
    }
}

impl<'v> StarlarkValue<'v> for TransitiveSetDefinition<'v> {
    starlark_type!("transitive_set_definition");

    fn export_as(&self, variable_name: &str, _: &mut Evaluator<'v, '_>) {
        // First export wins
        let mut id = self.id.borrow_mut();
        if id.is_none() {
            let new_id = Arc::new(TransitiveSetId {
                module_id: self.module_id.clone(),
                name: variable_name.to_owned(),
            });
            *id = Some(new_id.dupe());
        }
    }

    fn dir_attr(&self) -> Vec<String> {
        vec!["type".to_owned()]
    }

    fn has_attr(&self, attribute: &str) -> bool {
        attribute == "type"
    }

    fn get_attr(&self, attribute: &str, heap: &'v Heap) -> Option<Value<'v>> {
        if attribute == "type" {
            let id = self.id.borrow();
            let typ = id
                .as_ref()
                .map_or("transitive_set_definition", |id| id.name.as_str());
            Some(heap.alloc(typ))
        } else {
            None
        }
    }

    // TODO (torozco): extra_memory()?
}

#[derive(Display, ProvidesStaticType)]
#[display(fmt = "{}", id)]
pub struct FrozenTransitiveSetDefinition {
    /// The name of this transitive set. This is filed in by `export_as` when it's assigned to a
    /// top-level variable. This must be set before this is used.
    id: Arc<TransitiveSetId>,

    operations: TransitiveSetOperationsGen<FrozenValue>,
}

impl fmt::Debug for FrozenTransitiveSetDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "TransitiveSetDefinition({} declared in {})",
            self.id.name, self.id.module_id
        )
    }
}

impl Serialize for FrozenTransitiveSetDefinition {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        s.serialize_str(&format!("{}", self))
    }
}

impl<'v> StarlarkValue<'v> for FrozenTransitiveSetDefinition {
    starlark_type!("transitive_set_definition");

    fn dir_attr(&self) -> Vec<String> {
        vec!["type".to_owned()]
    }

    fn has_attr(&self, attribute: &str) -> bool {
        attribute == "type"
    }

    fn get_attr(&self, attribute: &str, heap: &'v Heap) -> Option<Value<'v>> {
        if attribute == "type" {
            let typ = self.id.name.as_str();
            Some(heap.alloc(typ))
        } else {
            None
        }
    }
}

starlark_simple_value!(FrozenTransitiveSetDefinition);

impl<'v> Freeze for TransitiveSetDefinition<'v> {
    type Frozen = FrozenTransitiveSetDefinition;

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        let Self {
            id,
            module_id: _,
            operations,
        } = self;

        let id = match id.into_inner() {
            Some(x) => x,
            None => {
                // Unfortunately we have no name or location for the definition at this point.
                return Err(TransitiveSetError::TransitiveSetNotAssigned.into());
            }
        };

        let operations = operations.freeze(freezer)?;

        Ok(FrozenTransitiveSetDefinition { id, operations })
    }
}

pub fn transitive_set_definition_from_value<'v>(
    x: Value<'v>,
) -> Option<&dyn TransitiveSetDefinitionLike<'v>> {
    if let Some(x) = x.downcast_ref::<TransitiveSetDefinition>() {
        Some(x as &dyn TransitiveSetDefinitionLike<'v>)
    } else if let Some(x) = x.downcast_ref::<FrozenTransitiveSetDefinition>() {
        Some(x as &dyn TransitiveSetDefinitionLike<'v>)
    } else {
        None
    }
}

pub trait TransitiveSetDefinitionLike<'v> {
    fn has_id(&self) -> bool;

    fn as_debug(&self) -> &dyn fmt::Debug;

    fn matches_type(&self, ty: &str) -> bool;

    fn operations(&self) -> &TransitiveSetOperations<'v>;
}

impl<'v> TransitiveSetDefinitionLike<'v> for TransitiveSetDefinition<'v> {
    fn has_id(&self) -> bool {
        Self::has_id(self)
    }

    fn as_debug(&self) -> &dyn fmt::Debug {
        self
    }

    fn matches_type(&self, ty: &str) -> bool {
        self.id.borrow().as_ref().map_or(false, |id| id.name == ty)
    }

    fn operations(&self) -> &TransitiveSetOperations<'v> {
        &self.operations
    }
}

impl<'v> TransitiveSetDefinitionLike<'v> for FrozenTransitiveSetDefinition {
    fn has_id(&self) -> bool {
        true
    }

    fn as_debug(&self) -> &dyn fmt::Debug {
        self
    }

    fn matches_type(&self, ty: &str) -> bool {
        self.id.name == ty
    }

    fn operations(&self) -> &TransitiveSetOperations<'v> {
        coerce(&self.operations)
    }
}
