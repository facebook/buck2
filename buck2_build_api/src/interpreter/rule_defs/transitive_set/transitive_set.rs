/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use anyhow::Context as _;
use gazebo::{any::ProvidesStaticType, coerce::Coerce, prelude::*};
use serde::{ser::SerializeMap, Serialize, Serializer};
use starlark::{
    environment::{Methods, MethodsBuilder, MethodsStatic},
    eval::Evaluator,
    values::{
        display::ContainerDisplayHelper, Freeze, Freezer, FrozenValue, Heap, StarlarkValue, Trace,
        Value, ValueLike,
    },
};

use crate::{
    artifact_groups::{deferred::TransitiveSetKey, ArtifactGroup, TransitiveSetProjectionKey},
    interpreter::rule_defs::{
        cmd_args::{SimpleCommandLineArtifactVisitor, StarlarkCommandLine, ValueAsCommandLineLike},
        transitive_set::{
            transitive_set_definition_from_value, traversal::TransitiveSetTraversal,
            TransitiveSetArgsProjection, TransitiveSetError, TransitiveSetIteratorGen,
        },
    },
};

#[derive(Debug, Clone, Trace, ProvidesStaticType)]
#[repr(C)]
pub struct TransitiveSetGen<V> {
    /// A Deferred key that maps back to this set. This is used to compute its inputs.
    #[trace(unsafe_ignore)]
    pub(crate) key: TransitiveSetKey,

    /// The TransitiveSetCallable that this set uses.
    pub(crate) definition: V,

    /// The immediate value of this node. If None, then this node will not yield anything when
    /// iterated over (but we'll still traverse to its children).
    pub(crate) node: Option<NodeGen<V>>,

    /// Pre-computed reductions. Those are arbitrary values based on the set's definition.
    pub(crate) reductions: Vec<V>,

    /// Further transitive sets.
    pub(crate) children: Vec<V>,
}

#[derive(Debug, Clone, Trace)]
#[repr(C)]
pub struct NodeGen<V> {
    /// The value
    pub value: V,

    /// Pre-computed args projections. Those are command lines.
    pub args_projections: Vec<V>,
}

unsafe impl<'v> Coerce<TransitiveSetGen<Value<'v>>> for TransitiveSetGen<FrozenValue> {}

impl<V: fmt::Display> fmt::Display for TransitiveSetGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut helper = ContainerDisplayHelper::begin(
            f,
            &format!("{}(", self.definition),
            if self.node.is_some() { 2 } else { 1 },
        )?;

        if let Some(node) = self.node.as_ref() {
            helper.keyed_item("value", "=", &node.value)?;
        }
        helper.item(format!("{} children", self.children.len()))?;
        helper.end(")")
    }
}

impl<'v, V: ValueLike<'v>> Serialize for TransitiveSetGen<V> {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut s = s.serialize_map(Some(3))?;
        s.serialize_entry("definition", &self.definition)?;
        if let Some(node) = self.node.as_ref() {
            s.serialize_entry("value", &node.value)?;
        }

        s.serialize_entry("children", &self.children.len())?;
        s.end()
    }
}

impl<V> TransitiveSetGen<V> {
    pub fn key(&self) -> &TransitiveSetKey {
        &self.key
    }

    fn extra_memory(&self) -> usize {
        self.children.capacity() * std::mem::size_of::<V>()
            + self.node.as_ref().map_or(0, |n| n.extra_memory())
    }
}

impl<V> NodeGen<V> {
    fn extra_memory(&self) -> usize {
        self.args_projections.capacity() * std::mem::size_of::<V>()
    }
}

impl<'v> NodeGen<Value<'v>> {
    fn freeze(self, freezer: &Freezer) -> anyhow::Result<NodeGen<FrozenValue>> {
        let Self {
            value,
            args_projections,
        } = self;

        let value = value.freeze(freezer)?;
        let args_projections = args_projections.into_try_map(|x| x.freeze(freezer))?;

        Ok(NodeGen {
            value,
            args_projections,
        })
    }
}

impl<'v, V: ValueLike<'v>> TransitiveSetGen<V> {
    fn matches_definition(&self, definition: Value<'v>) -> bool {
        definition.ptr_eq(self.definition.to_value())
    }

    pub fn get_projection_sub_inputs(
        &self,
        projection: usize,
    ) -> anyhow::Result<Vec<ArtifactGroup>> {
        let mut sub_inputs = Vec::new();

        if let Some(node) = self.node.as_ref() {
            let projection = node
                .args_projections
                .get(projection)
                .context("Invalid projection id")?;

            let mut visitor = SimpleCommandLineArtifactVisitor::new();
            projection
                .to_value()
                .as_command_line_err()?
                .visit_artifacts(&mut visitor)?;

            sub_inputs.extend(visitor.inputs);
        }

        // Reuse the same projection for children sets.
        for v in self.children.iter() {
            let v = TransitiveSet::from_value(v.to_value()).context("Invalid deferred")?;
            sub_inputs.push(ArtifactGroup::TransitiveSetProjection(
                TransitiveSetProjectionKey {
                    key: v.key().dupe(),
                    projection,
                },
            ));
        }
        Ok(sub_inputs)
    }
}

impl<'v, V> TransitiveSetGen<V>
where
    V: ValueLike<'v>,
    TransitiveSetGen<V>: StarlarkValue<'v> + TransitiveSetLike<'v>,
{
    pub fn iter<'a>(&'a self) -> TransitiveSetIteratorGen<'a, 'v, V>
    where
        'v: 'a,
    {
        TransitiveSetIteratorGen::new(self)
    }

    pub fn iter_values<'a>(&'a self) -> anyhow::Result<Box<dyn Iterator<Item = Value<'v>> + 'a>>
    where
        'v: 'a,
    {
        Ok(box self.iter().values().map(|node| node.value.to_value()))
    }
}

pub trait TransitiveSetLike<'v> {
    fn from_value(v: Value<'v>) -> Option<&'v Self>;
}

impl<'v> TransitiveSetLike<'v> for TransitiveSet<'v> {
    fn from_value(v: Value<'v>) -> Option<&'v Self> {
        TransitiveSet::from_value(v)
    }
}

impl<'v> TransitiveSetLike<'v> for FrozenTransitiveSet {
    fn from_value(v: Value<'v>) -> Option<&'v Self> {
        // FrozenTransitiveSet::from_value(v)

        v.downcast_ref::<FrozenTransitiveSet>()
    }
}

starlark_complex_value!(pub TransitiveSet);

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for TransitiveSetGen<V>
where
    Self: ProvidesStaticType + TransitiveSetLike<'v>,
{
    starlark_type!("transitive_set");

    fn extra_memory(&self) -> usize {
        TransitiveSetGen::extra_memory(self)
    }

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(transitive_set_methods)
    }

    fn matches_type(&self, ty: &str) -> bool {
        if ty == "transitive_set" {
            return true;
        }

        transitive_set_definition_from_value(self.definition.to_value())
            .map_or(false, |d| d.matches_type(ty))
    }
}

impl<'v> Freeze for TransitiveSet<'v> {
    type Frozen = FrozenTransitiveSet;

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        let Self {
            key,
            definition,
            node,
            reductions,
            children,
        } = self;
        let definition = definition.freeze(freezer)?;
        let node = node.into_try_map(|node| node.freeze(freezer))?;
        let children = children.into_try_map(|x| x.freeze(freezer))?;
        let reductions = reductions.into_try_map(|x| x.freeze(freezer))?;
        Ok(TransitiveSetGen {
            key,
            definition,
            node,
            reductions,
            children,
        })
    }
}

impl<'v> TransitiveSet<'v> {
    pub fn new(
        key: TransitiveSetKey,
        definition: Value<'v>,
        value: Option<Value<'v>>,
        children: impl IntoIterator<Item = Value<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Self> {
        let def = match transitive_set_definition_from_value(definition.to_value()) {
            Some(def) if def.has_id() => def,
            Some(..) => {
                return Err(TransitiveSetError::TransitiveSetUsedBeforeAssignment.into());
            }
            None => {
                return Err(TransitiveSetError::TransitiveSetDefinitionWasInvalid.into());
            }
        };

        let children = children.into_iter().collect::<Vec<_>>();
        let children_sets = children.try_map(|v| match TransitiveSet::from_value(*v) {
            Some(set) if set.matches_definition(definition) => Ok(set),
            Some(set) => {
                fn format_def(def: Value<'_>) -> String {
                    match transitive_set_definition_from_value(def) {
                        Some(def) => format!("{:?}", def.as_debug()),
                        None => "<invalid>".to_owned(),
                    }
                }
                Err(TransitiveSetError::TransitiveValueIsOfWrongType {
                    expected: format_def(definition),
                    got: format_def(set.definition),
                })
            }
            None => Err(TransitiveSetError::TransitiveValueIsNotTransitiveSet),
        })?;

        let node = value.into_try_map(|value| {
            let args_projections = def
                .operations()
                .args_projections
                .iter()
                .map(|(name, proj)| {
                    let cli = eval.heap().alloc(StarlarkCommandLine::new());
                    eval.eval_function(*proj, &[cli, value], &[])
                        .map_err(|error| TransitiveSetError::ProjectionError {
                            error,
                            name: name.clone(),
                        })?;
                    anyhow::Ok(cli)
                })
                .collect::<Result<Vec<_>, _>>()?;

            anyhow::Ok(NodeGen {
                value,
                args_projections,
            })
        })?;

        let reductions = def
            .operations()
            .reductions
            .iter()
            .enumerate()
            .map(|(idx, (name, reduce))| {
                let children_values = children_sets.try_map(|c| {
                    c.reductions
                        .get(idx)
                        .copied()
                        .with_context(|| format!("Child {} is missing reduction {}", c, idx))
                })?;
                let children_values = eval.heap().alloc_list(&children_values);

                let value = value.unwrap_or_else(Value::new_none);

                let reduced = eval
                    .eval_function(*reduce, &[children_values, value], &[])
                    .map_err(|error| TransitiveSetError::ReductionError {
                        error,
                        name: name.clone(),
                    })?;

                anyhow::Ok(reduced)
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Self {
            key,
            definition,
            node,
            reductions,
            children,
        })
    }

    pub fn new_from_values(
        key: TransitiveSetKey,
        definition: Value<'v>,
        value: Option<Value<'v>>,
        children: Option<Value<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Self> {
        let children = children
            .map(|v| v.iterate(eval.heap()))
            .transpose()?
            .into_iter()
            .flatten();

        Self::new(key, definition, value, children, eval)
    }
}

#[starlark_module]
fn transitive_set_methods(builder: &mut MethodsBuilder) {
    fn project_as_args<'v>(
        this: Value<'v>,
        projection: &str,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        let set = TransitiveSet::from_value(this).context("Invalid this")?;

        let def = transitive_set_definition_from_value(set.definition)
            .context("Invalid this.definition")?;

        let index = match def.operations().args_projections.get_index_of(projection) {
            Some(index) => index,
            None => {
                return Err(TransitiveSetError::ProjectionDoesNotExist {
                    projection: projection.into(),
                    valid_projections: def
                        .operations()
                        .args_projections
                        .keys()
                        .map(String::from)
                        .collect::<Vec<_>>(),
                }
                .into());
            }
        };

        Ok(heap.alloc(TransitiveSetArgsProjection {
            transitive_set: this,
            projection: index,
        }))
    }

    fn reduce<'v>(this: Value<'v>, reduction: &str) -> anyhow::Result<Value<'v>> {
        let set = TransitiveSet::from_value(this).context("Invalid this")?;

        let def = transitive_set_definition_from_value(set.definition)
            .context("Invalid this.definition")?;

        let index = match def.operations().reductions.get_index_of(reduction) {
            Some(index) => index,
            None => {
                return Err(TransitiveSetError::ReductionDoesNotExist {
                    reduction: reduction.into(),
                    valid_reductions: def
                        .operations()
                        .reductions
                        .keys()
                        .map(String::from)
                        .collect::<Vec<_>>(),
                }
                .into());
            }
        };

        set.reductions
            .get(index)
            .copied()
            .with_context(|| format!("Missing reduction {}", index))
    }

    fn traverse<'v>(this: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(heap.alloc(TransitiveSetTraversal { inner: this }))
    }
}
