/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::iter;

use allocative::Allocative;
use anyhow::Context as _;
use gazebo::any::ProvidesStaticType;
use gazebo::coerce::Coerce;
use gazebo::display::display_chain;
use gazebo::display::display_container;
use gazebo::display::display_pair;
use gazebo::prelude::*;
use serde::ser::SerializeMap;
use serde::Serialize;
use serde::Serializer;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::values::Freeze;
use starlark::values::Freezer;
use starlark::values::FrozenValue;
use starlark::values::Heap;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;

use crate::actions::impls::write_json::visit_json_artifacts;
use crate::actions::impls::write_json::UnregisteredWriteJsonAction;
use crate::artifact_groups::deferred::TransitiveSetKey;
use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::TransitiveSetProjectionKey;
use crate::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use crate::interpreter::rule_defs::transitive_set::transitive_set_definition::TransitiveSetProjectionKind;
use crate::interpreter::rule_defs::transitive_set::transitive_set_definition_from_value;
use crate::interpreter::rule_defs::transitive_set::traversal::TransitiveSetOrdering;
use crate::interpreter::rule_defs::transitive_set::traversal::TransitiveSetTraversal;
use crate::interpreter::rule_defs::transitive_set::BfsTransitiveSetIteratorGen;
use crate::interpreter::rule_defs::transitive_set::PostorderTransitiveSetIteratorGen;
use crate::interpreter::rule_defs::transitive_set::PreorderTransitiveSetIteratorGen;
use crate::interpreter::rule_defs::transitive_set::TopologicalTransitiveSetIteratorGen;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetArgsProjection;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetError;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetIteratorLike;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetJsonProjection;

#[derive(Debug, Clone, Trace, ProvidesStaticType, Allocative)]
#[repr(C)]
pub struct TransitiveSetGen<V> {
    /// A Deferred key that maps back to this set. This is used to compute its inputs.
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

#[derive(Debug, Clone, Trace, Allocative)]
#[repr(C)]
pub struct NodeGen<V> {
    /// The value
    pub value: V,

    /// Pre-computed projections.
    pub projections: Vec<V>,
}

unsafe impl<'v> Coerce<TransitiveSetGen<Value<'v>>> for TransitiveSetGen<FrozenValue> {}

impl<V: fmt::Display> fmt::Display for TransitiveSetGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_container(
            f,
            &format!("{}(", self.definition),
            ")",
            display_chain(
                self.node
                    .as_ref()
                    .map(|node| display_pair("value", "=", &node.value)),
                iter::once(format!("{} children", self.children.len())),
            ),
        )
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
}

impl<'v> NodeGen<Value<'v>> {
    fn freeze(self, freezer: &Freezer) -> anyhow::Result<NodeGen<FrozenValue>> {
        let Self { value, projections } = self;

        let value = value.freeze(freezer)?;
        let projections = projections.into_try_map(|x| x.freeze(freezer))?;

        Ok(NodeGen { value, projections })
    }
}

impl<'v, V: ValueLike<'v>> TransitiveSetGen<V> {
    fn matches_definition(&self, definition: Value<'v>) -> bool {
        definition.ptr_eq(self.definition.to_value())
    }

    pub fn projection_name(&'v self, projection: usize) -> anyhow::Result<&'v str> {
        let def = transitive_set_definition_from_value(self.definition.to_value())
            .context("Invalid definition")?;

        Ok(def
            .operations()
            .projections
            .get_index(projection)
            .context("Invalid projection id")?
            .0
            .as_str())
    }

    pub fn get_projection_value(&self, projection: usize) -> anyhow::Result<Option<V>> {
        match &self.node {
            None => Ok(None),
            Some(node) => Ok(Some(
                *node
                    .projections
                    .get(projection)
                    .context("Invalid projection id")?,
            )),
        }
    }

    pub fn get_projection_key(&self, projection: usize) -> TransitiveSetProjectionKey {
        TransitiveSetProjectionKey {
            key: self.key.dupe(),
            projection,
        }
    }

    pub fn get_projection_sub_inputs(
        &self,
        projection: usize,
    ) -> anyhow::Result<Vec<ArtifactGroup>> {
        let mut sub_inputs = Vec::new();

        if let Some(projection) = self.get_projection_value(projection)? {
            let mut visitor = SimpleCommandLineArtifactVisitor::new();
            // It's either an args-like or a json projection. visit_json_artifacts handles both the way we want.
            visit_json_artifacts(projection.to_value(), &mut visitor)?;
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
    pub fn iter<'a>(
        &'a self,
        ordering: TransitiveSetOrdering,
    ) -> Box<dyn TransitiveSetIteratorLike<'a, 'v, V> + 'a>
    where
        'v: 'a,
    {
        match ordering {
            TransitiveSetOrdering::Preorder => box PreorderTransitiveSetIteratorGen::new(self),
            TransitiveSetOrdering::Postorder => box PostorderTransitiveSetIteratorGen::new(self),
            TransitiveSetOrdering::Topological => {
                box TopologicalTransitiveSetIteratorGen::new(self)
            }
            TransitiveSetOrdering::Bfs => box BfsTransitiveSetIteratorGen::new(self),
        }
    }

    pub fn iter_values<'a>(
        &'a self,
        ordering: TransitiveSetOrdering,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Value<'v>> + 'a>>
    where
        'v: 'a,
    {
        Ok(box self
            .iter(ordering)
            .values()
            .map(|node| node.value.to_value()))
    }

    pub(super) fn iter_projection_values<'a>(
        &'a self,
        ordering: TransitiveSetOrdering,
        projection: usize,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Value<'v>> + 'a>>
    where
        'v: 'a,
    {
        let mut iter = self.iter(ordering).values().peekable();

        // Defensively, check the projection is valid. We know the set has the same definition
        // throughout so it'll be safe (enough) to unwrap if it is valid on the first one.
        if let Some(v) = iter.peek() {
            v.projections
                .get(projection)
                .context("Invalid projection")?;
        }

        Ok(box iter.map(move |node| node.projections.get(projection).unwrap().to_value()))
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
            None => {
                Err(TransitiveSetError::TransitiveValueIsNotTransitiveSet { got: v.to_string() })
            }
        })?;

        let node = value.into_try_map(|value| {
            let projections = def
                .operations()
                .projections
                .iter()
                .map(|(name, spec)| {
                    let projected_value = eval
                        .eval_function(spec.projection, &[value], &[])
                        .map_err(|error| TransitiveSetError::ProjectionError {
                            error,
                            name: name.clone(),
                        })?;
                    match spec.kind {
                        TransitiveSetProjectionKind::Args => {
                            TransitiveSetArgsProjection::as_command_line(projected_value)?;
                        }
                        TransitiveSetProjectionKind::Json => {
                            UnregisteredWriteJsonAction::validate(projected_value)?;
                        }
                    }
                    anyhow::Ok(projected_value)
                })
                .collect::<Result<Vec<_>, _>>()?;

            anyhow::Ok(NodeGen { value, projections })
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
    fn project_as_json<'v>(
        this: Value<'v>,
        projection: &str,
        #[starlark(require = named, default = "preorder")] ordering: &str,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        let set = TransitiveSet::from_value(this).context("Invalid this")?;

        let def = transitive_set_definition_from_value(set.definition)
            .context("Invalid this.definition")?;

        let index = def
            .operations()
            .get_index_of_projection(TransitiveSetProjectionKind::Json, projection)?;

        Ok(heap.alloc(TransitiveSetJsonProjection {
            transitive_set: this,
            projection: index,
            ordering: TransitiveSetOrdering::parse(ordering)?,
        }))
    }

    fn project_as_args<'v>(
        this: Value<'v>,
        projection: &str,
        #[starlark(require = named, default = "preorder")] ordering: &str,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        let set = TransitiveSet::from_value(this).context("Invalid this")?;

        let def = transitive_set_definition_from_value(set.definition)
            .context("Invalid this.definition")?;

        let index = def
            .operations()
            .get_index_of_projection(TransitiveSetProjectionKind::Args, projection)?;

        Ok(heap.alloc(TransitiveSetArgsProjection {
            transitive_set: this,
            projection: index,
            ordering: TransitiveSetOrdering::parse(ordering)?,
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

    fn traverse<'v>(
        this: Value<'v>,
        heap: &'v Heap,
        #[starlark(require = named, default = "preorder")] ordering: &str,
    ) -> anyhow::Result<Value<'v>> {
        Ok(heap.alloc(TransitiveSetTraversal {
            inner: this,
            ordering: TransitiveSetOrdering::parse(ordering)?,
        }))
    }

    #[starlark(attribute)]
    fn definition<'v>(this: Value<'v>) -> anyhow::Result<Value<'v>> {
        let set = TransitiveSet::from_value(this).context("Invalid this")?;
        Ok(set.definition)
    }
}
