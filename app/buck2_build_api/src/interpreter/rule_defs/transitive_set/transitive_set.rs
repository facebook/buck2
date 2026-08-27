/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::iter;
use std::sync::Arc;

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::artifact_type::OutputArtifact;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::deferred::base_deferred_key::BaseDeferredKey;
use buck2_error::BuckErrorContext;
use buck2_error::BuckErrorOptionContext;
use buck2_error::buck2_error;
use buck2_error::internal_error;
use display_container::display_pair;
use display_container::fmt_container;
use display_container::iter_display_chain;
use dupe::Dupe;
use gazebo::prelude::*;
use pagable::Pagable;
use pagable::pagable_typetag;
use serde::Serialize;
use serde::Serializer;
use serde::ser::SerializeMap;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::eval::Evaluator;
use starlark::type_matcher;
use starlark::values::FreezeBranded;
use starlark::values::FrozenValueTyped;
use starlark::values::StarlarkPagable;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::ValueOf;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTyped;
use starlark::values::list::AllocList;
use starlark::values::starlark_value;
use starlark::values::typing::TypeInstanceId;
use starlark::values::typing::TypeMatcher;
use starlark::values::typing::TypeMatcherDyn;

use crate::actions::impls::json::JsonUnpack;
use crate::actions::impls::json::validate_json;
use crate::actions::impls::json::visit_json_artifacts;
use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::TransitiveSetProjectionKey;
use crate::artifact_groups::TransitiveSetProjectionWrapper;
use crate::artifact_groups::deferred::TransitiveSetKey;
use crate::interpreter::rule_defs::artifact_tagging::ArtifactTag;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use crate::interpreter::rule_defs::transitive_set::FrozenTransitiveSetDefinition;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetArgsProjection;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetError;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetJsonProjection;
use crate::interpreter::rule_defs::transitive_set::transitive_set_definition::TransitiveSetDefinitionLike;
use crate::interpreter::rule_defs::transitive_set::transitive_set_definition::TransitiveSetProjectionKind;
use crate::interpreter::rule_defs::transitive_set::transitive_set_iterator::BfsTransitiveSetIterator;
use crate::interpreter::rule_defs::transitive_set::transitive_set_iterator::DfsTransitiveSetIterator;
use crate::interpreter::rule_defs::transitive_set::transitive_set_iterator::PostorderTransitiveSetIterator;
use crate::interpreter::rule_defs::transitive_set::transitive_set_iterator::PreorderTransitiveSetIterator;
use crate::interpreter::rule_defs::transitive_set::transitive_set_iterator::TopologicalTransitiveSetIterator;
use crate::interpreter::rule_defs::transitive_set::transitive_set_iterator::TransitiveSetIteratorLike;
use crate::interpreter::rule_defs::transitive_set::traversal::TransitiveSetOrdering;
use crate::interpreter::rule_defs::transitive_set::traversal::TransitiveSetTraversal;

#[derive(Clone, Debug, Allocative, Pagable)]
#[pagable_typetag(TypeMatcherDyn)]
pub(crate) struct TransitiveSetMatcher {
    pub(crate) type_instance_id: TypeInstanceId,
}

#[type_matcher]
impl TypeMatcher for TransitiveSetMatcher {
    fn matches(&self, value: Value) -> bool {
        let Some(tset) = ValueTyped::<TransitiveSet>::new(value) else {
            return false;
        };
        let exported = &tset.definition.as_ref().exported;
        // Ids, not pointers: page-in can leave two allocations of one logical definition alive.
        exported.set_type_instance_id == self.type_instance_id
    }
}

/// Compact bitfield for per-projection boolean flags, stored as a u64.
#[derive(
    Debug,
    Clone,
    Copy,
    Trace,
    FreezeBranded,
    Allocative,
    PartialEq,
    Eq,
    StarlarkPagable
)]
pub(crate) struct ProjectionBitSet(u64);

impl ProjectionBitSet {
    const MAX_PROJECTIONS: usize = 64;

    pub fn from_bools(bools: &[bool]) -> buck2_error::Result<Self> {
        if bools.len() > Self::MAX_PROJECTIONS {
            return Err(buck2_error!(
                buck2_error::ErrorTag::Input,
                "TransitiveSet has {} projections, but at most {} are supported",
                bools.len(),
                Self::MAX_PROJECTIONS,
            ));
        }
        let mut bits: u64 = 0;
        for (i, b) in bools.iter().enumerate() {
            if *b {
                bits |= 1u64 << i;
            }
        }
        Ok(Self(bits))
    }

    pub fn get(self, index: usize) -> buck2_error::Result<bool> {
        match self.0.checked_shr(index as u32) {
            Some(shifted) => Ok(shifted & 1 != 0),
            None => Err(internal_error!(
                "Projection index {} out of range (max {})",
                index,
                Self::MAX_PROJECTIONS
            )),
        }
    }
}

#[derive(
    Debug,
    Clone,
    Trace,
    FreezeBranded,
    ProvidesStaticType,
    Allocative,
    StarlarkPagable
)]
#[repr(C)]
pub struct TransitiveSet<'v> {
    /// A Deferred key that maps back to this set. This is used to compute its inputs.
    #[starlark_pagable(pagable)]
    #[freeze_branded(identity)]
    pub key: TransitiveSetKey,

    /// The TransitiveSetCallable that this set uses.
    pub(crate) definition: FrozenValueTyped<'v, FrozenTransitiveSetDefinition<'v>>,

    /// The immediate value of this node. If None, then this node will not yield anything when
    /// iterated over (but we'll still traverse to its children).
    pub(crate) node: Option<Node<'v>>,

    /// Pre-computed reductions. Those are arbitrary values based on the set's definition.
    pub(crate) reductions: Box<[Value<'v>]>,

    pub(crate) projection_path_resolution_may_require_artifact_value: ProjectionBitSet,

    pub(crate) projection_is_eligible_for_dedupe: ProjectionBitSet,

    /// Further transitive sets.
    pub children: Box<[Value<'v>]>,
}

#[derive(Debug, Clone, Trace, FreezeBranded, Allocative, StarlarkPagable)]
#[repr(C)]
pub struct Node<'v> {
    /// The value
    pub value: Value<'v>,

    /// Pre-computed projections.
    pub projections: Box<[Value<'v>]>,
}

impl<'v> fmt::Display for TransitiveSet<'v> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_container(
            f,
            &format!("{}(", self.definition),
            ")",
            iter_display_chain(
                self.node
                    .as_ref()
                    .map(|node| display_pair("value", "=", &node.value)),
                iter::once(format!("{} children", self.children.len())),
            ),
        )
    }
}

impl<'v> Serialize for TransitiveSet<'v> {
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

impl<'v> TransitiveSet<'v> {
    pub fn key(&self) -> &TransitiveSetKey {
        &self.key
    }

    /// Compares ids, not pointers: page-in can leave two allocations of one logical frozen module
    /// heap alive, so two definitions of the same `.bzl` variable need not share an address.
    fn matches_definition(
        &self,
        definition: FrozenValueTyped<'v, FrozenTransitiveSetDefinition<'v>>,
    ) -> bool {
        definition.as_ref().exported.set_type_instance_id
            == self.definition.as_ref().exported.set_type_instance_id
    }

    pub fn projection_name(&'v self, projection: usize) -> buck2_error::Result<&'v str> {
        let def = self.definition.as_ref();

        Ok(def
            .operations()
            .projections
            .get_index(projection)
            .internal_error("Invalid projection id")?
            .0
            .as_str())
    }

    pub fn get_projection_value(
        &self,
        projection: usize,
    ) -> buck2_error::Result<Option<Value<'v>>> {
        match &self.node {
            None => Ok(None),
            Some(node) => Ok(Some(
                *node
                    .projections
                    .get(projection)
                    .internal_error("Invalid projection id")?,
            )),
        }
    }

    pub fn get_projection_key(&self, projection: usize) -> TransitiveSetProjectionKey {
        TransitiveSetProjectionKey {
            key: self.key.dupe(),
            projection,
        }
    }

    pub(crate) fn definition(&self) -> FrozenValueTyped<'v, FrozenTransitiveSetDefinition<'v>> {
        self.definition
    }

    pub fn visit_projection_direct_inputs<V: CommandLineArtifactVisitor<'v>>(
        &self,
        projection: usize,
        visitor: &mut V,
    ) -> buck2_error::Result<()> {
        if let Some(projection) = self.get_projection_value(projection)? {
            // It's either an args-like or a json projection. visit_json_artifacts handles both the way we want.
            visit_json_artifacts(projection, visitor)?;
        }
        Ok(())
    }

    pub fn get_projection_sub_inputs(
        &self,
        projection: usize,
    ) -> buck2_error::Result<Vec<ArtifactGroup>> {
        let mut sub_inputs = Vec::new();

        if let Some(projection) = self.get_projection_value(projection)? {
            let mut visitor = SimpleCommandLineArtifactVisitor::new();
            // It's either an args-like or a json projection. visit_json_artifacts handles both the way we want.
            visit_json_artifacts(projection.to_value(), &mut visitor)?;
            sub_inputs.extend(visitor.inputs);
        }

        // Reuse the same projection for children sets.
        for v in self.children.iter() {
            let v = TransitiveSet::from_value(v.to_value()).internal_error("Invalid deferred")?;
            sub_inputs.push(ArtifactGroup::TransitiveSetProjection(Arc::new(
                TransitiveSetProjectionWrapper::new(
                    TransitiveSetProjectionKey {
                        key: v.key().dupe(),
                        projection,
                    },
                    v.projection_path_resolution_may_require_artifact_value
                        .get(projection)?,
                    v.projection_is_eligible_for_dedupe.get(projection)?,
                ),
            )));
        }
        Ok(sub_inputs)
    }
}

impl<'v> TransitiveSet<'v> {
    pub fn iter<'a>(
        &'a self,
        ordering: TransitiveSetOrdering,
    ) -> Box<dyn TransitiveSetIteratorLike<'a, 'v> + 'a>
    where
        'v: 'a,
    {
        match ordering {
            TransitiveSetOrdering::Preorder => Box::new(PreorderTransitiveSetIterator::new(self)),
            TransitiveSetOrdering::Postorder => Box::new(PostorderTransitiveSetIterator::new(self)),
            TransitiveSetOrdering::Topological => {
                Box::new(TopologicalTransitiveSetIterator::new(self))
            }
            TransitiveSetOrdering::Bfs => Box::new(BfsTransitiveSetIterator::new(self)),
            TransitiveSetOrdering::Dfs => Box::new(DfsTransitiveSetIterator::new(self)),
        }
    }

    pub fn iter_values<'a>(
        &'a self,
        ordering: TransitiveSetOrdering,
    ) -> buck2_error::Result<Box<dyn Iterator<Item = Value<'v>> + 'a>>
    where
        'v: 'a,
    {
        Ok(Box::new(
            self.iter(ordering)
                .values()
                .map(|node| node.value.to_value()),
        ))
    }

    pub(super) fn iter_projection_values<'a>(
        &'a self,
        ordering: TransitiveSetOrdering,
        projection: usize,
    ) -> buck2_error::Result<Box<dyn Iterator<Item = Value<'v>> + 'a>>
    where
        'v: 'a,
    {
        let mut iter = self.iter(ordering).values().peekable();

        // Defensively, check the projection is valid. We know the set has the same definition
        // throughout so it'll be safe (enough) to unwrap if it is valid on the first one.
        if let Some(v) = iter.peek() {
            v.projections
                .get(projection)
                .internal_error("Invalid projection")?;
        }

        Ok(Box::new(iter.map(move |node| {
            node.projections.get(projection).unwrap().to_value()
        })))
    }
}

starlark_complex_value_branded!(pub TransitiveSet);

starlark::methods_static!(TRANSITIVE_SET_METHODS = transitive_set_methods);

#[starlark_value(type = "TransitiveSet")]
impl<'v> StarlarkValue<'v> for TransitiveSet<'v> {
    fn get_methods() -> Option<&'static Methods> {
        Some(TRANSITIVE_SET_METHODS.methods())
    }
}

impl<'v> TransitiveSet<'v> {
    pub fn new(
        key: TransitiveSetKey,
        definition: FrozenValueTyped<'v, FrozenTransitiveSetDefinition<'v>>,
        value: Option<Value<'v>>,
        children: impl IntoIterator<Item = Value<'v>>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> buck2_error::Result<Self> {
        let def: &dyn TransitiveSetDefinitionLike = &*definition;
        if !def.has_id() {
            return Err(TransitiveSetError::TransitiveSetUsedBeforeAssignment.into());
        }

        let children = children.into_iter().collect::<Box<[_]>>();
        let children_sets = children.try_map(|v| match TransitiveSet::from_value(*v) {
            Some(set) if set.matches_definition(definition) => Ok(set),
            Some(set) => {
                fn format_def(def: &FrozenTransitiveSetDefinition<'_>) -> String {
                    format!("{:?}", def.as_debug())
                }
                Err(TransitiveSetError::TransitiveValueIsOfWrongType {
                    expected: format_def(&definition),
                    got: format_def(&set.definition),
                })
            }
            None => {
                Err(TransitiveSetError::TransitiveValueIsNotTransitiveSet { got: v.to_string() })
            }
        })?;

        let node = value.try_map(|value| {
            let projections = def
                .operations()
                .projections
                .iter()
                .map(|(name, spec)| {
                    let projected_value = eval
                        .eval_function(spec.projection.get(), &[value], &[])
                        .map_err(|error| TransitiveSetError::ProjectionError {
                            error: error.into(),
                            name: name.clone(),
                        })?;
                    match spec.kind {
                        TransitiveSetProjectionKind::Args => {
                            TransitiveSetArgsProjection::as_command_line(projected_value)?;
                        }
                        TransitiveSetProjectionKind::Json => {
                            validate_json(JsonUnpack::unpack_value_err(projected_value)?)?;
                        }
                    }
                    buck2_error::Ok(projected_value)
                })
                .collect::<Result<Box<[_]>, _>>()?;

            buck2_error::Ok(Node { value, projections })
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
                        .with_internal_error(|| format!("Child {c} is missing reduction {idx}"))
                })?;
                let children_values = eval.heap().alloc(AllocList(children_values));

                let value = value.unwrap_or_else(Value::new_none);

                let reduced = eval
                    .eval_function(reduce.get(), &[children_values, value], &[])
                    .map_err(|error| TransitiveSetError::ReductionError {
                        error: error.into(),
                        name: name.clone(),
                    })?;

                buck2_error::Ok(reduced)
            })
            .collect::<Result<Box<[_]>, _>>()?;

        let target_platform =
            if let BaseDeferredKey::TargetLabel(configured_label) = key.holder_key().owner() {
                Some(configured_label.cfg())
            } else {
                None
            };

        struct InputVisitor<'a> {
            path_resolution_may_require_artifact_value: bool,
            is_eligible_for_dedupe: bool,
            target_platform: Option<&'a ConfigurationData>,
        }

        impl<'a> InputVisitor<'a> {
            fn new(target_platform: Option<&'a ConfigurationData>) -> Self {
                Self {
                    path_resolution_may_require_artifact_value: false,
                    is_eligible_for_dedupe: true,
                    target_platform,
                }
            }
        }

        impl<'a, 'v> CommandLineArtifactVisitor<'v> for InputVisitor<'a> {
            fn visit_input(&mut self, input: ArtifactGroup, _tags: Vec<&ArtifactTag>) {
                if input.path_resolution_may_require_artifact_value() {
                    self.path_resolution_may_require_artifact_value = true;
                }

                if self.is_eligible_for_dedupe {
                    self.is_eligible_for_dedupe = input
                        .is_eligible_for_dedupe(self.target_platform)
                        == buck2_data::EligibleForDedupe::Eligible;
                }
            }

            fn visit_declared_output(
                &mut self,
                _artifact: OutputArtifact<'v>,
                _tags: Vec<&ArtifactTag>,
            ) {
            }

            fn visit_frozen_output(&mut self, _artifact: Artifact, _tags: Vec<&ArtifactTag>) {}
        }

        let (
            projection_path_resolution_may_require_artifact_value,
            projection_is_eligible_for_dedupe_iter,
        ): (Vec<bool>, Vec<bool>) = def
            .operations()
            .projections
            .iter()
            .enumerate()
            .map(|(idx, (_name, spec))| {
                let mut path_resolution_may_require_artifact_value = false;
                let mut is_eligible_for_dedupe = true;

                if let Some(node) = &node {
                    let projection = node
                        .projections
                        .get(idx)
                        .internal_error("Invalid projection id")?;

                    let mut visitor = InputVisitor::new(target_platform);
                    match spec.kind {
                        TransitiveSetProjectionKind::Args => {
                            TransitiveSetArgsProjection::as_command_line(*projection)?
                                .visit_artifacts(&mut visitor)?;
                        }
                        TransitiveSetProjectionKind::Json => {
                            visit_json_artifacts(*projection, &mut visitor)?
                        }
                    }
                    if visitor.path_resolution_may_require_artifact_value {
                        path_resolution_may_require_artifact_value = true;
                    }
                    if !visitor.is_eligible_for_dedupe {
                        is_eligible_for_dedupe = false;
                    }
                }

                for child in children_sets.iter() {
                    if child
                        .projection_path_resolution_may_require_artifact_value
                        .get(idx)?
                    {
                        path_resolution_may_require_artifact_value = true;
                    }

                    if is_eligible_for_dedupe
                        && !child.projection_is_eligible_for_dedupe.get(idx)?
                    {
                        let is_child_eligible_for_dedupe = child
                            .key
                            .holder_key()
                            .owner()
                            .configured_label()
                            .is_some_and(|l| l.cfg().is_marked_as_exec_platform());
                        if !is_child_eligible_for_dedupe {
                            is_eligible_for_dedupe = false;
                        }
                    }
                }

                Ok::<(bool, bool), buck2_error::Error>((
                    path_resolution_may_require_artifact_value,
                    is_eligible_for_dedupe,
                ))
            })
            .collect::<Result<Vec<(bool, bool)>, _>>()?
            .into_iter()
            .unzip();

        let projection_path_resolution_may_require_artifact_value =
            ProjectionBitSet::from_bools(&projection_path_resolution_may_require_artifact_value)
                .with_buck_error_context(|| {
                    format!("in transitive set {:?}", definition.as_debug())
                })?;
        let projection_is_eligible_for_dedupe =
            ProjectionBitSet::from_bools(&projection_is_eligible_for_dedupe_iter)
                .with_buck_error_context(|| {
                    format!("in transitive set {:?}", definition.as_debug())
                })?;

        Ok(Self {
            key,
            definition,
            node,
            reductions,
            projection_path_resolution_may_require_artifact_value,
            projection_is_eligible_for_dedupe,
            children,
        })
    }

    pub fn new_from_values(
        key: TransitiveSetKey,
        definition: FrozenValueTyped<'v, FrozenTransitiveSetDefinition<'v>>,
        value: Option<Value<'v>>,
        children: Option<Value<'v>>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<Self> {
        let children = children
            .map(|v| v.iterate(eval.heap()))
            .transpose()?
            .into_iter()
            .flatten();

        Self::new(key, definition, value, children, eval).map_err(Into::into)
    }
}

#[starlark_module]
fn transitive_set_methods(builder: &mut MethodsBuilder) {
    fn project_as_json<'v>(
        this: ValueOf<'v, &'v TransitiveSet<'v>>,
        projection: &str,
        #[starlark(require = named, default = "preorder")] ordering: &str,
    ) -> starlark::Result<TransitiveSetJsonProjection<'v>> {
        let def = this.typed.definition;

        let index = def
            .operations()
            .get_index_of_projection(TransitiveSetProjectionKind::Json, projection)?;

        Ok(TransitiveSetJsonProjection {
            transitive_set: ValueOfUnchecked::<FrozenTransitiveSet>::new(this.value),
            projection: index,
            ordering: TransitiveSetOrdering::parse(ordering)?,
        })
    }

    fn project_as_args<'v>(
        this: ValueOf<'v, &'v TransitiveSet<'v>>,
        projection: &str,
        #[starlark(require = named, default = "preorder")] ordering: &str,
    ) -> starlark::Result<TransitiveSetArgsProjection<'v>> {
        let def = this.typed.definition;

        let index = def
            .operations()
            .get_index_of_projection(TransitiveSetProjectionKind::Args, projection)?;

        Ok(TransitiveSetArgsProjection {
            transitive_set: ValueOfUnchecked::<FrozenTransitiveSet>::new(this.value),
            projection: index,
            ordering: TransitiveSetOrdering::parse(ordering)?,
        })
    }

    fn reduce<'v>(
        this: ValueOf<'v, &'v TransitiveSet<'v>>,
        reduction: &str,
    ) -> starlark::Result<Value<'v>> {
        let def = this.typed.definition;

        let index = match def.operations().reductions.get_index_of(reduction) {
            Some(index) => index,
            None => {
                return Err(
                    buck2_error::Error::from(TransitiveSetError::ReductionDoesNotExist {
                        reduction: reduction.into(),
                        valid_reductions: def
                            .operations()
                            .reductions
                            .keys()
                            .map(String::from)
                            .collect::<Vec<_>>(),
                    })
                    .into(),
                );
            }
        };

        Ok(this
            .typed
            .reductions
            .get(index)
            .copied()
            .with_internal_error(|| format!("Missing reduction {index}"))?)
    }

    fn traverse<'v>(
        this: ValueOf<'v, &'v TransitiveSet<'v>>,
        #[starlark(require = named, default = "preorder")] ordering: &str,
    ) -> starlark::Result<TransitiveSetTraversal<'v>> {
        Ok(TransitiveSetTraversal {
            inner: this.value,
            ordering: TransitiveSetOrdering::parse(ordering)?,
        })
    }

    #[starlark(attribute)]
    fn definition<'v>(
        this: ValueOf<'v, &'v TransitiveSet<'v>>,
    ) -> starlark::Result<FrozenValueTyped<'v, FrozenTransitiveSetDefinition<'v>>> {
        Ok(this.typed.definition())
    }

    #[starlark(attribute)]
    fn value<'v>(this: ValueOf<'v, &'v TransitiveSet<'v>>) -> starlark::Result<Value<'v>> {
        Ok(match this.typed.node.as_ref() {
            Some(node) => node.value,
            None => Value::new_none(),
        })
    }
    #[starlark(attribute)]
    fn children<'v>(this: ValueOf<'v, &'v TransitiveSet<'v>>) -> starlark::Result<Vec<Value<'v>>> {
        Ok(this.typed.children.to_vec())
    }
}
