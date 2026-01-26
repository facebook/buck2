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
use display_container::display_pair;
use display_container::fmt_container;
use display_container::iter_display_chain;
use dupe::Dupe;
use either::Either;
use gazebo::prelude::*;
use serde::Serialize;
use serde::Serializer;
use serde::ser::SerializeMap;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::type_matcher;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::FrozenValue;
use starlark::values::FrozenValueTyped;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;
use starlark::values::ValueOf;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTypedComplex;
use starlark::values::list::AllocList;
use starlark::values::starlark_value;
use starlark::values::typing::TypeInstanceId;
use starlark::values::typing::TypeMatcher;

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
use crate::interpreter::rule_defs::transitive_set::BfsTransitiveSetIteratorGen;
use crate::interpreter::rule_defs::transitive_set::DfsTransitiveSetIteratorGen;
use crate::interpreter::rule_defs::transitive_set::FrozenTransitiveSetDefinition;
use crate::interpreter::rule_defs::transitive_set::PostorderTransitiveSetIteratorGen;
use crate::interpreter::rule_defs::transitive_set::PreorderTransitiveSetIteratorGen;
use crate::interpreter::rule_defs::transitive_set::TopologicalTransitiveSetIteratorGen;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetArgsProjection;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetDefinition;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetError;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetIteratorLike;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetJsonProjection;
use crate::interpreter::rule_defs::transitive_set::transitive_set_definition::TransitiveSetDefinitionLike;
use crate::interpreter::rule_defs::transitive_set::transitive_set_definition::TransitiveSetProjectionKind;
use crate::interpreter::rule_defs::transitive_set::traversal::TransitiveSetOrdering;
use crate::interpreter::rule_defs::transitive_set::traversal::TransitiveSetTraversal;

#[derive(Clone, Debug, Allocative)]
pub(crate) struct TransitiveSetMatcher {
    pub(crate) type_instance_id: TypeInstanceId,
}

#[type_matcher]
impl TypeMatcher for TransitiveSetMatcher {
    fn matches(&self, value: Value) -> bool {
        let Some(tset) = ValueTypedComplex::<TransitiveSet>::new(value) else {
            return false;
        };
        let tset_definition: Value = match tset.unpack() {
            Either::Left(tset) => tset.definition.to_value(),
            Either::Right(tset) => tset.definition.to_value(),
        };
        let tset_definition = ValueTypedComplex::<TransitiveSetDefinition>::new(tset_definition)
            .expect("wrong type of definition");
        let exported = match tset_definition.unpack() {
            Either::Left(definition) => match definition.exported.get() {
                Some(definition) => definition,
                None => return false,
            },
            Either::Right(definition) => &definition.exported,
        };
        // TODO(nga): suboptimal: we could just compare to the pointer of the definition.
        exported.set_type_instance_id == self.type_instance_id
    }
}

#[derive(Debug, Clone, Trace, ProvidesStaticType, Allocative)]
#[repr(C)]
pub struct TransitiveSetGen<V: ValueLifetimeless> {
    /// A Deferred key that maps back to this set. This is used to compute its inputs.
    pub key: TransitiveSetKey,

    /// The TransitiveSetCallable that this set uses.
    pub(crate) definition: FrozenValueTyped<'static, FrozenTransitiveSetDefinition>,

    /// The immediate value of this node. If None, then this node will not yield anything when
    /// iterated over (but we'll still traverse to its children).
    pub(crate) node: Option<NodeGen<V>>,

    /// Pre-computed reductions. Those are arbitrary values based on the set's definition.
    pub(crate) reductions: Box<[V]>,

    /// For each projection, whether it uses content based paths or not.
    pub(crate) projection_uses_content_based_paths: Box<[bool]>,

    /// For each projection, whether it uses configuration based paths or not.
    pub(crate) projection_is_eligible_for_dedupe: Box<[bool]>,

    /// Further transitive sets.
    pub children: Box<[V]>,
}

#[derive(Debug, Clone, Trace, Allocative)]
#[repr(C)]
pub struct NodeGen<V: ValueLifetimeless> {
    /// The value
    pub value: V,

    /// Pre-computed projections.
    pub projections: Box<[V]>,
}

unsafe impl<'v> Coerce<TransitiveSetGen<Value<'v>>> for TransitiveSetGen<FrozenValue> {}

impl<V: ValueLifetimeless> fmt::Display for TransitiveSetGen<V> {
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

impl<V: ValueLifetimeless> TransitiveSetGen<V> {
    pub fn key(&self) -> &TransitiveSetKey {
        &self.key
    }
}

impl<'v> NodeGen<Value<'v>> {
    fn freeze(self, freezer: &Freezer) -> FreezeResult<NodeGen<FrozenValue>> {
        let Self { value, projections } = self;

        let value = value.freeze(freezer)?;
        let projections = projections.freeze(freezer)?;

        Ok(NodeGen { value, projections })
    }
}

impl<'v, V: ValueLike<'v>> TransitiveSetGen<V> {
    fn matches_definition(
        &self,
        definition: FrozenValueTyped<'v, FrozenTransitiveSetDefinition>,
    ) -> bool {
        definition.to_value().ptr_eq(self.definition.to_value())
    }

    pub fn projection_name(&'v self, projection: usize) -> buck2_error::Result<&'v str> {
        let def = self.definition.as_ref();

        Ok(def
            .operations()
            .projections
            .get_index(projection)
            .buck_error_context("Invalid projection id")?
            .0
            .as_str())
    }

    pub fn get_projection_value(&self, projection: usize) -> buck2_error::Result<Option<V>> {
        match &self.node {
            None => Ok(None),
            Some(node) => Ok(Some(
                *node
                    .projections
                    .get(projection)
                    .buck_error_context("Invalid projection id")?,
            )),
        }
    }

    pub fn get_projection_key(&self, projection: usize) -> TransitiveSetProjectionKey {
        TransitiveSetProjectionKey {
            key: self.key.dupe(),
            projection,
        }
    }

    pub(crate) fn definition(
        &self,
    ) -> buck2_error::Result<ValueTypedComplex<'v, TransitiveSetDefinition<'v>>> {
        ValueTypedComplex::unpack_value_err(self.definition.to_value())
            .internal_error("Must be a TransitiveSetDefinition")
    }
}

impl FrozenTransitiveSet {
    pub fn visit_projection_direct_inputs<'v, V: CommandLineArtifactVisitor<'v>>(
        &self,
        projection: usize,
        visitor: &mut V,
    ) -> buck2_error::Result<()> {
        if let Some(projection) = self.get_projection_value(projection)? {
            // It's either an args-like or a json projection. visit_json_artifacts handles both the way we want.
            visit_json_artifacts(projection.to_value(), visitor)?;
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
            let v =
                TransitiveSet::from_value(v.to_value()).buck_error_context("Invalid deferred")?;
            sub_inputs.push(ArtifactGroup::TransitiveSetProjection(Arc::new(
                TransitiveSetProjectionWrapper::new(
                    TransitiveSetProjectionKey {
                        key: v.key().dupe(),
                        projection,
                    },
                    v.projection_uses_content_based_paths[projection],
                    v.projection_is_eligible_for_dedupe[projection],
                ),
            )));
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
            TransitiveSetOrdering::Preorder => {
                Box::new(PreorderTransitiveSetIteratorGen::new(self))
            }
            TransitiveSetOrdering::Postorder => {
                Box::new(PostorderTransitiveSetIteratorGen::new(self))
            }
            TransitiveSetOrdering::Topological => {
                Box::new(TopologicalTransitiveSetIteratorGen::new(self))
            }
            TransitiveSetOrdering::Bfs => Box::new(BfsTransitiveSetIteratorGen::new(self)),
            TransitiveSetOrdering::Dfs => Box::new(DfsTransitiveSetIteratorGen::new(self)),
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
                .buck_error_context("Invalid projection")?;
        }

        Ok(Box::new(iter.map(move |node| {
            node.projections.get(projection).unwrap().to_value()
        })))
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

#[starlark_value(type = "TransitiveSet")]
impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for TransitiveSetGen<V>
where
    Self: ProvidesStaticType<'v> + TransitiveSetLike<'v>,
{
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(transitive_set_methods)
    }
}

impl<'v> Freeze for TransitiveSet<'v> {
    type Frozen = FrozenTransitiveSet;

    fn freeze(self, freezer: &Freezer) -> FreezeResult<Self::Frozen> {
        let Self {
            key,
            definition,
            node,
            reductions,
            projection_uses_content_based_paths,
            projection_is_eligible_for_dedupe,
            children,
        } = self;
        let definition = definition.freeze(freezer)?;
        let node = node.try_map(|node| node.freeze(freezer))?;
        let children = children.freeze(freezer)?;
        let reductions = reductions.freeze(freezer)?;
        let projection_uses_content_based_paths =
            projection_uses_content_based_paths.freeze(freezer)?;
        Ok(TransitiveSetGen {
            key,
            definition,
            node,
            reductions,
            projection_uses_content_based_paths,
            projection_is_eligible_for_dedupe,
            children,
        })
    }
}

impl<'v> TransitiveSet<'v> {
    pub fn new(
        key: TransitiveSetKey,
        definition: FrozenValueTyped<'v, FrozenTransitiveSetDefinition>,
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
                fn format_def(def: &FrozenTransitiveSetDefinition) -> String {
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

            buck2_error::Ok(NodeGen { value, projections })
        })?;

        let reductions =
            def.operations()
                .reductions
                .iter()
                .enumerate()
                .map(|(idx, (name, reduce))| {
                    let children_values = children_sets.try_map(|c| {
                        c.reductions.get(idx).copied().with_buck_error_context(|| {
                            format!("Child {c} is missing reduction {idx}")
                        })
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

        struct InputVisitor {
            target_platform: Option<ConfigurationData>,
            has_content_based_input: bool,
            is_eligible_for_dedupe: bool,
        }

        impl InputVisitor {
            fn new(target_platform: Option<ConfigurationData>) -> Self {
                Self {
                    target_platform,
                    has_content_based_input: false,
                    is_eligible_for_dedupe: true,
                }
            }
        }

        impl<'v> CommandLineArtifactVisitor<'v> for InputVisitor {
            fn visit_input(&mut self, input: ArtifactGroup, _tags: Vec<&ArtifactTag>) {
                if input.uses_content_based_path() {
                    self.has_content_based_input = true;
                }

                if self.is_eligible_for_dedupe {
                    self.is_eligible_for_dedupe =
                        input.is_eligible_for_dedupe(self.target_platform.as_ref());
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

        let owner = key.holder_key().owner();
        let target_platform = if let BaseDeferredKey::TargetLabel(configured_label) = owner {
            Some(configured_label.cfg().dupe())
        } else {
            None
        };

        let (projection_uses_content_based_paths_iter, projection_is_eligible_for_dedupe_iter): (
            Vec<bool>,
            Vec<bool>,
        ) = def
            .operations()
            .projections
            .iter()
            .enumerate()
            .map(|(idx, (_name, spec))| {
                let mut uses_content_based_paths = false;
                let mut is_eligible_for_dedupe = true;

                if let Some(node) = &node {
                    let projection = node
                        .projections
                        .get(idx)
                        .buck_error_context("Invalid projection id")?;

                    let mut visitor = InputVisitor::new(target_platform.dupe());
                    match spec.kind {
                        TransitiveSetProjectionKind::Args => {
                            TransitiveSetArgsProjection::as_command_line(*projection)?
                                .visit_artifacts(&mut visitor)?;
                        }
                        TransitiveSetProjectionKind::Json => {
                            visit_json_artifacts(*projection, &mut visitor)?
                        }
                    }
                    if visitor.has_content_based_input {
                        uses_content_based_paths = true;
                    }
                    if !visitor.is_eligible_for_dedupe {
                        is_eligible_for_dedupe = false;
                    }
                }

                for child in children_sets.iter() {
                    if *child
                        .projection_uses_content_based_paths
                        .get(idx)
                        .buck_error_context("Invalid projection id")?
                    {
                        uses_content_based_paths = true;
                    }

                    if is_eligible_for_dedupe
                        && !*child
                            .projection_is_eligible_for_dedupe
                            .get(idx)
                            .buck_error_context("Invalid projection id")?
                    {
                        let target_platform_ref = match target_platform {
                            Some(ref target_platform) => target_platform,
                            None => {
                                is_eligible_for_dedupe = false;
                                continue;
                            }
                        };
                        let is_child_eligible_for_dedupe = child
                            .key
                            .holder_key()
                            .owner()
                            .configured_label()
                            .is_some_and(|l| l.cfg() != target_platform_ref);
                        if !is_child_eligible_for_dedupe {
                            is_eligible_for_dedupe = false;
                        }
                    }
                }

                Ok::<(bool, bool), buck2_error::Error>((
                    uses_content_based_paths,
                    is_eligible_for_dedupe,
                ))
            })
            .collect::<Result<Vec<(bool, bool)>, _>>()?
            .into_iter()
            .unzip();

        let (projection_uses_content_based_paths, projection_is_eligible_for_dedupe) = (
            projection_uses_content_based_paths_iter.into_boxed_slice(),
            projection_is_eligible_for_dedupe_iter.into_boxed_slice(),
        );

        // Cast lifetime from 'v to 'static
        let definition =
            FrozenValueTyped::<FrozenTransitiveSetDefinition>::new(FrozenValueTyped::<
                FrozenTransitiveSetDefinition,
            >::to_frozen_value(
                definition
            ))
            .buck_error_context("internal error")?;
        Ok(Self {
            key,
            definition,
            node,
            reductions,
            projection_uses_content_based_paths,
            projection_is_eligible_for_dedupe,
            children,
        })
    }

    pub fn new_from_values(
        key: TransitiveSetKey,
        definition: FrozenValueTyped<'v, FrozenTransitiveSetDefinition>,
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
            .with_buck_error_context(|| format!("Missing reduction {index}"))?)
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
    ) -> starlark::Result<ValueTypedComplex<'v, TransitiveSetDefinition<'v>>> {
        Ok(this.typed.definition()?)
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
