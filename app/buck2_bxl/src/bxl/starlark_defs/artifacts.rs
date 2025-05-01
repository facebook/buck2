/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! bxl additional artifact types

use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;
use std::iter;

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::BaseArtifactKind;
use buck2_build_api::actions::calculation::ActionCalculation;
use buck2_build_api::actions::execute::action_executor::ActionOutputs;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::artifact_groups::ResolvedArtifactGroup;
use buck2_build_api::artifact_groups::calculation::ArtifactGroupCalculation;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_like::StarlarkArtifactLike;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_declared_artifact::StarlarkDeclaredArtifact;
use buck2_execute::path::artifact_path::ArtifactPath;
use derive_more::Display;
use dice::DiceComputations;
use dupe::Dupe;
use dupe::IterDupedExt;
use futures::FutureExt;
use indexmap::IndexSet;
use serde::Serialize;
use serde::Serializer;
use starlark::any::ProvidesStaticType;
use starlark::collections::SmallSet;
use starlark::collections::StarlarkHasher;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::ValueTyped;
use starlark::values::starlark_value;
use starlark::values::type_repr::StarlarkTypeRepr;

#[derive(Clone, Debug, Dupe, Trace, ProvidesStaticType, Allocative)]
#[repr(C)]
pub(crate) enum EnsuredArtifact {
    Artifact {
        artifact: StarlarkArtifact,
        abs: bool,
    },
    DeclaredArtifact {
        artifact: StarlarkDeclaredArtifact,
        abs: bool,
    },
}

#[derive(Clone, Debug, Trace, ProvidesStaticType, Allocative)]
#[repr(C)]
pub(crate) struct EnsuredArtifactGroupInner {
    pub(crate) ags: Vec<ArtifactGroup>,
}

pub(crate) async fn visit_artifact_path_without_associated_deduped(
    ags: &[ArtifactGroup],
    abs: bool,
    mut visitor: impl FnMut(ArtifactPath, bool) -> buck2_error::Result<()>,
    ctx: &mut DiceComputations<'_>,
) -> buck2_error::Result<()> {
    // If there's a case where a tset projection returns a projection, we want to make sure
    // we are not reprocessing the nested projection over again. Since we are using

    // `get_projection_sub_inputs()` to iterate through the tset, this is also needed to make
    // sure we aren't re-processing tests.
    let mut visited = SmallSet::new();

    let mut todo = Vec::new();
    todo.extend(ags.iter().duped());

    while let Some(ag) = todo.pop() {
        if !visited.insert(ag.dupe()) {
            continue;
        }
        match ag.resolved_artifact(ctx).await? {
            ResolvedArtifactGroup::Artifact(a) => {
                visitor(a.get_path(), abs)?;
            }
            ResolvedArtifactGroup::TransitiveSetProjection(t) => {
                let set = t.key.lookup(ctx).await?;
                todo.extend(set.get_projection_sub_inputs(t.projection)?);
            }
        }
    }

    Ok(())
}

#[derive(Clone, Dupe, Debug, Trace, ProvidesStaticType, Allocative)]
#[repr(C)]
pub(crate) struct EnsuredArtifactGroup<'v> {
    // Have `EnsuredArtifactGroup` be a wrapper around `EnsuredArtifactGroupInner` as a Starlark `Value`
    // so that we don't have to copy all of its artifact groups whenever we call `abs_path()` or `rel_path()`,
    // and we can instead just copy the `Value` (which is a pointer)
    pub(crate) inner: Value<'v>,
    pub(crate) abs: bool,
}

impl<'v> EnsuredArtifactGroup<'v> {
    pub(crate) fn new(ags: Vec<ArtifactGroup>, abs: bool, heap: &'v Heap) -> Self {
        EnsuredArtifactGroup {
            inner: heap.alloc(EnsuredArtifactGroupInner { ags }),

            abs,
        }
    }

    pub(crate) fn inner(&self) -> &Vec<ArtifactGroup> {
        &<&EnsuredArtifactGroupInner>::unpack_value(self.inner)
            .unwrap()
            .unwrap()
            .ags
    }

    pub(crate) async fn visit_artifact_path_without_associated_deduped(
        &self,
        visitor: impl FnMut(ArtifactPath, bool) -> buck2_error::Result<()>,
        ctx: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<()> {
        visit_artifact_path_without_associated_deduped(self.inner(), self.abs, visitor, ctx).await
    }
}

#[starlark_value(type = "bxl.EnsuredArtifactGroup", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for EnsuredArtifactGroup<'v>
where
    Self: ProvidesStaticType<'v>,
{
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(artifact_group_methods)
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> starlark::Result<()> {
        let _hash = self.inner.write_hash(hasher);
        self.abs.hash(hasher);
        Ok(())
    }
}

#[starlark_value(type = "bxl.EnsuredArtifactGroupInner", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for EnsuredArtifactGroupInner
where
    Self: ProvidesStaticType<'v>,
{
    fn write_hash(&self, hasher: &mut StarlarkHasher) -> starlark::Result<()> {
        self.ags.hash(hasher);
        Ok(())
    }
}

impl Hash for EnsuredArtifact {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_artifact().fingerprint().hash(state);
    }
}

impl PartialEq for EnsuredArtifact {
    fn eq(&self, other: &Self) -> bool {
        self.abs() == other.abs()
            && self.as_artifact().fingerprint() == other.as_artifact().fingerprint()
    }
}

impl Eq for EnsuredArtifact {}

#[derive(StarlarkTypeRepr, UnpackValue, Display)]
pub(crate) enum ArtifactArg<'v> {
    Artifact(&'v StarlarkArtifact),
    DeclaredArtifact(&'v StarlarkDeclaredArtifact),
}

impl<'v> ArtifactArg<'v> {
    pub(crate) fn into_ensured_artifact(self) -> EnsuredArtifact {
        match self {
            ArtifactArg::Artifact(artifact) => EnsuredArtifact::Artifact {
                artifact: artifact.dupe(),
                abs: false,
            },
            ArtifactArg::DeclaredArtifact(artifact) => EnsuredArtifact::DeclaredArtifact {
                artifact: artifact.dupe(),
                abs: false,
            },
        }
    }
}

impl EnsuredArtifact {
    pub(crate) fn as_artifact(&self) -> &dyn StarlarkArtifactLike {
        match self {
            EnsuredArtifact::Artifact { artifact, .. } => artifact as &dyn StarlarkArtifactLike,
            EnsuredArtifact::DeclaredArtifact { artifact, .. } => {
                artifact as &dyn StarlarkArtifactLike
            }
        }
    }

    pub(crate) fn abs(&self) -> bool {
        match self {
            EnsuredArtifact::Artifact { abs, .. } => *abs,
            EnsuredArtifact::DeclaredArtifact { abs, .. } => *abs,
        }
    }

    pub(crate) fn get_artifact_path(&self) -> ArtifactPath {
        match self {
            EnsuredArtifact::Artifact { artifact, .. } => artifact.get_artifact_path(),
            EnsuredArtifact::DeclaredArtifact { artifact, .. } => artifact.get_artifact_path(),
        }
    }
}

impl Display for EnsuredArtifact {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // this can't be converted to string in starlark and used as path arbitrarily.
        // this can only be printed as path via `ctx.output.print()`. Anywhere that wants to use
        // it as a path to commands should use the normal starlark artifacts to construct their
        // command
        write!(f, "<ensured {}>", self.as_artifact())
    }
}

impl<'v> Display for EnsuredArtifactGroup<'v> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // this can't be converted to string in starlark and used as path arbitrarily.
        // this can only be printed as path via `ctx.output.print()`. Anywhere that wants to use
        // it as a path to commands should use the normal starlark artifacts to construct their
        // command
        write!(f, "<ensured group>")
    }
}

impl Display for EnsuredArtifactGroupInner {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // This is not exposed to users.
        write!(f, "<ensured group inner>")
    }
}

impl<'v> AllocValue<'v> for EnsuredArtifact {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

impl<'v> AllocValue<'v> for EnsuredArtifactGroup<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

impl<'v> AllocValue<'v> for EnsuredArtifactGroupInner {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

impl Serialize for EnsuredArtifact {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        s.collect_str(self)
    }
}

impl<'v> Serialize for EnsuredArtifactGroup<'v> {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        s.collect_str(self)
    }
}

impl Serialize for EnsuredArtifactGroupInner {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        s.collect_str(self)
    }
}

#[starlark_value(type = "bxl.EnsuredArtifact", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for EnsuredArtifact
where
    Self: ProvidesStaticType<'v>,
{
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(ensured_artifact_methods)
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> starlark::Result<()> {
        let _artifact_write_hash = self.as_artifact().write_hash(hasher);
        self.abs().hash(hasher);
        Ok(())
    }
}

/// An artifact that will be materialized to buck-out at the end of the bxl invocation.
/// These artifacts can be printed to bxl's results. Doing so will print the path of the artifact
/// rather than the standard representation.
///
/// Ensured artifacts are serializable and hashable.
#[starlark_module]
fn ensured_artifact_methods(builder: &mut MethodsBuilder) {
    /// Converts this artifact to be printed by its absolute path. Note that this will only print out the
    /// absolute path via `ctx.output.print()`. Starlark's `print()` will print out the display info for an
    /// ensured artifact.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_abs_path(ctx):
    ///     actions = ctx.bxl_actions().actions
    ///     output = actions.write("my_output", "my_content")
    ///     ensured = ctx.output.ensure(output) # currently defaults to creating an EnsuredArtifact with a relative path
    ///
    ///     ensured_with_abs_path = ensured.abs_path() # create a new EnsuredArtifact with absolute path to reuse
    ///     print(ensured_with_abs_path) # should return something like <ensured artifact ... >
    ///     ctx.output.print(ensured_with_abs_path) # should return the absolute path of the artifact
    /// ```
    fn abs_path<'v>(
        this: ValueTyped<'v, EnsuredArtifact>,
        heap: &'v Heap,
    ) -> starlark::Result<ValueTyped<'v, EnsuredArtifact>> {
        if this.abs() {
            Ok(this)
        } else {
            let artifact = match &*this {
                EnsuredArtifact::Artifact { artifact, .. } => EnsuredArtifact::Artifact {
                    artifact: artifact.dupe(),
                    abs: true,
                },
                EnsuredArtifact::DeclaredArtifact { artifact, .. } => {
                    EnsuredArtifact::DeclaredArtifact {
                        artifact: artifact.dupe(),
                        abs: true,
                    }
                }
            };

            Ok(heap.alloc_typed(artifact))
        }
    }

    /// Converts this artifact to be printed by its path relative to the project root.
    /// Note that this will only print out the relative path via `ctx.output.print()`.
    /// Starlark's `print()` will print out the display info for an ensured artifact.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_rel_path(ctx):
    ///     actions = ctx.bxl_actions().actions
    ///     output = actions.write("my_output", "my_content")
    ///     ensured = ctx.output.ensure(output) # currently defaults to creating an EnsuredArtifact with a relative path
    ///
    ///     ensured_with_rel_path = ensured.rel_path() # create a new EnsuredArtifact with relative path to reuse
    ///     print(ensured_with_rel_path) # should return something like <ensured artifact ... >
    ///     ctx.output.print(ensured_with_rel_path) # should return the relative path of the artifact
    /// ```
    fn rel_path<'v>(
        this: ValueTyped<'v, EnsuredArtifact>,
        heap: &'v Heap,
    ) -> starlark::Result<ValueTyped<'v, EnsuredArtifact>> {
        if !this.abs() {
            Ok(this)
        } else {
            let artifact = match &*this {
                EnsuredArtifact::Artifact { artifact, .. } => EnsuredArtifact::Artifact {
                    artifact: artifact.dupe(),
                    abs: false,
                },
                EnsuredArtifact::DeclaredArtifact { artifact, .. } => {
                    EnsuredArtifact::DeclaredArtifact {
                        artifact: artifact.dupe(),
                        abs: false,
                    }
                }
            };

            Ok(heap.alloc_typed(artifact))
        }
    }
}

/// An artifact group that will be materialized to buck-out at the end of the bxl invocation.
/// These artifacts can be printed to bxl's results. Doing so will print the path of each artifact in
/// the group rather than the standard representation.
///
/// Ensured artifact groups are serializable and hashable.
#[starlark_module]
fn artifact_group_methods(builder: &mut MethodsBuilder) {
    /// Converts each artifact in this artifact group to be printed by its absolute path. Note that this will only print out the
    /// absolute path via `ctx.output.print()`. Starlark's `print()` will print out the display info for an
    /// ensured artifact group.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_abs_path(ctx):
    ///     # some target with RunInfo outputs
    ///     result = ctx.analysis("root//bin/kind:target_with_outputs")
    ///     ensured = ctx.output.ensure_multiple(result.providers()[RunInfo]) # currently defaults to creating an EnsuredArtifactGroup with a relative path
    ///
    ///     ensured_with_abs_path = ensured.abs_path() # create a new EnsuredArtifactGroup with absolute path to reuse
    ///     print(ensured_with_abs_path) # should return something like <ensured group ... >
    ///     ctx.output.print(ensured_with_abs_path) # should return the absolute path of the artifact
    /// ```
    fn abs_path<'v>(
        this: ValueTyped<'v, EnsuredArtifactGroup<'v>>,
        heap: &'v Heap,
    ) -> starlark::Result<ValueTyped<'v, EnsuredArtifactGroup<'v>>> {
        if this.abs {
            Ok(this)
        } else {
            let artifact = EnsuredArtifactGroup {
                inner: this.inner,
                abs: true,
            };

            Ok(heap.alloc_typed(artifact))
        }
    }

    /// Converts each artifact in this artifact group to be printed by its path relative to the project root.
    /// Note that this will only print out the relative path via `ctx.output.print()`.
    /// Starlark's `print()` will print out the display info for an ensured artifact group.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_rel_path(ctx):
    ///     # some target with RunInfo outputs
    ///     result = ctx.analysis("root//bin/kind:target_with_outputs")
    ///     ensured = ctx.output.ensure_multiple(result.providers()[RunInfo]) # currently defaults to creating an EnsuredArtifactGroup with a relative path
    ///
    ///     ensured_with_rel_path = ensured.rel_path() # create a new EnsuredArtifact with relative path to reuse
    ///     print(ensured_with_rel_path) # should return something like <ensured group ... >
    ///     ctx.output.print(ensured_with_rel_path) # should return the relative path of the artifact
    /// ```
    fn rel_path<'v>(
        this: ValueTyped<'v, EnsuredArtifactGroup<'v>>,
        heap: &'v Heap,
    ) -> starlark::Result<ValueTyped<'v, EnsuredArtifactGroup<'v>>> {
        if !this.abs {
            Ok(this)
        } else {
            let artifact = EnsuredArtifactGroup {
                inner: this.inner,
                abs: false,
            };

            Ok(heap.alloc_typed(artifact))
        }
    }
}

#[derive(Debug, Allocative)]
pub(crate) struct LazyBuildArtifact {
    /// The artifacts that are associated with this artifact. This is used to materialize.
    artifacts_to_build: IndexSet<ArtifactGroup>,
    artifact: StarlarkArtifact,
}

impl LazyBuildArtifact {
    pub(crate) fn new(artifact: &StarlarkArtifact) -> Self {
        let as_artifact = artifact as &dyn StarlarkArtifactLike;

        let bound_artifact = as_artifact.get_bound_artifact().unwrap();
        let associated_artifacts = as_artifact.get_associated_artifacts();

        let artifacts = associated_artifacts
            .iter()
            .flat_map(|v| v.iter())
            .cloned()
            .chain(iter::once(ArtifactGroup::Artifact(bound_artifact)))
            .collect::<IndexSet<_>>();

        LazyBuildArtifact {
            artifacts_to_build: artifacts,
            artifact: artifact.dupe(),
        }
    }

    pub(crate) fn artifact(&self) -> StarlarkArtifact {
        self.artifact.dupe()
    }

    pub(crate) async fn build_artifacts(
        &self,
        ctx: &mut DiceComputations<'_>,
    ) -> anyhow::Result<Vec<ActionOutputs>> {
        let res = ctx
            .try_compute_join(&self.artifacts_to_build, |ctx, artifact_group| {
                async move {
                    let artifact_group_values = ctx.ensure_artifact_group(artifact_group).await?;
                    let build_artifacts = artifact_group_values
                        .iter()
                        .filter_map(|(artifact, _value)| {
                            if let BaseArtifactKind::Build(artifact) = artifact.as_parts().0 {
                                Some(artifact.dupe())
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>();
                    ctx.try_compute_join(build_artifacts, |ctx, build_artifact| {
                        async move { ActionCalculation::build_artifact(ctx, &build_artifact).await }
                            .boxed()
                    })
                    .await
                }
                .boxed()
            })
            .await?;
        Ok(res.into_iter().flatten().collect::<Vec<_>>())
    }
}
