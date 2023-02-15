/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! bxl additional artifact types

use std::fmt::Display;
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;

use allocative::Allocative;
use anyhow::Context as _;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::calculation::Calculation;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_like::StarlarkArtifactLike;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkDeclaredArtifact;
use buck2_execute::path::artifact_path::ArtifactPath;
use dice::DiceComputations;
use dupe::Dupe;
use dupe::IterDupedExt;
use serde::Serialize;
use serde::Serializer;
use starlark::any::ProvidesStaticType;
use starlark::collections::SmallSet;
use starlark::collections::StarlarkHasher;
use starlark::docs::StarlarkDocs;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_type;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::ValueOf;

#[derive(Clone, Debug, Trace, ProvidesStaticType, StarlarkDocs, Allocative)]
#[repr(C)]
#[starlark_docs(directory = "bxl")]
pub enum EnsuredArtifact {
    Artifact {
        artifact: StarlarkArtifact,
        abs: bool,
    },
    DeclaredArtifact {
        artifact: StarlarkDeclaredArtifact,
        abs: bool,
    },
}

#[derive(Clone, Debug, Trace, ProvidesStaticType, StarlarkDocs, Allocative)]
#[repr(C)]
pub struct EnsuredArtifactGroupInner {
    pub(crate) ags: Vec<ArtifactGroup>,
}

pub async fn visit_artifact_path_without_associated_deduped<'v>(
    ags: &'v [ArtifactGroup],
    abs: bool,
    mut visitor: impl FnMut(ArtifactPath, bool) -> anyhow::Result<()>,
    ctx: &'v DiceComputations,
) -> anyhow::Result<()> {
    // If there's a case where a tset projection returns a projection, we want to make sure
    // we are not reprocessing the nested projection over again. Since we are using

    // `get_projection_sub_inputs()` to iterate through the tset, this is also needed to make
    // sure we aren't re-processing tests.
    let mut visited_artifact_groups = SmallSet::new();
    let mut visited_artifacts = SmallSet::new();

    let mut todo = Vec::new();
    todo.extend(ags.iter().duped());

    while let Some(ag) = todo.pop() {
        match ag {
            ArtifactGroup::Artifact(a) => {
                if !visited_artifacts.insert(a.dupe()) {
                    continue;
                }

                visitor(a.get_path(), abs)?;
            }
            ArtifactGroup::TransitiveSetProjection(t) => {
                if !visited_artifact_groups.insert(t.dupe()) {
                    continue;
                }

                let set = ctx
                    .compute_deferred_data(&t.key)
                    .await
                    .context("Failed to compute deferred for transitive set projection key")?;

                let set = set.as_transitive_set()?;

                todo.extend(set.get_projection_sub_inputs(t.projection)?);
            }
        }
    }

    Ok(())
}

#[derive(Clone, Debug, Trace, ProvidesStaticType, StarlarkDocs, Allocative)]
#[repr(C)]
#[starlark_docs(directory = "bxl")]
pub struct EnsuredArtifactGroup<'v> {
    // Have `EnsuredArtifactGroup` be a wrapper around `EnsuredArtifactGroupInner` as a Starlark `Value`
    // so that we don't have to copy all of its artifact groups whenever we call `abs_path()` or `rel_path()`,
    // and we can instead just copy the `Value` (which is a pointer)
    pub(crate) inner: Value<'v>,
    pub(crate) abs: bool,
}

impl<'v> EnsuredArtifactGroup<'v> {
    pub fn new(ags: Vec<ArtifactGroup>, abs: bool, heap: &'v Heap) -> Self {
        EnsuredArtifactGroup {
            inner: heap.alloc(EnsuredArtifactGroupInner { ags }),

            abs,
        }
    }

    pub fn inner(&self) -> &Vec<ArtifactGroup> {
        &<&EnsuredArtifactGroupInner>::unpack_value(self.inner)
            .unwrap()
            .ags
    }

    pub async fn visit_artifact_path_without_associated_deduped(
        &self,
        visitor: impl FnMut(ArtifactPath, bool) -> anyhow::Result<()>,
        ctx: &'v DiceComputations,
    ) -> anyhow::Result<()> {
        visit_artifact_path_without_associated_deduped(self.inner(), self.abs, visitor, ctx).await
    }
}

impl<'v> StarlarkValue<'v> for EnsuredArtifactGroup<'v>
where
    Self: ProvidesStaticType,
{
    starlark_type!("ensured_artifact_group");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(artifact_group_methods)
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        let _hash = self.inner.write_hash(hasher);
        self.abs.hash(hasher);
        Ok(())
    }
}

impl<'v> StarlarkValue<'v> for EnsuredArtifactGroupInner
where
    Self: ProvidesStaticType,
{
    starlark_type!("ensured_artifact_group_inner");

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        self.ags.hash(hasher);
        Ok(())
    }
}

impl<'v> UnpackValue<'v> for &'v EnsuredArtifactGroup<'v> {
    fn unpack_value(x: Value<'v>) -> Option<&'v EnsuredArtifactGroup<'v>> {
        x.downcast_ref()
    }
}

impl<'v> UnpackValue<'v> for &'v EnsuredArtifactGroupInner {
    fn unpack_value(x: Value<'v>) -> Option<&'v EnsuredArtifactGroupInner> {
        x.downcast_ref()
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

impl EnsuredArtifact {
    pub fn new<'v>(artifact: Value<'v>) -> anyhow::Result<Self> {
        let artifact = artifact
            .downcast_ref::<StarlarkArtifact>()
            .map(|o| EnsuredArtifact::Artifact {
                artifact: o.dupe(),
                abs: false,
            })
            .or_else(|| {
                artifact
                    .downcast_ref::<StarlarkDeclaredArtifact>()
                    .map(|o| EnsuredArtifact::DeclaredArtifact {
                        artifact: o.dupe(),
                        abs: false,
                    })
            });

        match artifact {
            Some(artifact) => Ok(artifact),
            None => Err(anyhow::anyhow!("must be artifact like")),
        }
    }

    pub fn as_artifact(&self) -> &dyn StarlarkArtifactLike {
        match self {
            EnsuredArtifact::Artifact { artifact, .. } => artifact as &dyn StarlarkArtifactLike,
            EnsuredArtifact::DeclaredArtifact { artifact, .. } => {
                artifact as &dyn StarlarkArtifactLike
            }
        }
    }

    pub fn abs(&self) -> bool {
        match self {
            EnsuredArtifact::Artifact { abs, .. } => *abs,
            EnsuredArtifact::DeclaredArtifact { abs, .. } => *abs,
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

impl<'v> StarlarkTypeRepr for &'v EnsuredArtifact {
    fn starlark_type_repr() -> String {
        EnsuredArtifact::get_type_starlark_repr()
    }
}

impl<'v> StarlarkTypeRepr for &'v EnsuredArtifactGroup<'v> {
    fn starlark_type_repr() -> String {
        EnsuredArtifactGroup::get_type_starlark_repr()
    }
}

impl<'v> StarlarkTypeRepr for &'v EnsuredArtifactGroupInner {
    fn starlark_type_repr() -> String {
        EnsuredArtifactGroupInner::get_type_starlark_repr()
    }
}

impl<'v> UnpackValue<'v> for &'v EnsuredArtifact {
    fn unpack_value(x: Value<'v>) -> Option<&'v EnsuredArtifact> {
        x.downcast_ref()
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

impl<'v> StarlarkValue<'v> for EnsuredArtifact
where
    Self: ProvidesStaticType,
{
    starlark_type!("ensured_artifact");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(artifact_methods)
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
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
fn artifact_methods(builder: &mut MethodsBuilder) {
    /// Converts this artifact to be printed by its absolute path. Note that this will only print out the
    /// absolute path via `ctx.output.print()`. Starlark's `print()` will print out the display info for an
    /// ensured artifact.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_abs_path(ctx):
    ///     actions = ctx.bxl_actions.action_factory()
    ///     output = actions.write("my_output", "my_content")
    ///     ensured = ctx.output.ensure(output) # currently defaults to creating an EnsuredArtifact with a relative path
    ///     
    ///     ensured_with_abs_path = ensured.abs_path() # create a new EnsuredArtifact with absolute path to reuse
    ///     print(ensured_with_abs_path) # should return something like <ensured artifact ... >
    ///     ctx.output.print(ensured_with_abs_path) # should return the absolute path of the artifact
    /// ```
    fn abs_path<'v>(
        this: ValueOf<'v, &'v EnsuredArtifact>,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        if this.typed.abs() {
            Ok(this.value)
        } else {
            let artifact = match this.typed {
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

            Ok(heap.alloc(artifact))
        }
    }

    /// Converts this artifact to be printed by its path relative to the project root.
    /// Note that this will only print out the relative path via `ctx.output.print()`.
    /// Starlark's `print()` will print out the display info for an ensured artifact.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_rel_path(ctx):
    ///     actions = ctx.bxl_actions.action_factory()
    ///     output = actions.write("my_output", "my_content")
    ///     ensured = ctx.output.ensure(output) # currently defaults to creating an EnsuredArtifact with a relative path
    ///     
    ///     ensured_with_rel_path = ensured.rel_path() # create a new EnsuredArtifact with relative path to reuse
    ///     print(ensured_with_rel_path) # should return something like <ensured artifact ... >
    ///     ctx.output.print(ensured_with_rel_path) # should return the relative path of the artifact
    /// ```
    fn rel_path<'v>(
        this: ValueOf<'v, &'v EnsuredArtifact>,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        if !this.typed.abs() {
            Ok(this.value)
        } else {
            let artifact = match this.typed {
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

            Ok(heap.alloc(artifact))
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
    /// ```text
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
        this: ValueOf<'v, &'v EnsuredArtifactGroup<'v>>,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        if this.typed.abs {
            Ok(this.value)
        } else {
            let artifact = EnsuredArtifactGroup {
                inner: this.typed.inner,
                abs: true,
            };

            Ok(heap.alloc(artifact))
        }
    }

    /// Converts each artifact in this artifact group to be printed by its path relative to the project root.
    /// Note that this will only print out the relative path via `ctx.output.print()`.
    /// Starlark's `print()` will print out the display info for an ensured artifact group.
    ///
    /// Sample usage:
    /// ```text
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
        this: ValueOf<'v, &'v EnsuredArtifactGroup<'v>>,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        if !this.typed.abs {
            Ok(this.value)
        } else {
            let artifact = EnsuredArtifactGroup {
                inner: this.typed.inner,
                abs: false,
            };

            Ok(heap.alloc(artifact))
        }
    }
}
