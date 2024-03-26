/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use starlark::environment::MethodsBuilder;
use starlark::typing::Ty;
use starlark::values::list::ListOf;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::StringValue;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueOf;

use crate::interpreter::rule_defs::artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkArtifactLike;
use crate::interpreter::rule_defs::artifact::StarlarkDeclaredArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkOutputArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkPromiseArtifact;
use crate::interpreter::rule_defs::artifact::ValueAsArtifactLike;

#[derive(StarlarkTypeRepr, UnpackValue)]
pub(crate) enum EitherArtifactRef<'v> {
    Artifact(&'v StarlarkArtifact),
    DeclaredArtifact(&'v StarlarkDeclaredArtifact),
    PromiseArtifact(&'v StarlarkPromiseArtifact),
}

impl<'v> StarlarkTypeRepr for &'v dyn StarlarkArtifactLike {
    fn starlark_type_repr() -> Ty {
        EitherArtifactRef::starlark_type_repr()
    }
}

impl<'v> UnpackValue<'v> for &'v dyn StarlarkArtifactLike {
    fn unpack_value(value: Value<'v>) -> Option<Self> {
        match EitherArtifactRef::unpack_value(value)? {
            EitherArtifactRef::Artifact(artifact) => Some(artifact),
            EitherArtifactRef::DeclaredArtifact(artifact) => Some(artifact),
            EitherArtifactRef::PromiseArtifact(artifact) => Some(artifact),
        }
    }
}

#[derive(StarlarkTypeRepr)]
pub enum EitherStarlarkArtifact {
    Artifact(StarlarkArtifact),
    DeclaredArtifact(StarlarkDeclaredArtifact),
    PromiseArtifact(StarlarkPromiseArtifact),
}

// TODO(nga): derive.
impl<'v> AllocValue<'v> for EitherStarlarkArtifact {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        match self {
            EitherStarlarkArtifact::Artifact(artifact) => heap.alloc(artifact),
            EitherStarlarkArtifact::DeclaredArtifact(artifact) => heap.alloc(artifact),
            EitherStarlarkArtifact::PromiseArtifact(artifact) => heap.alloc(artifact),
        }
    }
}

/// A single input or output file for an action.
///
/// There is no `.parent` method on `artifact`, but in most cases
/// `cmd_args(my_artifact).parent()` can be used to similar effect.
#[starlark_module]
pub(crate) fn artifact_methods(builder: &mut MethodsBuilder) {
    /// The base name of this artifact. e.g. for an artifact at `foo/bar`, this is `bar`
    #[starlark(attribute)]
    fn basename<'v>(
        this: &'v dyn StarlarkArtifactLike,
        heap: &'v Heap,
    ) -> anyhow::Result<StringValue<'v>> {
        this.basename(heap)
    }

    /// The file extension of this artifact. e.g. for an artifact at foo/bar.sh,
    /// this is `.sh`. If no extension is present, `""` is returned.
    #[starlark(attribute)]
    fn extension<'v>(
        this: &'v dyn StarlarkArtifactLike,
        heap: &'v Heap,
    ) -> anyhow::Result<StringValue<'v>> {
        this.extension(heap)
    }

    /// Whether the artifact represents a source file
    #[starlark(attribute)]
    fn is_source<'v>(this: &'v dyn StarlarkArtifactLike) -> anyhow::Result<bool> {
        this.is_source()
    }

    /// The `Label` of the rule that originally created this artifact. May also be None in
    /// the case of source files, or if the artifact has not be used in an action, or if the
    /// action was not created by a rule.
    #[starlark(attribute)]
    fn owner<'v>(
        this: &'v dyn StarlarkArtifactLike,
    ) -> anyhow::Result<Option<StarlarkConfiguredProvidersLabel>> {
        this.owner()
    }

    /// The interesting part of the path, relative to somewhere in the output directory.
    /// For an artifact declared as `foo/bar`, this is `foo/bar`.
    #[starlark(attribute)]
    fn short_path<'v>(
        this: &'v dyn StarlarkArtifactLike,
        heap: &Heap,
    ) -> anyhow::Result<StringValue<'v>> {
        this.short_path(heap)
    }

    /// Returns a `StarlarkOutputArtifact` instance, or fails if the artifact is
    /// either an `Artifact`, or is a bound `Artifact` (You cannot bind twice)
    fn as_output<'v>(
        this: ValueOf<'v, &'v dyn StarlarkArtifactLike>,
    ) -> anyhow::Result<StarlarkOutputArtifact<'v>> {
        this.typed.as_output(this.value)
    }

    /// Create an artifact that lives at path relative from this artifact.
    ///
    /// For example, if artifact foo is a directory containing a file bar, then `foo.project("bar")`
    /// yields the file bar. It is possible for projected artifacts to hide the prefix in order to
    /// have the short name of the resulting artifact only contain the projected path, by passing
    /// `hide_prefix = True` to `project()`.
    fn project<'v>(
        this: &'v dyn StarlarkArtifactLike,
        #[starlark(require = pos)] path: &str,
        #[starlark(require = named, default = false)] hide_prefix: bool,
    ) -> anyhow::Result<StarlarkDeclaredArtifact> {
        this.project(path, hide_prefix)
    }

    /// Returns a `StarlarkArtifact` instance which is identical to the original artifact, except
    /// with no associated artifacts
    fn without_associated_artifacts<'v>(
        this: &'v dyn StarlarkArtifactLike,
    ) -> anyhow::Result<EitherStarlarkArtifact> {
        this.without_associated_artifacts()
    }

    /// Returns a `StarlarkArtifact` instance which is identical to the original artifact, but with
    /// potentially additional artifacts. The artifacts must be bound.
    fn with_associated_artifacts<'v>(
        this: &'v dyn StarlarkArtifactLike,
        artifacts: ListOf<'v, ValueAsArtifactLike<'v>>,
    ) -> anyhow::Result<EitherStarlarkArtifact> {
        this.with_associated_artifacts(artifacts)
    }
}
