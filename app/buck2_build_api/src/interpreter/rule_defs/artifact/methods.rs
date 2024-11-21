/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::convert::Infallible;

use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_interpreter::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use starlark::environment::MethodsBuilder;
use starlark::typing::Ty;
use starlark::values::list::UnpackList;
use starlark::values::none::NoneOr;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::StringValue;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueOf;

use crate::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::starlark_artifact_like::StarlarkArtifactLike;
use crate::interpreter::rule_defs::artifact::starlark_artifact_like::ValueAsArtifactLike;
use crate::interpreter::rule_defs::artifact::starlark_declared_artifact::StarlarkDeclaredArtifact;
use crate::interpreter::rule_defs::artifact::starlark_output_artifact::StarlarkOutputArtifact;
use crate::interpreter::rule_defs::artifact::starlark_promise_artifact::StarlarkPromiseArtifact;

#[derive(StarlarkTypeRepr, UnpackValue)]
pub enum EitherArtifactRef<'v> {
    Artifact(&'v StarlarkArtifact),
    DeclaredArtifact(&'v StarlarkDeclaredArtifact),
    PromiseArtifact(&'v StarlarkPromiseArtifact),
}

impl<'v> StarlarkTypeRepr for &'v dyn StarlarkArtifactLike {
    type Canonical = <EitherArtifactRef<'v> as StarlarkTypeRepr>::Canonical;

    fn starlark_type_repr() -> Ty {
        EitherArtifactRef::starlark_type_repr()
    }
}

impl<'v> UnpackValue<'v> for &'v dyn StarlarkArtifactLike {
    type Error = Infallible;

    fn unpack_value_impl(value: Value<'v>) -> Result<Option<Self>, Self::Error> {
        match EitherArtifactRef::unpack_value_opt(value) {
            Some(EitherArtifactRef::Artifact(artifact)) => Ok(Some(artifact)),
            Some(EitherArtifactRef::DeclaredArtifact(artifact)) => Ok(Some(artifact)),
            Some(EitherArtifactRef::PromiseArtifact(artifact)) => Ok(Some(artifact)),
            None => Ok(None),
        }
    }
}

#[derive(StarlarkTypeRepr, AllocValue)]
pub enum EitherStarlarkArtifact {
    Artifact(StarlarkArtifact),
    DeclaredArtifact(StarlarkDeclaredArtifact),
    PromiseArtifact(StarlarkPromiseArtifact),
}

/// A single input or output file for an action.
///
/// There is no `.parent` method on `artifact`, but in most cases
/// `cmd_args(my_artifact, parent = 1)` can be used to similar effect.
#[starlark_module]
pub(crate) fn artifact_methods(builder: &mut MethodsBuilder) {
    /// The base name of this artifact. e.g. for an artifact at `foo/bar`, this is `bar`
    #[starlark(attribute)]
    fn basename<'v>(
        this: &'v dyn StarlarkArtifactLike,
        heap: &'v Heap,
    ) -> starlark::Result<StringValue<'v>> {
        Ok(this.basename(heap)?)
    }

    /// The file extension of this artifact. e.g. for an artifact at foo/bar.sh,
    /// this is `.sh`. If no extension is present, `""` is returned.
    #[starlark(attribute)]
    fn extension<'v>(
        this: &'v dyn StarlarkArtifactLike,
        heap: &'v Heap,
    ) -> starlark::Result<StringValue<'v>> {
        Ok(this.extension(heap)?)
    }

    /// Whether the artifact represents a source file
    #[starlark(attribute)]
    fn is_source<'v>(this: &'v dyn StarlarkArtifactLike) -> starlark::Result<bool> {
        Ok(this.is_source()?)
    }

    /// The `Label` of the rule that originally created this artifact. May also be None in
    /// the case of source files, or if the artifact has not be used in an action, or if the
    /// action was not created by a rule.
    #[starlark(attribute)]
    fn owner<'v>(
        this: &'v dyn StarlarkArtifactLike,
    ) -> starlark::Result<NoneOr<StarlarkConfiguredProvidersLabel>> {
        Ok(NoneOr::from_option(this.owner()?))
    }

    /// The interesting part of the path, relative to somewhere in the output directory.
    /// For an artifact declared as `foo/bar`, this is `foo/bar`.
    #[starlark(attribute)]
    fn short_path<'v>(
        this: &'v dyn StarlarkArtifactLike,
        heap: &Heap,
    ) -> starlark::Result<StringValue<'v>> {
        Ok(this.short_path(heap)?)
    }

    /// Returns a `StarlarkOutputArtifact` instance, or fails if the artifact is
    /// either an `Artifact`, or is a bound `Artifact` (You cannot bind twice)
    fn as_output<'v>(
        this: ValueOf<'v, &'v dyn StarlarkArtifactLike>,
    ) -> starlark::Result<StarlarkOutputArtifact<'v>> {
        Ok(this.typed.as_output(this.value)?)
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
    ) -> starlark::Result<EitherStarlarkArtifact> {
        let path = ForwardRelativePath::new(path)?;
        Ok(this.project(path, hide_prefix)?)
    }

    /// Returns a `StarlarkArtifact` instance which is identical to the original artifact, except
    /// with no associated artifacts
    fn without_associated_artifacts<'v>(
        this: &'v dyn StarlarkArtifactLike,
    ) -> starlark::Result<EitherStarlarkArtifact> {
        Ok(this.without_associated_artifacts()?)
    }

    /// Returns a `StarlarkArtifact` instance which is identical to the original artifact, but with
    /// potentially additional artifacts. The artifacts must be bound.
    fn with_associated_artifacts<'v>(
        this: &'v dyn StarlarkArtifactLike,
        artifacts: UnpackList<ValueAsArtifactLike<'v>>,
    ) -> starlark::Result<EitherStarlarkArtifact> {
        Ok(this.with_associated_artifacts(artifacts)?)
    }
}
