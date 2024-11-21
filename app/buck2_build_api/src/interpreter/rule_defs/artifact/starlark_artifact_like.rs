/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::convert::Infallible;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;

use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_execute::path::artifact_path::ArtifactPath;
use buck2_interpreter::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use starlark::collections::StarlarkHasher;
use starlark::typing::Ty;
use starlark::values::list::UnpackList;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::Heap;
use starlark::values::StringValue;
use starlark::values::UnpackValue;
use starlark::values::Value;

use crate::artifact_groups::promise::PromiseArtifactId;
use crate::artifact_groups::ArtifactGroup;
use crate::interpreter::rule_defs::artifact::associated::AssociatedArtifacts;
use crate::interpreter::rule_defs::artifact::methods::EitherStarlarkArtifact;
use crate::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::starlark_declared_artifact::StarlarkDeclaredArtifact;
use crate::interpreter::rule_defs::artifact::starlark_output_artifact::StarlarkOutputArtifact;
use crate::interpreter::rule_defs::artifact::starlark_promise_artifact::StarlarkPromiseArtifact;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;

/// The Starlark representation of an `Artifact`
///
/// The following fields are available in Starlark:
/// `.basename`: The base name of this artifact. e.g. for an artifact
///              at `foo/bar`, this is `bar`
/// `.extension`: The file extension of this artifact. e.g. for an artifact at foo/bar.sh,
///               this is `sh`. If no extension is present, an empty string is returned
/// `.is_source`: Whether the artifact represents a source file
/// `.owner`: The `Label` of the rule that originally created this artifact. May also be None in
///           the case of source files, or if the artifact has not be used in an action.
/// `as_output()`: Returns a `StarlarkOutputArtifact` instance, or fails if the artifact is
///                either an `Artifact`, or is a bound `DeclaredArtifact` (You cannot bind twice)
/// `.short_path`: The interesting part of the path, relative to somewhere in the output directory.
///                For an artifact declared as `foo/bar`, this is `foo/bar`.
/// This trait also has some common functionality for `StarlarkValue` that we want shared between
/// `StarlarkArtifact` and `StarlarkDeclaredArtifact`
pub trait StarlarkArtifactLike: Display {
    /// Returns an apppropriate error for when this is used in a location that expects an output declaration.
    fn as_output_error(&self) -> buck2_error::Error;

    /// Gets the bound main artifact, or errors if the artifact is not bound
    fn get_bound_artifact(&self) -> buck2_error::Result<Artifact>;

    /// Gets any associated artifacts that should be materialized along with the bound artifact
    fn get_associated_artifacts(&self) -> Option<&AssociatedArtifacts>;

    /// Return an interface for frozen and bound artifacts (`StarlarkArtifact`) to add to a CLI
    ///
    /// Returns None if this artifact isn't the correct type to be added to a CLI object
    fn as_command_line_like(&self) -> &dyn CommandLineArgLike;

    /// It's very important that the Hash/Eq of the StarlarkArtifactLike things doesn't change
    /// during freezing, otherwise Starlark invariants are broken. Use the fingerprint
    /// as the inputs to Hash/Eq to ensure they are consistent
    fn fingerprint(&self) -> ArtifactFingerprint<'_>;

    fn equals<'v>(&self, other: Value<'v>) -> starlark::Result<bool> {
        Ok(ValueAsArtifactLike::unpack_value(other)?
            .map_or(false, |other| self.fingerprint() == other.0.fingerprint()))
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> starlark::Result<()> {
        self.fingerprint().hash(hasher);
        Ok(())
    }

    /// Gets a copy of the StarlarkArtifact, ensuring that the artifact is bound.
    fn get_bound_starlark_artifact(&self) -> buck2_error::Result<StarlarkArtifact> {
        let artifact = self.get_bound_artifact()?;
        let associated_artifacts = self.get_associated_artifacts();
        Ok(StarlarkArtifact {
            artifact,
            associated_artifacts: associated_artifacts
                .map_or(AssociatedArtifacts::new(), |a| a.clone()),
        })
    }

    /// Gets the artifact group.
    fn get_artifact_group(&self) -> buck2_error::Result<ArtifactGroup>;

    fn basename<'v>(&'v self, heap: &'v Heap) -> buck2_error::Result<StringValue<'v>>;

    fn extension<'v>(&'v self, heap: &'v Heap) -> buck2_error::Result<StringValue<'v>>;

    fn is_source<'v>(&'v self) -> buck2_error::Result<bool>;

    fn owner<'v>(&'v self) -> buck2_error::Result<Option<StarlarkConfiguredProvidersLabel>>;

    fn short_path<'v>(&'v self, heap: &'v Heap) -> buck2_error::Result<StringValue<'v>>;

    fn as_output<'v>(&'v self, this: Value<'v>) -> buck2_error::Result<StarlarkOutputArtifact<'v>>;

    fn project<'v>(
        &'v self,
        path: &ForwardRelativePath,
        hide_prefix: bool,
    ) -> buck2_error::Result<EitherStarlarkArtifact>;

    fn without_associated_artifacts<'v>(&'v self) -> buck2_error::Result<EitherStarlarkArtifact>;

    fn with_associated_artifacts<'v>(
        &'v self,
        artifacts: UnpackList<ValueAsArtifactLike<'v>>,
    ) -> buck2_error::Result<EitherStarlarkArtifact>;
}

/// Helper type to unpack artifacts.
#[derive(StarlarkTypeRepr, UnpackValue)]
pub enum ValueAsArtifactLikeUnpack<'v> {
    Artifact(&'v StarlarkArtifact),
    DeclaredArtifact(&'v StarlarkDeclaredArtifact),
    PromiseArtifact(&'v StarlarkPromiseArtifact),
}

pub struct ValueAsArtifactLike<'v>(pub &'v dyn StarlarkArtifactLike);

impl<'v> StarlarkTypeRepr for ValueAsArtifactLike<'v> {
    type Canonical = <ValueAsArtifactLikeUnpack<'v> as StarlarkTypeRepr>::Canonical;

    fn starlark_type_repr() -> Ty {
        ValueAsArtifactLikeUnpack::starlark_type_repr()
    }
}

impl<'v> UnpackValue<'v> for ValueAsArtifactLike<'v> {
    type Error = Infallible;

    fn unpack_value_impl(value: Value<'v>) -> Result<Option<Self>, Self::Error> {
        match ValueAsArtifactLikeUnpack::unpack_value_opt(value) {
            Some(ValueAsArtifactLikeUnpack::Artifact(a)) => {
                Ok(Some(ValueAsArtifactLike(a as &dyn StarlarkArtifactLike)))
            }
            Some(ValueAsArtifactLikeUnpack::DeclaredArtifact(a)) => {
                Ok(Some(ValueAsArtifactLike(a as &dyn StarlarkArtifactLike)))
            }
            Some(ValueAsArtifactLikeUnpack::PromiseArtifact(a)) => {
                Ok(Some(ValueAsArtifactLike(a as &dyn StarlarkArtifactLike)))
            }
            None => Ok(None),
        }
    }
}

#[derive(PartialEq)]
pub enum ArtifactFingerprint<'a> {
    Normal {
        path: ArtifactPath<'a>,
        associated_artifacts: Option<&'a AssociatedArtifacts>,
    },
    Promise {
        id: PromiseArtifactId,
    },
}

impl Hash for ArtifactFingerprint<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match &self {
            ArtifactFingerprint::Normal {
                path,
                associated_artifacts,
            } => {
                path.hash(state);
                if let Some(associated) = associated_artifacts {
                    associated.len().hash(state);
                    associated.iter().for_each(|ag| ag.hash(state));
                }
            }
            ArtifactFingerprint::Promise { id } => id.hash(state),
        }
    }
}
