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
use std::hash::Hash;
use std::sync::atomic::AtomicI64;
use std::sync::atomic::Ordering;

use allocative::Allocative;
use dupe::Dupe;
use either::Either;
use starlark::any::ProvidesStaticType;
use starlark::collections::StarlarkHasher;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::values::Freeze;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;

use crate::interpreter::rule_defs::artifact_tagging::StarlarkTaggedCommandLine;
use crate::interpreter::rule_defs::artifact_tagging::StarlarkTaggedValue;
use crate::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;

/// ArtifactTag allows wrapping input and output artifacts in a command line with tags. Those tags
/// will be made visible to artifact visitors. The tags themselves don't have meaning on their own,
/// but they can be compared to each other, which allows grouping inputs and outputs in meaningful
/// categories. This is notably used for dep files to associate inputs tracked by a dep file with
/// the dep file itself.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Hash,
    Dupe,
    Freeze,
    Trace,
    ProvidesStaticType,
    NoSerialize,
    Allocative
)]
pub struct ArtifactTag {
    identity: u64,
}

impl ArtifactTag {
    pub fn new() -> Self {
        static LAST: AtomicI64 = AtomicI64::new(0);
        let identity = LAST.fetch_add(1, Ordering::Relaxed).wrapping_add(1);
        let Ok(identity) = identity.try_into() else {
            LAST.fetch_sub(1, Ordering::Relaxed);
            panic!("i64 overflow (should never happen)");
        };
        ArtifactTag { identity }
    }
}

impl fmt::Display for ArtifactTag {
    fn fmt(&self, w: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Do not include identity into display because it introduces non-determinism for starlark.
        write!(w, "ArtifactTag(...)")
    }
}

starlark_simple_value!(ArtifactTag);

#[starlark_value(type = "ArtifactTag")]
impl<'v> StarlarkValue<'v> for ArtifactTag {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(artifact_tag_methods)
    }

    fn equals(&self, other: Value<'v>) -> starlark::Result<bool> {
        Ok(match other.downcast_ref::<Self>() {
            Some(other) => self == other,
            None => false,
        })
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> starlark::Result<()> {
        Hash::hash(self, hasher);
        Ok(())
    }
}

/// An `ArtifactTag` is used to associate inputs and outputs in an action.
/// Tags are typically used with dependency files (dep files) to track which inputs are actually used
/// during action execution, enabling more accurate incremental builds.
///
/// For complete documentation including examples and the full workflow, see
/// [`ctx.actions.artifact_tag()`](../AnalysisActions#analysisactionsartifact_tag).
#[starlark_module]
fn artifact_tag_methods(_: &mut MethodsBuilder) {
    /// Tag both input and output artifacts with this tag.
    ///
    /// When artifacts are tagged, artifact visitors (like those used during action execution)
    /// can identify which artifacts belong to which category. This is commonly used with the
    /// `dep_files` parameter in `ctx.actions.run()` to associate inputs with their dependency files.
    ///
    /// Args:
    ///     inner: The artifact(s) or command line arguments. Can be a single artifact,
    ///            a list of artifacts, or cmd_args containing artifacts.
    ///
    /// Returns:
    ///     The tagged value. If `inner` is command-line-like, returns a tagged command line;
    ///     otherwise returns a tagged value.
    fn tag_artifacts<'v>(
        this: &ArtifactTag,
        inner: Value<'v>,
    ) -> starlark::Result<Either<StarlarkTaggedValue<'v>, StarlarkTaggedCommandLine<'v>>> {
        let value = StarlarkTaggedValue::new(inner, this.dupe());

        Ok(if ValueAsCommandLineLike::unpack_value(inner)?.is_some() {
            Either::Right(StarlarkTaggedCommandLine::new(value))
        } else {
            Either::Left(value)
        })
    }

    /// Tag only input artifacts with this tag (outputs are not tagged).
    ///
    /// This is similar to `tag_artifacts()`, but only applies the tag to input artifacts.
    /// This is useful when you want to track inputs separately from outputs.
    ///
    /// Args:
    ///     inner: The artifact(s) or command line arguments. Can be a single artifact,
    ///            a list of artifacts, or cmd_args containing artifacts.
    ///
    /// Returns:
    ///     The tagged value with tags applied only to inputs. If `inner` is command-line-like,
    ///     returns a tagged command line; otherwise returns a tagged value.
    fn tag_inputs<'v>(
        this: &ArtifactTag,
        inner: Value<'v>,
    ) -> starlark::Result<Either<StarlarkTaggedValue<'v>, StarlarkTaggedCommandLine<'v>>> {
        let value = StarlarkTaggedValue::inputs_only(inner, this.dupe());

        Ok(if ValueAsCommandLineLike::unpack_value(inner)?.is_some() {
            Either::Right(StarlarkTaggedCommandLine::new(value))
        } else {
            Either::Left(value)
        })
    }
}

#[starlark_module]
pub(crate) fn register_artifact_tag(globals: &mut GlobalsBuilder) {
    const ArtifactTag: StarlarkValueAsType<ArtifactTag> = StarlarkValueAsType::new();
}
