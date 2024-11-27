/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
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
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::StarlarkResultExt;

use crate::interpreter::rule_defs::artifact_tagging::TaggedCommandLine;
use crate::interpreter::rule_defs::artifact_tagging::TaggedValue;
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

#[starlark_value(type = "artifact_tag")]
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

#[starlark_module]
fn artifact_tag_methods(_: &mut MethodsBuilder) {
    fn tag_artifacts<'v>(
        this: &ArtifactTag,
        inner: Value<'v>,
    ) -> starlark::Result<Either<TaggedValue<'v>, TaggedCommandLine<'v>>> {
        let value = TaggedValue::new(inner, this.dupe());

        Ok(
            if ValueAsCommandLineLike::unpack_value(inner)
                .into_anyhow_result()?
                .is_some()
            {
                Either::Right(TaggedCommandLine::new(value))
            } else {
                Either::Left(value)
            },
        )
    }

    fn tag_inputs<'v>(
        this: &ArtifactTag,
        inner: Value<'v>,
    ) -> starlark::Result<Either<TaggedValue<'v>, TaggedCommandLine<'v>>> {
        let value = TaggedValue::inputs_only(inner, this.dupe());

        Ok(
            if ValueAsCommandLineLike::unpack_value(inner)
                .into_anyhow_result()?
                .is_some()
            {
                Either::Right(TaggedCommandLine::new(value))
            } else {
                Either::Left(value)
            },
        )
    }
}

#[starlark_module]
pub(crate) fn register_artifact_tag(globals: &mut GlobalsBuilder) {
    const ArtifactTag: StarlarkValueAsType<ArtifactTag> = StarlarkValueAsType::new();
}
