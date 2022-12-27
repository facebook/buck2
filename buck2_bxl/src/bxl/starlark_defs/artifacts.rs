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
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_like::StarlarkArtifactLike;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkDeclaredArtifact;
use gazebo::any::ProvidesStaticType;
use gazebo::prelude::Dupe;
use serde::Serialize;
use serde::Serializer;
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
#[starlark_docs_attrs(directory = "BXL/Output and Ensuring")]
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

impl Hash for EnsuredArtifact {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_artifact().fingerprint().hash(state);
        self.abs().hash(state);
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

impl<'v> AllocValue<'v> for EnsuredArtifact {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

impl<'v> StarlarkTypeRepr for &'v EnsuredArtifact {
    fn starlark_type_repr() -> String {
        EnsuredArtifact::get_type_starlark_repr()
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
