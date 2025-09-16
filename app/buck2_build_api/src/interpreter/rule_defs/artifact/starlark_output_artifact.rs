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
use std::fmt::Debug;
use std::fmt::Display;

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::OutputArtifact;
use dupe::Dupe;
use either::Either;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::values::AllocFrozenValue;
use starlark::values::AllocValue;
use starlark::values::Demand;
use starlark::values::Freeze;
use starlark::values::FrozenValue;
use starlark::values::FrozenValueTyped;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;
use starlark::values::ValueTyped;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::type_repr::StarlarkTypeRepr;

use crate::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::starlark_declared_artifact::StarlarkDeclaredArtifact;
use crate::interpreter::rule_defs::cmd_args::ArtifactPathMapper;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineContext;
use crate::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;
use crate::interpreter::rule_defs::cmd_args::command_line_arg_like_type::command_line_arg_like_impl;

/// Thin wrapper around `OutputArtifact`.
///
/// Allows actions to distinguish between inputs and outputs, and can validate whether the
/// underlying artifact is bound or not yet.
///
/// This type intentionally does not implement `Coerce`, as it breaks the invariant set by
/// `StarlarkOutputArtifact::new`. For this reason, we also do not provide a `impl UnpackValue for
/// &StarlarkOutputArtifact`. Typically, that impl would be written to unpack either an unfrozen
/// value or a frozen one which is then coerced, but we can't do the latter - so don't do either.
///
/// If you need to unpack this, explicitly use one of:
///  - `ValueTyped<StarlarkOutputArtifact>`
///  - `FrozenValueTyped<FrozenStarlarkOutputArtifact>`
///  - `ValueTypedComplex<StarlarkOutputArtifact>`
#[derive(
    Debug,
    Clone,
    Dupe,
    ProvidesStaticType,
    Trace,
    NoSerialize,
    Allocative,
    Freeze
)]
#[repr(C)]
pub struct StarlarkOutputArtifactGen<V: ValueLifetimeless> {
    declared_artifact: V, // StarlarkDeclaredArtifact or FrozenStarlarkArtifact
}

pub type StarlarkOutputArtifact<'v> = StarlarkOutputArtifactGen<Value<'v>>;

pub type FrozenStarlarkOutputArtifact = StarlarkOutputArtifactGen<FrozenValue>;

impl<'v> AllocValue<'v> for StarlarkOutputArtifact<'v> {
    #[inline]
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex(self)
    }
}

impl AllocFrozenValue for FrozenStarlarkOutputArtifact {
    #[inline]
    fn alloc_frozen_value(
        self,
        heap: &starlark::values::FrozenHeap,
    ) -> starlark::values::FrozenValue {
        heap.alloc_simple(self)
    }
}

impl<'v, V: ValueLike<'v>> Display for StarlarkOutputArtifactGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<output artifact for {}>",
            match self.unpack() {
                Either::Left(v) => v.artifact.get_path(),
                Either::Right(v) => v.artifact.get_path(),
            }
        )
    }
}

impl<'v, V: ValueLike<'v>> StarlarkOutputArtifactGen<V> {
    fn unpack(&self) -> Either<&'v StarlarkDeclaredArtifact<'v>, &'v StarlarkArtifact> {
        let v = self.declared_artifact.to_value();
        // Unwraps justified at construction time
        if let Some(v) = v.unpack_frozen() {
            Either::Right(v.downcast_ref().unwrap())
        } else {
            Either::Left(v.downcast_ref().unwrap())
        }
    }
}

impl<'v> StarlarkOutputArtifact<'v> {
    pub fn new(v: ValueTyped<'v, StarlarkDeclaredArtifact<'v>>) -> Self {
        Self {
            declared_artifact: v.to_value(),
        }
    }

    pub(crate) fn inner(&self) -> ValueTyped<'v, StarlarkDeclaredArtifact<'v>> {
        ValueTyped::new_err(self.declared_artifact).expect("Type checked at construction time")
    }

    pub fn artifact(&self) -> OutputArtifact<'v> {
        self.inner().output_artifact()
    }
}

impl FrozenStarlarkOutputArtifact {
    pub fn inner(&self) -> FrozenValueTyped<'_, StarlarkArtifact>
    // Ensures that this stops compiling if this changes
    where
        for<'v> StarlarkDeclaredArtifact<'v>: Freeze<Frozen = StarlarkArtifact>,
    {
        // Unwrap justified by construction of the type and the where clause
        FrozenValueTyped::new_err(self.declared_artifact).unwrap()
    }
}

#[starlark_value(type = "OutputArtifact")]
impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for StarlarkOutputArtifactGen<V>
where
    Self: ProvidesStaticType<'v> + Display + CommandLineArgLike<'v>,
{
    fn provide(&'v self, demand: &mut Demand<'_, 'v>) {
        demand.provide_value::<&dyn CommandLineArgLike>(self);
    }
}

impl<'v, V: ValueLike<'v>> CommandLineArgLike<'v> for StarlarkOutputArtifactGen<V> {
    fn register_me(&self) {
        command_line_arg_like_impl!(FrozenStarlarkOutputArtifact::starlark_type_repr());
    }

    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        ctx: &mut dyn CommandLineContext,
        _artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()> {
        match self.unpack() {
            Either::Left(_) => {
                // TODO: proper error message
                Err(buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Tier0,
                    "proper error here; we should not be adding mutable starlark objects to clis"
                ))
            }
            Either::Right(v) => {
                // We do not need to use the ArtifactPathMapper here as output artifacts are always
                // resolved to a known path since their content hash is not yet available.
                cli.push_location(ctx.resolve_output_artifact(&v.artifact)?);
                Ok(())
            }
        }
    }

    fn visit_artifacts(
        &self,
        visitor: &mut dyn CommandLineArtifactVisitor<'v>,
    ) -> buck2_error::Result<()> {
        match self.unpack() {
            Either::Left(v) => {
                visitor.visit_declared_output(v.output_artifact(), vec![]);
            }
            Either::Right(v) => {
                visitor.visit_frozen_output(v.artifact(), vec![]);
            }
        }
        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        false
    }

    fn visit_write_to_file_macros(
        &self,
        _visitor: &mut dyn WriteToFileMacroVisitor,
        _artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()> {
        Ok(())
    }
}

#[starlark_module]
pub(crate) fn register_output_artifact(globals: &mut GlobalsBuilder) {
    const OutputArtifact: StarlarkValueAsType<StarlarkOutputArtifact> = StarlarkValueAsType::new();
}
