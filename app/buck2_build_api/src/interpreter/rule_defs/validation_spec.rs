/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt::Display;
use std::fmt::Formatter;

use allocative::Allocative;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::values::Freeze;
use starlark::values::FreezeError;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::StringValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;
use starlark::values::ValueOf;
use starlark::values::ValueOfUncheckedGeneric;
use starlark::values::starlark_value;

use crate::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::starlark_artifact_like::StarlarkInputArtifactLike;
use crate::interpreter::rule_defs::artifact::starlark_artifact_like::ValueAsInputArtifactLike;
use crate::interpreter::rule_defs::artifact::starlark_artifact_like::ValueIsInputArtifactAnnotation;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum ValidationSpecError {
    #[error("Name of validation spec should not be empty")]
    EmptyName,
    #[error("Validation result artifact should be a build artifact, not a source one.")]
    ValidationResultIsSourceArtifact,
}

/// Value describing a single identifiable validation.
/// Validation is represented by a build artifact with defined structure.
/// Content of such artifact determines if validation is successful or not.
/// A collection of such objects forms a `ValidationInfo` provider
/// which describes how a given target should be validated.
#[derive(
    Debug,
    Trace,
    NoSerialize,
    Coerce,
    ProvidesStaticType,
    Allocative,
    Freeze
)]
#[freeze(validator = validate_validation_spec, bounds = "V: ValueLike<'freeze>")]
#[repr(C)]
pub struct StarlarkValidationSpecGen<V: ValueLifetimeless> {
    /// Name used to identify validation. Should be unique per target node.
    name: ValueOfUncheckedGeneric<V, String>,
    /// Build artifact which is the result of running a validation.
    /// Should contain JSON of defined schema setting API between Buck2 and user-created validators/scripts.
    validation_result: ValueOfUncheckedGeneric<V, ValueIsInputArtifactAnnotation>,

    /// Is validation optional, i.e., should it be skipped by default?
    /// By default validations are required unless this flag is specified.
    /// Optional validations are only run when explicitly requested via CLI arguments.
    optional: bool,
}

starlark_complex_value!(pub(crate) StarlarkValidationSpec);

impl<'v, V: ValueLike<'v>> StarlarkValidationSpecGen<V> {
    pub fn name(&self) -> &'v str {
        self.name
            .cast::<&str>()
            .unpack()
            .expect("type checked during construction or freezing")
    }

    pub fn validation_result(&self) -> &'v dyn StarlarkInputArtifactLike<'v> {
        ValueAsInputArtifactLike::unpack_value_opt(self.validation_result.get().to_value())
            .expect("type checked during construction or freezing")
            .0
    }

    pub fn optional(&self) -> bool {
        self.optional
    }
}

impl<'v, V: ValueLike<'v>> Display for StarlarkValidationSpecGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ValidationSpec(name={}, validation_result=", self.name)?;
        Display::fmt(&self.validation_result, f)?;
        write!(f, ")")
    }
}

fn validate_validation_spec<'v, V>(spec: &StarlarkValidationSpecGen<V>) -> buck2_error::Result<()>
where
    V: ValueLike<'v>,
{
    let name = spec.name.unpack()?;
    if name.is_empty() {
        return Err(ValidationSpecError::EmptyName.into());
    }
    let artifact =
        ValueAsInputArtifactLike::unpack_value_err(spec.validation_result.get().to_value())?;
    let artifact = match artifact.0.get_bound_artifact() {
        Ok(bound_artifact) => bound_artifact,
        Err(e) => {
            return Err(e.context("Validation result artifact should be bound."));
        }
    };
    if artifact.is_source() {
        return Err(ValidationSpecError::ValidationResultIsSourceArtifact.into());
    }
    Ok(())
}

#[starlark_value(type = "ValidationSpec")]
impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for StarlarkValidationSpecGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(validation_spec_methods)
    }
}

#[starlark_module]
fn validation_spec_methods(builder: &mut MethodsBuilder) {
    #[starlark(attribute)]
    /// Name identifying validation.
    fn name<'v>(
        this: &'v StarlarkValidationSpec,
        heap: Heap<'v>,
    ) -> starlark::Result<StringValue<'v>> {
        Ok(heap.alloc_str_intern(this.name()))
    }

    #[starlark(attribute)]
    /// Is validation optional.
    fn optional<'v>(this: &'v StarlarkValidationSpec) -> starlark::Result<bool> {
        Ok(this.optional())
    }

    #[starlark(attribute)]
    /// Artifact which is the result of running a validation.
    fn validation_result<'v>(
        this: &'v StarlarkValidationSpec,
    ) -> starlark::Result<StarlarkArtifact> {
        let artifact =
            ValueAsInputArtifactLike::unpack_value_err(this.validation_result.get().to_value())?;
        Ok(artifact.0.get_bound_starlark_artifact()?)
    }
}

#[starlark_module]
pub fn register_validation_spec(builder: &mut GlobalsBuilder) {
    #[starlark(as_type = FrozenStarlarkValidationSpec)]
    fn ValidationSpec<'v>(
        #[starlark(require = named)] name: StringValue<'v>,
        #[starlark(require = named)] validation_result: ValueOf<'v, ValueIsInputArtifactAnnotation>,
        #[starlark(require = named, default = false)] optional: bool,
    ) -> starlark::Result<StarlarkValidationSpec<'v>> {
        let result = StarlarkValidationSpec {
            name: name.to_value_of_unchecked().cast(),
            validation_result: validation_result.as_unchecked().cast(),
            optional,
        };
        validate_validation_spec(&result)?;
        Ok(result)
    }
}
