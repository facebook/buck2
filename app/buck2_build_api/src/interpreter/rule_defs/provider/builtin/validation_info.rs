/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;
use std::fmt::Debug;

use allocative::Allocative;
use buck2_build_api_derive::internal_provider;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::environment::GlobalsBuilder;
use starlark::values::list::ListRef;
use starlark::values::list::ListType;
use starlark::values::Freeze;
use starlark::values::FreezeError;
use starlark::values::FreezeResult;
use starlark::values::Trace;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;
use starlark::values::ValueOf;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueOfUncheckedGeneric;

use crate as buck2_build_api;
use crate::interpreter::rule_defs::validation_spec::FrozenStarlarkValidationSpec;
use crate::interpreter::rule_defs::validation_spec::StarlarkValidationSpec;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum ValidationInfoError {
    #[error("Expected `ValidationSpec` value, got `{0}`")]
    WrongSpecType(String),
    #[error("Multiple specs with same name `{0}` which is not allowed.")]
    SpecsWithDuplicateName(String),
    #[error("Validations should be a list of `ValidationSpec` values")]
    ValidationsAreNotListOfSpecs,
    #[error("`ValidationInfo` should contain at least one validation.")]
    ValidationSpecsEmpty,
}

/// Provider describing how a given target node should be validated.
/// Validations are run when target with `ValidationInfo` provider is a transitive
/// dependency of a requested target.
#[internal_provider(validation_info_creator)]
#[derive(Clone, Debug, Trace, Coerce, Freeze, ProvidesStaticType, Allocative)]
#[freeze(validator = validate_validation_info, bounds = "V: ValueLike<'freeze>")]
#[repr(transparent)]
pub struct ValidationInfoGen<V: ValueLifetimeless> {
    /// List of `ValidationSpec` values each representing a single validation.
    validations: ValueOfUncheckedGeneric<V, Vec<FrozenStarlarkValidationSpec>>,
}

fn validate_validation_info<'v, V>(info: &ValidationInfoGen<V>) -> buck2_error::Result<()>
where
    V: ValueLike<'v>,
{
    let values = ListRef::from_value(info.validations.get().to_value())
        .ok_or(buck2_error::Error::from(
            ValidationInfoError::ValidationsAreNotListOfSpecs,
        ))?
        .iter();
    let mut spec_names = HashSet::new();
    for value in values {
        let wrong_type_error = || ValidationInfoError::WrongSpecType(format!("{}", value));
        let name = if let Some(frozen_value) = value.unpack_frozen() {
            let spec = frozen_value
                .downcast_ref::<FrozenStarlarkValidationSpec>()
                .ok_or_else(wrong_type_error)?;
            spec.name()
        } else {
            let spec = value
                .downcast_ref::<StarlarkValidationSpec>()
                .ok_or_else(wrong_type_error)?;
            spec.name()
        };
        if !spec_names.insert(name) {
            return Err(ValidationInfoError::SpecsWithDuplicateName(name.to_owned()).into());
        }
    }
    if spec_names.is_empty() {
        return Err(ValidationInfoError::ValidationSpecsEmpty.into());
    }
    Ok(())
}

#[starlark_module]
fn validation_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(as_type = FrozenValidationInfo)]
    fn ValidationInfo<'v>(
        #[starlark(require = named)] validations: ValueOf<
            'v,
            ListType<&'v StarlarkValidationSpec<'v>>,
        >,
    ) -> starlark::Result<ValidationInfo<'v>> {
        let result = ValidationInfo {
            validations: ValueOfUnchecked::new(validations.value),
        };
        validate_validation_info(&result)?;
        Ok(result)
    }
}

impl FrozenValidationInfo {
    pub fn validations(&self) -> impl Iterator<Item = &FrozenStarlarkValidationSpec> {
        let it = ListRef::from_value(self.validations.get().to_value())
            .expect("type checked during construction or freezing")
            .iter();
        it.map(|x| {
            x.downcast_ref::<FrozenStarlarkValidationSpec>()
                .expect("type checked during construction or freezing")
        })
    }
}
