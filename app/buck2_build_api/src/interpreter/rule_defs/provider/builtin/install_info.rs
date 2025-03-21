/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_build_api_derive::internal_provider;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_error::BuckErrorContext;
use buck2_interpreter::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use starlark::any::ProvidesStaticType;
use starlark::collections::SmallMap;
use starlark::environment::GlobalsBuilder;
use starlark::values::dict::DictRef;
use starlark::values::dict::DictType;
use starlark::values::Coerce;
use starlark::values::Freeze;
use starlark::values::FreezeError;
use starlark::values::FreezeResult;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;
use starlark::values::ValueOf;
use starlark::values::ValueOfUncheckedGeneric;

use crate as buck2_build_api;
use crate::interpreter::rule_defs::artifact::starlark_artifact_like::ValueAsArtifactLike;

// Provider that signals a rule is installable (ex. android_binary)

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum InstallInfoProviderErrors {
    #[error("expected a label, got `{0}` (type `{1}`)")]
    ExpectedLabel(String, String),
    #[error("Expected a dictionary of artifacts but key `{key}` contained `{got}`")]
    ExpectedArtifact { key: String, got: String },
    #[error("Expected a dictionary with string keys, but got key `{0}`")]
    ExpectedStringKey(String),
    #[error("File with key `{key}`: `{artifact}` should not have any associated artifacts")]
    AssociatedArtifacts { key: String, artifact: String },
}

#[internal_provider(install_info_creator)]
#[derive(Clone, Coerce, Debug, Freeze, Trace, ProvidesStaticType, Allocative)]
#[repr(C)]
#[freeze(validator = validate_install_info, bounds = "V: ValueLike<'freeze>")]
pub struct InstallInfoGen<V: ValueLifetimeless> {
    // Label for the installer
    installer: ValueOfUncheckedGeneric<V, StarlarkConfiguredProvidersLabel>,
    // list of files that need to be installed
    files: ValueOfUncheckedGeneric<V, DictType<String, ValueAsArtifactLike<'static>>>,
}

impl<'v, V: ValueLike<'v>> InstallInfoGen<V> {
    pub fn get_installer(&self) -> buck2_error::Result<ConfiguredProvidersLabel> {
        let label = StarlarkConfiguredProvidersLabel::from_value(self.installer.get().to_value())
            .ok_or_else(|| {
                InstallInfoProviderErrors::ExpectedLabel(
                    self.installer.get().to_value().to_repr(),
                    self.installer.get().to_value().get_type().to_owned(),
                )
            })?
            .label()
            .to_owned();
        Ok(label)
    }

    fn get_files_dict(&self) -> DictRef<'v> {
        DictRef::from_value(self.files.get().to_value()).expect("Value is a Dict")
    }

    fn get_files_iter<'a>(
        files: &'a DictRef<'v>,
    ) -> impl Iterator<Item = buck2_error::Result<(&'v str, ValueAsArtifactLike<'v>)>> + 'a {
        files.iter().map(|(k, v)| {
            let k = k
                .unpack_str()
                .ok_or_else(|| InstallInfoProviderErrors::ExpectedStringKey(k.to_string()))?;
            Ok((
                k,
                ValueAsArtifactLike::unpack_value(v)?.ok_or_else(|| {
                    InstallInfoProviderErrors::ExpectedArtifact {
                        key: k.to_owned(),
                        got: v.get_type().to_owned(),
                    }
                })?,
            ))
        })
    }

    pub fn get_files(&self) -> buck2_error::Result<SmallMap<&'v str, Artifact>> {
        Self::get_files_iter(&self.get_files_dict())
            .map(|x| {
                let (k, v) = x?;
                Ok((
                    k,
                    v.0.get_bound_artifact()
                        .with_buck_error_context(|| format!("For key `{k}`"))?,
                ))
            })
            .collect()
    }
}

#[starlark_module]
fn install_info_creator(globals: &mut GlobalsBuilder) {
    fn InstallInfo<'v>(
        installer: ValueOf<'v, &'v StarlarkConfiguredProvidersLabel>,
        files: ValueOf<'v, DictType<&'v str, ValueAsArtifactLike<'v>>>,
    ) -> starlark::Result<InstallInfo<'v>> {
        let info = InstallInfo {
            installer: installer.as_unchecked().cast(),
            files: files.as_unchecked().cast(),
        };
        validate_install_info(&info)?;
        Ok(info)
    }
}

fn validate_install_info<'v, V>(info: &InstallInfoGen<V>) -> buck2_error::Result<()>
where
    V: ValueLike<'v>,
{
    for x in InstallInfoGen::<V>::get_files_iter(&info.get_files_dict()) {
        let (k, v) = x?;
        if let Some(other_artifacts) = v.0.get_associated_artifacts() {
            if !other_artifacts.is_empty() {
                return Err(InstallInfoProviderErrors::AssociatedArtifacts {
                    key: k.to_owned(),
                    artifact: v.0.to_string(),
                }
                .into());
            }
        }
    }
    Ok(())
}
