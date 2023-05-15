/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use anyhow::Context as _;
use buck2_build_api_derive::internal_provider;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_interpreter::types::label::Label;
use starlark::any::ProvidesStaticType;
use starlark::collections::SmallMap;
use starlark::environment::GlobalsBuilder;
use starlark::values::dict::DictRef;
use starlark::values::type_repr::DictType;
use starlark::values::Coerce;
use starlark::values::Freeze;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::ValueOf;
use thiserror::Error;

use crate::actions::artifact::artifact_type::Artifact;
use crate::interpreter::rule_defs::artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkArtifactLike;
use crate::interpreter::rule_defs::artifact::ValueAsArtifactLike;
// Provider that signals a rule is installable (ex. android_binary)

#[derive(Debug, Error)]
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
pub struct InstallInfoGen<V> {
    // Label for the installer
    #[provider(field_type = "Label")]
    installer: V,
    // list of files that need to be installed
    #[provider(field_type = "DictType<String, StarlarkArtifact>")]
    files: V,
}

impl<'v, V: ValueLike<'v>> InstallInfoGen<V> {
    pub fn get_installer(&self) -> anyhow::Result<ConfiguredProvidersLabel> {
        let label = Label::from_value(self.installer.to_value())
            .ok_or_else(|| {
                InstallInfoProviderErrors::ExpectedLabel(
                    self.installer.to_value().to_repr(),
                    self.installer.to_value().get_type().to_owned(),
                )
            })?
            .label()
            .to_owned();
        Ok(label)
    }

    fn get_files_dict(&self) -> DictRef<'v> {
        DictRef::from_value(self.files.to_value()).expect("Value is a Dict")
    }

    fn get_files_iter<'a>(
        files: &'a DictRef<'v>,
    ) -> impl Iterator<Item = anyhow::Result<(&'v str, &'v dyn StarlarkArtifactLike)>> + 'a {
        files.iter().map(|(k, v)| {
            let k = k
                .unpack_str()
                .ok_or_else(|| InstallInfoProviderErrors::ExpectedStringKey(k.to_string()))?;
            Ok((
                k,
                v.as_artifact()
                    .ok_or_else(|| InstallInfoProviderErrors::ExpectedArtifact {
                        key: k.to_owned(),
                        got: v.get_type().to_owned(),
                    })?,
            ))
        })
    }

    pub fn get_files(&self) -> anyhow::Result<SmallMap<&'v str, Artifact>> {
        Self::get_files_iter(&self.get_files_dict())
            .map(|x| {
                let (k, v) = x?;
                Ok((
                    k,
                    v.get_bound_artifact()
                        .with_context(|| format!("For key `{k}`"))?,
                ))
            })
            .collect()
    }
}

#[starlark_module]
fn install_info_creator(globals: &mut GlobalsBuilder) {
    fn InstallInfo<'v>(
        installer: ValueOf<'v, &'v Label>,
        files: ValueOf<'v, SmallMap<&'v str, Value<'v>>>,
    ) -> anyhow::Result<InstallInfo<'v>> {
        let info = InstallInfo {
            installer: *installer,
            files: files.value,
        };
        validate_install_info(&info)?;
        Ok(info)
    }
}

fn validate_install_info<'v, V>(info: &InstallInfoGen<V>) -> anyhow::Result<()>
where
    V: ValueLike<'v>,
{
    for x in InstallInfoGen::<V>::get_files_iter(&info.get_files_dict()) {
        let (k, v) = x?;
        if let Some(other_artifacts) = v.get_associated_artifacts() {
            if !other_artifacts.is_empty() {
                return Err(InstallInfoProviderErrors::AssociatedArtifacts {
                    key: k.to_owned(),
                    artifact: v.to_string(),
                }
                .into());
            }
        }
    }
    Ok(())
}
