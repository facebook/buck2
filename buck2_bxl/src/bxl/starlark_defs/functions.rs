/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Instant;

use anyhow::Context;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkArtifact;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProviderName;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_interpreter::types::label::Label;
use buck2_interpreter::types::label::StarlarkProvidersLabel;
use buck2_interpreter::types::target_label::StarlarkConfiguredTargetLabel;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use gazebo::prelude::*;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::list::FrozenList;
use starlark::values::list::List;
use starlark::values::Heap;
use starlark::values::StringValue;
use starlark::values::Value;
use starlark::values::ValueError;

use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;
use crate::bxl::starlark_defs::time::StarlarkInstant;

#[starlark_module]
pub fn register_label_function(builder: &mut GlobalsBuilder) {
    /// Converts a `TargetLabel` into its corresponding `ProvidersLabel` given the subtarget names,
    /// which is a list for each layer of subtarget
    fn sub_target<'v>(
        target: &StarlarkTargetLabel,
        #[starlark(default = FrozenList::empty())] subtarget_name: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkProvidersLabel<'v>> {
        let providers_name = value_to_providers_name(subtarget_name)?;

        Ok(StarlarkProvidersLabel::new(
            eval.heap(),
            ProvidersLabel::new(target.label().dupe(), providers_name),
        ))
    }

    /// Converts a `TargetLabel` into its corresponding `ProvidersLabel` given the subtarget name
    /// which is a list for each layer of subtarget
    fn configured_sub_target<'v>(
        target: &StarlarkConfiguredTargetLabel,
        #[starlark(default = FrozenList::empty())] subtarget_name: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Label<'v>> {
        let providers_name = value_to_providers_name(subtarget_name)?;

        Ok(Label::new(
            eval.heap(),
            ConfiguredProvidersLabel::new(target.label().dupe(), providers_name),
        ))
    }
}

#[starlark_module]
pub fn register_target_function(builder: &mut GlobalsBuilder) {
    fn target_set() -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        Ok(StarlarkTargetSet::from(TargetSet::new()))
    }
}

#[starlark_module]
pub fn register_artifact_function(builder: &mut GlobalsBuilder) {
    /// The project relative path of the source or build artifact.
    /// Note that this method returns an artifact path without asking for the artifact to be materialized,
    /// (i.e. it may not actually exist on the disk yet).
    fn get_path_without_materialization<'v>(
        this: &'v StarlarkArtifact,
        heap: &'v Heap,
        ctx: &'v BxlContext<'v>,
    ) -> anyhow::Result<StringValue<'v>> {
        let resolved = ctx
            .output_stream
            .artifact_fs
            .resolve(this.artifact().get_path())?;

        Ok(heap.alloc_str(resolved.as_str()))
    }
}

#[starlark_module]
pub fn register_instant_function(builder: &mut GlobalsBuilder) {
    fn now<'v>(heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(heap.alloc(StarlarkInstant(Instant::now())))
    }
}

fn value_to_providers_name<'v>(subtarget_name: Value<'v>) -> anyhow::Result<ProvidersName> {
    let subtarget = if let Some(list) = List::from_value(subtarget_name) {
        list.iter()
            .map(|name| {
                name.unpack_str()
                    .ok_or_else(|| {
                        anyhow::anyhow!(ValueError::IncorrectParameterTypeNamedWithExpected(
                            "subtarget_name".to_owned(),
                            "list of str or str".to_owned(),
                            name.get_type().to_owned(),
                        ))
                    })
                    .and_then(|name| {
                        ProviderName::new(name.to_owned())
                            .context("for parameter `subtarget_name`")
                            .map_err(|e| anyhow::anyhow!(e))
                    })
            })
            .collect::<anyhow::Result<Vec<_>>>()?
    } else if let Some(str) = subtarget_name.unpack_str() {
        vec![ProviderName::new(str.to_owned()).context("for parameter `subtarget_name`")?]
    } else {
        return Err(anyhow::anyhow!(
            ValueError::IncorrectParameterTypeNamedWithExpected(
                "subtarget_name".to_owned(),
                "list of str or str".to_owned(),
                subtarget_name.get_type().to_owned()
            )
        ));
    };

    Ok(if subtarget.is_empty() {
        ProvidersName::Default
    } else {
        ProvidersName::Named(subtarget)
    })
}
