use anyhow::Context;
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
use starlark::values::Value;
use starlark::values::ValueError;

use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;

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
