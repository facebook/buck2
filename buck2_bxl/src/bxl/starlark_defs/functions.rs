use anyhow::Context;
use buck2_core::provider::{ConfiguredProvidersLabel, ProviderName, ProvidersLabel, ProvidersName};
use buck2_interpreter::types::{
    label::{Label, StarlarkProvidersLabel},
    target_label::{StarlarkConfiguredTargetLabel, StarlarkTargetLabel},
};
use gazebo::prelude::*;
use starlark::{
    environment::GlobalsBuilder,
    eval::Evaluator,
    starlark_module,
    values::{
        list::{FrozenList, List},
        Value, ValueError,
    },
};

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
