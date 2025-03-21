/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_build_api::actions::execute::dice_data::set_fallback_executor_config;
use buck2_configured::execution::ExecutionPlatformsKey;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::execution_types::executor_config::CommandExecutorConfig;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::package::PackageLabel;
use buck2_core::plugins::PluginKindSet;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::label::label::TargetLabel;
use buck2_core::target::name::TargetName;
use buck2_interpreter_for_build::interpreter::calculation::InterpreterResultsKey;
use buck2_interpreter_for_build::super_package::package_value::SuperPackageValuesImpl;
use buck2_node::attrs::attr::Attribute;
use buck2_node::attrs::attr_type::any::AnyAttrType;
use buck2_node::attrs::attr_type::bool::BoolLiteral;
use buck2_node::attrs::attr_type::dep::DepAttr;
use buck2_node::attrs::attr_type::dep::DepAttrTransition;
use buck2_node::attrs::attr_type::dep::DepAttrType;
use buck2_node::attrs::attr_type::list::ListLiteral;
use buck2_node::attrs::attr_type::string::StringLiteral;
use buck2_node::attrs::attr_type::AttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::attrs::spec::internal::is_internal_attr;
use buck2_node::bzl_or_bxl_path::BzlOrBxlPath;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_node::nodes::eval_result::EvaluationResult;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::nodes::targets_map::TargetsMap;
use buck2_node::nodes::unconfigured::testing::TargetNodeExt;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::provider_id_set::ProviderIdSet;
use buck2_node::rule_type::RuleType;
use buck2_node::rule_type::StarlarkRuleType;
use buck2_node::super_package::SuperPackage;
use buck2_util::arc_str::ArcSlice;
use dice::testing::DiceBuilder;
use dice::UserComputationData;
use dupe::Dupe;
use starlark::collections::SmallMap;
use starlark_map::smallmap;

#[tokio::test]
async fn test_get_node() -> anyhow::Result<()> {
    let cfg = ConfigurationData::testing_new();
    let pkg = PackageLabel::testing_parse("cell//foo/bar");

    let name1 = TargetName::testing_new("t1");
    let label1 = TargetLabel::new(pkg.dupe(), name1.as_ref());

    let name2 = TargetName::testing_new("t2");
    let label2 = TargetLabel::new(pkg.dupe(), name2.as_ref());

    let rule_type = RuleType::Starlark(Arc::new(StarlarkRuleType {
        path: BzlOrBxlPath::Bzl(ImportPath::testing_new("cell//foo/bar:def.bzl")),
        name: "some_rule".to_owned(),
    }));
    let attrs1 = vec![
        (
            "bool_field",
            Attribute::new(None, "", AttrType::bool()),
            CoercedAttr::Bool(BoolLiteral(false)),
        ),
        (
            "another_field",
            Attribute::new(None, "", AttrType::string()),
            CoercedAttr::String(StringLiteral("some_string".into())),
        ),
        (
            "some_deps",
            Attribute::new(
                None,
                "",
                AttrType::list(AttrType::dep(ProviderIdSet::EMPTY, PluginKindSet::EMPTY)),
            ),
            CoercedAttr::List(ListLiteral(ArcSlice::new([CoercedAttr::Dep(
                ProvidersLabel::new(label2.dupe(), ProvidersName::Default),
            )]))),
        ),
    ];

    let node1 = TargetNode::testing_new(label1.dupe(), rule_type.dupe(), attrs1, None);

    let attrs2 = vec![
        (
            "bool_field",
            Attribute::new(None, "", AttrType::bool()),
            CoercedAttr::Bool(BoolLiteral(true)),
        ),
        (
            "another_field",
            Attribute::new(None, "", AttrType::string()),
            CoercedAttr::String(StringLiteral("another_string".into())),
        ),
        (
            "some_deps",
            Attribute::new(
                None,
                "",
                AttrType::list(AttrType::dep(ProviderIdSet::EMPTY, PluginKindSet::EMPTY)),
            ),
            AnyAttrType::empty_list(),
        ),
    ];

    let node2 = TargetNode::testing_new(label2.dupe(), rule_type.dupe(), attrs2, None);

    let eval_result = EvaluationResult::new(
        Arc::new(BuildFilePath::new(
            pkg.dupe(),
            FileNameBuf::unchecked_new("BUCK"),
        )),
        Vec::new(),
        SuperPackage::empty::<SuperPackageValuesImpl>()?,
        TargetsMap::from_iter([node1.dupe(), node2.dupe()]),
    );

    let mut data = UserComputationData::new();
    set_fallback_executor_config(&mut data.data, CommandExecutorConfig::testing_local());
    let computations = DiceBuilder::new()
        .mock_and_return(InterpreterResultsKey(pkg), Ok(Arc::new(eval_result)))
        .mock_and_return(ExecutionPlatformsKey, Ok(None))
        .build(data)?;
    let mut computations = computations.commit().await;

    let node = computations.get_target_node(&label1).await?;
    assert_eq!(node, node1);

    let node = computations.get_target_node(&label2).await?;
    assert_eq!(node, node2);

    let conf_attrs1 = smallmap![
        "bool_field" => ConfiguredAttr::Bool(BoolLiteral(false)),
        "another_field" =>
         ConfiguredAttr::String(StringLiteral("some_string".into())),
        "some_deps" =>
         ConfiguredAttr::List(ListLiteral(ArcSlice::new([
            ConfiguredAttr::Dep(Box::new(DepAttr {
                attr_type: DepAttrType::new(ProviderIdSet::EMPTY, DepAttrTransition::Identity(PluginKindSet::EMPTY)),
                label: ProvidersLabel::new(label2.dupe(), ProvidersName::Default)
                    .configure(cfg.dupe()),
            })),
        ]))),
    ];

    let conf_attrs2 = smallmap![
        "bool_field" => ConfiguredAttr::Bool(BoolLiteral(true)),
        "another_field" =>
         ConfiguredAttr::String(StringLiteral("another_string".into())),
        "some_deps" => ConfiguredAttr::List(
            ListLiteral(ArcSlice::new([]))
        ),
    ];

    let node = computations.get_target_node(&label1).await?;
    assert_eq!(node, node1);

    let node = computations.get_target_node(&label2).await?;
    assert_eq!(node, node2);

    let node = computations
        .get_configured_target_node(&label1.configure(cfg.dupe()))
        .await?;
    let node = node.require_compatible()?;
    let node_attrs: SmallMap<_, _> = node
        .attrs(AttrInspectOptions::All)
        .filter_map(|a| {
            if is_internal_attr(a.name) {
                None
            } else {
                Some((a.name, a.value))
            }
        })
        .collect();
    assert_eq!(node_attrs, conf_attrs1);

    let node = computations
        .get_configured_target_node(&label2.configure(cfg.dupe()))
        .await?;
    let node = node.require_compatible()?;
    let node_attrs: SmallMap<_, _> = node
        .attrs(AttrInspectOptions::All)
        .filter_map(|a| {
            if is_internal_attr(a.name) {
                None
            } else {
                Some((a.name, a.value))
            }
        })
        .collect();
    assert_eq!(node_attrs, conf_attrs2);

    Ok(())
}
