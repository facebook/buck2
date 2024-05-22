/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs;
use std::io::Cursor;

use buck2_core::fs::paths::abs_path::AbsPathBuf;

#[allow(unused_imports)]
#[allow(unused_extern_crates)]
#[allow(clippy::extra_unused_lifetimes)]
mod explain_generated;
use buck2_common::manifold::Bucket;
use buck2_common::manifold::ManifoldClient;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use buck2_node::attrs::display::AttrDisplayWithContextExt;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::attrs::internal::DEFAULT_TARGET_PLATFORM_ATTRIBUTE_FIELD;
use buck2_node::attrs::internal::EXEC_COMPATIBLE_WITH_ATTRIBUTE_FIELD;
use buck2_node::attrs::internal::LEGACY_TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD;
use buck2_node::attrs::internal::TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD;
use buck2_node::attrs::internal::TESTS_ATTRIBUTE_FIELD;
use buck2_node::attrs::internal::VISIBILITY_ATTRIBUTE_FIELD;
use buck2_node::attrs::internal::WITHIN_VIEW_ATTRIBUTE_FIELD;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_query::query::environment::QueryTarget;
use flatbuffers::FlatBufferBuilder;
use flatbuffers::WIPOffset;

// use crate::explain_generated::explain::Bool;

mod fbs {
    pub use crate::explain_generated::explain::BoolAttr;
    pub use crate::explain_generated::explain::BoolAttrArgs;
    pub use crate::explain_generated::explain::Build;
    pub use crate::explain_generated::explain::BuildArgs;
    pub use crate::explain_generated::explain::ConfiguredTargetNode;
    pub use crate::explain_generated::explain::ConfiguredTargetNodeArgs;
    pub use crate::explain_generated::explain::ListOfStringsAttr;
    pub use crate::explain_generated::explain::ListOfStringsAttrArgs;
    pub use crate::explain_generated::explain::StringAttr;
    pub use crate::explain_generated::explain::StringAttrArgs;
}

pub async fn main(
    data: Vec<ConfiguredTargetNode>,
    output: Option<&AbsPathBuf>,
    fbs_dump: Option<&AbsPathBuf>,
    allow_vpnless: bool,
    manifold_path: Option<String>,
) -> anyhow::Result<()> {
    let fbs = gen_fbs(data)?;

    let fbs = fbs.finished_data();
    let base64 = base64::encode(&fbs);

    // For dev purposes, dump the base64 encoded flatbuffer to a file
    if let Some(fbs_dump) = fbs_dump {
        fs::write(fbs_dump, &base64)?;
    }

    let html_out = {
        let html_in = include_str!("explain.html");
        let html_out = html_in.replace("XXDATAXX", &base64);
        // TODO: find a better way to assert it actually replaced something
        if html_in == html_out {
            return Err(anyhow::anyhow!("HTML template is not valid"));
        }
        html_out
    };

    let mut cursor = &mut Cursor::new(html_out.as_bytes());

    if let Some(o) = output {
        fs::write(o, &html_out)?
    };

    if let Some(p) = manifold_path {
        // TODO iguridi: compress before upload
        // TODO iguridi: write and upload concurrently
        // upload to manifold
        let manifold = ManifoldClient::new(allow_vpnless)?;

        manifold
            .read_and_upload(Bucket::EVENT_LOGS, &p, Default::default(), &mut cursor)
            .await?;
    }

    Ok(())
}

fn gen_fbs(data: Vec<ConfiguredTargetNode>) -> anyhow::Result<FlatBufferBuilder<'static>> {
    let mut builder = FlatBufferBuilder::new();

    let targets: Result<Vec<_>, _> = data
        .iter()
        .map(|node| target_to_fbs(&mut builder, node))
        .collect();

    let targets = builder.create_vector(&targets?);
    let build = fbs::Build::create(
        &mut builder,
        &fbs::BuildArgs {
            targets: Some(targets),
        },
    );
    builder.finish(build, None);
    Ok(builder)
}

fn target_to_fbs<'a>(
    builder: &'_ mut FlatBufferBuilder<'static>,
    node: &'_ ConfiguredTargetNode,
) -> anyhow::Result<WIPOffset<fbs::ConfiguredTargetNode<'a>>, anyhow::Error> {
    // special attrs
    let name = builder.create_shared_string(&node.name());
    let label = builder.create_shared_string(&node.label().to_string());
    let oncall = node.oncall().map(|v| builder.create_shared_string(v));
    let type_ = builder.create_shared_string(node.rule_type().name());
    let package = builder.create_shared_string(&node.buildfile_path().to_string());
    let target_configuration =
        builder.create_shared_string(&node.target_configuration().to_string());
    let execution_platform = builder.create_shared_string(&node.execution_platform()?.id());
    let deps = list_of_strings_to_fbs(
        builder,
        node.deps().map(|dep| dep.label().to_string()).collect(),
    );
    let plugins = list_of_strings_to_fbs(
        builder,
        node.plugin_lists()
            .iter()
            .map(|(kind, _, _)| kind.to_string())
            .collect(),
    );

    // internal attrs
    let default_target_platform = node.map_attr(DEFAULT_TARGET_PLATFORM_ATTRIBUTE_FIELD, |attr| {
        attr.and_then(|a| optional_string(a).map(|v| builder.create_shared_string(&v)))
    });
    let target_compatible_with = list_of_strings_to_fbs(
        builder,
        list_of_strings_attr(node, TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD),
    );
    let compatible_with = list_of_strings_to_fbs(
        builder,
        list_of_strings_attr(node, LEGACY_TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD),
    );
    let exec_compatible_with = list_of_strings_to_fbs(
        builder,
        list_of_strings_attr(node, EXEC_COMPATIBLE_WITH_ATTRIBUTE_FIELD),
    );
    let visibility = list_of_strings_to_fbs(
        builder,
        list_of_strings_attr(node, VISIBILITY_ATTRIBUTE_FIELD),
    );
    let within_view = list_of_strings_to_fbs(
        builder,
        list_of_strings_attr(node, WITHIN_VIEW_ATTRIBUTE_FIELD),
    );
    let tests = list_of_strings_to_fbs(builder, list_of_strings_attr(node, TESTS_ATTRIBUTE_FIELD));

    // defined attrs
    let mut bools = vec![];
    let mut strings = vec![];
    let mut list_of_strings: Vec<(&str, Vec<String>)> = vec![];
    for a in node.attrs(AttrInspectOptions::DefinedOnly) {
        match a.value {
            ConfiguredAttr::Bool(v) => bools.push((a.name, v.0)),
            ConfiguredAttr::String(v) => strings.push((a.name, v.0.to_string())),
            ConfiguredAttr::List(v) => {
                let mut list = vec![];
                v.0.iter().for_each(|v| {
                    match v {
                        ConfiguredAttr::String(v) => list.push(v.0.to_string()),
                        _ => {} // TODO iguridi: handle other list types
                    }
                });
                list_of_strings.push((a.name, list));
            }
            ConfiguredAttr::None => {}
            _ => {} // TODO iguridi: implement
        }
    }

    let list: Vec<_> = bools
        .into_iter()
        .map(|(key, value)| {
            let key = Some(builder.create_shared_string(key));
            fbs::BoolAttr::create(builder, &fbs::BoolAttrArgs { key, value })
        })
        .collect();
    let bool_attrs = Some(builder.create_vector(&list));

    let list: Vec<_> = strings
        .into_iter()
        .map(|(key, value)| {
            let key = Some(builder.create_shared_string(key));
            let value = Some(builder.create_shared_string(&value));
            fbs::StringAttr::create(builder, &fbs::StringAttrArgs { key, value })
        })
        .collect();
    let string_attrs = Some(builder.create_vector(&list));

    let list: Vec<_> = list_of_strings
        .into_iter()
        .map(|(key, value)| {
            let key = Some(builder.create_shared_string(key));
            let value = list_of_strings_to_fbs(builder, value);
            fbs::ListOfStringsAttr::create(builder, &fbs::ListOfStringsAttrArgs { key, value })
        })
        .collect();
    let list_of_strings_attrs = Some(builder.create_vector(&list));

    let target = fbs::ConfiguredTargetNode::create(
        builder,
        &fbs::ConfiguredTargetNodeArgs {
            // special attrs
            configured_target_label: Some(label),
            name: Some(name),
            type_: Some(type_),
            deps,
            package: Some(package),
            oncall,
            target_configuration: Some(target_configuration),
            execution_platform: Some(execution_platform),
            plugins,
            // internal attrs
            default_target_platform,
            target_compatible_with,
            compatible_with,
            exec_compatible_with,
            visibility,
            within_view,
            tests,
            // defined attrs
            bool_attrs,
            string_attrs,
            list_of_strings_attrs,
        },
    );
    Ok(target)
}

fn list_of_strings_attr(node: &ConfiguredTargetNode, attr_name: &str) -> Vec<String> {
    // Only works on attributes that are lists of strings
    node.map_attr(attr_name, |attr| {
        attr.and_then(|a| a.unpack_list())
            .into_iter()
            .flatten()
            .filter_map(optional_string)
            .collect::<Vec<String>>()
    })
}

fn list_of_strings_to_fbs<'a>(
    builder: &'_ mut FlatBufferBuilder<'static>,
    list: Vec<String>,
) -> Option<WIPOffset<flatbuffers::Vector<'static, flatbuffers::ForwardsUOffset<&'a str>>>> {
    let list = list
        .into_iter()
        .map(|v| builder.create_shared_string(&v))
        .collect::<Vec<WIPOffset<&str>>>();
    Some(builder.create_vector(&list))
}

fn optional_string(attr: &ConfiguredAttr) -> Option<String> {
    match attr {
        ConfiguredAttr::None => None,
        ConfiguredAttr::String(_v) => Some(attr.to_owned().as_display_no_ctx().to_string()),
        _ => None, // TODO iguridi: support other types
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::configuration::data::ConfigurationData;
    use buck2_core::execution_types::execution::ExecutionPlatform;
    use buck2_core::execution_types::execution::ExecutionPlatformResolution;
    use buck2_core::execution_types::executor_config::CommandExecutorConfig;
    use buck2_core::package::PackageLabel;
    use buck2_core::plugins::PluginKindSet;
    use buck2_core::provider::label::ProvidersLabel;
    use buck2_core::provider::label::ProvidersName;
    use buck2_core::target::label::label::TargetLabel;
    use buck2_core::target::name::TargetName;
    use buck2_node::attrs::attr::Attribute;
    use buck2_node::attrs::attr_type::bool::BoolLiteral;
    use buck2_node::attrs::attr_type::list::ListLiteral;
    use buck2_node::attrs::attr_type::string::StringLiteral;
    use buck2_node::attrs::attr_type::AttrType;
    use buck2_node::attrs::coerced_attr::CoercedAttr;
    use buck2_node::provider_id_set::ProviderIdSet;
    use buck2_util::arc_str::ArcSlice;
    use dupe::Dupe;

    use super::*;
    pub use crate::explain_generated::explain::Build;

    #[test]
    fn test_bool_attr() {
        let data = gen_data(vec![(
            "bool_field",
            Attribute::new(None, "", AttrType::bool()),
            CoercedAttr::Bool(BoolLiteral(false)),
        )]);

        let fbs = gen_fbs(data).unwrap();
        let fbs = fbs.finished_data();
        let build = flatbuffers::root::<Build>(fbs).unwrap();
        let target = build.targets().unwrap().get(0);

        assert_things(target, build);
        assert_eq!(
            target.bool_attrs().unwrap().get(0).key(),
            Some("bool_field")
        );
    }

    #[test]
    fn test_string_attr() {
        let data = gen_data(vec![(
            "another_field",
            Attribute::new(None, "", AttrType::string()),
            CoercedAttr::String(StringLiteral("some_string".into())),
        )]);

        let fbs = gen_fbs(data).unwrap();
        let fbs = fbs.finished_data();
        let build = flatbuffers::root::<Build>(fbs).unwrap();
        let target = build.targets().unwrap().get(0);

        assert_things(target, build);
        assert_eq!(target.string_attrs().unwrap().get(0).key(), Some("name"));
        assert_eq!(target.string_attrs().unwrap().get(0).value(), Some("foo"));
    }

    #[test]
    fn test_list_of_strings() {
        let pkg = PackageLabel::testing_parse("cell//foo/bar");
        let name = TargetName::testing_new("t2");
        let label = TargetLabel::new(pkg, name.as_ref());
        let data = gen_data(vec![(
            "some_deps",
            Attribute::new(
                None,
                "",
                AttrType::list(AttrType::dep(ProviderIdSet::EMPTY, PluginKindSet::EMPTY)),
            ),
            CoercedAttr::List(ListLiteral(ArcSlice::new([CoercedAttr::Dep(
                ProvidersLabel::new(label, ProvidersName::Default),
            )]))),
        )]);

        let fbs = gen_fbs(data).unwrap();
        let fbs = fbs.finished_data();
        let build = flatbuffers::root::<Build>(fbs).unwrap();
        let target = build.targets().unwrap().get(0);

        assert_things(target, build);
        assert_eq!(
            target.list_of_strings_attrs().unwrap().get(0).key(),
            Some("some_deps")
        );
    }

    fn assert_things(target: fbs::ConfiguredTargetNode<'_>, build: fbs::Build<'_>) {
        // special attrs
        assert_eq!(
            target.configured_target_label(),
            Some("cell//pkg:foo (<testing>#2c29d96c65b4379a)")
        );
        assert_eq!(target.name(), Some("foo"));
        assert_eq!(target.type_(), Some("foo_lib"));
        assert_eq!(target.package(), Some("cell//pkg:BUCK"));
        assert_eq!(target.oncall(), None);
        assert_eq!(target.default_target_platform(), None);
        assert_eq!(target.execution_platform(), Some("cell//pkg:bar"));
        assert_eq!(target.deps().unwrap().is_empty(), true);
        assert_eq!(target.plugins().unwrap().is_empty(), true);
        // internal attrs
        assert_eq!(target.default_target_platform(), None);
        assert!(target.target_compatible_with().unwrap().is_empty());
        assert!(target.compatible_with().unwrap().is_empty());
        assert!(target.exec_compatible_with().unwrap().is_empty());
        assert!(target.visibility().unwrap().is_empty());
        assert!(target.within_view().unwrap().is_empty());
        assert!(target.tests().unwrap().is_empty());

        let target2 = build.targets().unwrap().get(1);
        assert!(
            target2
                .configured_target_label()
                .unwrap()
                .contains("cell//pkg:baz (<testing>#"),
        );
    }

    fn gen_data(
        attrs: Vec<(
            &str,
            buck2_node::attrs::attr::Attribute,
            buck2_node::attrs::coerced_attr::CoercedAttr,
        )>,
    ) -> Vec<ConfiguredTargetNode> {
        // Setup data
        let target_label = TargetLabel::testing_parse("cell//pkg:foo");
        let configured_target_label = target_label.configure(ConfigurationData::testing_new());

        let execution_platform_resolution = {
            let platform_label = TargetLabel::testing_parse("cell//pkg:bar");
            let platform = ExecutionPlatform::platform(
                platform_label,
                ConfigurationData::testing_new(),
                CommandExecutorConfig::testing_local(),
            );
            ExecutionPlatformResolution::new(Some(platform), Vec::new())
        };

        let target = ConfiguredTargetNode::testing_new(
            configured_target_label,
            "foo_lib",
            execution_platform_resolution.dupe(),
            attrs,
        );

        let target_label2 = TargetLabel::testing_parse("cell//pkg:baz");
        let configured_target_label2 = target_label2.configure(ConfigurationData::testing_new());
        let target2 = ConfiguredTargetNode::testing_new(
            configured_target_label2,
            "foo_lib",
            execution_platform_resolution,
            vec![],
        );
        vec![target, target2]
    }
}
