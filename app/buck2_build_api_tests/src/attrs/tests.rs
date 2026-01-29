/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_analysis::attrs::resolve::configured_attr::ConfiguredAttrExt;
use buck2_build_api::interpreter::rule_defs::cmd_args::DefaultCommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use buck2_build_api::interpreter::rule_defs::provider::registration::register_builtin_providers;
use buck2_common::package_listing::listing::PackageListing;
use buck2_common::package_listing::listing::testing::PackageListingExt;
use buck2_core::cells::CellResolver;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::cells::name::CellName;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::execution_types::executor_config::PathSeparatorKind;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::buck_out_path::BuckOutPathResolver;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::package::PackageLabel;
use buck2_core::plugins::PluginKindSet;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_interpreter_for_build::attrs::coerce::attr_type::AttrTypeExt;
use buck2_interpreter_for_build::attrs::coerce::testing::coercion_ctx;
use buck2_interpreter_for_build::attrs::coerce::testing::coercion_ctx_listing;
use buck2_interpreter_for_build::attrs::coerce::testing::to_value;
use buck2_interpreter_for_build::interpreter::selector::register_select;
use buck2_node::attrs::attr_type::AttrType;
use buck2_node::attrs::coerced_deps_collector::CoercedDepsCollector;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use buck2_node::attrs::configuration_context::AttrConfigurationContext;
use buck2_node::attrs::configured_attr_info_for_tests::ConfiguredAttrInfoForTests;
use buck2_node::attrs::display::AttrDisplayWithContextExt;
use buck2_node::attrs::fmt_context::AttrFmtContext;
use buck2_node::attrs::testing::configuration_ctx;
use buck2_node::provider_id_set::ProviderIdSet;
use dupe::Dupe;
use fxhash::FxHashMap;
use gazebo::prelude::*;
use indoc::indoc;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Module;
use starlark::values::Heap;
use starlark::values::UnpackValue;
use starlark::values::Value;

use crate::attrs::resolve::testing::resolution_ctx;
use crate::attrs::resolve::testing::resolution_ctx_with_providers;

#[test]
fn test() -> buck2_error::Result<()> {
    Module::with_temp_heap(|env| {
        let globals = GlobalsBuilder::standard().with(register_select).build();

        // Check that `x` is captured with the function
        let value = to_value(
            &env,
            &globals,
            indoc!(
                r#"
                [[
                    ["hello", "world!"]
                    + select({
                        "//some:config": ["some"],
                        "DEFAULT": ["okay"] + select({
                            "//other:config": ["other"],
                            "DEFAULT": ["default", "for", "realz"],
                        }),
                    })
                    + ["..."]
                    + ["..."]
                ]]
                "#
            ),
        );

        let attr = AttrType::list(AttrType::list(AttrType::list(AttrType::string())));

        let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)?;
        assert_eq!(
            "[[[\"hello\", \"world!\"]+select({\"root//some:config\": [\"some\"], \"DEFAULT\": [\"okay\"]+select({\"root//other:config\": [\"other\"], \"DEFAULT\": [\"default\", \"for\", \"realz\"]})})+[\"...\"]+[\"...\"]]]",
            coerced.as_display_no_ctx().to_string()
        );

        let configured = coerced.configure(&attr, &configuration_ctx())?;
        assert_eq!(
            "[[[\"hello\", \"world!\", \"okay\", \"other\", \"...\", \"...\"]]]",
            configured.as_display_no_ctx().to_string()
        );

        let mut ctx = resolution_ctx(&env);
        let resolved = configured.resolve_single(PackageLabel::testing(), &mut ctx)?;
        assert_eq!(
            "[[[\"hello\", \"world!\", \"okay\", \"other\", \"...\", \"...\"]]]",
            resolved.to_string()
        );

        Ok(())
    })
}

#[test]
fn test_string() -> buck2_error::Result<()> {
    Module::with_temp_heap(|env| {
        let globals = GlobalsBuilder::standard().with(register_select).build();
        let attr = AttrType::string();
        let value = to_value(&env, &globals, r#""a" + select({"DEFAULT": "b"})"#);

        let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)?;
        let configured = coerced.configure(&attr, &configuration_ctx())?;
        assert_eq!(r#""ab""#, configured.as_display_no_ctx().to_string());

        Ok(())
    })
}

#[test]
fn test_invalid_concat_coercion_into_one_of() -> buck2_error::Result<()> {
    Module::with_temp_heap(|env| {
        let globals = GlobalsBuilder::standard().with(register_select).build();

        let value = to_value(
            &env,
            &globals,
            indoc!(
                r#"
            [True] + select({"DEFAULT": ["foo"]})
            "#
            ),
        );
        let attr = AttrType::one_of(vec![
            AttrType::list(AttrType::bool()),
            AttrType::list(AttrType::string()),
        ]);

        let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)?;
        let err = coerced
            .configure(&attr, &configuration_ctx())
            .expect_err("Should fail to concatenate configured lists");
        assert!(
            err.to_string()
                .contains("Cannot concatenate values coerced/configured to different oneof variants: `attrs.list(attrs.bool())` and `attrs.list(attrs.string())`"),
            "err: {err}"
        );
        Ok(())
    })
}

#[test]
fn test_concat_option_one_of() {
    Module::with_temp_heap(|env| {
        let globals = GlobalsBuilder::standard().with(register_select).build();

        let value = to_value(
            &env,
            &globals,
            indoc!(
                r#"
            ["foo"] + select({"DEFAULT": ["bar"]})
            "#
            ),
        );

        let attr = AttrType::option(AttrType::one_of(vec![
            AttrType::list(AttrType::string()),
            AttrType::list(AttrType::bool()),
        ]));

        let coerced = attr
            .coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)
            .unwrap();
        let configured = coerced.configure(&attr, &configuration_ctx()).unwrap();
        assert_eq!(
            r#"["foo", "bar"]"#,
            configured.as_display_no_ctx().to_string()
        );
        buck2_error::Ok(())
    })
    .unwrap();
}

#[test]
fn test_any() -> buck2_error::Result<()> {
    Heap::temp(|heap| {
        let value = heap.alloc(vec!["//some:target", "cell1//named:target[foo]"]);
        let attr = AttrType::any();

        let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)?;
        assert_eq!(
            "[\"//some:target\", \"cell1//named:target[foo]\"]",
            coerced.as_display_no_ctx().to_string()
        );
        let configured = coerced.configure(&attr, &configuration_ctx())?;
        assert_eq!(
            "[\"//some:target\", \"cell1//named:target[foo]\"]",
            configured.as_display_no_ctx().to_string()
        );

        let value = Value::new_none();
        let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)?;
        assert_eq!("None", coerced.as_display_no_ctx().to_string());
        let configured = coerced.configure(&attr, &configuration_ctx())?;
        assert_eq!("None", configured.as_display_no_ctx().to_string());

        let value = Value::new_bool(true);
        let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)?;
        assert_eq!("True", coerced.as_display_no_ctx().to_string());
        let configured = coerced.configure(&attr, &configuration_ctx())?;
        assert_eq!("True", configured.as_display_no_ctx().to_string());

        let value = heap.alloc(42);
        let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)?;
        assert_eq!("42", coerced.as_display_no_ctx().to_string());
        let configured = coerced.configure(&attr, &configuration_ctx())?;
        assert_eq!("42", configured.as_display_no_ctx().to_string());

        Ok(())
    })
}

#[test]
fn test_option() -> buck2_error::Result<()> {
    Heap::temp(|heap| {
        let attr = AttrType::option(AttrType::list(AttrType::string()));

        let value = heap.alloc(vec!["string1", "string2"]);
        let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)?;
        assert_eq!(
            "[\"string1\", \"string2\"]",
            coerced.as_display_no_ctx().to_string()
        );
        let configured = coerced.configure(&attr, &configuration_ctx())?;
        assert_eq!(
            "[\"string1\", \"string2\"]",
            configured.as_display_no_ctx().to_string()
        );

        let value = Value::new_none();
        let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)?;
        assert_eq!("None", coerced.as_display_no_ctx().to_string());
        let configured = coerced.configure(&attr, &configuration_ctx())?;
        assert_eq!("None", configured.as_display_no_ctx().to_string());

        Ok(())
    })
}

#[test]
fn test_dict() -> buck2_error::Result<()> {
    Module::with_temp_heap(|env| {
        let globals = GlobalsBuilder::standard().with(register_select).build();
        let value = to_value(&env, &globals, r#"{"b":["1"],"a":[]}"#);

        let attr = AttrType::dict(AttrType::string(), AttrType::list(AttrType::string()), true);
        let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)?;
        assert_eq!(
            "{\"a\": [], \"b\": [\"1\"]}",
            coerced.as_display_no_ctx().to_string()
        );
        let configured = coerced.configure(&attr, &configuration_ctx())?;
        assert_eq!(
            "{\"a\": [], \"b\": [\"1\"]}",
            configured.as_display_no_ctx().to_string()
        );

        let attr = AttrType::dict(
            AttrType::string(),
            AttrType::list(AttrType::string()),
            false,
        );
        let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)?;
        assert_eq!(
            "{\"b\": [\"1\"], \"a\": []}",
            coerced.as_display_no_ctx().to_string()
        );
        let configured = coerced.configure(&attr, &configuration_ctx())?;
        assert_eq!(
            "{\"b\": [\"1\"], \"a\": []}",
            configured.as_display_no_ctx().to_string()
        );

        let value = to_value(
            &env,
            &globals,
            r#"{"b":["1"], "a":[]} + select({"DEFAULT": { "c": []}})"#,
        );
        let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)?;
        let configured = coerced.configure(&attr, &configuration_ctx())?;
        assert_eq!(
            r#"{"b": ["1"], "a": [], "c": []}"#,
            configured.as_display_no_ctx().to_string()
        );

        Ok(())
    })
}

#[test]
fn test_one_of() -> buck2_error::Result<()> {
    Heap::temp(|heap| {
        let value = heap.alloc("one");
        let values = heap.alloc(vec!["test", "extra"]);

        let attr = AttrType::one_of(vec![AttrType::string(), AttrType::list(AttrType::string())]);
        let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)?;
        assert_eq!("\"one\"", coerced.as_display_no_ctx().to_string());
        let configured = coerced.configure(&attr, &configuration_ctx())?;
        assert_eq!("\"one\"", configured.as_display_no_ctx().to_string());

        let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), values)?;
        assert_eq!(
            "[\"test\", \"extra\"]",
            coerced.as_display_no_ctx().to_string()
        );
        let configured = coerced.configure(&attr, &configuration_ctx())?;
        assert_eq!(
            "[\"test\", \"extra\"]",
            configured.as_display_no_ctx().to_string()
        );

        let attr = AttrType::one_of(Vec::new());
        let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value);
        assert!(coerced.is_err());

        Ok(())
    })
}

#[test]
fn test_label() -> buck2_error::Result<()> {
    Heap::temp(|heap| {
        let value = heap.alloc(vec!["//some:target", "cell1//named:target[foo]"]);

        let attr = AttrType::list(AttrType::dep(ProviderIdSet::EMPTY, PluginKindSet::EMPTY));

        let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)?;
        assert_eq!(
            "[\"root//some:target\", \"cell1//named:target[foo]\"]",
            coerced.as_display_no_ctx().to_string()
        );

        let configured = coerced.configure(&attr, &configuration_ctx())?;
        assert_eq!(
            format!(
                "[\"root//some:target ({})\", \"cell1//named:target[foo] ({})\"]",
                ConfigurationData::testing_new(),
                ConfigurationData::testing_new()
            ),
            configured.as_display_no_ctx().to_string()
        );

        Ok(())
    })
}

#[test]
fn test_coerced_deps() -> buck2_error::Result<()> {
    Module::with_temp_heap(|env| {
        let globals = GlobalsBuilder::standard().with(register_select).build();

        let content = indoc!(
            r#"
            ["//some:target", "cell1//named:target[foo]"] + select({
                "//some:config": ["cell1//named:target[bar]"],
                "DEFAULT": ["cell1//:okay"] + select({
                    "cell1//other:config": ["//some:target2"],
                    "DEFAULT": ["//:default1", "//:default2"],
                }),
            }) + ["//:other"]
            "#
        );
        // Check that `x` is captured with the function
        let value = to_value(&env, &globals, content);

        let attr = AttrType::list(AttrType::dep(ProviderIdSet::EMPTY, PluginKindSet::EMPTY));
        let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)?;

        let mut visitor = CoercedDepsCollector::new();
        coerced.traverse(&attr, PackageLabel::testing(), &mut visitor)?;
        let CoercedDepsCollector {
            deps,
            configuration_deps,
            ..
        } = visitor;
        let deps: Vec<_> = deps.iter().map(|t| t.to_string()).collect();
        let config_deps: Vec<_> = configuration_deps.iter().map(|t| t.0.to_string()).collect();

        let expected_deps = vec![
            "root//some:target",
            "cell1//named:target",
            "cell1//:okay",
            "root//some:target2",
            "root//:default1",
            "root//:default2",
            "root//:other",
        ];

        assert_eq!(expected_deps, deps);

        let expected_config_deps = vec!["root//some:config", "cell1//other:config"];
        assert_eq!(expected_config_deps, config_deps);

        Ok(())
    })
}

#[test]
fn test_configured_deps() -> buck2_error::Result<()> {
    Module::with_temp_heap(|env| {
        let globals = GlobalsBuilder::standard().with(register_select).build();

        let content = indoc!(
            r#"
            ["//some:target", "cell1//named:target[foo]"] + select({
                "//some:config": ["cell1//named:target[bar]"],
                "DEFAULT": ["cell1//:okay"] + select({
                    "cell1//other:config": ["//some:target2"],
                    "DEFAULT": ["//:default1", "//:default2"],
                }),
            }) + ["//:other"]
            "#
        );
        // Check that `x` is captured with the function
        let value = to_value(&env, &globals, content);

        let attr = AttrType::list(AttrType::dep(ProviderIdSet::EMPTY, PluginKindSet::EMPTY));
        let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)?;
        let configured = coerced.configure(&attr, &configuration_ctx())?;

        let mut info = ConfiguredAttrInfoForTests::new();
        configured.traverse(PackageLabel::testing(), &mut info)?;

        let expected_deps = [
            "root//some:target",
            "cell1//named:target[foo]",
            "cell1//:okay",
            "root//:default1",
            "root//:default2",
            "root//:other",
        ];

        assert_eq!(
            expected_deps
                .to_vec()
                .map(|s| format!("{} ({})", s, ConfigurationData::testing_new())),
            info.deps
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
        );

        // Check also that execution deps are handled slightly differently.
        let attr_exec = AttrType::list(AttrType::exec_dep(ProviderIdSet::EMPTY));
        let coerced_exec = attr_exec.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)?;
        let configured_exec = coerced_exec.configure(&attr_exec, &configuration_ctx())?;
        let mut info = ConfiguredAttrInfoForTests::new();
        configured_exec.traverse(PackageLabel::testing(), &mut info)?;
        eprintln!("{info:?}");
        let exec_cfg = configuration_ctx().exec_cfg()?;
        assert_eq!(
            expected_deps.to_vec().map(|s| format!("{s} ({exec_cfg})")),
            info.execution_deps
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
        );

        Ok(())
    })
}

#[test]
fn test_resolved_deps() -> buck2_error::Result<()> {
    Module::with_temp_heap(|env| {
        let globals = GlobalsBuilder::standard()
            .with(register_select)
            .with(buck2_build_api::interpreter::rule_defs::register_rule_defs)
            .build();

        let content = indoc!(
            r#"
            ["//sub/dir:foo", "//sub/dir:foo[bar]"]
            "#
        );
        // Check that `x` is captured with the function
        let value = to_value(&env, &globals, content);

        let attr = AttrType::list(AttrType::dep(ProviderIdSet::EMPTY, PluginKindSet::EMPTY));
        let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)?;
        let configured = coerced.configure(&attr, &configuration_ctx())?;
        let mut resolution_ctx = resolution_ctx(&env);
        let resolved = configured.resolve_single(PackageLabel::testing(), &mut resolution_ctx)?;

        env.set("res", resolved);
        let content = indoc!(
            r#"
            foo = res[0]
            bar = res[1]
            def assert_eq(a, b):
                if a != b:
                    fail("Expected {} == {}".format(a, b))

            assert_eq(foo[DefaultInfo].sub_targets["bar"][DefaultInfo], bar[DefaultInfo])
            assert_eq(
                ("sub/dir", "foo", None),
                (foo.label.package, foo.label.name, foo.label.sub_target)
            )
            assert_eq(
                ("sub/dir", "foo", ["bar"]),
                (bar.label.package, bar.label.name, bar.label.sub_target),
            )
            None
            "#
        );

        let success = to_value(&env, &globals, content);
        assert!(success.is_none());
        Ok(())
    })
}

#[test]
fn test_dep_requires_providers() -> buck2_error::Result<()> {
    Module::with_temp_heap(|env| {
        let (mut resolution_ctx, provider_ids) = resolution_ctx_with_providers(&env);

        Heap::temp(|heap| {
            let foo_only = heap.alloc("//sub/dir:foo[foo_only]");

            let attr = AttrType::dep(provider_ids.dupe(), PluginKindSet::EMPTY);
            let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), foo_only)?;
            let configured = coerced.configure(&attr, &configuration_ctx())?;

            let err = configured
                .resolve_single(PackageLabel::testing(), &mut resolution_ctx)
                .expect_err("Should have failed");
            assert!(
                err.to_string()
                    .contains("Attribute requires a dep that provides `BarInfo`")
            );

            let foo_and_bar = heap.alloc("//sub/dir:foo[foo_and_bar]");

            let attr = AttrType::dep(provider_ids, PluginKindSet::EMPTY);
            let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), foo_and_bar)?;
            let configured = coerced.configure(&attr, &configuration_ctx())?;

            // This dep has both FooInfo and BarInfo, so it should resolve properly
            configured.resolve_single(PackageLabel::testing(), &mut resolution_ctx)?;

            Ok(())
        })
    })
}

#[test]
fn test_source_missing() {
    Heap::temp(|heap| {
        let value = heap.alloc(vec!["foo/bar.cpp"]);
        let attr = AttrType::list(AttrType::source(false));

        // FIXME: T85510500 Enable this test properly once we can error out on missing files
        match attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value) {
            Ok(_) => eprintln!("Todo, turn this into an error once T85510500 is fixed"),
            Err(e) => {
                let s = format!("{e:#}");
                assert!(
                    s.contains("Source file `foo/bar.cpp` does not exist"),
                    "Got error {s}"
                )
            }
        }
    })
}

#[test]
fn test_source_label() -> buck2_error::Result<()> {
    Heap::temp(|heap| {
        let value = heap.alloc(vec![
            "//some:target",
            "cell1//named:target[foo]",
            "foo/bar.cpp",
        ]);

        let attr = AttrType::list(AttrType::source(false));

        let coerced = attr.coerce(
            AttrIsConfigurable::Yes,
            &coercion_ctx_listing(PackageListing::testing_files(&["foo/bar.cpp"])),
            value,
        )?;
        assert_eq!(
            "[\"root//some:target\", \"cell1//named:target[foo]\", \"root//package/subdir/foo/bar.cpp\"]",
            coerced
                .as_display(&AttrFmtContext {
                    package: Some(PackageLabel::testing()),
                    options: Default::default(),
                })
                .to_string(),
        );

        let configured = coerced.configure(&attr, &configuration_ctx())?;
        assert_eq!(
            format!(
                "[{}, {}, {}]",
                format_args!(
                    "\"root//some:target ({})\"",
                    ConfigurationData::testing_new(),
                ),
                format_args!(
                    "\"cell1//named:target[foo] ({})\"",
                    ConfigurationData::testing_new()
                ),
                "\"root//package/subdir/foo/bar.cpp\"",
            ),
            configured
                .as_display(&AttrFmtContext {
                    package: Some(PackageLabel::testing()),
                    options: Default::default(),
                })
                .to_string(),
        );

        Ok(())
    })
}

#[test]
fn test_source_label_deps() -> buck2_error::Result<()> {
    Module::with_temp_heap(|env| {
        let globals = GlobalsBuilder::standard().with(register_select).build();

        let content = indoc!(
            r#"
            ["//some:target", "cell1//named:target[foo]", "some/target.cpp"] + select({
                "//some:config": ["cell1//named:target[bar]", "cell1/named/target/bar.cpp"],
                "DEFAULT": ["cell1//:okay", "cell1/okay.cpp"] + select({
                    "cell1//other:config": ["//some:target2", "some/target2.cpp"],
                    "DEFAULT": ["//:default1", "//:default2", "default.cpp"],
                }),
            }) + ["//:other", "other.cpp"]
            "#
        );
        // Check that `x` is captured with the function
        let value = to_value(&env, &globals, content);

        let attr = AttrType::list(AttrType::source(false));
        let coerced = attr.coerce(
            AttrIsConfigurable::Yes,
            &coercion_ctx_listing(PackageListing::testing_files(&[
                "some/target.cpp",
                "cell1/named/target/bar.cpp",
                "cell1/okay.cpp",
                "some/target2.cpp",
                "other.cpp",
                "default.cpp",
            ])),
            value,
        )?;

        let mut visitor = CoercedDepsCollector::new();
        coerced.traverse(&attr, PackageLabel::testing(), &mut visitor)?;
        let CoercedDepsCollector {
            deps,
            configuration_deps,
            ..
        } = visitor;
        let deps: Vec<_> = deps.iter().map(|t| t.to_string()).collect();
        let config_deps: Vec<_> = configuration_deps.iter().map(|t| t.0.to_string()).collect();

        let expected_deps = vec![
            "root//some:target",
            "cell1//named:target",
            "cell1//:okay",
            "root//some:target2",
            "root//:default1",
            "root//:default2",
            "root//:other",
        ];

        assert_eq!(expected_deps, deps);

        let expected_config_deps = vec!["root//some:config", "cell1//other:config"];
        assert_eq!(expected_config_deps, config_deps);

        Ok(())
    })
}

#[test]
fn test_source_label_resolution() -> buck2_error::Result<()> {
    fn resolve_and_test(
        content: &str,
        test_content: &str,
        files: &[&str],
    ) -> buck2_error::Result<()> {
        Module::with_temp_heap(|env| {
            let globals = GlobalsBuilder::standard()
                .with(register_select)
                .with(register_builtin_providers)
                .build();

            let value = to_value(&env, &globals, content);

            let attr = AttrType::list(AttrType::source(false));
            let coerced = attr.coerce(
                AttrIsConfigurable::Yes,
                &coercion_ctx_listing(PackageListing::testing_files(files)),
                value,
            )?;
            let configured = coerced.configure(&attr, &configuration_ctx())?;
            let mut resolution_ctx = resolution_ctx(&env);
            let resolved =
                configured.resolve_single(PackageLabel::testing(), &mut resolution_ctx)?;

            env.set("res", resolved);
            let success = to_value(&env, &globals, test_content);
            assert!(success.is_none());
            Ok(())
        })
    }

    let content = indoc!(r#"["//sub/dir:foo", "//sub/dir:foo[multiple]", "baz/quz.cpp"]"#);
    let test_content = indoc!(
        r#"
            def assert_eq(a, b):
                if a != b:
                    fail("Expected {} == {}".format(a, b))

            expected = ["default.cpp", "bar1.cpp", "bar2.cpp", "bar3.cpp", "quz.cpp"]
            names = [f.basename for f in res]
            assert_eq(expected, names)
            None
            "#
    );
    resolve_and_test(content, test_content, &["baz/quz.cpp"])?;

    let content = indoc!(r#"["//sub/dir:foo", "//sub/dir:foo[single]", "baz/quz.cpp"]"#);
    let test_content = indoc!(
        r#"
            def assert_eq(a, b):
                if a != b:
                    fail("Expected {} == {}".format(a, b))

            expected = ["default.cpp", "bar1.cpp", "quz.cpp"]
            names = [f.basename for f in res]
            assert_eq(expected, names)
            None
            "#
    );
    resolve_and_test(content, test_content, &["baz/quz.cpp"])?;

    let content = indoc!(r#"["//sub/dir:foo", "//sub/dir:foo[zero]", "baz/quz.cpp"]"#);
    let test_content = indoc!(
        r#"
            def assert_eq(a, b):
                if a != b:
                    fail("Expected {} == {}".format(a, b))

            expected = ["default.cpp", "quz.cpp"]
            names = [f.basename for f in res]
            assert_eq(expected, names)
            None
            "#
    );
    resolve_and_test(content, test_content, &["baz/quz.cpp"])
}

#[test]
fn test_single_source_label_fails_if_multiple_returned() -> buck2_error::Result<()> {
    Module::with_temp_heap(|env| {
        Heap::temp(|heap| {
            let value = heap.alloc("//sub/dir:foo[multiple]");

            let attr = AttrType::source(false);
            let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)?;
            let configured = coerced.configure(&attr, &configuration_ctx())?;
            let mut resolution_ctx = resolution_ctx(&env);
            let err = configured
                .resolve_single(PackageLabel::testing(), &mut resolution_ctx)
                .expect_err("Getting multiple values when expecting a single one should fail");

            assert!(err.to_string().contains("Expected a single artifact"));
            assert!(err.to_string().contains("3 artifacts"));
            Ok(())
        })
    })
}

#[test]
fn test_arg() -> buck2_error::Result<()> {
    Heap::temp(|heap| {
        let value = heap.alloc("$(exe //some:exe) --file=$(location \"//some:location\")");

        let attr = AttrType::arg(false);

        let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)?;
        // Note that targets are canonicalized.
        assert_eq!(
            "\"$(exe root//some:exe) --file=$(location root//some:location)\"",
            coerced.as_display_no_ctx().to_string()
        );
        let configured = coerced.configure(&attr, &configuration_ctx())?;
        assert_eq!(
            format!(
                "\"$(exe root//some:exe ({})) --file=$(location root//some:location ({}))\"",
                configuration_ctx().exec_cfg()?,
                ConfigurationData::testing_new(),
            ),
            configured.as_display_no_ctx().to_string()
        );

        let mut visitor = CoercedDepsCollector::new();
        coerced.traverse(&attr, PackageLabel::testing(), &mut visitor)?;
        let CoercedDepsCollector {
            deps, exec_deps, ..
        } = visitor;
        let deps: Vec<_> = deps.iter().map(|t| t.to_string()).collect();
        let exec_deps: Vec<_> = exec_deps.iter().map(|t| t.to_string()).collect();

        let mut info = ConfiguredAttrInfoForTests::new();
        configured.traverse(PackageLabel::testing(), &mut info)?;

        let expected_deps = vec!["root//some:location"];
        let expected_exec_deps = vec!["root//some:exe"];
        let expected_configured_deps = vec![format!(
            "root//some:location ({})",
            ConfigurationData::testing_new()
        )];
        let expected_configured_exec_deps = vec![format!(
            "root//some:exe ({})",
            configuration_ctx().exec_cfg()?
        )];

        assert_eq!(expected_deps, deps);
        assert_eq!(expected_exec_deps, exec_deps);

        assert_eq!(
            expected_configured_deps,
            info.deps
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
        );
        assert_eq!(
            expected_configured_exec_deps,
            info.execution_deps
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
        );

        Ok(())
    })
}

#[test]
fn test_bool() -> buck2_error::Result<()> {
    Module::with_temp_heap(|env| {
        let globals = GlobalsBuilder::standard().with(register_select).build();

        let value = to_value(
            &env,
            &globals,
            indoc!(
                r#"
                (
                    [True, False]
                    + select({
                        "//some:config": [True],
                        "DEFAULT": [False],
                    })
                    + [True]
                )
                "#
            ),
        );

        let attr = AttrType::list(AttrType::bool());

        let coerced = attr.coerce(AttrIsConfigurable::Yes, &coercion_ctx(), value)?;
        assert_eq!(
            "[True, False]+select({\"root//some:config\": [True], \"DEFAULT\": [False]})+[True]",
            coerced.as_display_no_ctx().to_string()
        );

        let configured = coerced.configure(&attr, &configuration_ctx())?;
        assert_eq!(
            "[True, False, False, True]",
            configured.as_display_no_ctx().to_string()
        );

        let mut ctx = resolution_ctx(&env);
        let resolved = configured.resolve_single(PackageLabel::testing(), &mut ctx)?;
        assert_eq!("[True, False, False, True]", resolved.to_string());

        Ok(())
    })
}

#[test]
fn test_user_placeholders() -> buck2_error::Result<()> {
    Module::with_temp_heap(|env| {
        let globals = GlobalsBuilder::standard()
            .with(register_select)
            .with(register_builtin_providers)
            .build();

        let resolve = |value: &str| {
            let attr = AttrType::arg(false);
            let coerced = attr.coerce(
                AttrIsConfigurable::Yes,
                &coercion_ctx(),
                to_value(&env, &globals, value),
            )?;
            let configured = coerced.configure(&attr, &configuration_ctx())?;
            let mut resolution_ctx = resolution_ctx(&env);
            configured
                .resolve_single(PackageLabel::testing(), &mut resolution_ctx)
                .map(|v| {
                    // TODO: this is way too unnecessarily verbose for a test.
                    let project_fs = ProjectRoot::new(
                        AbsNormPathBuf::try_from(std::env::current_dir().unwrap()).unwrap(),
                    )
                    .unwrap();
                    let fs = ArtifactFs::new(
                        CellResolver::testing_with_name_and_path(
                            CellName::testing_new("cell"),
                            CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new(
                                "cell_path".into(),
                            )),
                        ),
                        BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new(
                            "buck_out/v2".into(),
                        )),
                        project_fs,
                    );
                    let executor_fs = ExecutorFs::new(&fs, PathSeparatorKind::Unix);

                    let mut cli = Vec::<String>::new();
                    let mut ctx = DefaultCommandLineContext::new(&executor_fs);
                    ValueAsCommandLineLike::unpack_value_err(v)
                        .unwrap()
                        .0
                        .add_to_command_line(&mut cli, &mut ctx, &FxHashMap::default())
                        .unwrap();
                    cli.join(" ")
                })
        };

        assert_eq!("clang++", resolve(r#""$(CXX)""#)?);
        assert_eq!(
            "hello",
            resolve(r#""$(user_key //sub/dir:keyed_placeholder)""#)?
        );
        assert_eq!(
            "world",
            resolve(r#""$(key_with_args //sub/dir:keyed_placeholder)""#)?
        );
        assert_eq!(
            "big world",
            resolve(r#""$(key_with_args //sub/dir:keyed_placeholder big)""#)?
        );

        let value = r#""$(CXXabcdef)""#;
        match resolve(value) {
            Ok(..) => panic!("expected error resolving {value}"),
            Err(e) => {
                let expected = "no mapping for CXXabcdef";
                let message = format!("{e:?}");
                assert!(
                    message.contains(expected),
                    "expected `{message}` to contain `{expected}`"
                );
            }
        }

        let value = r#""$(missing_user_key //sub/dir:keyed_placeholder)""#;
        match resolve(value) {
            Ok(..) => panic!("expected error resolving {value}"),
            Err(e) => {
                let expected = "no mapping for missing_user_key";
                let message = format!("{e:?}");
                assert!(
                    message.contains(expected),
                    "expected `{message}` to contain `{expected}`"
                );
            }
        }

        Ok(())
    })
}
