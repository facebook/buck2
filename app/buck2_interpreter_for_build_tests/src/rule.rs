/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::build_defs::register_transitive_set;
use buck2_build_api::interpreter::rule_defs::register_rule_defs;
use buck2_common::result::SharedResult;
use buck2_core::bzl::ImportPath;
use buck2_interpreter::file_loader::LoadedModules;
use buck2_interpreter_for_build::interpreter::testing::Tester;
use buck2_interpreter_for_build::nodes::attr_spec::AttributeSpecExt;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::attrs::spec::AttributeSpec;
use buck2_node::nodes::unconfigured::testing::targets_to_json;
use indoc::indoc;
use serde_json::json;
use starlark::docs::DocFunction;
use starlark::docs::DocItem;
use starlark::docs::DocParam;
use starlark::docs::DocReturn;
use starlark::docs::DocString;
use starlark::docs::DocStringKind;
use starlark::docs::DocType;

fn rule_tester() -> Tester {
    let mut tester = Tester::new().unwrap();
    tester.additional_globals(register_transitive_set);
    tester.additional_globals(register_rule_defs);
    tester
}

#[test]
fn rule_creates_callable() -> anyhow::Result<()> {
    let mut tester = rule_tester();
    tester.run_starlark_test(indoc!(
        r#"
        def impl(ctx):
            pass

        string_attr = attrs.string(default="something", doc = "foo")
        frozen_rule = rule(
            impl=impl,
            attrs={
                "param1": string_attr,
                "param2": attrs.list(string_attr, default=[], doc = "foo"),
            }
        )

        def test():
            assert_eq(None, frozen_rule(name="target_name"))
            assert_eq("frozen_rule()", repr(frozen_rule))
        "#
    ))?;
    Ok(())
}

#[test]
fn freeze_fails_if_not_assigned() {
    let mut tester = rule_tester();
    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
        def impl(ctx):
            pass

        def test():
            string_attr = attrs.string(default="something", doc = "foo")
            nonfrozen_rule = rule(
                impl=impl,
                attrs={
                    "param1": string_attr,
                }
            )
            nonfrozen_rule(name="target_name")
        "#
        ),
        "may only be called after the module is loaded",
    );
}

#[test]
fn rule_disallows_reserved_names() {
    let mut tester = rule_tester();
    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
        def impl(ctx):
            pass

        string_attr = attrs.string(default="something", doc = "foo")
        frozen_rule = rule(
            impl=impl,
            attrs={
                "name": string_attr,
                "param1": string_attr,
            }
        )
        def test():
            frozen_rule(name="target_name")
        "#
        ),
        "not a valid attribute",
    );
}

#[test]
fn rule_unbound() {
    let mut tester = rule_tester();
    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
        def impl(ctx):
            pass
        rules = []
        rules.append(rule(
            impl=impl,
            attrs={}
        ))
        def test():
            pass
        "#
        ),
        "must be assigned to a variable",
    );
}

#[test]
fn udr_is_recorded() -> SharedResult<()> {
    let content = indoc!(
        r#"
        def impl(ctx):
            pass

        string_attr = attrs.string(default="some_default", doc="")
        mandatory_string_attr = attrs.string(default="", doc="")
        dep_attr = attrs.dep(default="//foo:baz")
        src_attr = attrs.source(default="//foo:baz")
        foo_binary = rule(
            impl=impl,
            attrs={
                "optional": string_attr,
                "other_optional": string_attr,
                "mandatory": mandatory_string_attr,
                "dep": dep_attr,
                "src": src_attr,
            },
        )

        def test():
            foo_binary(name="target1", mandatory="m1", optional="o1", dep=":bar", other_optional=None, src="file1.java")
            foo_binary(name="target2", mandatory="m2", other_optional="o1")
        "#
    );
    let mut tester = rule_tester();
    let result = tester.run_starlark_test(content)?;

    let expected = json!({
        "target1": {
            "name": "target1",
            "__type__": "root//some/package/defs.bzl:foo_binary",
            "compatible_with": [],
            "default_target_platform": null,
            "mandatory": "m1",
            "optional": "o1",
            "other_optional": "some_default",
            "dep": "root//some/package:bar",
            "exec_compatible_with": [],
            "src": "root//some/package/file1.java",
            "target_compatible_with": [],
            "tests": [],
            "visibility": [],
        },
        "target2": {
            "name": "target2",
            "__type__": "root//some/package/defs.bzl:foo_binary",
            "compatible_with": [],
            "default_target_platform": null,
            "mandatory": "m2",
            "optional": "some_default",
            "other_optional": "o1",
            "dep": "root//foo:baz",
            "exec_compatible_with": [],
            "src": "root//foo:baz",
            "target_compatible_with": [],
            "tests": [],
            "visibility": [],
        },
    });
    let actual = targets_to_json(
        &result,
        Tester::build_file_path().package(),
        AttrInspectOptions::All,
    )?;
    assert_eq!(expected, actual, "`{:#?}` != `{:#?}`", expected, actual);
    Ok(())
}

#[test]
fn udr_rejects_invalid_parameters() {
    let prefix = indoc!(
        r#"
        def impl(ctx):
            pass

        string_attr = attrs.string(default="some_default")
        mandatory_string_attr = attrs.string(doc="")
        foo_binary = rule(
            impl=impl,
            attrs={"optional":string_attr, "mandatory": mandatory_string_attr},
        )

        def test():
        "#
    );

    let missing_name = r#"    foo_binary(mandatory="s")"#;
    let invalid_name = r#"    foo_binary(name="bad name", mandatory="s")"#;
    let missing_mandatory = r#"    foo_binary(name="t1")"#;
    let wrong_type = r#"    foo_binary(name="t1", mandatory=1)"#;
    let unknown_param = r#"    foo_binary(name="t1", mandatory="m1", unknown="")"#;
    let duplicate_targets = format!(
        "    {}\n    {}",
        r#"foo_binary(name="t1", mandatory="m1")"#, r#"foo_binary(name="t1", mandatory="m2")"#,
    );

    let run = |content: &str, msg: &str| {
        let mut tester = rule_tester();
        tester.run_starlark_test_expecting_error(&format!("{}\n{}", prefix, content), msg);
    };

    run(
        missing_name,
        "Missing parameter `name` for call to foo_binary",
    );
    run(invalid_name, "Invalid target name `bad name`.");
    run(missing_mandatory, "Missing parameter `mandatory`");
    run(wrong_type, "coercing attribute `mandatory`");
    run(unknown_param, "Found `unknown` extra named parameter");
    run(
        &duplicate_targets,
        "Attempted to register target root//some/package:t1 twice",
    );
}

#[test]
fn option_allows_none() -> anyhow::Result<()> {
    let mut tester = rule_tester();
    tester.run_starlark_test_expecting_error(
        "def test():\n attrs.option(attrs.string(), default = 'test')",
        "parameter must be `None` or absent",
    );
    let mut tester = rule_tester();
    tester.run_starlark_test("def test():\n attrs.option(attrs.string(), default = None)")?;
    let mut tester = rule_tester();
    tester.run_starlark_test("def test():\n attrs.option(attrs.string())")?;
    let mut tester = rule_tester();
    tester.run_starlark_test_expecting_error(
        "def test():\n attrs.option(attrs.string(), default = select({'DEFAULT': 'test'}))",
        "parameter must be `None` or absent",
    );
    let mut tester = rule_tester();
    tester.run_starlark_test(
        "def test():\n attrs.option(attrs.string(), default = select({'DEFAULT': None}))",
    )?;
    Ok(())
}

#[test]
fn returns_documentation() -> anyhow::Result<()> {
    let bzl = indoc::indoc!(
        r#"def impl(ctx):
            pass

        any_attr = attrs.any(doc="any docs")
        arg_attr = attrs.arg(default="arg", doc="arg docs")
        bool_attr = attrs.bool(default=True, doc="bool docs")
        string_attr = attrs.string(default="string", doc="string docs")
        default_only_attr = attrs.default_only(attrs.string(default="default_only"), doc="default_only docs")
        dep_attr = attrs.dep(default="//:dep", doc="dep docs")
        dict_attr = attrs.dict(attrs.string(), attrs.bool(), default={"dict": True}, doc="dict docs")
        list_attr = attrs.list(attrs.string(), default=["list"], doc="list docs")
        one_of_attr = attrs.one_of(attrs.bool(), attrs.string(), default="", doc="one_of docs")
        option_attr = attrs.option(attrs.string(), default=None, doc="option docs")
        query_attr = attrs.query(doc="query docs")
        source_attr = attrs.source(default="//:src", doc="source docs")
        tuple_attr = attrs.tuple(attrs.bool(), attrs.string(), default=(True, "some string"), doc="tuple docs")

        doc = """Summary for foo_binary

        Details for foo_binary
        """

        foo_binary = rule(
            impl=impl,
            attrs={
                "any": any_attr,
                "arg": arg_attr,
                "bool": bool_attr,
                "default_only": default_only_attr,
                "dep": dep_attr,
                "dict": dict_attr,
                "list": list_attr,
                "one_of": one_of_attr,
                "option": option_attr,
                "query": query_attr,
                "source": source_attr,
                "string": string_attr,
                "tuple": tuple_attr,
            },
            doc = doc,
        )
        "#
    );

    fn arg(name: &str, type_string: &str, default: Option<&str>) -> DocParam {
        DocParam::Arg {
            name: name.to_owned(),
            docs: DocString::from_docstring(DocStringKind::Starlark, &format!("{} docs", name)),
            typ: Some(DocType {
                raw_type: type_string.to_owned(),
            }),
            default_value: default.map(String::from),
        }
    }

    // Grab the default parameters that are inserted into every rule.
    let empty_spec = AttributeSpec::from(vec![])?;
    let mut params = empty_spec.signature("foo_binary".to_owned()).documentation(
        empty_spec
            .starlark_types()
            .into_iter()
            .enumerate()
            .map(|(i, ds)| (i, DocType { raw_type: ds }))
            .collect(),
        empty_spec.docstrings(),
    );
    params.extend(vec![
        arg("any", "\"\"", None),
        arg("arg", "str.type", Some("_")),
        arg("bool", "bool.type", Some("_")),
        arg("default_only", "default_only", Some("_")),
        arg("dep", "str.type", Some("_")),
        arg("dict", "{str.type: bool.type}", Some("_")),
        arg("list", "[str.type]", Some("_")),
        arg("one_of", "[bool.type, str.type]", Some("_")),
        arg("option", "[None, str.type]", Some("_")),
        arg("query", "str.type", None),
        arg("source", "str.type", Some("_")),
        arg("string", "str.type", Some("_")),
        arg("tuple", "(bool.type, str.type)", Some("_")),
    ]);

    let expected_docs = DocItem::Function(DocFunction {
        docs: DocString::from_docstring(
            DocStringKind::Starlark,
            "Summary for foo_binary\n\nDetails for foo_binary",
        ),
        params,
        ret: DocReturn {
            docs: None,
            typ: Some(DocType {
                raw_type: "None".to_owned(),
            }),
        },
    });

    let tester = rule_tester();
    let res = tester.eval_import(
        &ImportPath::testing_new("root//:defs.bzl"),
        bzl,
        LoadedModules::default(),
    )?;
    let docs = res
        .env()
        .get("foo_binary")
        .expect("foo_binary to exist")
        .value()
        .documentation()
        .unwrap();

    assert_eq!(expected_docs, docs);

    Ok(())
}
