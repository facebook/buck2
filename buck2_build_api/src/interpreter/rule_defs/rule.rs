/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;
use std::fmt;
use std::fmt::Display;
use std::iter::Iterator;
use std::sync::Arc;

use buck2_core::bzl::ImportPath;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_interpreter::extra::BuildContext;
use buck2_interpreter::extra::ExtraContext;
use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use gazebo::dupe::Dupe;
use gazebo::prelude::*;
use itertools::Itertools;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Arguments;
use starlark::eval::Evaluator;
use starlark::eval::ParametersSpec;
use starlark::starlark_type;
use starlark::values::dict::DictOf;
use starlark::values::docs;
use starlark::values::docs::DocItem;
use starlark::values::docs::DocStringKind;
use starlark::values::AllocValue;
use starlark::values::Freeze;
use starlark::values::Freezer;
use starlark::values::FrozenValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;

use crate::interpreter::module_internals::ModuleInternals;
use crate::interpreter::rule_defs::attr::Attribute;
use crate::interpreter::rule_defs::transition::starlark::Transition;
use crate::nodes::attr_spec::AttributeSpec;
use crate::nodes::unconfigured::TargetNode;
use crate::nodes::StarlarkRuleType;

pub static NAME_ATTRIBUTE_FIELD: &str = "name";

/// The callable that's returned from a `rule()` call. Once frozen, and called, it adds targets'
/// parameters to the context
#[derive(Debug, Clone, ProvidesStaticType, Trace, NoSerialize)]
struct RuleCallable<'v> {
    /// The import path that contains the rule() call; stored here so we can retrieve extra
    /// information during `export_as()`
    #[trace(unsafe_ignore)]
    import_path: ImportPath,
    /// Once exported, the `import_path` and `name` of the callable. Used in DICE to retrieve rule
    /// implementations
    #[trace(unsafe_ignore)]
    id: RefCell<Option<StarlarkRuleType>>,
    /// The implementation function for this rule. Must be callable and take a
    /// ctx
    implementation: Value<'v>,
    // Field Name -> Attribute
    #[trace(unsafe_ignore)]
    attributes: Arc<AttributeSpec>,
    /// When specified, this transition will be applied to the target before configuring it.
    #[trace(unsafe_ignore)]
    cfg: Option<Arc<TransitionId>>,
    /// This rule is configuration rule, which means it is usable in configuration context.
    is_configuration_rule: bool,
    /// The raw docstring for this rule
    docs: Option<String>,
    /// When evaluating rule function, take only the `name` argument, ignore the others.
    ignore_attrs_for_profiling: bool,
}

impl<'v> Display for RuleCallable<'v> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.id.borrow() {
            Some(id) => write!(f, "{}()", id.name),
            None => write!(f, "<unbound rule>"),
        }
    }
}

/// Errors around rule declaration, instantiation, validation, etc
#[derive(Debug, thiserror::Error)]
pub enum RuleError {
    #[error("The output of rule() may only be called after the module is loaded")]
    RuleCalledBeforeFreezing,
    #[error("`{0}` is not a valid attribute name")]
    InvalidParameterName(String),
    #[error("Parameter `{0}` had no value provided, but it is mandatory")]
    MissingMandatoryParameter(String),
    #[error("Rule defined in `{0}` must be assigned to a variable, e.g. `my_rule = rule(...)`")]
    RuleNotAssigned(ImportPath),
}

impl<'v> AllocValue<'v> for RuleCallable<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex(self)
    }
}

impl<'v> StarlarkValue<'v> for RuleCallable<'v> {
    starlark_type!("rule");

    fn export_as(&self, variable_name: &str, _eval: &mut Evaluator<'v, '_>) {
        *self.id.borrow_mut() = Some(StarlarkRuleType {
            import_path: self.import_path.clone(),
            name: variable_name.to_owned(),
        });
    }

    fn invoke(
        &self,
        _me: Value<'v>,
        _args: &Arguments<'v, '_>,
        _eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        Err(RuleError::RuleCalledBeforeFreezing.into())
    }

    fn documentation(&self) -> Option<DocItem> {
        let name = self
            .id
            .borrow()
            .as_ref()
            .map_or_else(|| "unbound_rule".to_owned(), |rt| rt.name.clone());
        // TODO(nmj): These return 'None' for default values right now. It's going to take some
        //            refactoring to get that pulled out of the attributespec
        let parameters_spec = self.attributes.signature(name);

        let parameter_types = self
            .attributes
            .starlark_types()
            .into_iter()
            .enumerate()
            .map(|(i, t)| (i, docs::Type { raw_type: t }))
            .collect();
        let parameter_docs = self.attributes.docstrings();
        let function_docs = docs::Function::from_docstring(
            DocStringKind::Starlark,
            |_| parameters_spec.documentation(parameter_types, parameter_docs),
            Some(docs::Type {
                raw_type: Value::new_none().to_string(),
            }),
            self.docs.as_deref(),
        );

        Some(DocItem::Function(function_docs))
    }
}

impl<'v> Freeze for RuleCallable<'v> {
    type Frozen = FrozenRuleCallable;
    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        let frozen_impl = self.implementation.freeze(freezer)?;
        let rule_docs = self.documentation();
        let id = match self.id.into_inner() {
            Some(x) => x,
            None => return Err(RuleError::RuleNotAssigned(self.import_path).into()),
        };
        let rule_type = Arc::new(id);
        let rule_name = rule_type.name.to_owned();
        let signature = self.attributes.signature(rule_name).freeze(freezer)?;

        Ok(FrozenRuleCallable {
            implementation: frozen_impl,
            signature,
            rule_type,
            attributes: self.attributes,
            cfg: self.cfg,
            is_configuration_rule: self.is_configuration_rule,
            rule_docs,
            ignore_attrs_for_profiling: self.ignore_attrs_for_profiling,
        })
    }
}

#[derive(Debug, Display, ProvidesStaticType, NoSerialize)]
#[display(fmt = "{}()", "rule_type.name")]
pub struct FrozenRuleCallable {
    implementation: FrozenValue,
    signature: ParametersSpec<FrozenValue>,
    rule_type: Arc<StarlarkRuleType>,
    attributes: Arc<AttributeSpec>,
    cfg: Option<Arc<TransitionId>>,
    is_configuration_rule: bool,
    rule_docs: Option<DocItem>,
    ignore_attrs_for_profiling: bool,
}
starlark_simple_value!(FrozenRuleCallable);

impl FrozenRuleCallable {
    fn record_target<'v>(
        eval: &mut Evaluator<'v, '_>,
        target_node: TargetNode,
    ) -> anyhow::Result<()> {
        let internals = ModuleInternals::from_context(eval)?;
        internals.recorder().record(target_node)?;
        Ok(())
    }

    pub fn get_impl(&self) -> FrozenValue {
        self.implementation
    }
}

impl<'v> StarlarkValue<'v> for FrozenRuleCallable {
    starlark_type!("rule");

    fn invoke(
        &self,
        _me: Value<'v>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let record_target_call_stack =
            ModuleInternals::from_context(eval)?.record_target_call_stacks();
        let call_stack = if record_target_call_stack {
            Some(box eval.call_stack())
        } else {
            None
        };
        let param_count = args.len()?;
        self.signature.parser(args, eval, |param_parser, eval| {
            // The body of the callable returned by `rule()`.
            // Records the target in this package's `TargetMap`.
            let internals = ModuleInternals::from_context(eval)?;
            let buildfile_path = internals.buildfile_path().dupe();
            let target_node = TargetNode::from_params(
                eval,
                self.cfg.dupe(),
                param_parser,
                param_count,
                self.ignore_attrs_for_profiling,
                self.rule_type.dupe(),
                buildfile_path,
                self.is_configuration_rule,
                self.attributes.dupe(),
                call_stack,
            )?;
            FrozenRuleCallable::record_target(eval, target_node)?;
            Ok(Value::new_none())
        })
    }

    fn documentation(&self) -> Option<DocItem> {
        self.rule_docs.clone()
    }
}

#[starlark_module]
pub fn register_rule_function(builder: &mut GlobalsBuilder) {
    fn rule<'v>(
        implementation: Value<'v>,
        attrs: DictOf<'v, &'v str, &'v Attribute>,
        #[starlark(require = named)] cfg: Option<Value>,
        #[starlark(require = named, default = "")] doc: &str,
        #[allow(unused_variables)]
        #[starlark(require = named, default = false)]
        allow_unknown_attrs: bool,
        #[starlark(require = named, default = false)] is_configuration_rule: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        // TODO(nmj): Add default attributes in here like 'name', 'visibility', etc
        // TODO(nmj): Verify that names are valid. This is technically handled by the Params
        //                 objects, but will blow up in a friendlier way here.
        // TODO(cjhopman): Implement allow_unknown_attrs.

        let build_context = BuildContext::from_context(eval)?;
        let bzl_path = (*build_context
            .starlark_path
            .unpack_load_file()
            .ok_or_else(|| anyhow::anyhow!("`rule` can only be declared in bzl files"))?)
        .clone();
        let sorted_validated_attrs = attrs
            .to_dict()
            .into_iter()
            .sorted_by(|(k1, _), (k2, _)| Ord::cmp(k1, k2))
            .map(|(name, value)| {
                if name == NAME_ATTRIBUTE_FIELD {
                    Err(RuleError::InvalidParameterName(NAME_ATTRIBUTE_FIELD.to_owned()).into())
                } else {
                    Ok((name.to_owned(), value.clone()))
                }
            })
            .collect::<anyhow::Result<Vec<(String, Attribute)>>>()?;

        let cfg = cfg.try_map(|x| Transition::id_from_value(*x))?;

        Ok(eval.heap().alloc(RuleCallable {
            import_path: bzl_path,
            id: RefCell::new(None),
            implementation,
            attributes: Arc::new(AttributeSpec::from(sorted_validated_attrs)?),
            cfg,
            is_configuration_rule,
            docs: Some(doc.to_owned()),
            ignore_attrs_for_profiling: build_context.ignore_attrs_for_profiling,
        }))
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::result::SharedResult;
    use buck2_interpreter::file_loader::LoadedModules;
    use indoc::indoc;
    use serde_json::json;
    use starlark::values::docs;
    use starlark::values::docs::DocItem;
    use starlark::values::docs::DocString;
    use starlark::values::docs::DocStringKind;

    use crate::interpreter::testing::import;
    use crate::interpreter::testing::run_simple_starlark_test;
    use crate::interpreter::testing::run_starlark_bzl_test_expecting_error;
    use crate::interpreter::testing::run_starlark_test;
    use crate::interpreter::testing::run_starlark_test_expecting_error;
    use crate::interpreter::testing::Tester;
    use crate::nodes::attr_spec::AttributeSpec;
    use crate::nodes::unconfigured::testing::targets_to_json;

    #[test]
    fn rule_creates_callable() -> anyhow::Result<()> {
        run_simple_starlark_test(indoc!(
            r#"
            def impl(ctx):
                pass

            string_attr = attr.string(default="something", doc = "foo")
            frozen_rule = rule(
                implementation=impl,
                attrs={
                    "param1": string_attr,
                    "param2": attr.list(string_attr, default=[], doc = "foo"),
                }
            )

            def test():
                assert_eq(None, frozen_rule(name="target_name"))
                assert_eq("frozen_rule()", repr(frozen_rule))
            "#
        ))
    }

    #[test]
    fn freeze_fails_if_not_assigned() {
        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def impl(ctx):
                pass

            def test():
                string_attr = attr.string(default="something", doc = "foo")
                nonfrozen_rule = rule(
                    implementation=impl,
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
        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def impl(ctx):
                pass

            string_attr = attr.string(default="something", doc = "foo")
            frozen_rule = rule(
                implementation=impl,
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
        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def impl(ctx):
                pass
            rules = []
            rules.append(rule(
                implementation=impl,
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
        let result = run_starlark_test(indoc!(
            r#"
            def impl(ctx):
                pass

            string_attr = attr.string(default="some_default", doc="")
            mandatory_string_attr = attr.string(default="", doc="")
            dep_attr = attr.dep(default="//foo:baz")
            src_attr = attr.source(default="//foo:baz")
            foo_binary = rule(
                implementation=impl,
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
        ))?;

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
        let actual = targets_to_json(&result)?;
        assert_eq!(expected, actual, "`{:#?}` != `{:#?}`", expected, actual);
        Ok(())
    }

    #[test]
    fn udr_rejects_invalid_parameters() {
        let prefix = indoc!(
            r#"
            def impl(ctx):
                pass

            string_attr = attr.string(default="some_default")
            mandatory_string_attr = attr.string(doc="")
            foo_binary = rule(
                implementation=impl,
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
            run_starlark_test_expecting_error(&format!("{}\n{}", prefix, content), msg);
        };

        run(
            missing_name,
            "Missing parameter `name` for call to foo_binary",
        );
        run(invalid_name, "Invalid target name `bad name`.");
        run(missing_mandatory, "Missing parameter `mandatory`");
        run(wrong_type, "coercing attribute `mandatory`");
        run(unknown_param, "Found unknown extra named parameter");
        run(
            &duplicate_targets,
            "Attempted to register target root//some/package:t1 twice",
        );
    }

    #[test]
    fn option_allows_none() -> anyhow::Result<()> {
        run_starlark_test_expecting_error(
            "def test():\n attr.option(attr.string(), default = 'test')",
            "parameter must be `None` or absent",
        );
        run_starlark_test("def test():\n attr.option(attr.string(), default = None)")?;
        run_starlark_test("def test():\n attr.option(attr.string())")?;
        run_starlark_test_expecting_error(
            "def test():\n attr.option(attr.string(), default = select({'DEFAULT': 'test'}))",
            "parameter must be `None` or absent",
        );
        run_starlark_test(
            "def test():\n attr.option(attr.string(), default = select({'DEFAULT': None}))",
        )?;
        Ok(())
    }

    #[test]
    fn returns_documentation() -> anyhow::Result<()> {
        let bzl = indoc::indoc!(
            r#"def impl(ctx):
                pass

            any_attr = attr.any(doc="any docs")
            arg_attr = attr.arg(default="arg", doc="arg docs")
            bool_attr = attr.bool(default=True, doc="bool docs")
            string_attr = attr.string(default="string", doc="string docs")
            default_only_attr = attr.default_only(attr.string(default="default_only"), doc="default_only docs")
            dep_attr = attr.dep(default="//:dep", doc="dep docs")
            dict_attr = attr.dict(attr.string(), attr.bool(), default={"dict": True}, doc="dict docs")
            list_attr = attr.list(attr.string(), default=["list"], doc="list docs")
            one_of_attr = attr.one_of(attr.bool(), attr.string(), default="", doc="one_of docs")
            option_attr = attr.option(attr.string(), default=None, doc="option docs")
            query_attr = attr.query(doc="query docs")
            source_attr = attr.source(default="//:src", doc="source docs")
            tuple_attr = attr.tuple(attr.bool(), attr.string(), default=(True, "some string"), doc="tuple docs")

            doc = """Summary for foo_binary

            Details for foo_binary
            """

            foo_binary = rule(
                implementation=impl,
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

        fn arg(name: &str, type_string: &str, default: Option<&str>) -> docs::Param {
            docs::Param::Arg {
                name: name.to_owned(),
                docs: DocString::from_docstring(DocStringKind::Starlark, &format!("{} docs", name)),
                typ: Some(docs::Type {
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
                .map(|(i, ds)| (i, docs::Type { raw_type: ds }))
                .collect(),
            empty_spec.docstrings(),
        );
        params.extend(vec![
            arg("any", "\"\"", Some("None")),
            arg("arg", "str.type", Some("None")),
            arg("bool", "bool.type", Some("None")),
            arg("default_only", "default_only", Some("None")),
            arg("dep", "str.type", Some("None")),
            arg("dict", "{str.type: bool.type}", Some("None")),
            arg("list", "[str.type]", Some("None")),
            arg("one_of", "[bool.type, str.type]", Some("None")),
            arg("option", "[None, str.type]", Some("None")),
            arg("query", "str.type", None),
            arg("source", "str.type", Some("None")),
            arg("string", "str.type", Some("None")),
            arg("tuple", "(bool.type, str.type)", Some("None")),
        ]);

        let expected_docs = DocItem::Function(docs::Function {
            docs: DocString::from_docstring(
                DocStringKind::Starlark,
                "Summary for foo_binary\n\nDetails for foo_binary",
            ),
            params,
            ret: docs::Return {
                docs: None,
                typ: Some(docs::Type {
                    raw_type: "None".to_owned(),
                }),
            },
        });

        let tester = Tester::new()?;
        let res = tester.eval_import(
            &import("root", "", "defs.bzl"),
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
}
