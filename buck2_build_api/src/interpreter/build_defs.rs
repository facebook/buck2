/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter::extra::BuildContext;
use buck2_interpreter::extra::ExtraContext;
use buck2_interpreter::functions::host_info::register_host_info;
use buck2_interpreter::functions::read_config::register_read_config;
use either::Either;
use starlark::collections::SmallMap;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::values::docs::DocString;
use starlark::values::docs::DocStringKind;
use starlark::values::Value;

use crate::interpreter::module_internals::ModuleInternals;
use crate::interpreter::rule_defs::provider::callable::ProviderCallable;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetDefinition;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetOperations;

#[starlark_module]
fn natives(builder: &mut GlobalsBuilder) {
    /// This should be called "target exists", not "rule exists"
    /// (if this should exist at all).
    fn rule_exists(name: &str, eval: &mut Evaluator) -> anyhow::Result<bool> {
        Ok(ModuleInternals::from_context(eval)?.target_exists(name))
    }

    fn provider(
        #[starlark(default = "")] doc: &str,
        fields: Either<Vec<String>, SmallMap<&str, &str>>,
        eval: &mut Evaluator,
    ) -> anyhow::Result<ProviderCallable> {
        let docstring = DocString::from_docstring(DocStringKind::Starlark, doc);
        let path = BuildContext::from_context(eval)?.starlark_path.path();

        let (field_names, field_docs) = match fields {
            Either::Left(f) => {
                let docs = vec![None; f.len()];
                (f, docs)
            }
            Either::Right(fields_with_docs) => {
                let mut field_names = Vec::with_capacity(fields_with_docs.len());
                let mut field_docs = Vec::with_capacity(fields_with_docs.len());
                for (name, docs) in fields_with_docs {
                    field_names.push(name.to_owned());
                    field_docs.push(DocString::from_docstring(DocStringKind::Starlark, docs));
                }
                (field_names, field_docs)
            }
        };
        Ok(ProviderCallable::new(
            path.into_owned(),
            docstring,
            field_docs,
            field_names,
        ))
    }

    fn transitive_set<'v>(
        args_projections: Option<SmallMap<String, Value<'v>>>,
        reductions: Option<SmallMap<String, Value<'v>>>,
        eval: &mut Evaluator,
    ) -> anyhow::Result<TransitiveSetDefinition<'v>> {
        let build_context = BuildContext::from_context(eval)?;
        Ok(TransitiveSetDefinition::new(
            build_context.starlark_path.id().clone(),
            TransitiveSetOperations {
                args_projections: args_projections.unwrap_or_default(),
                reductions: reductions.unwrap_or_default(),
            },
        ))
    }

    fn implicit_package_symbol<'v>(
        name: &str,
        default: Option<Value<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let internals = ModuleInternals::from_context(eval)?;
        match internals.get_package_implicit(name) {
            None => Ok(default.unwrap_or_else(Value::new_none)),
            Some(v) => {
                // FIXME(ndmitchell): Document why this is safe
                Ok(unsafe { v.unchecked_frozen_value().to_value() })
            }
        }
    }
}

pub(crate) fn register_natives(builder: &mut GlobalsBuilder) {
    natives(builder);
    register_host_info(builder);
    register_read_config(builder);
}

#[cfg(test)]
mod tests {
    use buck2_common::package_listing::listing::testing::PackageListingExt;
    use buck2_common::package_listing::listing::PackageListing;
    use buck2_core::build_file_path::BuildFilePath;
    use buck2_core::bzl::ImportPath;
    use buck2_interpreter::file_loader::LoadedModules;
    use indoc::indoc;
    use serde_json::json;

    use crate::interpreter::testing::buildfile;
    use crate::interpreter::testing::cells;
    use crate::interpreter::testing::import;
    use crate::interpreter::testing::run_simple_starlark_test;
    use crate::interpreter::testing::Tester;
    use crate::nodes::unconfigured::testing::targets_to_json;
    use crate::nodes::unconfigured::AttrInspectOptions;

    #[test]
    fn prelude_is_included() -> anyhow::Result<()> {
        let mut tester = Tester::new()?;
        let prelude_path = ImportPath::unchecked_new("root", "prelude", "prelude.bzl");
        tester.set_prelude(prelude_path.clone());

        let prelude =
            tester.eval_import(&prelude_path, "some_var = 1", LoadedModules::default())?;
        let mut loaded_modules = LoadedModules::default();
        loaded_modules
            .map
            .insert(prelude_path.id().clone(), prelude);

        // The prelude should be included in build files, and in .bzl files that are not in the
        // prelude's package
        let build_file = BuildFilePath::unchecked_new("root", "prelude", "TARGETS.v2");
        assert!(
            tester
                .eval_build_file_with_loaded_modules(
                    &build_file,
                    "other_var = some_var",
                    loaded_modules.clone(),
                    PackageListing::testing_empty()
                )
                .is_ok(),
            "build files in the prelude package should have access to the prelude"
        );

        let import = ImportPath::unchecked_new("root", "not_prelude", "sibling.bzl");
        assert!(
            tester
                .eval_import(&import, "other_var = some_var", loaded_modules.clone())
                .is_ok(),
            ".bzl files not in the prelude package should have access to the prelude"
        );

        let import = ImportPath::unchecked_new("root", "prelude", "defs.bzl");
        assert!(
            tester
                .eval_import(&import, "other_var = some_var", loaded_modules)
                .is_err(),
            "bzl files in the prelude package should NOT have access to the prelude"
        );

        Ok(())
    }

    #[test]
    fn test_package_import() -> anyhow::Result<()> {
        let mut tester = Tester::with_cells(cells(Some(indoc!(
            r#"
            [buildfile]
                package_includes = src=>//include.bzl::func_alias=some_func
        "#
        )))?)?;

        let import_path = import("root", "", "include.bzl");
        tester.add_import(
            &import_path,
            indoc!(
                r#"
            def _impl(ctx):
                pass
            export_file = rule(implementation=_impl, attrs = {})

            def some_func(name):
                export_file(name = name)
        "#
            ),
        )?;

        let build_path = buildfile("root", "src/package");
        let eval_result = tester.eval_build_file(
            &build_path,
            indoc!(
                r#"
                implicit_package_symbol("func_alias")(
                    implicit_package_symbol("missing", "DEFAULT")
                )
                "#
            ),
            PackageListing::testing_files(&["file1.java", "file2.java"]),
        )?;
        assert_eq!(build_path.package(), eval_result.package());
        assert_eq!(
            json!({
                    "DEFAULT": {
                        "__type__": "root//include.bzl:export_file",
                        "compatible_with": [],
                        "default_target_platform": null,
                        "exec_compatible_with": [],
                        "name": "DEFAULT",
                        "target_compatible_with": [],
                        "tests": [],
                        "visibility": [],
                    },
            }),
            targets_to_json(eval_result.targets(), AttrInspectOptions::All)?
        );
        Ok(())
    }

    #[test]
    fn test_provider() -> anyhow::Result<()> {
        // TODO: test restricting field names
        run_simple_starlark_test(indoc!(
            r#"
            SomeInfo = provider(fields=["x", "y"])
            SomeOtherInfo = provider(fields={"x": "docs for x", "y": "docs for y"})
            DocInfo = provider(doc="Some docs", fields=["x", "y"])

            def test():
                instance = SomeInfo(x = 2, y = True)
                assert_eq(2, instance.x)
                assert_eq(True, instance.y)
                assert_eq(SomeInfo(x = 2, y = True), instance)

                instance = SomeOtherInfo(x = 2, y = True)
                assert_eq(2, instance.x)
                assert_eq(True, instance.y)
                assert_eq(SomeOtherInfo(x = 2, y = True), instance)

                instance = DocInfo(x = 2, y = True)
                assert_eq(2, instance.x)
                assert_eq(True, instance.y)
                assert_eq(DocInfo(x = 2, y = True), instance)
            "#
        ))
    }

    #[test]
    fn test_read_config() -> anyhow::Result<()> {
        run_simple_starlark_test(indoc!(
            r#"
            def test():
                assert_eq("default", read_config("missing_section", "key", "default"))
                assert_eq("default", read_config("section", "missing_key", "default"))
                assert_eq(1, read_config("section", "missing_key", 1))
                assert_eq(None, read_config("section", "missing_key", None))

                assert_eq("value", read_config("section", "key", "default"))
                assert_eq("value", read_config("section", "key"))

                assert_eq("1", read_config("section", "other"))
                assert_eq("hello world!", read_config("section", "multiline"))
                assert_eq("okay", read_config("config", "key"))
            "#
        ))?;
        Ok(())
    }

    #[test]
    fn test_host_info() -> anyhow::Result<()> {
        run_simple_starlark_test(indoc!(
            r#"
            def test():
                assert_eq(True, host_info().os.is_linux)
                assert_eq(False, host_info().os.is_macos)
                assert_eq(False, host_info().os.is_macos)

                assert_eq(True, host_info().arch.is_x86_64)
                assert_eq(False, host_info().arch.is_arm)
                assert_eq(False, host_info().arch.is_mipsel64)

            "#
        ))?;
        Ok(())
    }

    #[test]
    fn test_buck_v2() -> anyhow::Result<()> {
        run_simple_starlark_test(indoc!(
            r#"
            def test():
                assert_eq(True, hasattr(host_info(), "buck2"))
                assert_eq(False, hasattr(host_info(), "buck1"))
        "#
        ))
    }

    #[test]
    fn eval() -> anyhow::Result<()> {
        run_simple_starlark_test(indoc!(
            r#"
            def _impl(ctx):
                pass
            export_file = rule(implementation=_impl, attrs = {})

            def test():
                assert_eq("some/package", __internal__.package_name())
                assert_eq("@root", __internal__.repository_name())

                assert_eq(package_name(), __internal__.package_name())
                assert_eq(repository_name(), __internal__.repository_name())

                assert_eq(package_name(), get_base_path())

                export_file(name = "rule_name")
                assert_eq(True, rule_exists("rule_name"))
                assert_eq(False, rule_exists("not_rule_name"))

                print("some message")
                print("multiple", "strings")
            "#
        ))
    }
}
