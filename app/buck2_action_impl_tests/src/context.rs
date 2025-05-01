/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::analysis::registry::AnalysisRegistry;
use buck2_build_api::interpreter::rule_defs::context::AnalysisContext;
use buck2_build_api::interpreter::rule_defs::plugins::AnalysisPlugins;
use buck2_build_api::interpreter::rule_defs::register_rule_defs;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::deferred::base_deferred_key::BaseDeferredKey;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::target::label::label::TargetLabel;
use buck2_execute::digest_config::DigestConfig;
use buck2_interpreter::file_type::StarlarkFileType;
use buck2_interpreter::from_freeze::from_freeze_error;
use dupe::Dupe;
use indoc::indoc;
use maplit::hashmap;
use starlark::collections::SmallMap;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::eval::ReturnFileLoader;
use starlark::syntax::AstModule;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::structs::AllocStruct;

fn run_ctx_test(
    content: &str,
    result_handler: impl FnOnce(starlark::Result<Value>) -> buck2_error::Result<()>,
) -> buck2_error::Result<()> {
    let func_mod = Module::new();
    let globals = GlobalsBuilder::standard().with(register_rule_defs).build();
    let prelude = indoc!(
        r#"
         def assert_eq(a, b):
             if a != b:
                 fail("Expected {}, got {}".format(a, b))
         "#
    );
    let full_content = format!("{}\n{}", prelude, content);

    {
        let mut eval = Evaluator::new(&func_mod);
        let ast = AstModule::parse(
            "foo.bzl",
            full_content,
            &StarlarkFileType::Bzl.dialect(false),
        )
        .unwrap();
        eval.eval_module(ast, &globals).unwrap();
    };
    let frozen_func_mod = func_mod.freeze().map_err(from_freeze_error)?;
    let test_function = frozen_func_mod.get("test").unwrap();

    let modules = hashmap!["func_mod" => &frozen_func_mod];

    let env = Module::new();
    let file_loader = ReturnFileLoader { modules: &modules };
    let test_function = test_function.owned_value(env.frozen_heap());
    let mut eval = Evaluator::new(&env);
    eval.set_loader(&file_loader);
    let label = TargetLabel::testing_parse("root//foo/bar:some_name")
        .configure(ConfigurationData::testing_new());
    let registry = AnalysisRegistry::new_from_owner(
        BaseDeferredKey::TargetLabel(label.dupe()),
        ExecutionPlatformResolution::unspecified(),
    )?;
    let attributes = eval
        .heap()
        .alloc_typed_unchecked(AllocStruct([("name", "some_name")]))
        .cast();
    let plugins = eval
        .heap()
        .alloc_typed(AnalysisPlugins::new(SmallMap::new()))
        .into();

    let ctx = eval.heap().alloc(AnalysisContext::prepare(
        eval.heap(),
        Some(attributes),
        Some(label),
        Some(plugins),
        registry,
        DigestConfig::testing_default(),
    ));

    let returned = eval.eval_function(test_function, &[ctx], &[]);
    result_handler(returned)
}

#[test]
fn ctx_instantiates() -> buck2_error::Result<()> {
    let content = indoc!(
        r#"
         def test(ctx):
             assert_eq("foo/bar", ctx.label.package)
             assert_eq("some_name", ctx.label.name)
             assert_eq(None, ctx.label.sub_target)
             return ctx.attrs.name
         "#
    );
    run_ctx_test(content, |ret| {
        assert_eq!("some_name", ret.unwrap().unpack_str().unwrap());
        Ok(())
    })
}

#[test]
fn declare_output_declares_outputs() -> buck2_error::Result<()> {
    let content = indoc!(
        r#"
         def test(c):
             out = c.actions.declare_output("foo/bar.cpp")
             return (out.basename, out.short_path)
         "#
    );

    run_ctx_test(content, |ret| {
        let a = <(&str, &str)>::unpack_value(ret.unwrap()).unwrap().unwrap();
        assert_eq!("bar.cpp", a.0);
        assert_eq!("foo/bar.cpp", a.1);
        Ok(())
    })
}

#[test]
fn declare_output_with_prefix() -> buck2_error::Result<()> {
    let content = indoc!(
        r#"
         def test(c):
             out = c.actions.declare_output("out/test", "foo/bar.cpp")
             return (out.basename, out.short_path)
         "#
    );

    run_ctx_test(content, |ret| {
        let a = <(&str, &str)>::unpack_value(ret.unwrap()).unwrap().unwrap();
        assert_eq!("bar.cpp", a.0);
        assert_eq!("foo/bar.cpp", a.1);
        Ok(())
    })
}

#[test]
fn declare_output_dot() -> buck2_error::Result<()> {
    let content = indoc!(
        r#"
         def test(c):
             return c.actions.declare_output("magic", ".")
         "#
    );

    let expect = "expected a normalized path";
    run_ctx_test(content, |ret| match ret {
        Err(e) if e.to_string().contains(expect) => Ok(()),
        _ => panic!(
            "Expected a specific failure containing `{}`, got {:?}",
            expect, ret
        ),
    })
}

#[test]
fn declare_output_dot_bad() -> buck2_error::Result<()> {
    let content = indoc!(
        r#"
         def test(c):
             return c.actions.declare_output("..")
         "#
    );

    let expect = "expected a normalized path";
    run_ctx_test(content, |ret| match ret {
        Err(e) if e.to_string().contains(expect) => Ok(()),
        _ => panic!(
            "Expected a specific failure containing `{}`, got {:?}",
            expect, ret
        ),
    })
}

#[test]
fn declare_output_dotdot() -> buck2_error::Result<()> {
    let content = indoc!(
        r#"
         def test(c):
             return c.actions.declare_output("foo/..")
         "#
    );

    let expect = "expected a normalized path";
    run_ctx_test(content, |ret| match ret {
        Err(e) if e.to_string().contains(expect) => Ok(()),
        _ => panic!(
            "Expected a specific failure containing `{}`, got {:?}",
            expect, ret
        ),
    })
}

#[test]
fn declare_output_require_bound() -> buck2_error::Result<()> {
    let content = indoc!(
        r#"
         def test(c):
             a = c.actions.declare_output("a")
             b = c.actions.declare_output("b")
             c.actions.run([a, b.as_output()], category = "test_category")
         "#
    );

    let expect = "must be bound by now";
    run_ctx_test(content, |ret| match ret {
        Err(e) if e.to_string().contains(expect) => Ok(()),
        _ => panic!(
            "Expected a specific failure containing `{}`, got {:?}",
            expect, ret
        ),
    })
}
