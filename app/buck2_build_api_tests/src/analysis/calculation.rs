/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashMap;
use std::sync::Arc;

use buck2_build_api::actions::execute::dice_data::set_fallback_executor_config;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::build::detailed_aggregated_metrics::dice::SetDetailedAggregatedMetricsEventHandler;
use buck2_build_api::interpreter::rule_defs::provider::builtin::default_info::DefaultInfoCallable;
use buck2_build_api::interpreter::rule_defs::provider::callable::register_provider;
use buck2_build_api::interpreter::rule_defs::provider::registration::register_builtin_providers;
use buck2_build_api::keep_going::HasKeepGoing;
use buck2_build_api::spawner::BuckSpawner;
use buck2_common::dice::data::testing::SetTestingIoProvider;
use buck2_common::legacy_configs::configs::LegacyBuckConfig;
use buck2_common::package_listing::listing::PackageListing;
use buck2_common::package_listing::listing::testing::PackageListingExt;
use buck2_configured::execution::ExecutionPlatformsKey;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellResolver;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path_with_allowed_relative_dir::CellPathWithAllowedRelativeDir;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::cells::name::CellName;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::execution_types::executor_config::CommandExecutorConfig;
use buck2_core::fs::project::ProjectRootTemp;
use buck2_core::package::PackageLabel;
use buck2_core::provider::id::ProviderId;
use buck2_core::provider::id::testing::ProviderIdExt;
use buck2_core::target::label::interner::ConcurrentTargetLabelInterner;
use buck2_core::target::label::label::TargetLabel;
use buck2_events::dispatch::EventDispatcher;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::digest_config::SetDigestConfig;
use buck2_interpreter::dice::starlark_debug::SetStarlarkDebugger;
use buck2_interpreter::extra::InterpreterHostArchitecture;
use buck2_interpreter::extra::InterpreterHostPlatform;
use buck2_interpreter::file_loader::LoadedModules;
use buck2_interpreter::paths::module::OwnedStarlarkModulePath;
use buck2_interpreter_for_build::interpreter::calculation::InterpreterResultsKey;
use buck2_interpreter_for_build::interpreter::configuror::BuildInterpreterConfiguror;
use buck2_interpreter_for_build::interpreter::dice_calculation_delegate::testing::EvalImportKey;
use buck2_interpreter_for_build::interpreter::interpreter_setup::setup_interpreter_basic;
use buck2_interpreter_for_build::interpreter::testing::Tester;
use buck2_interpreter_for_build::rule::register_rule_function;
use dice::UserComputationData;
use dice::testing::DiceBuilder;
use dupe::Dupe;
use indoc::indoc;
use itertools::Itertools;
use starlark_map::ordered_map::OrderedMap;

#[tokio::test]
async fn test_analysis_calculation() -> anyhow::Result<()> {
    let bzlfile = ImportPath::testing_new("cell//pkg:foo.bzl");
    let resolver = CellResolver::testing_with_names_and_paths(&[
        (
            CellName::testing_new("root"),
            CellRootPathBuf::testing_new(""),
        ),
        (
            CellName::testing_new("cell"),
            CellRootPathBuf::testing_new("cell"),
        ),
    ]);
    let mut interpreter = Tester::with_cells((
        CellAliasResolver::new(CellName::testing_new("cell"), HashMap::new())?,
        resolver.dupe(),
        LegacyBuckConfig::empty(),
        CellPathWithAllowedRelativeDir::new(CellPath::testing_new("cell//pkg"), None),
    ))?;
    interpreter.additional_globals(register_rule_function);
    interpreter.additional_globals(register_provider);
    interpreter.additional_globals(register_builtin_providers);
    let module = interpreter
        .eval_import(
            &bzlfile,
            indoc!(r#"
                            FooInfo = provider(fields=["str"])

                            def impl(ctx):
                                str = ""
                                if ctx.attrs.dep:
                                    str = ctx.attrs.dep[FooInfo].str
                                return [FooInfo(str=(str + ctx.attrs.str)), DefaultInfo()]
                            foo_binary = rule(impl=impl, attrs={"dep": attrs.option(attrs.dep(providers=[FooInfo]), default = None), "str": attrs.string()})
                        "#),
            LoadedModules::default(),
        )?;

    let buildfile = BuildFilePath::testing_new("cell//pkg:BUCK");
    let eval_res = interpreter.eval_build_file_with_loaded_modules(
        &buildfile,
        indoc!(
            r#"
                    load(":foo.bzl", "FooInfo", "foo_binary")

                    foo_binary(
                        name = "rule1",
                        str = "a",
                        dep = ":rule2",
                    )
                    foo_binary(
                        name = "rule2",
                        str = "b",
                        dep = ":rule3",
                    )
                    foo_binary(
                        name = "rule3",
                        str = "c",
                        dep = None,
                    )
                "#
        ),
        LoadedModules {
            map: OrderedMap::from_iter([(
                OwnedStarlarkModulePath::LoadFile(bzlfile.clone()),
                module.dupe(),
            )]),
        },
        PackageListing::testing_new(&[], "BUCK"),
    )?;

    let fs = ProjectRootTemp::new()?;
    let mut dice = DiceBuilder::new()
        .mock_and_return(
            EvalImportKey(OwnedStarlarkModulePath::LoadFile(bzlfile.clone())),
            Ok(module),
        )
        .mock_and_return(
            InterpreterResultsKey(PackageLabel::testing_parse("cell//pkg")),
            Ok(Arc::new(eval_res)),
        )
        .mock_and_return(ExecutionPlatformsKey, Ok(None))
        .set_data(|data| {
            data.set_testing_io_provider(&fs);
            data.set_digest_config(DigestConfig::testing_default());
            data.set_detailed_aggregated_metrics_event_handler(None);
        })
        .build({
            let mut data = UserComputationData::new();
            data.set_keep_going(true);
            data.set_starlark_debugger_handle(None);
            set_fallback_executor_config(&mut data.data, CommandExecutorConfig::testing_local());
            data.data.set(EventDispatcher::null());
            data.spawner = Arc::new(BuckSpawner::current_runtime().unwrap());
            data
        })?;
    setup_interpreter_basic(
        &mut dice,
        resolver,
        BuildInterpreterConfiguror::new(
            None,
            InterpreterHostPlatform::Linux,
            InterpreterHostArchitecture::X86_64,
            None,
            false,
            false,
            None,
            Arc::new(ConcurrentTargetLabelInterner::default()),
        )?,
    )?;
    let mut dice = dice.commit().await;

    let analysis = dice
        .get_analysis_result(
            &TargetLabel::testing_parse("cell//pkg:rule1")
                .configure(ConfigurationData::testing_new()),
        )
        .await?
        .require_compatible()?;

    assert_eq!(analysis.analysis_values().iter_actions().count(), 0);

    assert_eq!(
        analysis
            .providers()
            .unwrap()
            .value()
            .provider_names()
            .iter()
            .sorted()
            .eq(vec!["DefaultInfo", "FooInfo"]),
        true
    );

    assert_eq!(
        analysis
            .providers()
            .unwrap()
            .value()
            .get_provider_raw(&ProviderId::testing_new(bzlfile.path().clone(), "FooInfo"))
            .is_some(),
        true
    );
    assert_eq!(
        analysis
            .providers()
            .unwrap()
            .value()
            .get_provider_raw(DefaultInfoCallable::provider_id())
            .is_some(),
        true
    );

    Ok(())
}
