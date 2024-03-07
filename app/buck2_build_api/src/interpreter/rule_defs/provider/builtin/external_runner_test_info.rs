/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::iter::empty;
use std::iter::once;

use allocative::Allocative;
use anyhow::Context as _;
use buck2_build_api_derive::internal_provider;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_interpreter::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use either::Either;
use indexmap::IndexMap;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::environment::GlobalsBuilder;
use starlark::values::dict::DictRef;
use starlark::values::list::ListRef;
use starlark::values::none::NoneOr;
use starlark::values::none::NoneType;
use starlark::values::tuple::TupleRef;
use starlark::values::type_repr::DictType;
use starlark::values::Freeze;
use starlark::values::FrozenValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;

use crate::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineContext;
use crate::interpreter::rule_defs::command_executor_config::StarlarkCommandExecutorConfig;
use crate::interpreter::rule_defs::resolved_macro::ResolvedStringWithMacros;

/// Provider that signals that a rule can be tested using an external runner. This is the
/// Buck1-compatible API for tests.
#[internal_provider(external_runner_test_info_creator)]
#[derive(Clone, Debug, Trace, Coerce, Freeze, ProvidesStaticType, Allocative)]
#[freeze(validator = validate_external_runner_test_info, bounds = "V: ValueLike<'freeze>")]
#[repr(C)]
pub struct ExternalRunnerTestInfoGen<V> {
    /// A Starlark value representing the type of this test.
    #[provider(field_type = String)]
    test_type: V,

    /// A Starlark value representing the command for this test. The external test runner is what
    /// gives meaning to this command.
    /// This is of type `list[str | ArgLike]`.
    #[provider(field_type = Vec<Either<String, FrozenValue>>)]
    command: V,

    /// A Starlark value representing the environment for this test. Here again, the external test
    /// runner is what will this meaning.
    /// This is of type `dict[str, ArgLike]`.
    #[provider(field_type = DictType<String, FrozenValue>)]
    env: V,

    /// A starlark value representing the labels for this test.
    #[provider(field_type = Vec<String>)]
    labels: V,

    /// A starlark value representing the contacts for this test. This is largely expected to be an
    /// oncall, though it's not validated in any way.
    #[provider(field_type = Vec<String>)]
    contacts: V,

    /// Whether this test should use relative paths
    #[provider(field_type = bool)]
    use_project_relative_paths: V,

    /// Whether this test should run from the project root, as opposed to the cell root
    ///
    /// Defaults to `True`.
    #[provider(field_type = bool)]
    run_from_project_root: V,

    /// Default executor to use to run tests. If none is
    /// passed we will default to the execution platform.
    #[provider(field_type = StarlarkCommandExecutorConfig)]
    default_executor: V,

    /// Executors that Tpx can use to override the default executor.
    #[provider(field_type = DictType<String, StarlarkCommandExecutorConfig>)]
    executor_overrides: V,

    /// Mapping from a local resource type to a target with a corresponding provider.
    /// Required types are passed from test runner.
    /// If the value for a corresponding type is omitted it means local resource
    /// should be ignored when executing tests even if those are passed as required from test runner.
    #[provider(field_type = DictType<String, Option<StarlarkConfiguredProvidersLabel>>)]
    local_resources: V,
}

// NOTE: All the methods here unwrap because we validate at freeze time.
impl FrozenExternalRunnerTestInfo {
    pub fn test_type(&self) -> &str {
        self.test_type.to_value().unpack_str().unwrap()
    }

    pub fn command(&self) -> impl Iterator<Item = TestCommandMember<'_>> {
        unwrap_all(iter_test_command(self.command.to_value()))
    }

    pub fn env(&self) -> impl Iterator<Item = (&str, &dyn CommandLineArgLike)> {
        unwrap_all(iter_test_env(self.env.to_value()))
    }

    pub fn labels(&self) -> impl Iterator<Item = &str> {
        unwrap_all(iter_opt_str_list(self.labels.to_value(), "labels"))
    }

    pub fn contacts(&self) -> impl Iterator<Item = &str> {
        unwrap_all(iter_opt_str_list(self.contacts.to_value(), "contacts"))
    }

    pub fn use_project_relative_paths(&self) -> bool {
        NoneOr::<bool>::unpack_value(self.use_project_relative_paths.to_value())
            .unwrap()
            .into_option()
            .unwrap_or_else(buck2_core::is_open_source)
    }

    pub fn run_from_project_root(&self) -> bool {
        NoneOr::<bool>::unpack_value(self.run_from_project_root.to_value())
            .unwrap()
            .into_option()
            .unwrap_or(true)
    }

    pub fn default_executor(&self) -> Option<&StarlarkCommandExecutorConfig> {
        unpack_opt_executor(self.default_executor.to_value()).unwrap()
    }

    /// Access a specific executor override.
    pub fn executor_override(&self, key: &str) -> Option<&StarlarkCommandExecutorConfig> {
        let executor_overrides = DictRef::from_value(self.executor_overrides.to_value()).unwrap();
        executor_overrides
            .get_str(key)
            .map(|v| StarlarkCommandExecutorConfig::from_value(v.to_value()).unwrap())
    }

    pub fn local_resources(&self) -> IndexMap<&str, Option<&ConfiguredProvidersLabel>> {
        unwrap_all(iter_local_resources(self.local_resources.to_value())).collect()
    }

    pub fn visit_artifacts(
        &self,
        visitor: &mut dyn CommandLineArtifactVisitor,
    ) -> anyhow::Result<()> {
        for member in self.command() {
            match member {
                TestCommandMember::Literal(..) => {}
                TestCommandMember::Arglike(arglike) => {
                    arglike.visit_artifacts(visitor)?;
                }
            }
        }

        for (_, arglike) in self.env() {
            arglike.visit_artifacts(visitor)?;
        }

        // Ignoring local resources as those are built on-demand.

        Ok(())
    }
}

pub enum TestCommandMember<'v> {
    Literal(&'v str),
    Arglike(&'v dyn CommandLineArgLike),
}

impl<'v> TestCommandMember<'v> {
    pub fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        context: &mut dyn CommandLineContext,
    ) -> anyhow::Result<()> {
        match self {
            Self::Literal(literal) => literal.add_to_command_line(cli, context),
            Self::Arglike(arglike) => arglike.add_to_command_line(cli, context),
        }
    }
}

fn iter_value<'v>(value: Value<'v>) -> anyhow::Result<impl Iterator<Item = Value<'v>> + 'v> {
    match (ListRef::from_value(value), TupleRef::from_value(value)) {
        (Some(list), None) => Ok(Either::Left(list.iter())),
        (None, Some(tuple)) => Ok(Either::Right(tuple.iter())),
        _ => Err(anyhow::anyhow!(
            "Expected a list or a tuple, got `{}`",
            value
        )),
    }
}

fn iter_test_command<'v>(
    command: Value<'v>,
) -> impl Iterator<Item = anyhow::Result<TestCommandMember<'v>>> {
    if command.is_none() {
        return Either::Left(Either::Left(empty()));
    }

    let iterable = match iter_value(command) {
        Ok(v) => v,
        Err(e) => {
            return Either::Left(Either::Right(once(Err(e.context("Invalid `command`")))));
        }
    };

    Either::Right(iterable.map(|item| {
        if let Some(s) = item.unpack_str() {
            return Ok(TestCommandMember::Literal(s));
        }

        if let Some(s) = item.downcast_ref::<ResolvedStringWithMacros>() {
            if let Some(s) = s.downcast_str() {
                return Ok(TestCommandMember::Literal(s));
            }
        }

        let arglike = ValueAsCommandLineLike::unpack_value_err(item)
            .with_context(|| format!("Invalid item in `command`: {}", item))?
            .0;

        Ok(TestCommandMember::Arglike(arglike))
    }))
}

fn iter_test_env<'v>(
    env: Value<'v>,
) -> impl Iterator<Item = anyhow::Result<(&'v str, &'v dyn CommandLineArgLike)>> {
    if env.is_none() {
        return Either::Left(Either::Left(empty()));
    }

    let env = match DictRef::from_value(env) {
        Some(env) => env,
        None => {
            return Either::Left(Either::Right(once(Err(anyhow::anyhow!(
                "Invalid `env`: Expected a dict, got: `{}`",
                env
            )))));
        }
    };

    let env = env.iter().collect::<Vec<_>>();

    Either::Right(env.into_iter().map(|(key, value)| {
        let key = key
            .unpack_str()
            .with_context(|| format!("Invalid key in `env`: Expected a str, got: `{}`", key))?;

        let arglike = ValueAsCommandLineLike::unpack_value_err(value)
            .with_context(|| format!("Invalid value in `env` for key `{}`", key))?
            .0;

        Ok((key, arglike))
    }))
}

fn iter_opt_str_list<'v>(
    list: Value<'v>,
    name: &'static str,
) -> impl Iterator<Item = anyhow::Result<&'v str>> {
    if list.is_none() {
        return Either::Left(Either::Left(empty()));
    }

    let iterable = match iter_value(list) {
        Ok(v) => v,
        Err(e) => {
            return Either::Left(Either::Right(once(Err(
                e.context(format!("Invalid `{}`", name))
            ))));
        }
    };

    Either::Right(iterable.map(move |item| {
        let item = item
            .unpack_str()
            .with_context(|| format!("Invalid item in `{}`: {}", name, item))?;

        Ok(item)
    }))
}

fn iter_executor_overrides<'v>(
    executor_overrides: Value<'v>,
) -> impl Iterator<Item = anyhow::Result<(&'v str, &'v StarlarkCommandExecutorConfig)>> {
    if executor_overrides.is_none() {
        return Either::Left(Either::Left(empty()));
    }

    let executor_overrides = match DictRef::from_value(executor_overrides) {
        Some(executor_overrides) => executor_overrides,
        None => {
            return Either::Left(Either::Right(once(Err(anyhow::anyhow!(
                "Invalid `executor_overrides`: Expected a dict, got: `{}`",
                executor_overrides
            )))));
        }
    };

    let executor_overrides = executor_overrides.iter().collect::<Vec<_>>();

    Either::Right(executor_overrides.into_iter().map(|(key, value)| {
        let key = key.unpack_str().with_context(|| {
            format!(
                "Invalid key in `executor_overrides`: Expected a str, got: `{}`",
                key
            )
        })?;

        let config = StarlarkCommandExecutorConfig::from_value(value)
            .with_context(|| format!("Invalid value in `executor_overrides` for key `{}`", key))?;

        Ok((key, config))
    }))
}

fn iter_local_resources<'v>(
    local_resources: Value<'v>,
) -> impl Iterator<Item = anyhow::Result<(&'v str, Option<&'v ConfiguredProvidersLabel>)>> {
    if local_resources.is_none() {
        return Either::Left(Either::Left(empty()));
    }

    let local_resources = match DictRef::from_value(local_resources) {
        Some(local_resources) => local_resources,
        None => {
            return Either::Left(Either::Right(once(Err(anyhow::anyhow!(
                "Invalid `local_resources`: Expected a dict, got: `{}`",
                local_resources
            )))));
        }
    };

    let local_resources = local_resources.iter().collect::<Vec<_>>();

    Either::Right(local_resources.into_iter().map(|(key, value)| {
        let key = key.unpack_str().with_context(|| {
            format!(
                "Invalid key in `local_resources`: Expected a str, got: `{}`",
                key
            )
        })?;

        let resource = if value.is_none() {
            None
        } else {
            Some(
                StarlarkConfiguredProvidersLabel::from_value(value)
                    .ok_or_else(|| {
                        anyhow::anyhow!(format!(
                            "Invalid value in `local_resources` for key `{}`",
                            key
                        ))
                    })?
                    .label(),
            )
        };

        Ok((key, resource))
    }))
}

fn unpack_opt_executor<'v>(
    executor: Value<'v>,
) -> anyhow::Result<Option<&'v StarlarkCommandExecutorConfig>> {
    if executor.is_none() {
        return Ok(None);
    }

    let executor = StarlarkCommandExecutorConfig::from_value(executor)
        .with_context(|| format!("Value is not an executor config: `{}`", executor))?;

    Ok(Some(executor))
}

fn check_all<I, T>(it: I) -> anyhow::Result<()>
where
    I: IntoIterator<Item = anyhow::Result<T>>,
{
    for e in it {
        e?;
    }
    Ok(())
}

fn unwrap_all<I, T>(it: I) -> impl Iterator<Item = T>
where
    I: IntoIterator<Item = anyhow::Result<T>>,
{
    it.into_iter().map(|e| e.unwrap())
}

fn validate_external_runner_test_info<'v, V>(
    info: &ExternalRunnerTestInfoGen<V>,
) -> anyhow::Result<()>
where
    V: ValueLike<'v>,
{
    check_all(iter_test_command(info.command.to_value()))?;
    check_all(iter_test_env(info.env.to_value()))?;
    check_all(iter_opt_str_list(info.labels.to_value(), "labels"))?;
    check_all(iter_opt_str_list(info.contacts.to_value(), "contacts"))?;
    check_all(iter_executor_overrides(info.executor_overrides.to_value()))?;
    check_all(iter_local_resources(info.local_resources.to_value()))?;
    NoneOr::<bool>::unpack_value(info.use_project_relative_paths.to_value())
        .context("`use_project_relative_paths` must be a bool if provided")?;
    NoneOr::<bool>::unpack_value(info.run_from_project_root.to_value())
        .context("`run_from_project_root` must be a bool if provided")?;
    unpack_opt_executor(info.default_executor.to_value()).context("Invalid `default_executor`")?;
    info.test_type
        .to_value()
        .unpack_str()
        .context("`type` must be a str")?;
    Ok(())
}

#[starlark_module]
fn external_runner_test_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(as_type = FrozenExternalRunnerTestInfo)]
    fn ExternalRunnerTestInfo<'v>(
        r#type: Value<'v>,
        #[starlark(default = NoneType)] command: Value<'v>,
        #[starlark(default = NoneType)] env: Value<'v>,
        #[starlark(default = NoneType)] labels: Value<'v>,
        #[starlark(default = NoneType)] contacts: Value<'v>,
        #[starlark(default = NoneType)] use_project_relative_paths: Value<'v>,
        #[starlark(default = NoneType)] run_from_project_root: Value<'v>,
        #[starlark(default = NoneType)] default_executor: Value<'v>,
        #[starlark(default = NoneType)] executor_overrides: Value<'v>,
        #[starlark(default = NoneType)] local_resources: Value<'v>,
    ) -> anyhow::Result<ExternalRunnerTestInfo<'v>> {
        let res = ExternalRunnerTestInfo {
            test_type: r#type,
            command,
            env,
            labels,
            contacts,
            use_project_relative_paths,
            run_from_project_root,
            default_executor,
            executor_overrides,
            local_resources,
        };
        validate_external_runner_test_info(&res)?;
        Ok(res)
    }
}
