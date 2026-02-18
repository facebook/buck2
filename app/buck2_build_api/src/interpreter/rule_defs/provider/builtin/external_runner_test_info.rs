/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::iter::empty;
use std::iter::once;

use allocative::Allocative;
use buck2_build_api_derive::internal_provider;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_error::BuckErrorContext;
use buck2_error::buck2_error;
use buck2_error::internal_error;
use buck2_interpreter::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use either::Either;
use indexmap::IndexMap;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::environment::GlobalsBuilder;
use starlark::values::Freeze;
use starlark::values::FreezeError;
use starlark::values::FrozenValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueOfUncheckedGeneric;
use starlark::values::dict::DictRef;
use starlark::values::dict::DictType;
use starlark::values::list::ListRef;
use starlark::values::none::NoneOr;
use starlark::values::none::NoneType;
use starlark::values::tuple::TupleRef;

use crate as buck2_build_api;
use crate::interpreter::rule_defs::cmd_args::ArtifactPathMapper;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineContext;
use crate::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use crate::interpreter::rule_defs::command_executor_config::StarlarkCommandExecutorConfig;
use crate::interpreter::rule_defs::provider::builtin::worker_info::FrozenWorkerInfo;
use crate::interpreter::rule_defs::provider::builtin::worker_info::WorkerInfo;
use crate::interpreter::rule_defs::required_test_local_resource::StarlarkRequiredTestLocalResource;
use crate::interpreter::rule_defs::resolved_macro::ResolvedStringWithMacros;

/// Provider that signals that a rule can be tested using an external runner. This is the
/// Buck1-compatible API for tests.
#[internal_provider(external_runner_test_info_creator)]
#[derive(Clone, Debug, Trace, Coerce, Freeze, ProvidesStaticType, Allocative)]
#[freeze(validator = validate_external_runner_test_info, bounds = "V: ValueLike<'freeze>")]
#[repr(C)]
pub struct ExternalRunnerTestInfoGen<V: ValueLifetimeless> {
    /// A Starlark value representing the type of this test.
    test_type: ValueOfUncheckedGeneric<V, String>,

    /// A Starlark value representing the command for this test. The external test runner is what
    /// gives meaning to this command.
    command: ValueOfUncheckedGeneric<V, Vec<Either<String, FrozenValue>>>,

    /// A Starlark value representing the environment for this test. Here again, the external test
    /// runner is what will this meaning.
    /// This is of type `dict[str, ArgLike]`.
    env: ValueOfUncheckedGeneric<V, DictType<String, FrozenValue>>,

    /// A starlark value representing the labels for this test.
    labels: ValueOfUncheckedGeneric<V, Vec<String>>,

    /// A starlark value representing the contacts for this test. This is largely expected to be an
    /// oncall, though it's not validated in any way.
    contacts: ValueOfUncheckedGeneric<V, Vec<String>>,

    /// Whether this test should use relative paths
    use_project_relative_paths: ValueOfUncheckedGeneric<V, bool>,

    /// Whether this test should run from the project root, as opposed to the cell root
    ///
    /// Defaults to `True`.
    run_from_project_root: ValueOfUncheckedGeneric<V, bool>,

    /// Default executor to use to run tests. If none is
    /// passed we will default to the execution platform.
    default_executor: ValueOfUncheckedGeneric<V, StarlarkCommandExecutorConfig>,

    /// Executors that Tpx can use to override the default executor.
    executor_overrides: ValueOfUncheckedGeneric<V, DictType<String, StarlarkCommandExecutorConfig>>,

    /// Mapping from a local resource type to a target with a corresponding provider.
    /// Required types are passed from test runner.
    /// If the value for a corresponding type is omitted it means local resource
    /// should be ignored when executing tests even if those are passed as required from test runner.
    local_resources:
        ValueOfUncheckedGeneric<V, DictType<String, Option<StarlarkConfiguredProvidersLabel>>>,

    /// List of local resource types which should be set up additionally to those which are
    /// passed from test runner. Allows specifying local resources on a per-rule basis.
    required_local_resources: ValueOfUncheckedGeneric<V, Vec<StarlarkRequiredTestLocalResource>>,

    /// Configuration needed to spawn a new worker. This worker will be used to run every single
    /// command related to test execution, including listing.
    worker: ValueOfUncheckedGeneric<V, FrozenWorkerInfo>,
}

// NOTE: All the methods here unwrap because we validate at freeze time.
impl FrozenExternalRunnerTestInfo {
    pub fn test_type(&self) -> &str {
        self.test_type.to_value().get().unpack_str().unwrap()
    }

    pub fn command<'v>(&self) -> impl Iterator<Item = TestCommandMember<'v>> {
        unwrap_all(iter_test_command(self.command.get().to_value()))
    }

    pub fn env<'v>(&self) -> impl Iterator<Item = (&'v str, &'v dyn CommandLineArgLike<'v>)> {
        unwrap_all(iter_test_env(self.env.get().to_value()))
    }

    pub fn labels(&self) -> impl Iterator<Item = &str> {
        unwrap_all(iter_opt_str_list(self.labels.get().to_value(), "labels"))
    }

    pub fn contacts(&self) -> impl Iterator<Item = &str> {
        unwrap_all(iter_opt_str_list(
            self.contacts.get().to_value(),
            "contacts",
        ))
    }

    pub fn use_project_relative_paths(&self) -> bool {
        NoneOr::<bool>::unpack_value(self.use_project_relative_paths.get().to_value())
            .unwrap()
            .unwrap()
            .into_option()
            .unwrap_or_else(buck2_core::is_open_source)
    }

    pub fn run_from_project_root(&self) -> bool {
        NoneOr::<bool>::unpack_value(self.run_from_project_root.get().to_value())
            .unwrap()
            .unwrap()
            .into_option()
            .unwrap_or(true)
    }

    pub fn default_executor(&self) -> Option<&StarlarkCommandExecutorConfig> {
        unpack_opt_executor(self.default_executor.get().to_value()).unwrap()
    }

    pub fn has_executor_overrides(&self) -> bool {
        !self.executor_overrides.get().to_value().is_none()
    }

    /// Access a specific executor override.
    pub fn executor_override(&self, key: &str) -> Option<&StarlarkCommandExecutorConfig> {
        let executor_overrides =
            DictRef::from_value(self.executor_overrides.get().to_value()).unwrap();
        executor_overrides
            .get_str(key)
            .map(|v| StarlarkCommandExecutorConfig::from_value(v.to_value()).unwrap())
    }

    pub fn local_resources(&self) -> IndexMap<&str, Option<&ConfiguredProvidersLabel>> {
        unwrap_all(iter_local_resources(self.local_resources.get().to_value())).collect()
    }

    pub fn required_local_resources(
        &self,
    ) -> impl Iterator<Item = &StarlarkRequiredTestLocalResource> {
        let val = self.required_local_resources.get().to_value();
        if val.is_none() {
            return Either::Left(empty());
        }
        Either::Right(
            iter_value(val)
                .expect("checked during construction")
                .map(|v| {
                    StarlarkRequiredTestLocalResource::from_value(v)
                        .expect("checked during construction")
                }),
        )
    }

    pub fn worker(&self) -> Option<&WorkerInfo<'_>> {
        unpack_opt_worker(self.worker.get().to_value()).unwrap()
    }

    pub fn visit_artifacts(
        &self,
        visitor: &mut dyn CommandLineArtifactVisitor<'_>,
    ) -> buck2_error::Result<()> {
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
    Arglike(&'v dyn CommandLineArgLike<'v>),
}

impl<'v> TestCommandMember<'v> {
    pub fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        context: &mut dyn CommandLineContext,
        artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()> {
        match self {
            Self::Literal(literal) => {
                literal.add_to_command_line(cli, context, artifact_path_mapping)
            }
            Self::Arglike(arglike) => {
                arglike.add_to_command_line(cli, context, artifact_path_mapping)
            }
        }
    }
}

fn iter_value<'v>(value: Value<'v>) -> buck2_error::Result<impl Iterator<Item = Value<'v>> + 'v> {
    match Either::<&ListRef, &TupleRef>::unpack_value_err(value)? {
        Either::Left(list) => Ok(list.iter()),
        Either::Right(tuple) => Ok(tuple.iter()),
    }
}

fn iter_test_command<'v>(
    command: Value<'v>,
) -> impl Iterator<Item = buck2_error::Result<TestCommandMember<'v>>> {
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
            .with_buck_error_context(|| format!("Invalid item in `command`: {item}"))?
            .0;

        Ok(TestCommandMember::Arglike(arglike))
    }))
}

fn iter_test_env<'v>(
    env: Value<'v>,
) -> impl Iterator<Item = buck2_error::Result<(&'v str, &'v dyn CommandLineArgLike<'v>)>> {
    if env.is_none() {
        return Either::Left(Either::Left(empty()));
    }

    let env = match DictRef::from_value(env) {
        Some(env) => env,
        None => {
            return Either::Left(Either::Right(once(Err(buck2_error!(
                buck2_error::ErrorTag::Input,
                "Invalid `env`: Expected a dict, got: `{}`",
                env
            )))));
        }
    };

    let env = env.iter().collect::<Vec<_>>();

    Either::Right(env.into_iter().map(|(key, value)| {
        let key = key
            .unpack_str()
            .ok_or_else(|| internal_error!("Invalid key in `env`: Expected a str, got: `{key}`"))?;

        let arglike = ValueAsCommandLineLike::unpack_value_err(value)
            .with_buck_error_context(|| format!("Invalid value in `env` for key `{key}`"))?
            .0;

        Ok((key, arglike))
    }))
}

fn iter_opt_str_list<'v>(
    list: Value<'v>,
    name: &'static str,
) -> impl Iterator<Item = buck2_error::Result<&'v str>> {
    if list.is_none() {
        return Either::Left(Either::Left(empty()));
    }

    let iterable = match iter_value(list) {
        Ok(v) => v,
        Err(e) => {
            return Either::Left(Either::Right(once(Err(
                e.context(format!("Invalid `{name}`"))
            ))));
        }
    };

    Either::Right(iterable.map(move |item| {
        let item = item
            .unpack_str()
            .ok_or_else(|| internal_error!("Invalid item in `{name}`: {item}"))?;

        Ok(item)
    }))
}

fn iter_executor_overrides<'v>(
    executor_overrides: Value<'v>,
) -> impl Iterator<Item = buck2_error::Result<(&'v str, &'v StarlarkCommandExecutorConfig)>> {
    if executor_overrides.is_none() {
        return Either::Left(Either::Left(empty()));
    }

    let executor_overrides = match DictRef::from_value(executor_overrides) {
        Some(executor_overrides) => executor_overrides,
        None => {
            return Either::Left(Either::Right(once(Err(buck2_error!(
                buck2_error::ErrorTag::Input,
                "Invalid `executor_overrides`: Expected a dict, got: `{}`",
                executor_overrides
            )))));
        }
    };

    let executor_overrides = executor_overrides.iter().collect::<Vec<_>>();

    Either::Right(executor_overrides.into_iter().map(|(key, value)| {
        let key = key.unpack_str().ok_or_else(|| {
            internal_error!("Invalid key in `executor_overrides`: Expected a str, got: `{key}`")
        })?;

        let config = StarlarkCommandExecutorConfig::from_value(value).ok_or_else(|| {
            internal_error!("Invalid value in `executor_overrides` for key `{key}`")
        })?;

        Ok((key, config))
    }))
}

fn iter_local_resources<'v>(
    local_resources: Value<'v>,
) -> impl Iterator<Item = buck2_error::Result<(&'v str, Option<&'v ConfiguredProvidersLabel>)>> {
    if local_resources.is_none() {
        return Either::Left(Either::Left(empty()));
    }

    let local_resources = match DictRef::from_value(local_resources) {
        Some(local_resources) => local_resources,
        None => {
            return Either::Left(Either::Right(once(Err(buck2_error!(
                buck2_error::ErrorTag::Input,
                "Invalid `local_resources`: Expected a dict, got: `{}`",
                local_resources
            )))));
        }
    };

    let local_resources = local_resources.iter().collect::<Vec<_>>();

    Either::Right(local_resources.into_iter().map(|(key, value)| {
        let key = key.unpack_str().ok_or_else(|| {
            internal_error!("Invalid key in `local_resources`: Expected a str, got: `{key}`")
        })?;

        let resource = if value.is_none() {
            None
        } else {
            Some(
                StarlarkConfiguredProvidersLabel::from_value(value)
                    .ok_or_else(|| {
                        buck2_error!(
                            buck2_error::ErrorTag::Input,
                            "{}",
                            format!("Invalid value in `local_resources` for key `{}`", key)
                        )
                    })?
                    .label(),
            )
        };

        Ok((key, resource))
    }))
}

fn unpack_opt_executor<'v>(
    executor: Value<'v>,
) -> buck2_error::Result<Option<&'v StarlarkCommandExecutorConfig>> {
    if executor.is_none() {
        return Ok(None);
    }

    let executor = StarlarkCommandExecutorConfig::from_value(executor)
        .ok_or_else(|| internal_error!("Value is not an executor config: `{executor}`"))?;

    Ok(Some(executor))
}

fn unpack_opt_worker<'v>(worker: Value<'v>) -> buck2_error::Result<Option<&'v WorkerInfo<'v>>> {
    if worker.is_none() {
        return Ok(None);
    }

    let worker = WorkerInfo::from_value(worker)
        .ok_or_else(|| internal_error!("Value is not a worker: `{worker}`"))?;

    Ok(Some(worker))
}

fn check_all<I, T>(it: I) -> buck2_error::Result<()>
where
    I: IntoIterator<Item = buck2_error::Result<T>>,
{
    for e in it {
        e?;
    }
    Ok(())
}

fn unwrap_all<I, T>(it: I) -> impl Iterator<Item = T>
where
    I: IntoIterator<Item = buck2_error::Result<T>>,
{
    it.into_iter().map(|e| e.unwrap())
}

fn validate_external_runner_test_info<'v, V>(
    info: &ExternalRunnerTestInfoGen<V>,
) -> buck2_error::Result<()>
where
    V: ValueLike<'v>,
{
    check_all(iter_test_command(info.command.get().to_value()))?;
    check_all(iter_test_env(info.env.get().to_value()))?;
    check_all(iter_opt_str_list(info.labels.get().to_value(), "labels"))?;
    check_all(iter_opt_str_list(
        info.contacts.get().to_value(),
        "contacts",
    ))?;
    check_all(iter_executor_overrides(
        info.executor_overrides.get().to_value(),
    ))?;

    let provided_local_resources =
        iter_local_resources(info.local_resources.get().to_value())
            .collect::<buck2_error::Result<IndexMap<&str, Option<&ConfiguredProvidersLabel>>>>()?;

    let required_local_resources = info.required_local_resources.get().to_value();
    if !required_local_resources.is_none() {
        for resource_type in iter_value(required_local_resources).buck_error_context("`required_local_resources` should be a list or a tuple of `RequiredTestLocalResource` objects")? {
            let resource_type = StarlarkRequiredTestLocalResource::from_value(resource_type)
                .ok_or_else(|| buck2_error!(buck2_error::ErrorTag::Input, "`required_local_resources` should only contain `RequiredTestLocalResource` values, got {}", resource_type))?;
            if !provided_local_resources.contains_key(&resource_type.name as &str) {
                return Err(buck2_error!(
                    buck2_error::ErrorTag::Input,
                    "`required_local_resources` contains `{}` which is not present in `local_resources`",
                    resource_type.name
                ));
            }
        }
    }

    NoneOr::<bool>::unpack_value(info.use_project_relative_paths.get().to_value())?.ok_or_else(
        || internal_error!("`use_project_relative_paths` must be a bool if provided"),
    )?;
    NoneOr::<bool>::unpack_value(info.run_from_project_root.get().to_value())?
        .ok_or_else(|| internal_error!("`run_from_project_root` must be a bool if provided"))?;
    unpack_opt_executor(info.default_executor.get().to_value())
        .buck_error_context("Invalid `default_executor`")?;
    unpack_opt_worker(info.worker.get().to_value()).buck_error_context("Invalid `worker`")?;
    info.test_type
        .get()
        .to_value()
        .unpack_str()
        .ok_or_else(|| internal_error!("`type` must be a str"))?;
    Ok(())
}

#[starlark_module]
fn external_runner_test_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(as_type = FrozenExternalRunnerTestInfo)]
    fn ExternalRunnerTestInfo<'v>(
        r#type: Value<'v>,
        // TODO(nga): these need types.
        // TODO(nga): parameters should be either named or positional, not both.
        #[starlark(default = NoneType)] command: Value<'v>,
        #[starlark(default = NoneType)] env: Value<'v>,
        #[starlark(default = NoneType)] labels: Value<'v>,
        #[starlark(default = NoneType)] contacts: Value<'v>,
        #[starlark(default = NoneType)] use_project_relative_paths: Value<'v>,
        #[starlark(default = NoneType)] run_from_project_root: Value<'v>,
        #[starlark(default = NoneType)] default_executor: Value<'v>,
        #[starlark(default = NoneType)] executor_overrides: Value<'v>,
        #[starlark(default = NoneType)] local_resources: Value<'v>,
        #[starlark(default = NoneType)] required_local_resources: Value<'v>,
        #[starlark(default = NoneType)] worker: Value<'v>,
    ) -> starlark::Result<ExternalRunnerTestInfo<'v>> {
        let res = ExternalRunnerTestInfo {
            test_type: ValueOfUnchecked::new(r#type),
            command: ValueOfUnchecked::new(command),
            env: ValueOfUnchecked::new(env),
            labels: ValueOfUnchecked::new(labels),
            contacts: ValueOfUnchecked::new(contacts),
            use_project_relative_paths: ValueOfUnchecked::new(use_project_relative_paths),
            run_from_project_root: ValueOfUnchecked::new(run_from_project_root),
            default_executor: ValueOfUnchecked::new(default_executor),
            executor_overrides: ValueOfUnchecked::new(executor_overrides),
            local_resources: ValueOfUnchecked::new(local_resources),
            required_local_resources: ValueOfUnchecked::new(required_local_resources),
            worker: ValueOfUnchecked::new(worker),
        };
        validate_external_runner_test_info(&res)?;
        Ok(res)
    }
}
