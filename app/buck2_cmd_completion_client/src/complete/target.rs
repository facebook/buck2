/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::time::Instant;

use buck2_cli_proto::new_generic::CompleteRequest;
use buck2_cli_proto::new_generic::NewGenericRequest;
use buck2_cli_proto::new_generic::NewGenericResponse;
use buck2_cli_proto::ClientContext;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::command_outcome::CommandOutcome;
use buck2_client_ctx::common::target_cfg::TargetCfgOptions;
use buck2_client_ctx::common::ui::CommonConsoleOptions;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::common::CommonStarlarkOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::daemon::client::FlushingBuckdClient;
use buck2_client_ctx::exit_result::ExitCode;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::StreamingCommand;
use buck2_common::invocation_roots::InvocationRoots;
use buck2_common::legacy_configs::cells::BuckConfigBasedCells;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use clap::ArgMatches;
use futures::future::BoxFuture;
use futures::FutureExt;
use tokio::time;

use super::path_sanitizer::PathSanitizer;
use super::results::CompletionResults;

type CompleteCallback = fn(CommandOutcome<Vec<String>>) -> ExitResult;

pub(crate) trait TargetResolver: Send {
    fn resolve(&mut self, partial_target: String) -> BoxFuture<CommandOutcome<Vec<String>>>;
}

pub(crate) struct CompleteTargetCommand {
    target_cfg: TargetCfgOptions,

    cwd: AbsNormPathBuf,
    package: String,
    partial_target: String,

    deadline: Instant,
    callback: CompleteCallback,
}

impl CompleteTargetCommand {
    pub(crate) fn new(
        cwd: &AbsNormPath,
        package: String,
        partial_target: String,
        deadline: Instant,
        callback: CompleteCallback,
    ) -> Self {
        let target_cfg = TargetCfgOptions::default();
        Self {
            target_cfg,
            cwd: cwd.to_owned(),
            package,
            partial_target,
            deadline,
            callback,
        }
    }
}

#[async_trait::async_trait]
impl StreamingCommand for CompleteTargetCommand {
    const COMMAND_NAME: &'static str = "complete";

    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: &ArgMatches,
        ctx: &mut ClientCommandContext<'_>,
    ) -> ExitResult {
        let buckd_client = buckd.with_flushing();
        let context = ctx.client_context(matches, &self)?;
        let mut target_resolver = DaemonTargetResolver {
            buckd_client,
            context,
            target_cfg: self.target_cfg,
        };

        let completer = TargetCompleter::new(&self.cwd, &ctx.paths()?.roots, &mut target_resolver)
            .await
            .expect("Failed to create target completer");
        let task = completer.complete(&self.package, &self.partial_target);

        let remaining_time = self.deadline.saturating_duration_since(Instant::now());
        match time::timeout(remaining_time, task).await {
            Ok(CommandOutcome::Success(completions)) => {
                (self.callback)(CommandOutcome::Success(completions))
            }
            Ok(CommandOutcome::Failure(err)) => err,
            Err(_) => ExitResult::status(ExitCode::Timeout),
        }
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        CommonConsoleOptions::none_ref()
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        CommonEventLogOptions::default_ref()
    }

    fn build_config_opts(&self) -> &CommonBuildConfigurationOptions {
        CommonBuildConfigurationOptions::reuse_current_config_ref()
    }

    fn starlark_opts(&self) -> &CommonStarlarkOptions {
        CommonStarlarkOptions::default_ref()
    }
}
pub(crate) struct TargetCompleter<'a> {
    cwd: AbsNormPathBuf,
    cell_configs: Arc<BuckConfigBasedCells>,
    target_resolver: &'a mut dyn TargetResolver,
    results: CompletionResults<'a>,
}

impl<'a> TargetCompleter<'a> {
    pub(crate) async fn new(
        cwd: &AbsNormPath,
        roots: &'a InvocationRoots,
        target_resolver: &'a mut dyn TargetResolver,
    ) -> anyhow::Result<Self> {
        let cell_configs = Arc::new(
            BuckConfigBasedCells::parse_with_config_args(
                &roots.project_root,
                &[],
                ProjectRelativePath::empty(),
            )
            .await?,
        );
        Ok(Self {
            cwd: cwd.to_owned(),
            cell_configs: cell_configs.clone(),
            target_resolver,
            results: CompletionResults::new(roots, cell_configs.clone()),
        })
    }

    /// Complete the target in a partial label.
    ///
    /// Returns a collection of possible completions, each including the partial
    /// target including the cell/package name(s).
    pub(crate) async fn complete(
        mut self,
        given_package: &str,
        partial_target: &str,
    ) -> CommandOutcome<Vec<String>> {
        let sanitizer = PathSanitizer::new(&self.cell_configs, &self.cwd).await?;
        let path = sanitizer.sanitize(given_package)?;
        let completions = self
            .target_resolver
            .resolve(path.given().to_owned() + ":")
            .await?;

        for label in completions {
            let target = label.split(':').next_back().unwrap();
            if target.starts_with(partial_target) {
                let completion = path.given().to_owned() + ":" + target;
                self.results.insert(&completion);
            }
        }
        CommandOutcome::Success(self.results.into())
    }
}

struct DaemonTargetResolver<'a, 'b> {
    buckd_client: FlushingBuckdClient<'a, 'b>,
    context: ClientContext,
    target_cfg: TargetCfgOptions,
}

impl<'a, 'b> TargetResolver for DaemonTargetResolver<'a, 'b> {
    fn resolve(&mut self, partial_target: String) -> BoxFuture<CommandOutcome<Vec<String>>> {
        let request = NewGenericRequest::Complete(CompleteRequest {
            target_cfg: self.target_cfg.target_cfg(),
            partial_target,
        });
        self.buckd_client
            .new_generic(self.context.clone(), request, None)
            .then(|res| async move {
                match res {
                    Ok(CommandOutcome::Success(NewGenericResponse::Complete(res))) => {
                        CommandOutcome::Success(res.completions)
                    }
                    Ok(CommandOutcome::Success(_)) => CommandOutcome::Failure(ExitResult::bail(
                        "Unexpected response type from generic command",
                    )),
                    Ok(CommandOutcome::Failure(result)) => CommandOutcome::Failure(result),
                    Err(e) => CommandOutcome::Failure(ExitResult::err(e)),
                }
            })
            .boxed()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use buck2_common::invocation_roots::find_invocation_roots;
    use futures::future;

    use super::*;

    fn paths_to_test_data() -> &'static [&'static str] {
        &[
            "fbcode/buck2/app/buck2_cmd_completions_client/test_data",
            "app/buck2_cmd_completion_client/test_data",
            "test_data",
        ]
    }

    fn in_dir(d: &str) -> anyhow::Result<(InvocationRoots, AbsNormPathBuf)> {
        let cwd = AbsNormPathBuf::new(std::env::current_dir().unwrap())?;

        for path in paths_to_test_data() {
            let candidate = cwd.join_normalized(path)?.join_normalized(d)?;
            if candidate.exists() {
                return Ok((find_invocation_roots(&candidate)?, candidate));
            }
        }

        Err(anyhow::anyhow!("test_data directory not found"))
    }

    fn in_root() -> anyhow::Result<(InvocationRoots, AbsNormPathBuf)> {
        let cwd = AbsNormPathBuf::new(std::env::current_dir().unwrap())?;

        for path in paths_to_test_data() {
            let candidate = cwd.join_normalized(path)?;
            if candidate.exists() {
                return Ok((find_invocation_roots(&candidate)?, candidate));
            }
        }

        Err(anyhow::anyhow!("test_data directory not found"))
    }

    async fn target_complete_helper(
        uut: TargetCompleter<'_>,
        partial_target: &str,
    ) -> CommandOutcome<Vec<String>> {
        match partial_target.split(':').collect::<Vec<_>>()[..] {
            [package, partial_target] => uut.complete(package, partial_target).await,
            _ => panic!("unexpected target {}", partial_target),
        }
    }

    type TestResult = Result<(), ExitResult>;

    struct FakeTargetResolver {
        target_responses: HashMap<String, Vec<String>>,
    }

    impl FakeTargetResolver {
        fn new() -> Self {
            FakeTargetResolver {
                target_responses: HashMap::new(),
            }
        }
        fn add_response(&mut self, request: &str, response: Vec<&str>) {
            self.target_responses.insert(
                request.to_owned(),
                response.into_iter().map(|s| s.to_owned()).collect(),
            );
        }
    }

    impl TargetResolver for FakeTargetResolver {
        fn resolve(&mut self, partial_target: String) -> BoxFuture<CommandOutcome<Vec<String>>> {
            let res = self.target_responses.get(&partial_target).unwrap();
            Box::pin(future::ready(CommandOutcome::Success(res.clone())))
        }
    }

    #[tokio::test]
    async fn test_handles_degenerate_buck_directory_with_no_targets() -> TestResult {
        let (roots, cwd) = in_root()?;
        let mut resolver = FakeTargetResolver::new();
        resolver.add_response("baredir0:", vec![]);
        let uut = TargetCompleter::new(&cwd, &roots, &mut resolver).await?;

        let actual = target_complete_helper(uut, "baredir0:").await?;

        let expected: Vec<String> = vec![];
        assert_eq!(actual, expected);
        Ok(())
    }

    #[tokio::test]
    async fn test_provides_targets_for_path_ending_with_a_colon() -> TestResult {
        let (roots, cwd) = in_root()?;
        let mut resolver = FakeTargetResolver::new();
        resolver.add_response(
            "baredir0/buckdir0b:",
            vec![
                "root//baredir0/buckdir0b:target1",
                "root//baredir0/buckdir0b:target2",
                "root//baredir0/buckdir0b:target3",
            ],
        );
        let uut = TargetCompleter::new(&cwd, &roots, &mut resolver).await?;

        let actual = target_complete_helper(uut, "baredir0/buckdir0b:").await?;

        assert_eq!(
            actual,
            vec![
                "baredir0/buckdir0b:target1",
                "baredir0/buckdir0b:target2",
                "baredir0/buckdir0b:target3",
            ],
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_provides_targets_in_nested_cell() -> TestResult {
        let (roots, cwd) = in_dir("cell1")?;
        let mut resolver = FakeTargetResolver::new();
        resolver.add_response(
            "buck2:",
            vec!["cell1//buck2:buck2", "cell1//buck2:symlinked_buck2_and_tpx"],
        );
        let uut = TargetCompleter::new(&cwd, &roots, &mut resolver).await?;

        let actual = target_complete_helper(uut, "buck2:").await?;

        assert_eq!(
            actual,
            vec!["buck2:buck2", "buck2:symlinked_buck2_and_tpx",]
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_a_partial_target() -> TestResult {
        let (roots, cwd) = in_dir("cell1")?;
        let mut resolver = FakeTargetResolver::new();
        resolver.add_response(
            "buck2:",
            vec!["cell1//buck2:buck2", "cell1//buck2:symlinked_buck2_and_tpx"],
        );
        let uut = TargetCompleter::new(&cwd, &roots, &mut resolver).await?;

        let actual = target_complete_helper(uut, "buck2:bu").await?;

        assert_eq!(actual, vec!["buck2:buck2",]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_targets_for_fully_qualified_cell() -> TestResult {
        let (roots, cwd) = in_root()?;
        let mut resolver = FakeTargetResolver::new();
        resolver.add_response("cell1//:", vec!["cell1//:target1"]);
        let uut = TargetCompleter::new(&cwd, &roots, &mut resolver).await?;

        let actual = target_complete_helper(uut, "cell1//:").await?;

        assert_eq!(actual, vec!["cell1//:target1"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_other_cell_canonical_path_targets_from_subdirectory_in_this_cell()
    -> TestResult {
        let (roots, cwd) = in_dir("baredir0")?;
        let mut resolver = FakeTargetResolver::new();
        resolver.add_response(
            "cell1//buck2:",
            vec!["cell1//buck2:buck2", "cell1//buck2:symlinked_buck2_and_tpx"],
        );
        let uut = TargetCompleter::new(&cwd, &roots, &mut resolver).await?;

        let actual = target_complete_helper(uut, "cell1//buck2:").await?;

        assert_eq!(
            actual,
            vec!["cell1//buck2:buck2", "cell1//buck2:symlinked_buck2_and_tpx"]
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_expands_cell_to_canonical_in_middle_of_input_text_with_target_colon() -> TestResult
    {
        let (roots, cwd) = in_root()?;
        let mut resolver = FakeTargetResolver::new();
        resolver.add_response(
            "cell1//buck2:",
            vec!["cell1//buck2:buck2", "cell1//buck2:symlinked_buck2_and_tpx"],
        );
        let uut = TargetCompleter::new(&cwd, &roots, &mut resolver).await?;

        let actual = target_complete_helper(uut, "cell1/buck2:").await?;

        assert_eq!(
            actual,
            vec!["cell1//buck2:buck2", "cell1//buck2:symlinked_buck2_and_tpx",]
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_expands_cell_to_canonical_in_middle_of_input_text_with_partial_target()
    -> TestResult {
        let (roots, cwd) = in_root()?;
        let mut resolver = FakeTargetResolver::new();
        resolver.add_response(
            "cell1//buck2:",
            vec!["cell1//buck2:buck2", "cell1//buck2:symlinked_buck2_and_tpx"],
        );
        let uut = TargetCompleter::new(&cwd, &roots, &mut resolver).await?;

        let actual = target_complete_helper(uut, "cell1/buck2:bu").await?;

        assert_eq!(actual, vec!["cell1//buck2:buck2"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_expands_targets_for_a_bare_colon_in_a_buck_directory() -> TestResult {
        let (roots, cwd) = in_dir("cell1/buck2")?;
        let mut resolver = FakeTargetResolver::new();
        resolver.add_response(
            ":",
            vec!["cell1//buck2:buck2", "cell1//buck2:symlinked_buck2_and_tpx"],
        );
        let uut = TargetCompleter::new(&cwd, &roots, &mut resolver).await?;

        let actual = target_complete_helper(uut, ":").await?;

        assert_eq!(actual, vec![":buck2", ":symlinked_buck2_and_tpx",]);
        Ok(())
    }

    #[tokio::test]
    async fn test_returns_nothing_for_a_bare_colon_in_a_non_buck_directory() -> TestResult {
        let (roots, cwd) = in_dir("baredir0")?;
        let mut resolver = FakeTargetResolver::new();
        // This is weird, but it reflects the behavior of the server API
        // when given an invalid ":" as its argument
        resolver.add_response(":", vec![]);
        let uut = TargetCompleter::new(&cwd, &roots, &mut resolver).await?;

        let actual = target_complete_helper(uut, ":").await?;

        assert_eq!(actual.len(), 0);
        Ok(())
    }

    #[tokio::test]
    async fn test_target_completion_works_correctly_with_aliased_cells() -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("cell1/buck2/prelude")?;
        let mut target_resolver = FakeTargetResolver::new();
        target_resolver.add_response(
            "cell1_alias//buck2:",
            vec!["cell1//buck2:buck2", "cell1//buck2:symlinked_buck2_and_tpx"],
        );
        let uut = TargetCompleter::new(&cwd, &roots, &mut target_resolver).await?;

        let actual = target_complete_helper(uut, "cell1_alias//buck2:").await?;

        assert_eq!(
            actual,
            vec![
                "cell1_alias//buck2:buck2",
                "cell1_alias//buck2:symlinked_buck2_and_tpx",
            ]
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_target_completion_fails_with_error_with_nonexistent_alias() -> anyhow::Result<()>
    {
        let (roots, cwd) = in_dir("cell1/buck2")?;
        let mut target_resolver = FakeTargetResolver::new();
        target_resolver.add_response(
            "cell1_alias//buck2:",
            vec![
                "cell1//buck2:buck2",
                "cell1//buck2:symlinked_buck2_and_tpx",
                "",
            ],
        );
        let uut = TargetCompleter::new(&cwd, &roots, &mut target_resolver).await?;

        match target_complete_helper(uut, "cell1_alias//buck2:").await {
            CommandOutcome::Success(_) => panic!("Expected error"),
            CommandOutcome::Failure(_) => Ok(()),
        }
    }
}
