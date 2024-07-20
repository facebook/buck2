/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeSet;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use buck2_cli_proto::targets_request;
use buck2_cli_proto::targets_request::OutputFormat;
use buck2_cli_proto::TargetsRequest;
use buck2_cli_proto::TargetsResponse;
use buck2_client_ctx::command_outcome::CommandOutcome;
use buck2_common::buildfiles::parse_buildfile_name;
use buck2_common::invocation_roots::InvocationRoots;
use buck2_common::legacy_configs::cells::BuckConfigBasedCells;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use futures::future::BoxFuture;

use crate::commands::completion::buck_path::BuckPath;

pub(crate) trait TargetsResolver: Send {
    fn resolve(
        &mut self,
        req: TargetsRequest,
    ) -> BoxFuture<anyhow::Result<CommandOutcome<TargetsResponse>>>;
}

pub(crate) struct PackageCompleter<'a> {
    cwd: PathBuf,
    roots: &'a InvocationRoots,
    cell_configs: Arc<BuckConfigBasedCells>,
    results: PatternResults<'a>,
}

impl<'a> PackageCompleter<'a> {
    pub(crate) async fn new(cwd: &Path, roots: &'a InvocationRoots) -> anyhow::Result<Self> {
        let cell_configs = Arc::new(
            BuckConfigBasedCells::parse_with_config_args(
                &roots.project_root,
                &[],
                ProjectRelativePath::empty(),
            )
            .await?,
        );
        Ok(Self {
            cwd: cwd.to_path_buf(),
            roots,
            cell_configs: cell_configs.clone(),
            results: PatternResults::new(roots, cell_configs.clone()),
        })
    }

    /// Complete the package portion of a partial pattern.
    ///
    /// Returns a collection of possible completions, each generally including the
    /// partial pattern. The partial pattern might not be returned as-is when
    /// completion logic is able to unambiguously normalize the partial pattern,
    /// such as partials which cross cell boundaries. In this case, normalized
    /// completion(s) are returned.
    pub(crate) async fn complete(mut self, given_path: &str) -> anyhow::Result<Vec<String>> {
        if given_path == "/" {
            self.results.insert_dir(&self.roots.cell_root, "//").await;
        } else {
            if given_path == "//" {
                self.results
                    .insert_package_if_buildfile_exists(&self.roots.cell_root, "//")
                    .await;
            } else {
                if given_path == "" {
                    self.results
                        .insert_path(
                            &BuckPath::new(&self.cell_configs.cell_resolver, &self.cwd, "//")
                                .await?,
                        )
                        .await;
                }
                self.complete_partial_cells(given_path).await?;
            }

            let path =
                BuckPath::new(&self.cell_configs.cell_resolver, &self.cwd, given_path).await?;
            if path.given() != given_path && completes_to_dir(&self.cwd, &path)? {
                // There are potential completions to this string, but we're
                // transforming it on the first tab to minimize surprise and help
                // the user learn to type paths the right way
                self.results.insert(path.given());
            } else {
                self.complete_dir(&path).await?;
            }
        }
        Ok(self.results.to_vec())
    }

    async fn complete_partial_cells(&mut self, given_path: &str) -> Result<(), anyhow::Error> {
        let cell_resolver = &self.cell_configs.cell_resolver;
        let alias_resolver = self
            .cell_configs
            .get_cell_alias_resolver_for_cwd_fast(
                &self.roots.project_root,
                &self
                    .roots
                    .project_root
                    .relativize(&AbsNormPath::new(&self.cwd)?)?,
            )
            .await?;
        for (cell_alias, cell_name) in alias_resolver.mappings() {
            let canonical_cell_root = cell_alias.as_str().to_owned() + "//";
            let cell = cell_resolver.get(cell_name)?;
            if canonical_cell_root.starts_with(given_path) {
                let cell_abs_path = self
                    .roots
                    .project_root
                    .root()
                    .join_normalized(cell.path().as_project_relative_path())?;
                if canonical_cell_root == given_path {
                    self.results
                        .insert_package_if_buildfile_exists(&cell_abs_path, &canonical_cell_root)
                        .await;
                } else {
                    self.results
                        .insert_dir(&cell_abs_path, &canonical_cell_root)
                        .await;
                }
            }
        }
        Ok(())
    }

    async fn complete_dir(&mut self, partial: &BuckPath) -> CommandOutcome<()> {
        if partial.is_full_dir() {
            self.complete_subdirs(partial).await
        } else {
            self.complete_dir_fragment(partial).await
        }
    }

    async fn complete_subdirs(&mut self, partial: &BuckPath) -> CommandOutcome<()> {
        let partial_dir = partial.abs_path();

        let given_dir = partial.given();

        for entry in partial_dir.read_dir()?.flatten() {
            if entry.path().is_dir() {
                let path = BuckPath::new(
                    &self.cell_configs.cell_resolver,
                    &self.cwd,
                    &(given_dir.to_owned() + &file_name_string(&entry)),
                )
                .await?;
                if path.cell_name() == partial.cell_name() {
                    self.results.insert_path(&path).await;
                }
            }
        }
        CommandOutcome::Success(())
    }

    async fn complete_dir_fragment(&mut self, partial: &BuckPath) -> CommandOutcome<()> {
        let partial_path = partial.abs_path();
        let partial_base = partial_path.file_name().unwrap().to_str().unwrap();

        let given_dir = &partial.given()[..partial.given().len() - partial_base.len()];

        let mut scan_dir = self.cwd.to_path_buf();
        if let Some(offset_dir) = partial_path.parent() {
            scan_dir = scan_dir.join(offset_dir);
        }

        for entry_result in scan_dir.read_dir()? {
            let entry = entry_result?;
            if entry.path().is_dir() && file_name_string(&entry).starts_with(partial_base) {
                let given_expanded = BuckPath::new(
                    &self.cell_configs.cell_resolver,
                    &self.cwd,
                    &(given_dir.to_owned() + &file_name_string(&entry)),
                )
                .await?;
                self.results.insert_path(&given_expanded).await;
            }
        }
        CommandOutcome::Success(())
    }
}

pub(crate) struct TargetCompleter<'a> {
    cwd: PathBuf,
    cell_configs: Arc<BuckConfigBasedCells>,
    target_resolver: &'a mut dyn TargetsResolver,
    results: PatternResults<'a>,
}

impl<'a> TargetCompleter<'a> {
    pub(crate) async fn new(
        cwd: &Path,
        roots: &'a InvocationRoots,
        target_resolver: &'a mut dyn TargetsResolver,
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
            cwd: cwd.to_path_buf(),
            cell_configs: cell_configs.clone(),
            target_resolver,
            results: PatternResults::new(roots, cell_configs.clone()),
        })
    }

    /// Complete the target in a partial pattern.
    ///
    /// Returns a collection of possible completions, each including the partial
    /// pattern.
    pub(crate) async fn complete(
        mut self,
        given_path: &str,
        partial_target: &str,
    ) -> CommandOutcome<Vec<String>> {
        let path = BuckPath::new(&self.cell_configs.cell_resolver, &self.cwd, given_path).await?;
        let response = self
            .target_resolver
            .resolve(TargetsRequest {
                target_patterns: vec![path.given().to_owned() + ":"],
                output_format: OutputFormat::Text.into(),
                // FIXME -- is it safe to take defaults?
                targets: Some(targets_request::Targets::Other(targets_request::Other {
                    ..Default::default()
                })),

                ..Default::default()
            })
            .await??;

        // The response comes back one per line, plus a trailing newline
        let pattern_text = response.serialized_targets_output;
        for pattern in pattern_text.split('\n') {
            if pattern != "" {
                let mut pattern_parts = pattern.split(':');
                let target = pattern_parts.next_back().unwrap();
                if target.starts_with(partial_target) {
                    let completion = path.given().to_owned() + ":" + target;
                    self.results.insert(&completion);
                }
            }
        }

        CommandOutcome::Success(self.results.to_vec())
    }
}

fn completes_to_dir(cwd: &Path, partial: &BuckPath) -> anyhow::Result<bool> {
    let partial_path = partial.abs_path();
    let partial_base = partial_path.file_name().unwrap().to_str().unwrap();

    let mut scan_dir = cwd.to_path_buf();
    if let Some(offset_dir) = partial_path.parent() {
        scan_dir = scan_dir.join(offset_dir);
    }

    for entry_result in scan_dir.read_dir()? {
        let entry = entry_result?;
        if entry.path().is_dir() && file_name_string(&entry).starts_with(partial_base) {
            return Ok(true);
        }
    }
    Ok(false)
}

fn file_name_string(entry: &std::fs::DirEntry) -> String {
    entry.file_name().into_string().unwrap()
}

struct PatternResults<'a> {
    roots: &'a InvocationRoots,
    cell_configs: Arc<BuckConfigBasedCells>,
    results: BTreeSet<String>,
}

impl<'a> PatternResults<'a> {
    fn new(roots: &'a InvocationRoots, cell_configs: Arc<BuckConfigBasedCells>) -> Self {
        Self {
            roots,
            cell_configs,
            results: BTreeSet::<String>::new(),
        }
    }

    fn insert(&mut self, pattern: &str) -> &mut Self {
        self.results.insert(pattern.to_owned());
        self
    }

    async fn insert_path(&mut self, path: &BuckPath) -> &mut Self {
        self.insert_dir(&path.abs_path(), path.given()).await
    }

    async fn insert_dir(&mut self, abs_dir: &AbsNormPath, nickname: &str) -> &mut Self {
        if nickname.ends_with("//") {
            self.insert(nickname);
            self.insert_package_if_buildfile_exists(abs_dir, nickname)
                .await;
        } else if nickname.ends_with('/') {
            self.insert(nickname);
        } else {
            self.insert(&format!("{}/", nickname));
            self.insert_package_if_buildfile_exists(abs_dir, nickname)
                .await;
        }
        self
    }

    async fn insert_package_if_buildfile_exists(
        &mut self,
        abs_dir: &AbsNormPath,
        nickname: &str,
    ) -> &mut Self {
        for f in self.buildfile_names(abs_dir).await.unwrap() {
            if abs_dir.join(f).exists() {
                self.insert(&format!("{}:", nickname));
                break;
            }
        }
        self
    }

    async fn buildfile_names(&mut self, abs_dir: &AbsNormPath) -> anyhow::Result<Vec<FileNameBuf>> {
        let cell_relative_to_project = self.roots.project_root.relativize(abs_dir)?;
        let cell_configs = &self.cell_configs;
        let cell_name = cell_configs.cell_resolver.find(&cell_relative_to_project)?;
        let config = cell_configs
            .parse_single_cell(cell_name, &self.roots.project_root)
            .await?;
        parse_buildfile_name(&config)
    }

    fn to_vec(self) -> Vec<String> {
        self.results.iter().map(String::from).collect()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use buck2_common::invocation_roots::find_invocation_roots;
    use futures::future;

    use super::*;

    struct FakeTargetsResolver {
        target_responses: HashMap<String, String>,
    }
    impl FakeTargetsResolver {
        fn new() -> Self {
            FakeTargetsResolver {
                target_responses: HashMap::<String, String>::new(),
            }
        }
        fn add_response(&mut self, request: &str, response: &str) {
            self.target_responses
                .insert(request.to_owned(), response.to_owned());
        }
    }
    impl TargetsResolver for FakeTargetsResolver {
        fn resolve(
            &mut self,
            req: TargetsRequest,
        ) -> BoxFuture<anyhow::Result<CommandOutcome<TargetsResponse>>> {
            let res = TargetsResponse {
                serialized_targets_output: match self.target_responses.get(&req.target_patterns[0])
                {
                    Some(targets) => targets.to_owned(),
                    None => "".to_owned(),
                },

                ..Default::default()
            };
            Box::pin(future::ok(CommandOutcome::Success(res)))
        }
    }

    fn paths_to_test_data() -> &'static [&'static str] {
        &[
            "fbcode/buck2/app/buck2_client/test_data",
            "app/buck2_client/test_data",
            "test_data",
        ]
    }

    fn in_dir(d: &str) -> anyhow::Result<(InvocationRoots, PathBuf)> {
        let cwd = std::env::current_dir().unwrap();

        let mut candidate = PathBuf::new();
        for path in paths_to_test_data() {
            candidate = cwd.join(path).join(d);
            if candidate.exists() {
                break;
            }
        }

        assert!(candidate.exists(), "test_data directory not found");
        Ok((find_invocation_roots(&candidate)?, candidate))
    }

    fn in_root() -> anyhow::Result<(InvocationRoots, PathBuf)> {
        let cwd = std::env::current_dir().unwrap();

        let mut candidate = PathBuf::new();
        for path in paths_to_test_data() {
            candidate = cwd.join(path);
            if candidate.exists() {
                break;
            }
        }

        assert!(candidate.exists(), "test_data directory not found");
        Ok((find_invocation_roots(&candidate)?, candidate))
    }

    async fn target_complete_helper(
        uut: TargetCompleter<'_>,
        partial_pattern: &str,
    ) -> CommandOutcome<Vec<String>> {
        match partial_pattern.split(':').collect::<Vec<_>>()[..] {
            [pattern, partial_target] => uut.complete(pattern, partial_target).await,
            _ => panic!("unexpected pattern {}", partial_pattern),
        }
    }

    #[tokio::test]
    async fn test_expands_top_level_directory() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("bare").await?;

        assert_eq!(actual, vec!["baredir0/"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_expands_subdirectory() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("baredir0/bare").await?;

        assert_eq!(actual, vec!["baredir0/baredir0a/"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_expands_subdirectory_with_buck_targets() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("baredir0/buck").await?;

        assert_eq!(actual, vec!["baredir0/buckdir0b/", "baredir0/buckdir0b:",]);
        Ok(())
    }

    #[tokio::test]
    async fn test_provides_subdirectory_and_target_alternatives() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("baredir0/buckdir0b").await?;

        assert_eq!(actual, vec!["baredir0/buckdir0b/", "baredir0/buckdir0b:",]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_partial_paths_and_matched_target_dirs() -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("baredir0")?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("b").await?;

        assert_eq!(actual, vec!["baredir0a/", "buckdir0b/", "buckdir0b:",]);
        Ok(())
    }

    #[tokio::test]
    async fn test_handles_degenerate_buck_directory_with_no_targets() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let mut target_resolver = FakeTargetsResolver::new();
        target_resolver.add_response("baredir0:", "");
        let uut = TargetCompleter::new(&cwd, &roots, &mut target_resolver).await?;

        let actual = target_complete_helper(uut, "baredir0:").await?;

        let expected: Vec<String> = vec![];
        assert_eq!(actual, expected);
        Ok(())
    }

    #[tokio::test]
    async fn test_provides_targets_for_path_ending_with_a_colon() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let mut target_resolver = FakeTargetsResolver::new();
        target_resolver.add_response(
            "baredir0/buckdir0b:",
            &([
                "root//baredir0/buckdir0b:target1",
                "root//baredir0/buckdir0b:target2",
                "root//baredir0/buckdir0b:target3",
                "",
            ]
            .join("\n")),
        );
        let uut = TargetCompleter::new(&cwd, &roots, &mut target_resolver).await?;

        let actual = target_complete_helper(uut, "baredir0/buckdir0b:").await?;

        assert_eq!(
            actual,
            vec![
                "baredir0/buckdir0b:target1",
                "baredir0/buckdir0b:target2",
                "baredir0/buckdir0b:target3",
            ]
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_provides_targets_in_nested_cell() -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("cell1")?;
        let mut target_resolver = FakeTargetsResolver::new();
        target_resolver.add_response(
            "buck2:",
            &([
                "cell1//buck2:buck2",
                "cell1//buck2:symlinked_buck2_and_tpx",
                "",
            ]
            .join("\n")),
        );
        let uut = TargetCompleter::new(&cwd, &roots, &mut target_resolver).await?;

        let actual = target_complete_helper(uut, "buck2:").await?;

        assert_eq!(
            actual,
            vec!["buck2:buck2", "buck2:symlinked_buck2_and_tpx",]
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_a_partial_target() -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("cell1")?;
        let mut target_resolver = FakeTargetsResolver::new();
        target_resolver.add_response(
            "buck2:",
            &([
                "cell1//buck2:buck2",
                "cell1//buck2:symlinked_buck2_and_tpx",
                "",
            ]
            .join("\n")),
        );
        let uut = TargetCompleter::new(&cwd, &roots, &mut target_resolver).await?;

        let actual = target_complete_helper(uut, "buck2:bu").await?;

        assert_eq!(actual, vec!["buck2:buck2",]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_cell_name_when_given_cell_root() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("cel").await?;

        assert_eq!(
            actual,
            vec![
                "cell1//",
                "cell1//:",
                "cell2//",
                "cell3a//",
                "cell3a//:",
                "cell3b//"
            ]
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_cell_name_when_given_cell_root_as_directory() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("cell1/").await?;

        assert_eq!(actual, vec!["cell1//", "cell1//:"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_identifies_non_local_cell_name() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("cell3a").await?;

        assert_eq!(actual, vec!["cell3a//", "cell3a//:"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_non_local_cell_name_when_provided_with_a_single_slash()
    -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("cell3b/").await?;

        assert_eq!(actual, vec!["cell3b//"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_non_local_cell_name() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("cell3").await?;

        assert_eq!(actual, vec!["cell3a//", "cell3a//:", "cell3b//"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_mix_of_non_local_cell_name_and_local_dirs() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("cell").await?;

        assert_eq!(
            actual,
            vec![
                "cell1//",
                "cell1//:",
                "cell2//",
                "cell3a//",
                "cell3a//:",
                "cell3b//"
            ]
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_multiple_partial_dirs() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("baredir0/b").await?;

        assert_eq!(
            actual,
            vec![
                "baredir0/baredir0a/",
                "baredir0/buckdir0b/",
                "baredir0/buckdir0b:"
            ]
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_as_both_directory_and_target_root() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("baredir0/buckdir0b").await?;

        assert_eq!(actual, vec!["baredir0/buckdir0b/", "baredir0/buckdir0b:",]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_subdirs_as_well_as_target_colon_for_fully_qualified_cell()
    -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("cell1//").await?;

        assert_eq!(actual, vec!["cell1//:", "cell1//buck2/", "cell1//buck2:"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_cell_correctly_from_a_different_cell() -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("cell2")?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("cell1//").await?;

        assert_eq!(actual, vec!["cell1//:", "cell1//buck2/", "cell1//buck2:"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_targets_for_fully_qualified_cell() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let mut target_resolver = FakeTargetsResolver::new();
        target_resolver.add_response("cell1//:", &(["cell1//:target1", ""].join("\n")));
        let uut = TargetCompleter::new(&cwd, &roots, &mut target_resolver).await?;

        let actual = target_complete_helper(uut, "cell1//:").await?;

        assert_eq!(actual, vec!["cell1//:target1"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_directory_in_different_cell_to_canonical_name() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("cell1/buck2").await?;

        assert_eq!(actual, vec!["cell1//buck2"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_partial_dir_in_different_cell_to_canonical_name() -> anyhow::Result<()>
    {
        let (roots, cwd) = in_root()?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("cell1/b").await?;

        assert_eq!(actual, vec!["cell1//b"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_canonical_path_in_other_cell_to_with_slash_or_colon()
    -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("cell1//buck2").await?;

        assert_eq!(actual, vec!["cell1//buck2/", "cell1//buck2:"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_canonical_path_to_other_cell_from_subdirectory_in_this_cell()
    -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("baredir0")?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("cell1//buck2").await?;

        assert_eq!(actual, vec!["cell1//buck2/", "cell1//buck2:"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_other_cell_canonical_path_targets_from_subdirectory_in_this_cell()
    -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("baredir0")?;
        let mut target_resolver = FakeTargetsResolver::new();
        target_resolver.add_response(
            "cell1//buck2:",
            &([
                "cell1//buck2:buck2",
                "cell1//buck2:symlinked_buck2_and_tpx",
                "",
            ]
            .join("\n")),
        );
        let uut = TargetCompleter::new(&cwd, &roots, &mut target_resolver).await?;

        let actual = target_complete_helper(uut, "cell1//buck2:").await?;

        assert_eq!(
            actual,
            vec!["cell1//buck2:buck2", "cell1//buck2:symlinked_buck2_and_tpx"]
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_in_subdirectory_can_complete_subdirs_of_project_root_with_canonical_cell()
    -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("baredir0")?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("root//").await?;

        assert_eq!(actual, vec!["root//baredir0/", "root//dir3/",]);
        Ok(())
    }

    #[tokio::test]
    async fn test_backwards_path_to_different_cell_root_directory_completes_to_canonical_cell_name()
    -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("baredir0")?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("../cell1").await?;

        assert_eq!(actual, vec!["cell1//"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_partial_subdirectory_name_expands_to_all_matches() -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("baredir0")?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("b").await?;

        assert_eq!(actual, vec!["baredir0a/", "buckdir0b/", "buckdir0b:",]);
        Ok(())
    }

    #[tokio::test]
    async fn test_expands_cell_to_canonical_in_middle_of_input_text_with_target_colon()
    -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let mut target_resolver = FakeTargetsResolver::new();
        target_resolver.add_response(
            "cell1//buck2:",
            &([
                "cell1//buck2:buck2",
                "cell1//buck2:symlinked_buck2_and_tpx",
                "",
            ]
            .join("\n")),
        );
        let uut = TargetCompleter::new(&cwd, &roots, &mut target_resolver).await?;

        let actual = target_complete_helper(uut, "cell1/buck2:").await?;

        assert_eq!(
            actual,
            vec!["cell1//buck2:buck2", "cell1//buck2:symlinked_buck2_and_tpx",]
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_expands_cell_to_canonical_in_middle_of_input_text_with_partial_target()
    -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let mut target_resolver = FakeTargetsResolver::new();
        target_resolver.add_response(
            "cell1//buck2:",
            &([
                "cell1//buck2:buck2",
                "cell1//buck2:symlinked_buck2_and_tpx",
                "",
            ]
            .join("\n")),
        );
        let uut = TargetCompleter::new(&cwd, &roots, &mut target_resolver).await?;

        let actual = target_complete_helper(uut, "cell1/buck2:bu").await?;

        assert_eq!(actual, vec!["cell1//buck2:buck2"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_expands_targets_for_a_bare_colon_in_a_buck_directory() -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("cell1/buck2")?;
        let mut target_resolver = FakeTargetsResolver::new();
        target_resolver.add_response(
            ":",
            &([
                "cell1//buck2:buck2",
                "cell1//buck2:symlinked_buck2_and_tpx",
                "",
            ]
            .join("\n")),
        );
        let uut = TargetCompleter::new(&cwd, &roots, &mut target_resolver).await?;

        let actual = target_complete_helper(uut, ":").await?;

        assert_eq!(actual, vec![":buck2", ":symlinked_buck2_and_tpx",]);
        Ok(())
    }

    #[tokio::test]
    async fn test_returns_nothing_for_a_bare_colon_in_a_non_buck_directory() -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("baredir0")?;
        let mut target_resolver = FakeTargetsResolver::new();
        let uut = TargetCompleter::new(&cwd, &roots, &mut target_resolver).await?;

        let actual = target_complete_helper(uut, ":").await?;

        assert_eq!(actual.len(), 0);
        Ok(())
    }

    #[tokio::test]
    async fn test_empty_string_completes_both_cells_and_dirs() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("").await?;

        assert_eq!(
            actual,
            vec![
                "//",
                "baredir0/",
                "cell1//",
                "cell1//:",
                "cell2//",
                "cell3a//",
                "cell3a//:",
                "cell3b//",
                "dir3/",
                "prelude//",
                "root//"
            ]
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_single_slash_to_double_slash_in_root() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("/").await?;

        assert_eq!(actual, vec!["//"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_root_children_of_double_slash() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("//").await?;

        assert_eq!(actual, vec!["//baredir0/", "//dir3/"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_single_slash_to_double_slash_in_sub_cell() -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("cell1")?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("/").await?;

        assert_eq!(actual, vec!["//", "//:"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_cell_children_of_double_slash() -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("cell1")?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("//").await?;

        assert_eq!(actual, vec!["//:", "//buck2/", "//buck2:"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_normalizes_absolute_paths() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;
        let absolute_partial = cwd.join("bare");

        let actual = uut.complete(absolute_partial.to_str().unwrap()).await?;

        assert_eq!(actual, vec!["root//bare"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_bails_on_nonexistent_path() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("nonexistent").await?;

        assert_eq!(actual.len(), 0);
        Ok(())
    }

    #[tokio::test]
    async fn test_bails_on_nonexistent_path_in_subcell() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("cell1//nonexistent").await?;

        assert_eq!(actual.len(), 0);
        Ok(())
    }

    #[tokio::test]
    async fn test_package_completion_completes_packages_with_cell_aliases_as_aliases()
    -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("cell1/buck2/prelude")?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("cell1_alias//b").await?;

        assert_eq!(actual, vec!["cell1_alias//buck2/", "cell1_alias//buck2:"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_package_completion_only_uses_aliases_in_cells_definining_them()
    -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("cell1/buck2")?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual_result = uut.complete("cell1_alias//b").await;

        assert!(actual_result.is_err());
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_cell_aliases_alongside_cells() -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("cell1/buck2/prelude")?;
        let uut = PackageCompleter::new(&cwd, &roots).await?;

        let actual = uut.complete("cell1").await?;

        assert_eq!(
            actual,
            vec!["cell1//", "cell1//:", "cell1_alias//", "cell1_alias//:"]
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_target_completion_works_correctly_with_aliased_cells() -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("cell1/buck2/prelude")?;
        let mut target_resolver = FakeTargetsResolver::new();
        target_resolver.add_response(
            "cell1_alias//buck2:",
            &([
                "cell1//buck2:buck2",
                "cell1//buck2:symlinked_buck2_and_tpx",
                "",
            ]
            .join("\n")),
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
        let mut target_resolver = FakeTargetsResolver::new();
        target_resolver.add_response(
            "cell1_alias//buck2:",
            &([
                "cell1//buck2:buck2",
                "cell1//buck2:symlinked_buck2_and_tpx",
                "",
            ]
            .join("\n")),
        );
        let uut = TargetCompleter::new(&cwd, &roots, &mut target_resolver).await?;

        match target_complete_helper(uut, "cell1_alias//buck2:").await {
            CommandOutcome::Success(_) => panic!("Expected error"),
            CommandOutcome::Failure(_) => Ok(()),
        }
    }
}
