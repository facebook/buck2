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

/// Complete the package portion of a partial pattern.
///
/// Returns a collection of possible completions, each generally including the
/// partial pattern. The partial pattern might not be returned as-is when
/// completion logic is able to unambiguously normalize the partial pattern,
/// such as partials which cross cell boundaries. In this case, normalized
/// completion(s) are returned.
pub(crate) async fn complete_package(
    roots: &InvocationRoots,
    cwd: &Path,
    given_partial: &str,
) -> anyhow::Result<Vec<String>> {
    let mut results = PatternResults::new(roots);

    if given_partial == "/" {
        results.insert_dir(&roots.cell_root, "//").await;
    } else {
        if given_partial == "//" {
            results
                .insert_package_if_buildfile_exists(&roots.cell_root, "//")
                .await;
        } else {
            if given_partial == "" {
                results.insert_path(&BuckPath::new(cwd, "//").await?).await;
            }

            insert_partially_complete_cells(&mut results, roots, given_partial).await?;
        }

        let partial = BuckPath::new(cwd, given_partial).await?;
        if partial.given() != given_partial && completes_to_dir(cwd, &partial)? {
            // There are potential completions to this string, but we're
            // transforming it on the first tab to minimize surprise and help
            // the user learn to type paths the right way
            results.insert(partial.given());
        } else {
            complete_dir(&mut results, cwd, &partial).await?;
        }
    }
    Ok(results.out().into())
}

async fn insert_partially_complete_cells(
    results: &mut PatternResults<'_>,
    roots: &InvocationRoots,
    given_partial: &str,
) -> anyhow::Result<()> {
    let cell_resolver = BuckConfigBasedCells::parse_with_config_args(
        &roots.project_root,
        &[],
        ProjectRelativePath::empty(),
    )
    .await?
    .cell_resolver;

    for (cell_name, cell) in cell_resolver.cells() {
        let canonical_cell_root = format!("{}//", cell_name.as_str());
        if canonical_cell_root.starts_with(given_partial) {
            let cell_abs_path = roots
                .project_root
                .root()
                .join_normalized(cell.path().as_project_relative_path())?;
            if canonical_cell_root == given_partial {
                results
                    .insert_package_if_buildfile_exists(&cell_abs_path, &canonical_cell_root)
                    .await;
            } else {
                results
                    .insert_dir(&cell_abs_path, &canonical_cell_root)
                    .await;
            }
        }
    }
    Ok(())
}

/// Complete the target in a partial pattern.
///
/// Returns a collection of possible completions, each including the partial
/// pattern.
pub(crate) async fn complete_target(
    roots: &InvocationRoots,
    target_resolver: &mut dyn TargetsResolver,
    cwd: &Path,
    given_path: &str,
    partial_target: &str,
) -> CommandOutcome<Vec<String>> {
    let path = BuckPath::new(cwd, given_path).await?;
    let response = target_resolver
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

    let mut ret = PatternResults::new(roots);

    // The response comes back one per line, plus a trailing newline
    let pattern_text = response.serialized_targets_output;
    for pattern in pattern_text.split('\n') {
        if pattern != "" {
            let mut pattern_parts = pattern.split(':');
            let target = pattern_parts.next_back().unwrap();
            if target.starts_with(partial_target) {
                let completion = path.given().to_owned() + ":" + target;
                ret.insert(&completion);
            }
        }
    }

    CommandOutcome::Success(ret.out())
}

async fn complete_dir(
    results: &mut PatternResults<'_>,
    cwd: &Path,
    partial: &BuckPath,
) -> CommandOutcome<()> {
    if partial.is_full_dir() {
        complete_subdirs(results, cwd, partial).await
    } else {
        complete_dir_fragment(results, cwd, partial).await
    }
}

async fn complete_subdirs(
    results: &mut PatternResults<'_>,
    cwd: &Path,
    partial: &BuckPath,
) -> CommandOutcome<()> {
    let partial_dir = partial.abs_path();

    let given_dir = partial.given();

    for entry in partial_dir.read_dir()?.flatten() {
        if entry.path().is_dir() {
            let path =
                BuckPath::new(cwd, &(given_dir.to_owned() + &file_name_string(&entry))).await?;
            if path.cell_name() == partial.cell_name() {
                results.insert_path(&path).await;
            }
        }
    }
    CommandOutcome::Success(())
}

async fn complete_dir_fragment(
    results: &mut PatternResults<'_>,
    cwd: &Path,
    partial: &BuckPath,
) -> CommandOutcome<()> {
    let partial_path = partial.abs_path();
    let partial_base = partial_path.file_name().unwrap().to_str().unwrap();

    let given_dir = &partial.given()[..partial.given().len() - partial_base.len()];

    let mut scan_dir = cwd.to_path_buf();
    if let Some(offset_dir) = partial_path.parent() {
        scan_dir = scan_dir.join(offset_dir);
    }

    for entry_result in scan_dir.read_dir()? {
        let entry = entry_result?;
        if entry.path().is_dir() && file_name_string(&entry).starts_with(partial_base) {
            let given_expanded =
                BuckPath::new(cwd, &(given_dir.to_owned() + &file_name_string(&entry))).await?;
            results.insert_path(&given_expanded).await;
        }
    }
    CommandOutcome::Success(())
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
    out: BTreeSet<String>,
    roots: &'a InvocationRoots,
}

impl<'a> PatternResults<'a> {
    fn new(roots: &'a InvocationRoots) -> Self {
        PatternResults {
            out: BTreeSet::<String>::new(),
            roots,
        }
    }

    fn out(self) -> Vec<String> {
        self.out.into_iter().collect()
    }

    fn insert(&mut self, pattern: &str) -> &mut Self {
        self.out.insert(pattern.to_owned());
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

    async fn buildfile_names(&self, abs_dir: &AbsNormPath) -> anyhow::Result<Vec<FileNameBuf>> {
        let project_root = &self.roots.project_root;
        let cell_relative_to_project = project_root.relativize(abs_dir)?;
        let cell_configs = BuckConfigBasedCells::parse_with_config_args(
            project_root,
            &[],
            &cell_relative_to_project,
        )
        .await?;
        let cell_name = cell_configs.cell_resolver.find(&cell_relative_to_project)?;
        let config = cell_configs
            .parse_single_cell(cell_name, project_root)
            .await?;
        parse_buildfile_name(&config)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::path::PathBuf;

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

    async fn complete_target_helper(
        roots: &InvocationRoots,
        resolver: &mut dyn TargetsResolver,
        cwd: &Path,
        partial_pattern: &str,
    ) -> CommandOutcome<Vec<String>> {
        match partial_pattern.split(':').collect::<Vec<_>>()[..] {
            [pattern, partial_target] => {
                complete_target(&roots, resolver, cwd, pattern, partial_target).await
            }
            _ => panic!("unexpected pattern {}", partial_pattern),
        }
    }

    #[tokio::test]
    async fn test_expands_top_level_directory() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;

        let actual = complete_package(&roots, &cwd, "bare").await?;

        assert_eq!(actual, vec!["baredir0/"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_expands_subdirectory() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;

        let actual = complete_package(&roots, &cwd, "baredir0/bare").await?;

        assert_eq!(actual, vec!["baredir0/baredir0a/"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_expands_subdirectory_with_buck_targets() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;

        let actual = complete_package(&roots, &cwd, "baredir0/buck").await?;

        assert_eq!(actual, vec!["baredir0/buckdir0b/", "baredir0/buckdir0b:",]);
        Ok(())
    }

    #[tokio::test]
    async fn test_provides_subdirectory_and_target_alternatives() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;

        let actual = complete_package(&roots, &cwd, "baredir0/buckdir0b").await?;

        assert_eq!(actual, vec!["baredir0/buckdir0b/", "baredir0/buckdir0b:",]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_partial_paths_and_matched_target_dirs() -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("baredir0")?;

        let actual = complete_package(&roots, &cwd, "b").await?;

        assert_eq!(actual, vec!["baredir0a/", "buckdir0b/", "buckdir0b:",]);
        Ok(())
    }

    #[tokio::test]
    async fn test_handles_degenerate_buck_directory_with_no_targets() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let mut target_resolver = FakeTargetsResolver::new();
        target_resolver.add_response("baredir0:", "");

        let actual =
            complete_target_helper(&roots, &mut target_resolver, &cwd, "baredir0:").await?;

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

        let actual =
            complete_target_helper(&roots, &mut target_resolver, &cwd, "baredir0/buckdir0b:")
                .await?;

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

        let actual = complete_target_helper(&roots, &mut target_resolver, &cwd, "buck2:").await?;

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

        let actual = complete_target_helper(&roots, &mut target_resolver, &cwd, "buck2:bu").await?;

        assert_eq!(actual, vec!["buck2:buck2",]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_cell_name_when_given_cell_root() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;

        let actual = complete_package(&roots, &cwd, "cel").await?;

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

        let actual = complete_package(&roots, &cwd, "cell1/").await?;

        assert_eq!(actual, vec!["cell1//", "cell1//:"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_identifies_non_local_cell_name() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;

        let actual = complete_package(&roots, &cwd, "cell3a").await?;

        assert_eq!(actual, vec!["cell3a//", "cell3a//:"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_non_local_cell_name_when_provided_with_a_single_slash()
    -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;

        let actual = complete_package(&roots, &cwd, "cell3b/").await?;

        assert_eq!(actual, vec!["cell3b//"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_non_local_cell_name() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;

        let actual = complete_package(&roots, &cwd, "cell3").await?;

        assert_eq!(actual, vec!["cell3a//", "cell3a//:", "cell3b//"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_mix_of_non_local_cell_name_and_local_dirs() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;

        let actual = complete_package(&roots, &cwd, "cell").await?;

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

        let actual = complete_package(&roots, &cwd, "baredir0/b").await?;

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

        let actual = complete_package(&roots, &cwd, "baredir0/buckdir0b").await?;

        assert_eq!(actual, vec!["baredir0/buckdir0b/", "baredir0/buckdir0b:",]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_subdirs_as_well_as_target_colon_for_fully_qualified_cell()
    -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;

        let actual = complete_package(&roots, &cwd, "cell1//").await?;

        assert_eq!(actual, vec!["cell1//:", "cell1//buck2/", "cell1//buck2:"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_cell_correctly_from_a_different_cell() -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("cell2")?;

        let actual = complete_package(&roots, &cwd, "cell1//").await?;

        assert_eq!(actual, vec!["cell1//:", "cell1//buck2/", "cell1//buck2:"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_targets_for_fully_qualified_cell() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let mut target_resolver = FakeTargetsResolver::new();
        target_resolver.add_response("cell1//:", &(["cell1//:target1", ""].join("\n")));

        let actual = complete_target_helper(&roots, &mut target_resolver, &cwd, "cell1//:").await?;

        assert_eq!(actual, vec!["cell1//:target1"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_directory_in_different_cell_to_canonical_name() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;

        let actual = complete_package(&roots, &cwd, "cell1/buck2").await?;

        assert_eq!(actual, vec!["cell1//buck2"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_partial_dir_in_different_cell_to_canonical_name() -> anyhow::Result<()>
    {
        let (roots, cwd) = in_root()?;

        let actual = complete_package(&roots, &cwd, "cell1/b").await?;

        assert_eq!(actual, vec!["cell1//b"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_canonical_path_in_other_cell_to_with_slash_or_colon()
    -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;

        let actual = complete_package(&roots, &cwd, "cell1//buck2").await?;

        assert_eq!(actual, vec!["cell1//buck2/", "cell1//buck2:"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_canonical_path_to_other_cell_from_subdirectory_in_this_cell()
    -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("baredir0")?;

        let actual = complete_package(&roots, &cwd, "cell1//buck2").await?;

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

        let actual =
            complete_target_helper(&roots, &mut target_resolver, &cwd, "cell1//buck2:").await?;

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

        let actual = complete_package(&roots, &cwd, "root//").await?;

        assert_eq!(actual, vec!["root//baredir0/", "root//dir3/",]);
        Ok(())
    }

    #[tokio::test]
    async fn test_backwards_path_to_different_cell_root_directory_completes_to_canonical_cell_name()
    -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("baredir0")?;

        let actual = complete_package(&roots, &cwd, "../cell1").await?;

        assert_eq!(actual, vec!["cell1//"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_partial_subdirectory_name_expands_to_all_matches() -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("baredir0")?;

        let actual = complete_package(&roots, &cwd, "b").await?;

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

        let actual =
            complete_target_helper(&roots, &mut target_resolver, &cwd, "cell1/buck2:").await?;

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

        let actual =
            complete_target_helper(&roots, &mut target_resolver, &cwd, "cell1/buck2:bu").await?;

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

        let actual = complete_target_helper(&roots, &mut target_resolver, &cwd, ":").await?;

        assert_eq!(actual, vec![":buck2", ":symlinked_buck2_and_tpx",]);
        Ok(())
    }

    #[tokio::test]
    async fn test_returns_nothingfor_a_bare_colon_in_a_non_buck_directory() -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("baredir0")?;
        let mut target_resolver = FakeTargetsResolver::new();

        let actual = complete_target_helper(&roots, &mut target_resolver, &cwd, ":").await?;

        assert_eq!(actual.len(), 0);
        Ok(())
    }

    #[tokio::test]
    async fn test_empty_string_completes_both_cells_and_dirs() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;

        let actual = complete_package(&roots, &cwd, "").await?;

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

        let actual = complete_package(&roots, &cwd, "/").await?;

        assert_eq!(actual, vec!["//"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_root_children_of_double_slash() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;

        let actual = complete_package(&roots, &cwd, "//").await?;

        assert_eq!(actual, vec!["//baredir0/", "//dir3/"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_single_slash_to_double_slash_in_sub_cell() -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("cell1")?;

        let actual = complete_package(&roots, &cwd, "/").await?;

        assert_eq!(actual, vec!["//", "//:"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_cell_children_of_double_slash() -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("cell1")?;

        let actual = complete_package(&roots, &cwd, "//").await?;

        assert_eq!(actual, vec!["//:", "//buck2/", "//buck2:"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_normalizes_absolute_paths() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let absolute_partial = cwd.join("bare");

        let actual = complete_package(&roots, &cwd, absolute_partial.to_str().unwrap()).await?;

        assert_eq!(actual, vec!["root//bare"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_bails_on_nonexistent_path() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;

        let actual = complete_package(&roots, &cwd, "nonexistent").await?;

        assert_eq!(actual.len(), 0);
        Ok(())
    }

    #[tokio::test]
    async fn test_bails_on_nonexistent_path_in_subcell() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;

        let actual = complete_package(&roots, &cwd, "cell1//nonexistent").await?;

        assert_eq!(actual.len(), 0);
        Ok(())
    }
}
