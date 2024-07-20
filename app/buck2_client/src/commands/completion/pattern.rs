/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

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
use futures::future::BoxFuture;
use relative_path::RelativePath;

pub(crate) trait TargetsResolver: Send {
    fn resolve(
        &mut self,
        req: TargetsRequest,
    ) -> BoxFuture<anyhow::Result<CommandOutcome<TargetsResponse>>>;
}

/// Does this pattern need target completion?
///
/// Allows upper layers to determine whether to call complete_dir() or
/// complete_target(). This is denormalized because there is threading
/// timeout logic that upper layers must perform.
pub(crate) fn completing_target(partial: &str) -> bool {
    partial.contains(':')
}

/// Complete the target in a partial pattern.
///
/// Returns a collection of possible completions, each including the partial
/// pattern.
pub(crate) async fn complete_target(
    target_resolver: &mut dyn TargetsResolver,
    partial: &str,
) -> CommandOutcome<Vec<String>> {
    let mut partial_parts = partial.split(':');
    let path = partial_parts.next().unwrap();
    let target_fragment = partial_parts.next().unwrap();

    let response = target_resolver
        .resolve(TargetsRequest {
            target_patterns: vec![path.to_owned() + ":"],
            output_format: OutputFormat::Text.into(),
            // FIXME -- is it safe to take defaults?
            targets: Some(targets_request::Targets::Other(targets_request::Other {
                ..Default::default()
            })),

            ..Default::default()
        })
        .await??;

    let mut ret = Vec::<String>::new();

    // The response comes back one per line, plus a trailing newline
    let pattern_text = response.serialized_targets_output;
    for pattern in pattern_text.split('\n') {
        if pattern != "" {
            let mut pattern_parts = pattern.split(':');
            let target = pattern_parts.next_back().unwrap();
            if target.starts_with(target_fragment) {
                let completion = path.to_owned() + ":" + target;
                ret.push(completion.to_owned());
            }
        }
    }

    CommandOutcome::Success(ret)
}

/// Complete the directory portion of a partial pattern.
///
/// Returns a collection of possible completions, each generally including the
/// partial pattern. The partial pattern might not be returned as-is when
/// completion logic is able to unambiguously normalize the partial pattern,
/// such as partials which cross cell boundaries. In this case, normalized
/// completion(s) are returned.
pub(crate) async fn complete_dir(
    roots: &InvocationRoots,
    cwd: &Path,
    partial: &str,
) -> CommandOutcome<Vec<String>> {
    let cwd_abs = AbsNormPath::new(cwd)?;
    let cwd_relative = roots.project_root.relativize(cwd_abs)?;
    let root_config =
        &BuckConfigBasedCells::parse_with_config_args(&roots.project_root, &[], &cwd_relative)
            .await?
            .root_config;
    let buildfile_names = parse_buildfile_name(root_config)?;

    let partial_path = Path::new(partial);
    let partial_base = partial_path.file_name().unwrap().to_str().unwrap();
    let partial_dir = partial_path.parent().unwrap();

    let mut scan_dir = cwd.to_path_buf();
    if let Some(offset_dir) = partial_path.parent() {
        scan_dir = scan_dir.join(offset_dir);
    }

    let mut ret = Vec::<String>::new();
    for entry_result in scan_dir.read_dir()? {
        let entry = entry_result?;
        if entry.path().is_dir() && file_name_string(&entry).starts_with(partial_base) {
            let safe_partial_dir = RelativePath::from_path(&partial_dir)?;
            let dir = safe_partial_dir.join(entry.file_name().to_str().unwrap());
            ret.push(format!("{}/", dir));

            for f in &buildfile_names {
                if cwd.join(dir.to_path("")).join(f).exists() {
                    ret.push(format!("{}:", dir.as_str()));
                    break;
                }
            }
        }
    }
    CommandOutcome::Success(ret)
}

fn file_name_string(entry: &std::fs::DirEntry) -> String {
    entry.file_name().into_string().unwrap()
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

    async fn complete(
        roots: &InvocationRoots,
        target_resolver: &mut dyn TargetsResolver,
        cwd: &Path,
        partial: &str,
    ) -> CommandOutcome<Vec<String>> {
        match completing_target(partial) {
            true => complete_target(target_resolver, partial).await,
            false => complete_dir(&roots, cwd, partial).await,
        }
    }

    #[tokio::test]
    async fn test_expands_top_level_directory() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let mut target_resolver = FakeTargetsResolver::new();

        let actual = complete(&roots, &mut target_resolver, &cwd, "bare").await?;

        assert_eq!(actual, vec!["baredir0/"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_expands_subdirectory() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let mut target_resolver = FakeTargetsResolver::new();

        let actual = complete(&roots, &mut target_resolver, &cwd, "baredir0/bare").await?;

        assert_eq!(actual, vec!["baredir0/baredir0a/"]);
        Ok(())
    }

    #[tokio::test]
    async fn test_expands_subdirectory_with_buck_targets() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let mut target_resolver = FakeTargetsResolver::new();

        let actual = complete(&roots, &mut target_resolver, &cwd, "baredir0/buck").await?;

        assert_eq!(actual, vec!["baredir0/buckdir0b/", "baredir0/buckdir0b:",]);
        Ok(())
    }

    #[tokio::test]
    async fn test_provides_subdirectory_and_target_alternatives() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let mut target_resolver = FakeTargetsResolver::new();

        let actual = complete(&roots, &mut target_resolver, &cwd, "baredir0/buckdir0b").await?;

        assert_eq!(actual, vec!["baredir0/buckdir0b/", "baredir0/buckdir0b:",]);
        Ok(())
    }

    #[tokio::test]
    async fn test_completes_partial_paths_and_matched_target_dirs() -> anyhow::Result<()> {
        let (roots, cwd) = in_dir("baredir0")?;
        let mut target_resolver = FakeTargetsResolver::new();

        let actual = complete(&roots, &mut target_resolver, &cwd, "b").await?;

        assert_eq!(actual, vec!["baredir0a/", "buckdir0b/", "buckdir0b:",]);
        Ok(())
    }

    #[tokio::test]
    async fn test_handles_degenerate_buck_directory_with_no_targets() -> anyhow::Result<()> {
        let (roots, cwd) = in_root()?;
        let mut target_resolver = FakeTargetsResolver::new();
        target_resolver.add_response("baredir0:", "");

        let actual = complete(&roots, &mut target_resolver, &cwd, "baredir0:").await?;

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

        let actual = complete(&roots, &mut target_resolver, &cwd, "baredir0/buckdir0b:").await?;

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

        let actual = complete(&roots, &mut target_resolver, &cwd, "buck2:").await?;

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

        let actual = complete(&roots, &mut target_resolver, &cwd, "buck2:bu").await?;

        assert_eq!(actual, vec!["buck2:buck2",]);
        Ok(())
    }

    // lex_provider_pattern()
    // normalize_package()
    // parse_target_pattern()
    // ParsedPattern::parse_relative()
    // ParsedPattern::parse_relaxed()

    // echo -n 'In ~/fbsource, expect "fbcode" -> "fbcode//" "fbcode//:" ... '
    // echo -n 'In ~/fbsource, expect "fbcode/" -> "fbcode//" "fbcode//:" ... '
    // echo -n 'In ~/fbsource, expect "ovr_config" -> "ovr_config//" ... '
    // echo -n 'In ~/fbsource, expect "ovr_" -> "ovr_config//" ... '
    // echo -n 'In ~/fbsource, expect "ovr" -> "ovr_config//" "ovrsource-legacy/" ... '
    // echo -n 'In ~/fbsource, expect "ovr_config/" -> "ovr_config//" ... '
    // echo -n 'In ~/fbsource, expect "ovr_config//" -> "ovr_config//" "ovr_config//:" "ovr_config//asic/" etc ... '
    // echo -n 'In ~/fbsource, expect "fbcode/buck2" -> "fbcode//buck2/" "fbcode//buck2:" ... '
    // echo -n 'In ~/fbsource, expect "fbcode//buck2" -> "fbcode//buck2/" "fbcode//buck2:" ... '
    // echo -n 'In ~/fbsource/xplat, expect "fbcode//buck2" -> "fbcode//buck2/" "fbcode//buck2:" ... '
    // echo -n 'In ~/fbsource/xplat, expect "fbcode//buck2:" -> fbcode//buck2:buck2" "fbcode//buck2:symlinked_buck2_and_tpx" ... '
    // echo -n 'In ~/fbsource/xplat, expect "fbsource//" -> (subdirs only) ... '
    // echo -n 'In ~/fbsource/xplat, expect "../fbcode" -> "fbcode//" ... '
    // echo -n 'In ~/fbsource/xplat/arfx, expect "desktop" -> "desktop/" "desktop_core/" "desktop_player/" "desktop_player:" ... '
    // # echo -n 'In ~/fbsource, expect "fbcode/buck2:" -> "fbcode//buck2:" ... '
    // # echo -n 'In ~/fbsource, expect "fbcode/buck2:bu" -> "fbcode//buck2:buck2" ... '
    // handles trailing colon in a directory that doesn't have a buck file
    // handles raw ':' by returning partial path
}
