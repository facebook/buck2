/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Write as _;
use std::io::Write as _;
use std::path::Path;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_build_api::calculation::load_patterns;
use buck2_build_api::nodes::hacks::value_to_json;
use buck2_build_api::nodes::lookup::ConfiguredTargetNodeLookup;
use buck2_build_api::nodes::lookup::TargetNodeLookup;
use buck2_cli_proto::targets_request::TargetHashFileMode;
use buck2_cli_proto::targets_request::TargetHashGraphType;
use buck2_cli_proto::TargetsRequest;
use buck2_cli_proto::TargetsResponse;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_core::cells::CellResolver;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::ParsedPattern;
use buck2_core::target::label::TargetLabel;
use buck2_core::target::name::TargetName;
use buck2_interpreter_for_build::interpreter::calculation::InterpreterCalculation;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::nodes::attributes::DEPS;
use buck2_node::nodes::attributes::INPUTS;
use buck2_node::nodes::attributes::PACKAGE;
use buck2_node::nodes::attributes::TARGET_CALL_STACK;
use buck2_node::nodes::attributes::TARGET_HASH;
use buck2_node::nodes::attributes::TYPE;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use dice::DiceTransaction;
use dupe::Dupe;
use dupe::OptionDupedExt;
use futures::stream::FuturesUnordered;
use futures::stream::StreamExt;
use itertools::Itertools;
use regex::RegexSet;

use crate::json::quote_json_string;
use crate::target_hash::BuckTargetHash;
use crate::target_hash::TargetHashes;
use crate::target_hash::TargetHashesFileMode;

struct TargetInfo<'a> {
    node: &'a TargetNode,
    target_hash: Option<BuckTargetHash>,
}

#[allow(unused_variables)]
trait TargetPrinter: Send {
    fn begin(&mut self, output: &mut String) {}
    fn end(&mut self, stats: &Stats, output: &mut String) {}
    /// Called between each target/package_error
    fn separator(&mut self, output: &mut String) {}
    fn target(&mut self, package: PackageLabel, target_info: TargetInfo<'_>, output: &mut String) {}
    fn package_error(&mut self, package: PackageLabel, error: &anyhow::Error, output: &mut String) {
    }
}

struct JsonPrinter {
    attributes: Option<RegexSet>,
    attr_inspect_opts: AttrInspectOptions,
    target_call_stacks: bool,
}

impl TargetPrinter for JsonPrinter {
    fn begin(&mut self, output: &mut String) {
        output.push_str("[\n");
    }
    fn end(&mut self, _stats: &Stats, output: &mut String) {
        output.push_str("\n]\n");
    }
    fn separator(&mut self, output: &mut String) {
        output.push_str(",\n");
    }

    fn target(&mut self, package: PackageLabel, target_info: TargetInfo<'_>, output: &mut String) {
        output.push_str("  {\n");
        let mut first = true;

        fn print_attr(
            this: &mut JsonPrinter,
            output: &mut String,
            first: &mut bool,
            k: &str,
            v: impl FnOnce() -> String,
        ) {
            if let Some(filter) = &this.attributes {
                if !filter.is_match(k) {
                    return;
                }
            }
            if !*first {
                output.push_str(",\n");
            }
            *first = false;
            write!(output, "    \"{}\": {}", k, v()).unwrap();
        }

        print_attr(self, output, &mut first, TYPE, || {
            quote_json_string(&target_info.node.rule_type().to_string())
        });
        print_attr(self, output, &mut first, DEPS, || {
            format!(
                "[{}]",
                target_info
                    .node
                    .deps()
                    .map(|d| quote_json_string(&d.to_string()))
                    .join(", ")
            )
        });

        print_attr(self, output, &mut first, INPUTS, || {
            format!(
                "[{}]",
                target_info
                    .node
                    .inputs()
                    .map(|x| quote_json_string(&x.to_string()))
                    .join(", ")
            )
        });

        if let Some(BuckTargetHash(hash)) = target_info.target_hash {
            print_attr(self, output, &mut first, TARGET_HASH, || {
                format!("\"{hash:032x}\"")
            });
        }
        print_attr(self, output, &mut first, PACKAGE, || {
            format!("\"{}\"", package)
        });

        for a in target_info.node.attrs(self.attr_inspect_opts) {
            print_attr(self, output, &mut first, a.name, || {
                value_to_json(a.value, target_info.node.label().pkg())
                    .unwrap()
                    .to_string()
            });
        }

        if self.target_call_stacks {
            match target_info.node.call_stack() {
                Some(call_stack) => {
                    print_attr(self, output, &mut first, TARGET_CALL_STACK, || {
                        quote_json_string(&call_stack)
                    });
                }
                None => {
                    // Should not happen.
                }
            }
        }

        if !first {
            output.push('\n');
        }
        output.push_str("  }");
    }

    fn package_error(&mut self, package: PackageLabel, error: &anyhow::Error, output: &mut String) {
        output.push_str("  {\n");
        writeln!(output, "    \"{}\": \"{}\",", PACKAGE, package).unwrap();
        writeln!(
            output,
            "    \"buck.error\": {}",
            quote_json_string(&format!("{:?}", error))
        )
        .unwrap();
        output.push_str("    }");
    }
}

#[derive(Debug, Default)]
struct Stats {
    errors: u64,
    success: u64,
    targets: u64,
}

struct StatsPrinter;

impl TargetPrinter for StatsPrinter {
    fn end(&mut self, stats: &Stats, output: &mut String) {
        writeln!(output, "{:?}", stats).unwrap()
    }
}

struct TargetNamePrinter {
    target_call_stacks: bool,
    target_hash_graph_type: TargetHashGraphType,
}
impl TargetPrinter for TargetNamePrinter {
    fn target(&mut self, package: PackageLabel, target_info: TargetInfo<'_>, output: &mut String) {
        if self.target_hash_graph_type != TargetHashGraphType::None {
            match target_info.target_hash {
                Some(BuckTargetHash(hash)) => writeln!(
                    output,
                    "{package}:{name} {hash:032x}",
                    name = target_info.node.label().name(),
                )
                .unwrap(),
                None => {} // print nothing if there is no hash and show_target_hash is specified.
            };
        } else {
            writeln!(output, "{}:{}", package, target_info.node.label().name()).unwrap();
        }
        if self.target_call_stacks {
            match target_info.node.call_stack() {
                Some(call_stack) => {
                    for line in call_stack.lines() {
                        writeln!(output, "  {}", line).unwrap();
                    }
                }
                None => {
                    // Should not happen.
                }
            }
        }
    }
}

struct TargetHashOptions {
    file_mode: TargetHashesFileMode,
    fast_hash: bool,
    graph_type: TargetHashGraphType,
    recursive: bool,
}

impl TargetHashOptions {
    fn new(
        request: &TargetsRequest,
        cell_resolver: &CellResolver,
        fs: &ProjectRoot,
        cwd: &ProjectRelativePath,
    ) -> anyhow::Result<Self> {
        let file_mode = TargetHashFileMode::from_i32(request.target_hash_file_mode)
            .expect("buck cli should send valid target hash file mode");
        let file_mode = match file_mode {
            TargetHashFileMode::PathsOnly => {
                let modified_paths = request
                    .target_hash_modified_paths
                    .iter()
                    .map(|path| {
                        cell_resolver.get_cell_path_from_abs_or_rel_path(Path::new(path), fs, cwd)
                    })
                    .collect::<anyhow::Result<_>>()?;
                TargetHashesFileMode::PathsOnly(modified_paths)
            }
            TargetHashFileMode::PathsAndContents => TargetHashesFileMode::PathsAndContents,
            TargetHashFileMode::NoFiles => TargetHashesFileMode::None,
        };

        Ok(Self {
            file_mode,
            fast_hash: request.target_hash_use_fast_hash,
            graph_type: TargetHashGraphType::from_i32(request.target_hash_graph_type)
                .expect("buck cli should send valid target hash graph type"),
            recursive: request.target_hash_recursive,
        })
    }
}

pub async fn targets_command(
    server_ctx: Box<dyn ServerCommandContextTrait>,
    req: TargetsRequest,
) -> anyhow::Result<TargetsResponse> {
    run_server_command(TargetsServerCommand { req }, server_ctx).await
}

struct TargetsServerCommand {
    req: TargetsRequest,
}

#[async_trait]
impl ServerCommandTemplate for TargetsServerCommand {
    type StartEvent = buck2_data::TargetsCommandStart;
    type EndEvent = buck2_data::TargetsCommandEnd;
    type Response = TargetsResponse;

    async fn command<'v>(
        &self,
        server_ctx: &'v dyn ServerCommandContextTrait,
        dice: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        targets(server_ctx, dice, &self.req).await
    }

    fn is_success(&self, _response: &Self::Response) -> bool {
        // No response if we failed.
        true
    }
}

fn create_printer(request: &TargetsRequest) -> anyhow::Result<Box<dyn TargetPrinter>> {
    let is_json = request.json || !request.output_attributes.is_empty();
    if is_json {
        Ok(box JsonPrinter {
            attributes: if request.output_attributes.is_empty() {
                None
            } else {
                Some(RegexSet::new(&request.output_attributes)?)
            },
            attr_inspect_opts: if request.include_default_attributes {
                AttrInspectOptions::All
            } else {
                AttrInspectOptions::DefinedOnly
            },
            target_call_stacks: request.target_call_stacks,
        })
    } else if request.stats {
        Ok(box StatsPrinter)
    } else {
        Ok(box TargetNamePrinter {
            target_call_stacks: request.target_call_stacks,
            target_hash_graph_type: TargetHashGraphType::from_i32(request.target_hash_graph_type)
                .expect("buck cli should send valid target hash graph type"),
        })
    }
}

async fn targets(
    server_ctx: &dyn ServerCommandContextTrait,
    dice: DiceTransaction,
    request: &TargetsRequest,
) -> anyhow::Result<TargetsResponse> {
    // TODO(nmj): Rather than returning fully formatted data in the TargetsResponse, we should
    //            instead return structured data, and return *that* to the CLI. The CLI should
    //            then handle printing. The current approach is just a temporary hack to fix some
    //            issues with printing to stdout.

    let cwd = server_ctx.working_dir();
    let cell_resolver = dice.get_cell_resolver().await?;
    let parsed_target_patterns = parse_patterns_from_cli_args::<TargetName>(
        &request.target_patterns,
        &cell_resolver,
        &dice.get_legacy_configs().await?,
        cwd,
    )?;

    if request.unstable_resolve_aliases {
        return targets_resolve_aliases(dice, request, parsed_target_patterns).await;
    }

    let target_platform =
        target_platform_from_client_context(request.context.as_ref(), &cell_resolver, cwd).await?;

    let fs = server_ctx.project_root();

    let mut printer = create_printer(request)?;
    let (error_count, results_to_print) = targets_batch(
        server_ctx,
        dice,
        &mut *printer,
        parsed_target_patterns,
        target_platform,
        TargetHashOptions::new(request, &cell_resolver, fs, cwd)?,
        request.keep_going,
    )
    .await?;
    Ok(TargetsResponse {
        error_count,
        serialized_targets_output: results_to_print,
    })
}

async fn targets_resolve_aliases(
    dice: DiceTransaction,
    request: &TargetsRequest,
    parsed_target_patterns: Vec<ParsedPattern<TargetName>>,
) -> anyhow::Result<TargetsResponse> {
    // If we are only asked to resolve aliases, then don't expand any of the patterns, and just
    // print them out. This expects the aliases to resolve to individual targets.
    let parsed_target_patterns = std::iter::zip(&request.target_patterns, parsed_target_patterns)
        .map(|(alias, pattern)| match pattern {
            ParsedPattern::Target(package, target_name) => Ok((package, target_name)),
            _ => Err(anyhow::anyhow!(
                "Invalid alias (does not expand to a single target): `{}`",
                alias.value
            )),
        })
        .collect::<Result<Vec<_>, _>>()?;

    let packages = parsed_target_patterns
        .iter()
        .map(|(package, _name)| package.dupe())
        .collect::<HashSet<_>>();

    let packages = packages
        .into_iter()
        .map(|package| {
            let dice = &dice;
            async move {
                (
                    package.dupe(),
                    dice.get_interpreter_results(package.dupe()).await,
                )
            }
        })
        .collect::<FuturesUnordered<_>>()
        .collect::<HashMap<_, _>>()
        .await;

    let mut output = String::new();

    for (alias, (package, target_name)) in
        std::iter::zip(&request.target_patterns, &parsed_target_patterns)
    {
        // NOTE: We don't technically need the node to get the label, but we need the node to
        // validate it exists.
        let node = packages
            .get(package)
            .with_context(|| format!("Package does not exist: `{}`", package))
            .and_then(|package_data| {
                package_data
                    .as_ref()
                    .map_err(|e| e.dupe())
                    .with_context(|| format!("Package cannot be evaluated: `{}`", package))?
                    .resolve_target(target_name)
                    .with_context(|| {
                        format!(
                            "Target does not exist in package `{}`: `{}`",
                            package, target_name,
                        )
                    })
            })
            .with_context(|| format!("Invalid alias: `{}`", alias.value))?;

        writeln!(output, "{}", node.label())?;
    }

    Ok(TargetsResponse {
        error_count: 0,
        serialized_targets_output: output,
    })
}

async fn targets_batch(
    server_ctx: &dyn ServerCommandContextTrait,
    dice: DiceTransaction,
    printer: &mut dyn TargetPrinter,
    parsed_patterns: Vec<ParsedPattern<TargetName>>,
    target_platform: Option<TargetLabel>,
    hash_options: TargetHashOptions,
    keep_going: bool,
) -> anyhow::Result<(u64, String)> {
    let results = load_patterns(&dice, parsed_patterns).await?;

    let target_hashes = match hash_options.graph_type {
        TargetHashGraphType::Configured => Some(
            TargetHashes::compute::<ConfiguredTargetNode, _>(
                dice.dupe(),
                ConfiguredTargetNodeLookup(&dice),
                results.iter_loaded_targets_by_package().collect(),
                target_platform,
                hash_options.file_mode,
                hash_options.fast_hash,
                hash_options.recursive,
            )
            .await?,
        ),
        TargetHashGraphType::Unconfigured => Some(
            TargetHashes::compute::<TargetNode, _>(
                dice.dupe(),
                TargetNodeLookup(&dice),
                results.iter_loaded_targets_by_package().collect(),
                target_platform,
                hash_options.file_mode,
                hash_options.fast_hash,
                hash_options.recursive,
            )
            .await?,
        ),
        _ => None,
    };

    let mut output = String::new();
    printer.begin(&mut output);
    let mut stats = Stats::default();
    let mut needs_separator = false;
    for (package, result) in results.iter() {
        match result {
            Ok(res) => {
                stats.success += 1;
                for (_, node) in res.iter() {
                    stats.targets += 1;
                    let target_hash = target_hashes
                        .as_ref()
                        .and_then(|hashes| hashes.get(node.label()))
                        .duped()
                        .transpose()?;
                    if needs_separator {
                        printer.separator(&mut output);
                    }
                    needs_separator = true;
                    printer.target(
                        package.dupe(),
                        TargetInfo { node, target_hash },
                        &mut output,
                    )
                }
            }
            Err(e) => {
                stats.errors += 1;
                if keep_going {
                    if needs_separator {
                        printer.separator(&mut output);
                    }
                    needs_separator = true;
                    printer.package_error(package.dupe(), e.inner(), &mut output);
                } else {
                    writeln!(
                        server_ctx.stderr()?,
                        "Error parsing {}\n{:?}",
                        package,
                        e.inner()
                    )?;
                }
            }
        }
    }
    printer.end(&stats, &mut output);
    if !keep_going && stats.errors != 0 {
        // Simpler error so that we don't print long errors twice (when exiting buck2)
        let package_str = if stats.errors == 1 {
            "package"
        } else {
            "packages"
        };
        Err(anyhow::anyhow!(
            "Failed to parse {} {}",
            stats.errors,
            package_str
        ))
    } else {
        Ok((stats.errors, output))
    }
}
