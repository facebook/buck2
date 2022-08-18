/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;
use std::fmt::Write;
use std::path::Path;

use buck2_build_api::calculation::load_patterns;
use buck2_build_api::nodes::hacks::value_to_json;
use buck2_build_api::nodes::lookup::ConfiguredTargetNodeLookup;
use buck2_build_api::nodes::lookup::TargetNodeLookup;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::package::Package;
use buck2_core::pattern::ParsedPattern;
use buck2_core::pattern::TargetPattern;
use buck2_core::target::TargetLabel;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_server::ctx::ServerCommandContext;
use buck2_server::daemon::common::parse_patterns_from_cli_args;
use buck2_server::daemon::common::target_platform_from_client_context;
use cli_proto::targets_request::TargetHashFileMode;
use cli_proto::targets_request::TargetHashGraphType;
use cli_proto::TargetsRequest;
use cli_proto::TargetsResponse;
use dice::DiceTransaction;
use gazebo::prelude::*;
use itertools::Itertools;
use regex::RegexSet;

use crate::daemon::json::quote_json_string;
use crate::target_hash::BuckTargetHash;
use crate::target_hash::TargetHashes;

struct TargetInfo<'a> {
    node: &'a TargetNode,
    target_hash: Option<BuckTargetHash>,
}

trait TargetPrinter: Send {
    fn begin(&mut self) {}
    fn end(&mut self) -> String;
    fn package(&mut self, _package: &Package) {}
    fn package_end(&mut self) {}
    fn target(&mut self, _package: &Package, _target_info: TargetInfo<'_>) {}
    fn err(&mut self, package: &Package, e: &anyhow::Error) {
        eprintln!("Error parsing {}", package);
        eprintln!("{:?}", e);
    }
}

struct JsonPrinter {
    attributes: Option<RegexSet>,
    attr_inspect_opts: AttrInspectOptions,
    target_idx: u32,
    json_string: String,
    target_call_stacks: bool,
}

impl TargetPrinter for JsonPrinter {
    fn begin(&mut self) {
        writeln!(self.json_string, "[").unwrap();
    }
    fn end(&mut self) -> String {
        writeln!(self.json_string, "\n]").unwrap();
        std::mem::take(&mut self.json_string)
    }

    fn package_end(&mut self) {
        // ignored
    }

    fn package(&mut self, _package: &Package) {
        // ignored
    }

    fn target(&mut self, package: &Package, target_info: TargetInfo<'_>) {
        if self.target_idx != 0 {
            writeln!(self.json_string, ",").unwrap();
        }
        self.target_idx += 1;
        writeln!(self.json_string, "    {{",).unwrap();
        let mut first = true;

        let mut print_attr = |k: &'_ str, v: &'_ str| {
            if let Some(filter) = &self.attributes {
                if !filter.is_match(k) {
                    return;
                }
            }
            if first {
                writeln!(self.json_string).unwrap();
            } else {
                writeln!(self.json_string, ",").unwrap();
            }
            first = false;
            write!(self.json_string, "      \"{}\": {}", k, v).unwrap();
        };
        print_attr(
            "$type",
            &quote_json_string(&target_info.node.rule_type().to_string()),
        );
        print_attr(
            "$deps",
            &format!(
                "[{}]",
                target_info
                    .node
                    .deps()
                    .map(|d| quote_json_string(&d.to_string()))
                    .join(", ")
            ),
        );

        if let Some(BuckTargetHash(hash)) = target_info.target_hash {
            print_attr("$target_hash", &format!("\"{:x}\"", hash));
        }
        print_attr("$package", &format!("\"{}\"", package));

        for (k, v) in target_info.node.attrs(self.attr_inspect_opts) {
            print_attr(k, &value_to_json(v).unwrap().to_string());
        }

        if self.target_call_stacks {
            match target_info.node.call_stack() {
                Some(call_stack) => {
                    print_attr("$target_call_stack", &quote_json_string(&call_stack));
                }
                None => {
                    // Should not happen.
                }
            }
        }

        if !first {
            writeln!(self.json_string).unwrap();
        }

        write!(self.json_string, "    }}").unwrap();
    }
}

#[derive(Debug)]
struct StatsPrinter {
    errors: u64,
    success: u64,
    targets: u64,
}

impl StatsPrinter {
    fn new() -> Self {
        Self {
            errors: 0,
            success: 0,
            targets: 0,
        }
    }
}

impl TargetPrinter for StatsPrinter {
    fn end(&mut self) -> String {
        format!("{:?}", self)
    }

    fn package(&mut self, _package: &Package) {
        self.success += 1;
    }

    fn target(&mut self, _package: &Package, _target_info: TargetInfo<'_>) {
        self.targets += 1;
    }

    fn err(&mut self, package: &Package, e: &anyhow::Error) {
        self.errors += 1;
        eprintln!("Error parsing {}", package);
        eprintln!("{:?}", e);
    }
}

struct TargetNamePrinter {
    display_string: String,
    target_call_stacks: bool,
    target_hash_graph_type: TargetHashGraphType,
}
impl TargetPrinter for TargetNamePrinter {
    fn end(&mut self) -> String {
        std::mem::take(&mut self.display_string)
    }

    fn target(&mut self, package: &Package, target_info: TargetInfo<'_>) {
        if self.target_hash_graph_type != TargetHashGraphType::None {
            match target_info.target_hash {
                Some(BuckTargetHash(hash)) => writeln!(
                    self.display_string,
                    "{}:{} {:x}",
                    package,
                    target_info.node.label().name(),
                    hash
                )
                .unwrap(),
                None => {} // print nothing if there is no hash and show_target_hash is specified.
            };
        } else {
            writeln!(
                self.display_string,
                "{}:{}",
                package,
                target_info.node.label().name()
            )
            .unwrap();
        }
        if self.target_call_stacks {
            match target_info.node.call_stack() {
                Some(call_stack) => {
                    for line in call_stack.lines() {
                        writeln!(self.display_string, "  {}", line).unwrap();
                    }
                }
                None => {
                    // Should not happen.
                }
            }
        }
    }
}

struct TargetsOptions {
    target_hash_mode: TargetHashFileMode,
    target_hash_modified_paths: HashSet<CellPath>,
    use_fast_hash: bool,
    target_hash_graph_type: TargetHashGraphType,
}

pub(crate) async fn targets(
    server_ctx: ServerCommandContext,
    request: TargetsRequest,
) -> anyhow::Result<TargetsResponse> {
    // TODO(nmj): Rather than returning fully formatted data in the TargetsResponse, we should
    //            instead return structured data, and return *that* to the CLI. The CLI should
    //            then handle printing. The current approach is just a temporary hack to fix some
    //            issues with printing to stdout.
    let is_json = request.json || !request.output_attributes.is_empty();

    let attributes = if request.output_attributes.is_empty() {
        None
    } else {
        Some(RegexSet::new(
            request.output_attributes.iter().map(|v| format!("^{}$", v)),
        )?)
    };

    let mut printer: Box<dyn TargetPrinter> = if is_json {
        box JsonPrinter {
            attributes,
            attr_inspect_opts: if request.include_default_attributes {
                AttrInspectOptions::All
            } else {
                AttrInspectOptions::DefinedOnly
            },
            target_idx: 0,
            json_string: String::new(),
            target_call_stacks: request.target_call_stacks,
        }
    } else if request.stats {
        box StatsPrinter::new()
    } else {
        box TargetNamePrinter {
            display_string: String::new(),
            target_call_stacks: request.target_call_stacks,
            target_hash_graph_type: TargetHashGraphType::from_i32(request.target_hash_graph_type)
                .expect("buck cli should send valid target hash graph type"),
        }
    };

    let fs = server_ctx.project_root();
    let cwd = &server_ctx.working_dir;

    let ctx = server_ctx.dice_ctx().await?;
    let cell_resolver = ctx.get_cell_resolver().await?;

    let target_platform =
        target_platform_from_client_context(request.context.as_ref(), &cell_resolver, cwd).await?;

    let parsed_target_patterns = parse_patterns_from_cli_args::<TargetPattern>(
        &request.target_patterns,
        &cell_resolver,
        &ctx.get_legacy_configs().await?,
        cwd,
    )?;

    // If we are only asked to resolve aliases, then don't expand any of the patterns, and just
    // print them out. This expects the aliases to resolve to individual targets.
    if request.unstable_resolve_aliases {
        let mut output = String::new();

        for (alias, pattern) in std::iter::zip(&request.target_patterns, parsed_target_patterns) {
            let (package, target_name) = pattern.as_literal(&alias.value)?;

            writeln!(output, "{}", TargetLabel::new(package, target_name),)?;
        }

        return Ok(TargetsResponse {
            serialized_targets_output: output,
        });
    }

    let target_hash_modified_paths = request
        .target_hash_modified_paths
        .iter()
        .map(|path| cell_resolver.get_cell_path_from_abs_or_rel_path(Path::new(path), fs, cwd))
        .collect::<anyhow::Result<_>>()?;

    let results_to_print = parse_and_get_results(
        ctx,
        &mut *printer,
        parsed_target_patterns,
        target_platform,
        TargetsOptions {
            target_hash_mode: TargetHashFileMode::from_i32(request.target_hash_file_mode)
                .expect("buck cli should send valid target hash file mode"),
            target_hash_modified_paths,
            use_fast_hash: request.target_hash_use_fast_hash,
            target_hash_graph_type: TargetHashGraphType::from_i32(request.target_hash_graph_type)
                .expect("buck cli should send valid target hash graph type"),
        },
    )
    .await?;
    Ok(TargetsResponse {
        serialized_targets_output: results_to_print,
    })
}

async fn parse_and_get_results(
    ctx: DiceTransaction,
    printer: &mut dyn TargetPrinter,
    parsed_patterns: Vec<ParsedPattern<TargetPattern>>,
    target_platform: Option<TargetLabel>,
    options: TargetsOptions,
) -> anyhow::Result<String> {
    let results = load_patterns(&ctx, parsed_patterns).await?;
    let vec_results: Vec<_> = results.iter_loaded_targets_by_package().collect();

    let target_hashes = match options.target_hash_graph_type {
        TargetHashGraphType::Configured => Some(
            TargetHashes::compute::<ConfiguredTargetNode, _>(
                ctx.dupe(),
                ConfiguredTargetNodeLookup(&ctx),
                vec_results,
                target_platform,
                options.target_hash_mode,
                options.target_hash_modified_paths,
                options.use_fast_hash,
            )
            .await?,
        ),
        TargetHashGraphType::Unconfigured => Some(
            TargetHashes::compute::<TargetNode, _>(
                ctx.dupe(),
                TargetNodeLookup(&ctx),
                vec_results,
                target_platform,
                options.target_hash_mode,
                options.target_hash_modified_paths,
                options.use_fast_hash,
            )
            .await?,
        ),
        _ => None,
    };

    printer.begin();
    let mut error_count = 0;
    for (package, result) in results.iter() {
        match result {
            Ok(res) => {
                printer.package(package);
                for (_, node) in res.iter() {
                    let target_hash = target_hashes
                        .as_ref()
                        .and_then(|hashes| hashes.get(node.label()))
                        .transpose()?;
                    printer.target(package, TargetInfo { node, target_hash })
                }
                printer.package_end();
            }
            Err(e) => {
                printer.err(package, e.inner());
                error_count += 1;
            }
        }
    }
    let stdout = printer.end();
    if error_count != 0 {
        // Simpler error so that we don't print long errors twice (when exiting buck2)
        let package_str = if error_count == 1 {
            "package"
        } else {
            "packages"
        };
        Err(anyhow::anyhow!(
            "Failed to parse {} {}",
            error_count,
            package_str
        ))
    } else {
        Ok(stdout)
    }
}
