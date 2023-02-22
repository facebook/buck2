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
use std::fs::File;
use std::io::BufWriter;
use std::io::Write as _;
use std::mem;
use std::path::Path;
use std::sync::Arc;
use std::sync::Mutex;

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
use buck2_common::pattern::package_roots::find_package_roots_stream;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_common::result::ToSharedResultExt;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::PackageSpec;
use buck2_core::pattern::ParsedPattern;
use buck2_core::pattern::PatternType;
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
use buck2_node::nodes::eval_result::EvaluationResult;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use dice::DiceComputations;
use dice::DiceTransaction;
use dupe::Dupe;
use dupe::IterDupedExt;
use dupe::OptionDupedExt;
use futures::stream::FuturesUnordered;
use futures::stream::StreamExt;
use futures::Stream;
use gazebo::prelude::*;
use itertools::Itertools;
use regex::RegexSet;
use starlark_map::small_set::SmallSet;

use crate::json::quote_json_string;
use crate::target_hash::BuckTargetHash;
use crate::target_hash::TargetHashes;
use crate::target_hash::TargetHashesFileMode;

struct TargetInfo<'a> {
    node: &'a TargetNode,
    target_hash: Option<BuckTargetHash>,
}

#[allow(unused_variables)]
trait TargetFormatter: Send + Sync {
    fn begin(&self, buffer: &mut String) {}
    fn end(&self, stats: &Stats, buffer: &mut String) {}
    /// Called between each target/imports/package_error
    fn separator(&self, buffer: &mut String) {}
    fn target(&self, package: PackageLabel, target_info: TargetInfo<'_>, buffer: &mut String) {}
    fn imports(
        &self,
        source: &CellPath,
        imports: &[ImportPath],
        package: Option<PackageLabel>,
        buffer: &mut String,
    ) {
    }
    fn package_error(&self, package: PackageLabel, error: &anyhow::Error, buffer: &mut String) {}
}

struct JsonFormat {
    attributes: Option<RegexSet>,
    attr_inspect_opts: AttrInspectOptions,
    target_call_stacks: bool,
    json_lines: bool,
}

impl JsonFormat {
    fn entry_start(&self, buffer: &mut String) {
        if self.json_lines {
            buffer.push('{');
        } else {
            buffer.push_str("  {\n");
        }
    }

    fn entry_end(&self, buffer: &mut String, first: bool) {
        if self.json_lines {
            buffer.push_str("}\n");
        } else {
            if !first {
                buffer.push('\n');
            }
            buffer.push_str("  }");
        }
    }

    fn entry_item(&self, buffer: &mut String, first: &mut bool, key: &str, value: &str) {
        if *first {
            *first = false;
        } else if self.json_lines {
            buffer.push(',');
        } else {
            buffer.push_str(",\n");
        }
        if self.json_lines {
            write!(buffer, "\"{}\":{}", key, value).unwrap();
        } else {
            write!(buffer, "    \"{}\": {}", key, value).unwrap();
        }
    }
}

impl TargetFormatter for JsonFormat {
    fn begin(&self, buffer: &mut String) {
        if !self.json_lines {
            buffer.push_str("[\n");
        }
    }
    fn end(&self, _stats: &Stats, buffer: &mut String) {
        if !self.json_lines {
            buffer.push_str("\n]\n");
        }
    }
    fn separator(&self, buffer: &mut String) {
        if !self.json_lines {
            buffer.push_str(",\n");
        }
    }

    fn target(&self, package: PackageLabel, target_info: TargetInfo<'_>, buffer: &mut String) {
        self.entry_start(buffer);
        let mut first = true;

        fn print_attr(
            this: &JsonFormat,
            buffer: &mut String,
            first: &mut bool,
            k: &str,
            v: impl FnOnce() -> String,
        ) {
            if let Some(filter) = &this.attributes {
                if !filter.is_match(k) {
                    return;
                }
            }
            this.entry_item(buffer, first, k, &v());
        }

        print_attr(self, buffer, &mut first, TYPE, || {
            quote_json_string(&target_info.node.rule_type().to_string())
        });
        print_attr(self, buffer, &mut first, DEPS, || {
            format!(
                "[{}]",
                target_info
                    .node
                    .deps()
                    .map(|d| quote_json_string(&d.to_string()))
                    .join(", ")
            )
        });

        print_attr(self, buffer, &mut first, INPUTS, || {
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
            print_attr(self, buffer, &mut first, TARGET_HASH, || {
                format!("\"{hash:032x}\"")
            });
        }
        print_attr(self, buffer, &mut first, PACKAGE, || {
            format!("\"{}\"", package)
        });

        for a in target_info.node.attrs(self.attr_inspect_opts) {
            print_attr(self, buffer, &mut first, a.name, || {
                value_to_json(a.value, target_info.node.label().pkg())
                    .unwrap()
                    .to_string()
            });
        }

        if self.target_call_stacks {
            match target_info.node.call_stack() {
                Some(call_stack) => {
                    print_attr(self, buffer, &mut first, TARGET_CALL_STACK, || {
                        quote_json_string(&call_stack)
                    });
                }
                None => {
                    // Should not happen.
                }
            }
        }
        self.entry_end(buffer, first);
    }

    fn imports(
        &self,
        source: &CellPath,
        imports: &[ImportPath],
        package: Option<PackageLabel>,
        buffer: &mut String,
    ) {
        self.entry_start(buffer);
        let mut first = true;
        if let Some(package) = package {
            self.entry_item(
                buffer,
                &mut first,
                PACKAGE,
                &quote_json_string(&package.to_string()),
            );
        }
        self.entry_item(
            buffer,
            &mut first,
            "buck.file",
            &quote_json_string(&source.to_string()),
        );
        self.entry_item(
            buffer,
            &mut first,
            "buck.imports",
            &format!(
                "[{}]",
                imports
                    .map(|d| quote_json_string(&d.path().to_string()))
                    .join(", ")
            ),
        );
        self.entry_end(buffer, first);
    }

    fn package_error(&self, package: PackageLabel, error: &anyhow::Error, buffer: &mut String) {
        self.entry_start(buffer);
        let mut first = true;
        self.entry_item(
            buffer,
            &mut first,
            PACKAGE,
            &quote_json_string(&package.to_string()),
        );
        self.entry_item(
            buffer,
            &mut first,
            "buck.error",
            &quote_json_string(&format!("{:?}", error)),
        );
        self.entry_end(buffer, first);
    }
}

#[derive(Debug, Default)]
struct Stats {
    errors: u64,
    success: u64,
    targets: u64,
}

impl Stats {
    fn merge(&mut self, stats: &Stats) {
        self.errors += stats.errors;
        self.success += stats.success;
        self.targets += stats.targets;
    }
}

struct StatsFormat;

impl TargetFormatter for StatsFormat {
    fn end(&self, stats: &Stats, buffer: &mut String) {
        writeln!(buffer, "{:?}", stats).unwrap()
    }
}

struct TargetNameFormat {
    target_call_stacks: bool,
    target_hash_graph_type: TargetHashGraphType,
}
impl TargetFormatter for TargetNameFormat {
    fn target(&self, package: PackageLabel, target_info: TargetInfo<'_>, buffer: &mut String) {
        if self.target_hash_graph_type != TargetHashGraphType::None {
            match target_info.target_hash {
                Some(BuckTargetHash(hash)) => writeln!(
                    buffer,
                    "{package}:{name} {hash:032x}",
                    name = target_info.node.label().name(),
                )
                .unwrap(),
                None => {} // print nothing if there is no hash and show_target_hash is specified.
            };
        } else {
            writeln!(buffer, "{}:{}", package, target_info.node.label().name()).unwrap();
        }
        if self.target_call_stacks {
            match target_info.node.call_stack() {
                Some(call_stack) => {
                    for line in call_stack.lines() {
                        writeln!(buffer, "  {}", line).unwrap();
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
    ) -> anyhow::Result<Self> {
        let file_mode = TargetHashFileMode::from_i32(request.target_hash_file_mode)
            .expect("buck cli should send valid target hash file mode");
        let file_mode = match file_mode {
            TargetHashFileMode::PathsOnly => {
                let modified_paths = request
                    .target_hash_modified_paths
                    .iter()
                    .map(|path| {
                        let path = AbsPath::new(Path::new(&path))?;
                        cell_resolver.get_cell_path_from_abs_path(path, fs)
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

enum Outputter {
    Stdout,
    File(BufWriter<File>),
}

impl Outputter {
    fn new(request: &TargetsRequest) -> anyhow::Result<Self> {
        match &request.output {
            None => Ok(Self::Stdout),
            Some(file) => Ok(Self::File(BufWriter::new(
                File::create(file).with_context(|| {
                    format!("Failed to open file `{}` for `targets` output ", file)
                })?,
            ))),
        }
    }

    fn write1(
        &mut self,
        server_ctx: &dyn ServerCommandContextTrait,
        x: &str,
    ) -> anyhow::Result<()> {
        match self {
            Self::Stdout => server_ctx.stdout()?.write_all(x.as_bytes())?,
            Self::File(f) => f.write_all(x.as_bytes())?,
        }
        Ok(())
    }

    fn write2(
        &mut self,
        server_ctx: &dyn ServerCommandContextTrait,
        x: &str,
        y: &str,
    ) -> anyhow::Result<()> {
        match self {
            Self::Stdout => {
                let mut stdout = server_ctx.stdout()?;
                stdout.write_all(x.as_bytes())?;
                stdout.write_all(y.as_bytes())?;
            }
            Self::File(f) => {
                f.write_all(x.as_bytes())?;
                f.write_all(y.as_bytes())?;
            }
        }
        Ok(())
    }

    /// If this outputter should write anything to a file, do so, and return whatever buffer is left over.
    fn write_to_file(&mut self, buffer: String) -> anyhow::Result<String> {
        match self {
            Self::Stdout => Ok(buffer),
            Self::File(f) => {
                f.write_all(buffer.as_bytes())?;
                Ok(String::new())
            }
        }
    }

    fn flush(&mut self) -> anyhow::Result<()> {
        match self {
            Self::Stdout => Ok(()),
            Self::File(f) => Ok(f.flush()?),
        }
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

fn crate_formatter(request: &TargetsRequest) -> anyhow::Result<Arc<dyn TargetFormatter>> {
    let is_json = request.json || request.json_lines || !request.output_attributes.is_empty();
    if is_json {
        Ok(Arc::new(JsonFormat {
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
            json_lines: request.json_lines,
        }))
    } else if request.stats {
        Ok(Arc::new(StatsFormat))
    } else {
        Ok(Arc::new(TargetNameFormat {
            target_call_stacks: request.target_call_stacks,
            target_hash_graph_type: TargetHashGraphType::from_i32(request.target_hash_graph_type)
                .expect("buck cli should send valid target hash graph type"),
        }))
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

    let mut outputter = Outputter::new(request)?;

    let response = if request.unstable_resolve_aliases {
        targets_resolve_aliases(dice, request, parsed_target_patterns).await?
    } else if request.streaming {
        let formatter = crate_formatter(request)?;
        let hashing = match TargetHashGraphType::from_i32(request.target_hash_graph_type)
            .expect("buck cli should send valid target hash graph type")
        {
            TargetHashGraphType::None => None,
            _ => Some(request.target_hash_use_fast_hash),
        };

        let res = targets_streaming(
            server_ctx,
            dice,
            formatter,
            &mut outputter,
            parsed_target_patterns,
            request.keep_going,
            request.cached,
            request.imports,
            hashing,
        )
        .await;
        // Make sure we always flush the outputter, even on failure, as we may have partially written to it
        outputter.flush()?;
        res?
    } else {
        let formatter = crate_formatter(request)?;
        let target_platform =
            target_platform_from_client_context(request.context.as_ref(), &cell_resolver, cwd)
                .await?;
        let fs = server_ctx.project_root();
        targets_batch(
            server_ctx,
            dice,
            &*formatter,
            parsed_target_patterns,
            target_platform,
            TargetHashOptions::new(request, &cell_resolver, fs)?,
            request.keep_going,
        )
        .await?
    };
    let response = TargetsResponse {
        error_count: response.error_count,
        serialized_targets_output: outputter.write_to_file(response.serialized_targets_output)?,
    };
    outputter.flush()?;
    Ok(response)
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
                    dice.get_interpreter_results(package.dupe())
                        .await
                        .shared_error(),
                )
            }
        })
        .collect::<FuturesUnordered<_>>()
        .collect::<HashMap<_, _>>()
        .await;

    let mut buffer = String::new();

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

        writeln!(buffer, "{}", node.label())?;
    }

    Ok(TargetsResponse {
        error_count: 0,
        serialized_targets_output: buffer,
    })
}

fn mk_error(errors: u64) -> anyhow::Error {
    // Simpler error so that we don't print long errors twice (when exiting buck2)
    let package_str = if errors == 1 { "package" } else { "packages" };
    anyhow::anyhow!("Failed to parse {} {}", errors, package_str)
}

async fn targets_batch(
    server_ctx: &dyn ServerCommandContextTrait,
    dice: DiceTransaction,
    formatter: &dyn TargetFormatter,
    parsed_patterns: Vec<ParsedPattern<TargetName>>,
    target_platform: Option<TargetLabel>,
    hash_options: TargetHashOptions,
    keep_going: bool,
) -> anyhow::Result<TargetsResponse> {
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

    let mut buffer = String::new();
    formatter.begin(&mut buffer);
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
                        formatter.separator(&mut buffer);
                    }
                    needs_separator = true;
                    formatter.target(
                        package.dupe(),
                        TargetInfo { node, target_hash },
                        &mut buffer,
                    )
                }
            }
            Err(e) => {
                stats.errors += 1;
                if keep_going {
                    if needs_separator {
                        formatter.separator(&mut buffer);
                    }
                    needs_separator = true;
                    formatter.package_error(package.dupe(), e.inner(), &mut buffer);
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
    formatter.end(&stats, &mut buffer);
    if !keep_going && stats.errors != 0 {
        Err(mk_error(stats.errors))
    } else {
        Ok(TargetsResponse {
            error_count: stats.errors,
            serialized_targets_output: buffer,
        })
    }
}

async fn targets_streaming(
    server_ctx: &dyn ServerCommandContextTrait,
    dice: DiceTransaction,
    formatter: Arc<dyn TargetFormatter>,
    outputter: &mut Outputter,
    parsed_patterns: Vec<ParsedPattern<TargetName>>,
    keep_going: bool,
    cached: bool,
    imports: bool,
    fast_hash: Option<bool>, // None = no hashing
) -> anyhow::Result<TargetsResponse> {
    #[derive(Default)]
    struct Res {
        stats: Stats,           // Stats to merge in
        stderr: Option<String>, // Print to stderr (and break)
        stdout: String,         // Print to stdout
    }

    let imported = Arc::new(Mutex::new(SmallSet::new()));

    let mut packages = stream_packages(&dice, parsed_patterns)
        .map(|x| {
            let formatter = formatter.dupe();
            let imported = imported.dupe();

            dice.temporary_spawn(move |dice| async move {
                let mut res = Res::default();
                let (package, spec) = x?;
                match load_package(&dice, package.dupe(), spec, cached).await {
                    Ok((eval_result, targets)) => {
                        res.stats.success += 1;
                        if imports {
                            let eval_imports = eval_result.imports();
                            formatter.imports(
                                &eval_result.buildfile_path().path(),
                                eval_imports,
                                Some(package.dupe()),
                                &mut res.stdout,
                            );
                            imported
                                .lock()
                                .unwrap()
                                .extend(eval_imports.iter().cloned());
                        }
                        for (i, node) in targets.iter().enumerate() {
                            res.stats.targets += 1;
                            if imports || i != 0 {
                                formatter.separator(&mut res.stdout);
                            }
                            formatter.target(
                                package.dupe(),
                                TargetInfo {
                                    node,
                                    target_hash: fast_hash.map(|fast| {
                                        TargetHashes::compute_immediate_one(node, fast)
                                    }),
                                },
                                &mut res.stdout,
                            )
                        }
                    }
                    Err(e) => {
                        res.stats.errors += 1;
                        formatter.package_error(package.dupe(), &e, &mut res.stdout);
                        if !keep_going {
                            res.stderr = Some(format!("Error parsing {}\n{:?}", package, e));
                        }
                    }
                }
                anyhow::Ok(res)
            })
        })
        // Use unlimited parallelism - tokio will restrict us anyway
        .buffer_unordered(1000000);

    let mut buffer = String::new();
    formatter.begin(&mut buffer);
    let mut stats = Stats::default();
    let mut needs_separator = false;
    while let Some(res) = packages.next().await {
        let res = res?;
        stats.merge(&res.stats);
        if let Some(stderr) = res.stderr {
            writeln!(server_ctx.stderr()?, "{}", stderr)?;
            return Err(mk_error(stats.errors));
        }
        if !res.stdout.is_empty() {
            if needs_separator {
                formatter.separator(&mut buffer);
            }
            needs_separator = true;
            outputter.write2(server_ctx, &buffer, &res.stdout)?;
            buffer.clear();
        }
    }

    // Recursively chase down all imported paths
    let mut todo = mem::take(&mut *imported.lock().unwrap());
    let mut seen_imported = HashSet::new();
    while let Some(path) = todo.pop() {
        if seen_imported.insert(path.path().clone()) {
            // If these lead to an error, that's surpsing (we had a working module with it loaded)
            // so we should always propagate the error here (even with keep_going)
            if needs_separator {
                formatter.separator(&mut buffer);
            }
            needs_separator = true;
            // No need to parallelise these this step because it will already be on the DICE graph
            let loaded = dice.get_loaded_module_from_import_path(&path).await?;
            let imports = loaded.imports().cloned().collect::<Vec<_>>();
            formatter.imports(path.path(), &imports, None, &mut buffer);
            todo.extend(imports);
            outputter.write1(server_ctx, &buffer)?;
            buffer.clear();
        }
    }

    formatter.end(&stats, &mut buffer);
    Ok(TargetsResponse {
        error_count: stats.errors,
        serialized_targets_output: buffer,
    })
}

/// Given the patterns, separate into those which have an explicit package, and those which are recursive
fn stream_packages<T: PatternType>(
    dice: &DiceComputations,
    patterns: Vec<ParsedPattern<T>>,
) -> impl Stream<Item = anyhow::Result<(PackageLabel, PackageSpec<T>)>> {
    let mut spec = ResolvedPattern::<T>::new();
    let mut recursive_paths = Vec::new();

    for pattern in patterns {
        match pattern {
            ParsedPattern::Target(package, target) => {
                spec.add_target(package.dupe(), &target);
            }
            ParsedPattern::Package(package) => {
                spec.add_package(package.dupe());
            }
            ParsedPattern::Recursive(package) => {
                recursive_paths.push(package);
            }
        }
    }

    futures::stream::iter(spec.specs.into_iter().map(Ok))
        .chain(find_package_roots_stream(dice, recursive_paths).map(|x| Ok((x?, PackageSpec::All))))
}

async fn load_package(
    dice: &DiceComputations,
    package: PackageLabel,
    spec: PackageSpec<TargetName>,
    cached: bool,
) -> anyhow::Result<(Arc<EvaluationResult>, Vec<TargetNode>)> {
    let result = if cached {
        dice.get_interpreter_results(package.dupe()).await?
    } else {
        dice.get_interpreter_results_uncached(package.dupe())
            .await?
    };

    let targets = match spec {
        PackageSpec::Targets(targets) => {
            targets.into_try_map(|target| anyhow::Ok(result.resolve_target(&target)?.dupe()))?
        }
        PackageSpec::All => result.targets().values().duped().collect(),
    };
    Ok((result, targets))
}
