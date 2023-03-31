/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Write;
use std::sync::Arc;

use anyhow::Context;
use buck2_build_api::nodes::hacks::value_to_json;
use buck2_cli_proto::targets_request;
use buck2_cli_proto::targets_request::OutputFormat;
use buck2_cli_proto::targets_request::TargetHashGraphType;
use buck2_cli_proto::TargetsRequest;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::package::PackageLabel;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::nodes::attributes::DEPS;
use buck2_node::nodes::attributes::INPUTS;
use buck2_node::nodes::attributes::PACKAGE;
use buck2_node::nodes::attributes::TARGET_CALL_STACK;
use buck2_node::nodes::attributes::TARGET_HASH;
use buck2_node::nodes::attributes::TYPE;
use buck2_node::nodes::unconfigured::TargetNode;
use gazebo::prelude::SliceExt;
use itertools::Itertools;
use regex::RegexSet;

use crate::json::quote_json_string;
use crate::target_hash::BuckTargetHash;

#[derive(Debug, thiserror::Error)]
enum FormatterError {
    #[error("Attributes can only be specified when output format is JSON (internal error)")]
    AttrsOnlyWithJson,
    #[error("`output_format` is not set (internal error)")]
    OutputFormatNotSet,
}

pub(crate) struct TargetInfo<'a> {
    pub(crate) node: &'a TargetNode,
    pub(crate) target_hash: Option<BuckTargetHash>,
}

#[allow(unused_variables)]
pub(crate) trait TargetFormatter: Send + Sync {
    fn begin(&self, buffer: &mut String) {}
    fn end(&self, stats: &Stats, buffer: &mut String) {}
    /// Called between each target/imports/package_error
    fn separator(&self, buffer: &mut String) {}
    fn target(&self, target_info: TargetInfo<'_>, buffer: &mut String) {}
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

pub(crate) struct JsonWriter {
    pub(crate) json_lines: bool,
}

impl JsonWriter {
    pub(crate) fn begin(&self, buffer: &mut String) {
        if !self.json_lines {
            buffer.push_str("[\n");
        }
    }

    pub(crate) fn end(&self, buffer: &mut String) {
        if !self.json_lines {
            buffer.push_str("\n]\n");
        }
    }

    pub(crate) fn separator(&self, buffer: &mut String) {
        if !self.json_lines {
            buffer.push_str(",\n");
        }
    }

    pub(crate) fn entry_start(&self, buffer: &mut String) {
        if self.json_lines {
            buffer.push('{');
        } else {
            buffer.push_str("  {\n");
        }
    }

    pub(crate) fn entry_end(&self, buffer: &mut String, first: bool) {
        if self.json_lines {
            buffer.push_str("}\n");
        } else {
            if !first {
                buffer.push('\n');
            }
            buffer.push_str("  }");
        }
    }

    pub(crate) fn entry_item(&self, buffer: &mut String, first: &mut bool, key: &str, value: &str) {
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

struct JsonFormat {
    attributes: Option<RegexSet>,
    attr_inspect_opts: AttrInspectOptions,
    target_call_stacks: bool,
    writer: JsonWriter,
}

impl TargetFormatter for JsonFormat {
    fn begin(&self, buffer: &mut String) {
        self.writer.begin(buffer)
    }

    fn end(&self, _stats: &Stats, buffer: &mut String) {
        self.writer.end(buffer)
    }

    fn separator(&self, buffer: &mut String) {
        self.writer.separator(buffer)
    }

    fn target(&self, target_info: TargetInfo<'_>, buffer: &mut String) {
        self.writer.entry_start(buffer);
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
            this.writer.entry_item(buffer, first, k, &v());
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

        if let Some(hash) = target_info.target_hash {
            print_attr(self, buffer, &mut first, TARGET_HASH, || {
                format!("\"{hash}\"")
            });
        }
        print_attr(self, buffer, &mut first, PACKAGE, || {
            format!("\"{}\"", target_info.node.label().pkg())
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
        self.writer.entry_end(buffer, first);
    }

    fn imports(
        &self,
        source: &CellPath,
        imports: &[ImportPath],
        package: Option<PackageLabel>,
        buffer: &mut String,
    ) {
        self.writer.entry_start(buffer);
        let mut first = true;
        if let Some(package) = package {
            self.writer.entry_item(
                buffer,
                &mut first,
                PACKAGE,
                &quote_json_string(&package.to_string()),
            );
        }
        self.writer.entry_item(
            buffer,
            &mut first,
            "buck.file",
            &quote_json_string(&source.to_string()),
        );
        self.writer.entry_item(
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
        self.writer.entry_end(buffer, first);
    }

    fn package_error(&self, package: PackageLabel, error: &anyhow::Error, buffer: &mut String) {
        self.writer.entry_start(buffer);
        let mut first = true;
        self.writer.entry_item(
            buffer,
            &mut first,
            PACKAGE,
            &quote_json_string(&package.to_string()),
        );
        self.writer.entry_item(
            buffer,
            &mut first,
            "buck.error",
            &quote_json_string(&format!("{:?}", error)),
        );
        self.writer.entry_end(buffer, first);
    }
}

#[derive(Debug, Default)]
pub(crate) struct Stats {
    pub(crate) errors: u64,
    pub(crate) success: u64,
    pub(crate) targets: u64,
}

impl Stats {
    pub(crate) fn merge(&mut self, stats: &Stats) {
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
    fn target(&self, target_info: TargetInfo<'_>, buffer: &mut String) {
        if self.target_hash_graph_type != TargetHashGraphType::None {
            match target_info.target_hash {
                Some(hash) => {
                    writeln!(buffer, "{label} {hash}", label = target_info.node.label()).unwrap()
                }
                None => {} // print nothing if there is no hash and show_target_hash is specified.
            };
        } else {
            writeln!(buffer, "{}", target_info.node.label()).unwrap();
        }
        if self.target_call_stacks {
            print_target_call_stack_after_target(buffer, target_info.node.call_stack().as_deref());
        }
    }
}

pub(crate) fn print_target_call_stack_after_target(out: &mut String, call_stack: Option<&str>) {
    if let Some(call_stack) = call_stack {
        for line in call_stack.lines() {
            writeln!(out, "  {}", line).unwrap();
        }
    }
}

pub(crate) fn create_formatter(
    request: &TargetsRequest,
    other: &targets_request::Other,
) -> anyhow::Result<Arc<dyn TargetFormatter>> {
    let output_format = OutputFormat::from_i32(request.output_format)
        .context("Invalid value of `output_format` (internal error)")?;

    match output_format {
        OutputFormat::Json | OutputFormat::JsonLines => {}
        _ => {
            // Self-check.
            if !other.output_attributes.is_empty() {
                return Err(FormatterError::AttrsOnlyWithJson.into());
            }
        }
    }

    match output_format {
        OutputFormat::Unknown => Err(FormatterError::OutputFormatNotSet.into()),
        OutputFormat::Stats => Ok(Arc::new(StatsFormat)),
        OutputFormat::Text => Ok(Arc::new(TargetNameFormat {
            target_call_stacks: other.target_call_stacks,
            target_hash_graph_type: TargetHashGraphType::from_i32(other.target_hash_graph_type)
                .expect("buck cli should send valid target hash graph type"),
        })),
        OutputFormat::Json | OutputFormat::JsonLines => Ok(Arc::new(JsonFormat {
            attributes: if other.output_attributes.is_empty() {
                None
            } else {
                Some(RegexSet::new(&other.output_attributes)?)
            },
            attr_inspect_opts: if other.include_default_attributes {
                AttrInspectOptions::All
            } else {
                AttrInspectOptions::DefinedOnly
            },
            target_call_stacks: other.target_call_stacks,
            writer: JsonWriter {
                json_lines: output_format == OutputFormat::JsonLines,
            },
        })),
    }
}
