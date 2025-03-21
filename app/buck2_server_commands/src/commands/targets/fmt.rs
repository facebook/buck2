/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeSet;
use std::fmt::Write;
use std::sync::Arc;

use buck2_cli_proto::targets_request;
use buck2_cli_proto::targets_request::OutputFormat;
use buck2_cli_proto::targets_request::TargetHashGraphType;
use buck2_cli_proto::HasClientContext;
use buck2_cli_proto::TargetsRequest;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::package::PackageLabel;
use buck2_error::internal_error;
use buck2_error::BuckErrorContext;
use buck2_node::attrs::hacks::value_to_json;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::nodes::attributes::DEPS;
use buck2_node::nodes::attributes::INPUTS;
use buck2_node::nodes::attributes::ONCALL;
use buck2_node::nodes::attributes::PACKAGE;
use buck2_node::nodes::attributes::PACKAGE_VALUES;
use buck2_node::nodes::attributes::TARGET_CALL_STACK;
use buck2_node::nodes::attributes::TARGET_HASH;
use buck2_node::nodes::attributes::TYPE;
use buck2_node::nodes::unconfigured::TargetNodeRef;
use buck2_node::super_package::SuperPackage;
use buck2_util::indent::indent;
use gazebo::prelude::SliceExt;
use regex::RegexSet;

use crate::json::QuotedJson;
use crate::target_hash::BuckTargetHash;

pub(crate) struct TargetInfo<'a> {
    pub(crate) node: TargetNodeRef<'a>,
    pub(crate) target_hash: Option<BuckTargetHash>,
    pub(crate) super_package: &'a SuperPackage,
}

fn package_error_to_stderr(package: PackageLabel, error: &buck2_error::Error, stderr: &mut String) {
    writeln!(stderr, "Error parsing {package}\n{error:?}").unwrap();
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
    fn package_error(
        &self,
        package: PackageLabel,
        error: &buck2_error::Error,
        stdout: &mut String,
        stderr: &mut String,
    ) {
        package_error_to_stderr(package, error, stderr);
    }
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

    pub(crate) fn entry_item(
        &self,
        buffer: &mut String,
        first: &mut bool,
        key: &str,
        value: QuotedJson,
    ) {
        if *first {
            *first = false;
        } else if self.json_lines {
            buffer.push(',');
        } else {
            buffer.push_str(",\n");
        }
        if !self.json_lines {
            buffer.push_str("    ");
        }
        write!(
            buffer,
            "{}:{}",
            serde_json::to_string(key).unwrap(),
            value.as_str()
        )
        .unwrap();
    }
}

struct JsonFormat {
    attributes: Option<RegexSet>,
    attr_inspect_opts: AttrInspectOptions,
    target_call_stacks: bool,
    package_values: Option<RegexSet>,
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
            v: impl FnOnce() -> QuotedJson,
        ) {
            if let Some(filter) = &this.attributes {
                if !filter.is_match(k) {
                    return;
                }
            }
            this.writer.entry_item(buffer, first, k, v());
        }

        print_attr(self, buffer, &mut first, TYPE, || {
            QuotedJson::quote_str(&target_info.node.rule_type().to_string())
        });
        print_attr(self, buffer, &mut first, DEPS, || {
            QuotedJson::list(target_info.node.deps().map(QuotedJson::quote_display))
        });

        print_attr(self, buffer, &mut first, INPUTS, || {
            QuotedJson::list(target_info.node.inputs().map(QuotedJson::quote_display))
        });

        if let Some(hash) = target_info.target_hash {
            print_attr(self, buffer, &mut first, TARGET_HASH, || {
                QuotedJson::quote_display(hash)
            });
        }
        print_attr(self, buffer, &mut first, PACKAGE, || {
            QuotedJson::quote_display(target_info.node.label().pkg())
        });

        if let Some(filter) = &self.package_values {
            print_attr(self, buffer, &mut first, PACKAGE_VALUES, || {
                let package_values = serde_json::Value::Object(
                    target_info
                        .super_package
                        .package_values()
                        .package_values_json()
                        // TODO(nga): return error.
                        .unwrap()
                        .iter()
                        .filter(|(k, _)| filter.is_match(k.as_str()))
                        .map(|(k, v)| (k.as_str().to_owned(), v.clone()))
                        .collect(),
                );
                QuotedJson::from_serde_json_value(package_values)
            });
        }

        if let Some(oncall) = target_info.node.oncall() {
            print_attr(self, buffer, &mut first, ONCALL, || {
                QuotedJson::quote_display(oncall)
            });
        }

        for a in target_info.node.attrs(self.attr_inspect_opts) {
            print_attr(self, buffer, &mut first, a.name, || {
                QuotedJson::from_serde_json_value(
                    value_to_json(a.value, target_info.node.label().pkg()).unwrap(),
                )
            });
        }

        if self.target_call_stacks {
            match target_info.node.call_stack() {
                Some(call_stack) => {
                    print_attr(self, buffer, &mut first, TARGET_CALL_STACK, || {
                        QuotedJson::quote_str(&call_stack)
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
                QuotedJson::quote_str(&package.to_string()),
            );
        }
        self.writer.entry_item(
            buffer,
            &mut first,
            "buck.file",
            QuotedJson::quote_str(&source.to_string()),
        );
        self.writer.entry_item(
            buffer,
            &mut first,
            "buck.imports",
            QuotedJson::list(imports.map(|d| QuotedJson::quote_display(d.path()))),
        );
        self.writer.entry_end(buffer, first);
    }

    fn package_error(
        &self,
        package: PackageLabel,
        error: &buck2_error::Error,
        stdout: &mut String,
        stderr: &mut String,
    ) {
        // When an error happens we print it to stdout (as a JSON entry) and to stderr (as a human message).
        // If the user has keep-going turned on, they'll get the JSON on stdout, but also have the error message appear on stderr.
        // If the user has keep-going turned off, they'll only see one error message and then abort.
        package_error_to_stderr(package, error, stderr);
        self.writer.entry_start(stdout);
        let mut first = true;
        self.writer.entry_item(
            stdout,
            &mut first,
            PACKAGE,
            QuotedJson::quote_display(package),
        );
        self.writer.entry_item(
            stdout,
            &mut first,
            "buck.error",
            QuotedJson::quote_str(&format!("{error:?}")),
        );
        self.writer.entry_end(stdout, first);
    }
}

#[derive(Debug, Default)]
pub(crate) struct Stats {
    pub(crate) errors: u64,
    error_tags: BTreeSet<buck2_error::ErrorTag>,
    pub(crate) success: u64,
    pub(crate) targets: u64,
}

impl Stats {
    pub(crate) fn merge(&mut self, stats: &Stats) {
        self.errors += stats.errors;
        self.success += stats.success;
        self.targets += stats.targets;
    }

    pub(crate) fn add_error(&mut self, e: &buck2_error::Error) {
        self.error_tags.extend(e.tags());
        self.errors += 1;
    }

    pub(crate) fn to_error(&self) -> Option<buck2_error::Error> {
        if self.errors == 0 {
            return None;
        }
        // Simpler error so that we don't print long errors twice (when exiting buck2)
        let package_str = if self.errors == 1 {
            "package"
        } else {
            "packages"
        };

        #[derive(buck2_error::Error, Debug)]
        #[buck2(tag = Input)]
        enum TargetsError {
            #[error("Failed to parse {0} {1}")]
            FailedToParse(u64, &'static str),
        }

        let mut e = buck2_error::Error::from(TargetsError::FailedToParse(self.errors, package_str));
        e = e.tag(self.error_tags.iter().copied());
        Some(e.into())
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
        write!(out, "{}", indent("  ", call_stack)).unwrap();
    }
}

pub(crate) fn create_formatter(
    request: &TargetsRequest,
    other: &targets_request::Other,
) -> buck2_error::Result<Arc<dyn TargetFormatter>> {
    let output_format = OutputFormat::try_from(request.output_format)
        .internal_error("Invalid value of `output_format`")?;

    let target_call_stacks = request.client_context()?.target_call_stacks;

    match output_format {
        OutputFormat::Json | OutputFormat::JsonLines => {}
        _ => {
            // Self-check.
            if !other.output_attributes.is_empty() {
                return Err(internal_error!(
                    "Attributes can only be specified when output format is JSON"
                ));
            }
        }
    }

    match output_format {
        OutputFormat::Unknown => Err(internal_error!("`output_format` is not set")),
        OutputFormat::Stats => Ok(Arc::new(StatsFormat)),
        OutputFormat::Text => Ok(Arc::new(TargetNameFormat {
            target_call_stacks,
            target_hash_graph_type: TargetHashGraphType::try_from(other.target_hash_graph_type)
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
            target_call_stacks,
            package_values: if other.package_values.is_empty() {
                None
            } else {
                Some(RegexSet::new(&other.package_values)?)
            },
            writer: JsonWriter {
                json_lines: output_format == OutputFormat::JsonLines,
            },
        })),
    }
}
