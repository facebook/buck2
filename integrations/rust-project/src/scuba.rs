/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use rustc_hash::FxHashMap;
use serde::Serialize;
use tracing::field::Visit;
use tracing_core::span::Attributes;
use tracing_core::span::Id;
use tracing_core::Subscriber;
use tracing_subscriber::layer::Context;
use tracing_subscriber::layer::Layer;
use tracing_subscriber::registry::LookupSpan;

const ALLOWED_FIELDS: &[&str] = &[
    // common
    "root_span",
    "duration_ms",
    // develop
    "develop_input",
    "project_root",
    "relative_paths",
    // check
    "saved_file",
    "use_clippy",
];

#[derive(Clone, Debug, Serialize)]
#[serde(untagged)]
enum Field {
    String(String),
    Int(i64),
}

#[derive(Clone, Debug)]
struct Fields(FxHashMap<&'static str, Field>);

impl Fields {
    fn new() -> Self {
        Self(FxHashMap::default())
    }

    fn record(&mut self, name: &'static str, value: Field) {
        if ALLOWED_FIELDS.contains(&name) {
            self.0.insert(name, value);
        }
    }
}

impl Visit for Fields {
    fn record_str(&mut self, field: &tracing_core::Field, value: &str) {
        self.record(field.name(), Field::String(value.to_owned()));
    }

    fn record_i64(&mut self, field: &tracing_core::Field, value: i64) {
        self.record(field.name(), Field::Int(value));
    }

    fn record_u64(&mut self, field: &tracing_core::Field, value: u64) {
        if let Ok(value) = i64::try_from(value) {
            self.record(field.name(), Field::Int(value));
        }
    }

    fn record_i128(&mut self, field: &tracing_core::Field, value: i128) {
        if let Ok(value) = i64::try_from(value) {
            self.record(field.name(), Field::Int(value));
        }
    }

    fn record_u128(&mut self, field: &tracing_core::Field, value: u128) {
        if let Ok(value) = i64::try_from(value) {
            self.record(field.name(), Field::Int(value));
        }
    }

    fn record_debug(&mut self, field: &tracing_core::Field, value: &dyn fmt::Debug) {
        self.record(field.name(), Field::String(format!("{:?}", value)));
    }
}

enum FieldsWriter {
    Scuba,
    Stdout,
}

pub struct ScubaLayer<S> {
    _s: std::marker::PhantomData<S>,
    writer: FieldsWriter,
}

impl<S> ScubaLayer<S> {
    pub fn new(log_scuba_to_stdout: bool) -> Self {
        Self {
            _s: std::marker::PhantomData,
            writer: if log_scuba_to_stdout {
                FieldsWriter::Stdout
            } else {
                FieldsWriter::Scuba
            },
        }
    }
}

impl<S> Default for ScubaLayer<S> {
    fn default() -> Self {
        Self::new(false)
    }
}

impl<S> Layer<S> for ScubaLayer<S>
where
    S: Subscriber + for<'a> LookupSpan<'a>,
{
    fn on_new_span(&self, attrs: &Attributes<'_>, id: &Id, ctx: Context<'_, S>) {
        let span = ctx.span(id).expect("Unable to find span; this is a bug");
        let mut fields = Fields::new();
        attrs.record(&mut fields);
        span.extensions_mut().insert(fields);
    }

    fn on_record(&self, id: &Id, values: &tracing::span::Record<'_>, ctx: Context<'_, S>) {
        let span = ctx.span(id).expect("Unable to find span; this is a bug");
        let mut ext = span.extensions_mut();
        let data = ext
            .get_mut::<Fields>()
            .expect("Missing extensions; this is a bug");
        values.record(data);
    }

    fn on_event(&self, event: &tracing::Event<'_>, ctx: Context<'_, S>) {
        let mut event_fields = Fields::new();
        event.record(&mut event_fields);

        let Some(span) = ctx.lookup_current() else {
            return;
        };

        let mut extensions = span.extensions_mut();

        let Some(span_fields) = extensions.get_mut::<Fields>() else {
            return;
        };

        span_fields.0.extend(event_fields.0);
    }

    fn on_close(&self, id: tracing_core::span::Id, ctx: Context<'_, S>) {
        let span = ctx.span(&id).expect("Unable to find span; this is a bug");
        match span.parent() {
            Some(parent) => {
                // accumulate fields from the current span to its parent
                let cur_span_extensions = span.extensions();
                let cur_span_fields = cur_span_extensions
                    .get::<Fields>()
                    .expect("Missing extensions; this is a bug");

                let mut parent_extensions = parent.extensions_mut();
                let parent_fields = parent_extensions
                    .get_mut::<Fields>()
                    .expect("Missing extensions; this is a bug");

                parent_fields.0.extend(cur_span_fields.0.clone());
            }
            None => {
                let mut extensions = span.extensions_mut();
                let mut fields = extensions
                    .remove::<Fields>()
                    .expect("Missing extensions; this is a bug");

                fields.record("root_span", Field::String(span.name().to_owned()));

                match self.writer {
                    FieldsWriter::Scuba => write_to_scuba(fields),
                    FieldsWriter::Stdout => {
                        println!("{}", serde_json::to_string(&fields.0).unwrap())
                    }
                }
            }
        }
    }
}

#[cfg(fbcode_build)]
fn write_to_scuba(fields: Fields) {
    let fb = fbinit::expect_init();
    let mut sample = scuba::ScubaSampleBuilder::new(fb, "rust_project");
    for (k, v) in fields.0 {
        let v = match v {
            Field::String(s) => scuba::ScubaValue::Normal(s),
            Field::Int(i) => scuba::ScubaValue::Int(i),
        };
        sample.add(k, v);
    }
    sample.add("unixname", whoami::username());
    sample.add("hostname", whoami::hostname());

    // RA_PROXY_SESSION_ID is an environment variable set by the VS Code extension when it starts
    // rust-analyzer-proxy. rust-analyzer-proxy then starts rust-analyzer with the same
    // environment, and rust-analyzer invokes rust-project with the inherited environment.
    if let Ok(session_id) = std::env::var("RA_PROXY_SESSION_ID") {
        sample.add("session_id", session_id);
    }

    sample.log();
}

#[cfg(not(fbcode_build))]
fn write_to_scuba(_fields: Fields) {}
