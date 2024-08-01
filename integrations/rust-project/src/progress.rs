/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Write;

use rustc_hash::FxHashMap;
use serde::Serialize;
use serde_json::Value;
use tracing::span;
use tracing_subscriber::fmt::MakeWriter;
use tracing_subscriber::Layer;

pub(crate) struct ProgressLayer<S, W> {
    _s: std::marker::PhantomData<S>,
    writer: W,
}

impl<S, W> ProgressLayer<S, W> {
    pub(crate) fn new(writer: W) -> Self {
        Self {
            _s: std::marker::PhantomData,
            writer,
        }
    }
}

#[derive(Serialize, Debug, Clone, PartialEq)]
struct Out<'a> {
    #[serde(flatten)]
    event_fields: &'a FxHashMap<String, serde_json::Value>,
    #[serde(flatten)]
    span_fields: &'a FxHashMap<String, serde_json::Value>,
}

#[derive(Debug, Clone, PartialEq)]
struct ProgressStorage {
    data: FxHashMap<String, serde_json::Value>,
}

impl<S, W> Layer<S> for ProgressLayer<S, W>
where
    S: tracing::Subscriber + for<'a> tracing_subscriber::registry::LookupSpan<'a>,
    W: for<'a> MakeWriter<'a> + 'static,
{
    fn on_new_span(
        &self,
        attrs: &span::Attributes<'_>,
        id: &span::Id,
        ctx: tracing_subscriber::layer::Context<'_, S>,
    ) {
        let span = ctx.span(id).unwrap();
        let mut extensions = span.extensions_mut();

        let mut fields = FxHashMap::default();
        let mut visitor = JsonVisitor(&mut fields);
        attrs.record(&mut visitor);

        extensions.insert(ProgressStorage { data: fields });
    }

    fn on_event(&self, event: &tracing::Event<'_>, ctx: tracing_subscriber::layer::Context<'_, S>) {
        let mut event_fields = FxHashMap::default();
        let mut visitor = JsonVisitor(&mut event_fields);
        event.record(&mut visitor);

        let span_fields = match ctx.lookup_current() {
            Some(span) => {
                let ext = span.extensions();
                if let Some(storage) = ext.get::<ProgressStorage>() {
                    storage.data.clone()
                } else {
                    FxHashMap::default()
                }
            }
            _ => FxHashMap::default(),
        };

        if !event_fields.contains_key("kind") {
            event_fields.insert("kind".to_owned(), Value::String("progress".to_owned()));
        }

        let out = Out {
            event_fields: &event_fields,
            span_fields: &span_fields,
        };
        let out = serde_json::to_string(&out).unwrap();
        let mut writer = self.writer.make_writer();
        writeln!(writer, "{}", out).expect("unable to write");
    }
}

struct JsonVisitor<'a>(&'a mut FxHashMap<String, serde_json::Value>);

impl<'a> tracing::field::Visit for JsonVisitor<'a> {
    fn record_str(&mut self, field: &tracing::field::Field, value: &str) {
        let value: String = if field.name() == "project" {
            serde_json::from_str(value).unwrap()
        } else {
            String::from(value)
        };
        self.0
            .insert(field.name().to_owned(), serde_json::Value::from(value));
    }

    fn record_debug(&mut self, field: &tracing::field::Field, value: &dyn std::fmt::Debug) {
        self.0.insert(
            field.name().to_owned(),
            serde_json::Value::from(format!("{:?}", value)),
        );
    }
}
