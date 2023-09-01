/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;

use crossbeam::channel::Sender;
use lsp_types::notification::Notification;
use lsp_types::WorkDoneProgress;
use tracing::span;
use tracing_subscriber::Layer;

pub(crate) struct ProgressLayer<S> {
    _s: std::marker::PhantomData<S>,
    sender: Sender<lsp_server::Message>,
}

impl<S> ProgressLayer<S> {
    pub(crate) fn new(sender: Sender<lsp_server::Message>) -> Self {
        ProgressLayer {
            _s: std::marker::PhantomData,
            sender,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct ProgressStorage;

impl<S> Layer<S> for ProgressLayer<S>
where
    S: tracing::Subscriber + for<'a> tracing_subscriber::registry::LookupSpan<'a>,
{
    fn on_new_span(
        &self,
        attrs: &span::Attributes<'_>,
        id: &span::Id,
        ctx: tracing_subscriber::layer::Context<'_, S>,
    ) {
        if attrs.metadata().target() != "lsp_progress" {
            return;
        }

        let span = ctx.span(id).unwrap();
        let mut extensions = span.extensions_mut();
        extensions.insert(ProgressStorage);

        let mut fields = BTreeMap::new();
        let mut visitor = StringVisitor(&mut fields);
        attrs.record(&mut visitor);

        let begin = lsp_types::WorkDoneProgressBegin {
            title: String::from(attrs.metadata().name()),
            cancellable: Some(true),
            message: Some(String::from("resolving targets")),
            percentage: None,
        };

        let token: lsp_types::NumberOrString =
            lsp_types::ProgressToken::String("rust-project/discoverBuckTargets".to_owned());

        let notification = lsp_server::Notification::new(
            lsp_types::notification::Progress::METHOD.to_owned(),
            lsp_types::ProgressParams {
                token,
                value: lsp_types::ProgressParamsValue::WorkDone(WorkDoneProgress::Begin(begin)),
            },
        );

        let _err = self.sender.send(notification.into());
    }

    fn on_event(&self, event: &tracing::Event<'_>, _: tracing_subscriber::layer::Context<'_, S>) {
        let mut fields = BTreeMap::new();
        let mut visitor = StringVisitor(&mut fields);
        event.record(&mut visitor);

        let message = fields.get("message").map(|value| value.to_owned());

        let token: lsp_types::NumberOrString =
            lsp_types::ProgressToken::String("rust-project/discoverBuckTargets".to_owned());

        let report = lsp_types::WorkDoneProgressReport {
            message,
            cancellable: Some(true),
            percentage: None,
        };

        let report: lsp_types::ProgressParamsValue =
            lsp_types::ProgressParamsValue::WorkDone(WorkDoneProgress::Report(report));
        let notification = lsp_server::Notification::new(
            lsp_types::notification::Progress::METHOD.to_owned(),
            lsp_types::ProgressParams {
                token,
                value: report,
            },
        );

        let _err = self.sender.send(notification.into());
    }

    fn on_close(&self, id: span::Id, ctx: tracing_subscriber::layer::Context<'_, S>) {
        let span = ctx.span(&id).unwrap();
        let extensions = span.extensions();
        if extensions.get::<ProgressStorage>().is_none() {
            return;
        }

        let token: lsp_types::NumberOrString =
            lsp_types::ProgressToken::String("rust-project/discoverBuckTargets".to_owned());

        let end = lsp_types::WorkDoneProgressEnd {
            message: Some(String::from("resolving targets")),
        };

        let report: lsp_types::ProgressParamsValue =
            lsp_types::ProgressParamsValue::WorkDone(WorkDoneProgress::End(end));

        let notification = lsp_server::Notification::new(
            lsp_types::notification::Progress::METHOD.to_owned(),
            lsp_types::ProgressParams {
                token,
                value: report,
            },
        );

        let _err = self.sender.send(notification.into());
    }
}

struct StringVisitor<'a>(&'a mut BTreeMap<String, String>);

impl<'a> tracing::field::Visit for StringVisitor<'a> {
    fn record_str(&mut self, field: &tracing::field::Field, value: &str) {
        self.0.insert(field.name().to_owned(), String::from(value));
    }

    fn record_debug(&mut self, field: &tracing::field::Field, value: &dyn std::fmt::Debug) {
        self.0
            .insert(field.name().to_owned(), format!("{:?}", value));
    }
}
