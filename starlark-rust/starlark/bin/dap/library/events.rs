/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use debugserver_types::*;
use gazebo::prelude::*;
use serde::Serialize;
use serde_json::Value;

use crate::dap::library::stream::{log, send};

#[derive(Debug, Clone, Dupe)]
pub(crate) struct Client {
    _private: (),
}

impl Client {
    pub(crate) fn new() -> Self {
        Self { _private: () }
    }

    pub(crate) fn log(&self, x: &str) {
        log(x)
    }

    fn event(&self, x: impl Serialize) {
        send(serde_json::to_value(&x).unwrap())
    }

    pub(crate) fn event_stopped(&self, body: StoppedEventBody) {
        self.event(StoppedEvent {
            type_: "event".to_owned(),
            seq: 0,
            event: "stopped".to_owned(),
            body,
        })
    }

    pub(crate) fn event_initialized(&self, body: Option<Value>) {
        self.event(InitializedEvent {
            type_: "event".to_owned(),
            seq: 0,
            event: "initialized".to_owned(),
            body,
        })
    }

    pub(crate) fn event_exited(&self, body: ExitedEventBody) {
        self.event(ExitedEvent {
            type_: "event".to_owned(),
            seq: 0,
            event: "exited".to_owned(),
            body,
        })
    }

    pub(crate) fn event_terminated(&self, body: Option<TerminatedEventBody>) {
        self.event(TerminatedEvent {
            type_: "event".to_owned(),
            seq: 0,
            event: "terminated".to_owned(),
            body,
        })
    }

    pub(crate) fn event_output(&self, body: OutputEventBody) {
        self.event(OutputEvent {
            type_: "event".to_owned(),
            seq: 0,
            event: "output".to_owned(),
            body,
        })
    }
}
