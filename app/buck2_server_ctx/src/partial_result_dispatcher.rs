/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::marker::PhantomData;

use buck2_cli_proto::partial_result;
use buck2_cli_proto::PartialResult;
use buck2_events::dispatch::EventDispatcher;

use crate::stdout_partial_output::StdoutPartialOutput;

/// A typed partial result dispatcher. Each command can only send one kind of partial result, hence
/// the typing.
pub struct PartialResultDispatcher<T> {
    dispatcher: EventDispatcher,
    result_type: PhantomData<T>,
}

impl<T> PartialResultDispatcher<T>
where
    T: Into<partial_result::PartialResult>,
{
    pub fn new(dispatcher: EventDispatcher) -> Self {
        Self {
            dispatcher,
            result_type: PhantomData,
        }
    }

    /// NOTE: This doesn't actually require &mut self but that's been reasonable to have for the
    /// predecessor to this (stdout) so keeping it this way.
    pub fn emit(&mut self, res: T) {
        self.dispatcher.control_event(PartialResult {
            partial_result: Some(res.into()),
        });
    }
}

impl PartialResultDispatcher<buck2_cli_proto::StdoutBytes> {
    pub fn as_writer(&mut self) -> StdoutPartialOutput<'_> {
        StdoutPartialOutput::new(self)
    }
}

/// An uninhabited type for methods that do not produce partial results.
pub enum NoPartialResult {}

impl From<NoPartialResult> for partial_result::PartialResult {
    fn from(v: NoPartialResult) -> Self {
        match v {}
    }
}
