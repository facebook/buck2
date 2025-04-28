/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;
use std::pin::Pin;

use buck2_util::late_binding::LateBinding;

use crate::ctx::ServerCommandContextTrait;
use crate::partial_result_dispatcher::NoPartialResult;
use crate::partial_result_dispatcher::PartialResultDispatcher;

pub static TEST_COMMAND: LateBinding<
    for<'a> fn(
        ctx: &'a (dyn ServerCommandContextTrait + 'a),
        partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
        req: buck2_cli_proto::TestRequest,
    ) -> Pin<
        Box<dyn Future<Output = buck2_error::Result<buck2_cli_proto::TestResponse>> + Send + 'a>,
    >,
> = LateBinding::new("TEST_COMMAND");
