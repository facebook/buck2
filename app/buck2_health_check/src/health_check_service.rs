/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(dead_code)] // Will be used in the next diff

use crate::report::Report;

#[async_trait::async_trait]
pub(crate) trait HealthCheckService: Sync + Send {
    async fn update_context(
        &mut self,
        event: &buck2_health_check_proto::HealthCheckContextEvent,
    ) -> buck2_error::Result<()>;

    async fn run_checks(&mut self) -> buck2_error::Result<Vec<Report>>;
}
