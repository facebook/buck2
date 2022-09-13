/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;

use derive_more::Display;

use crate::execute::action_digest::ActionDigest;

#[derive(Debug, Display, Clone)]
pub enum CommandExecutionKind {
    /// This action was executed locally.
    #[display(fmt = "local")]
    Local {
        // Even though this did not run on RE, we still produced this, so we might as well report
        // it.
        digest: ActionDigest,
        command: Vec<String>,
        env: HashMap<String, String>,
    },
    /// This action was executed via a remote executor.
    #[display(fmt = "remote")]
    Remote { digest: ActionDigest },
    /// This action was served by the action cache and not executed.
    #[display(fmt = "action_cache")]
    ActionCache { digest: ActionDigest },
}

impl CommandExecutionKind {
    pub fn as_enum(&self) -> buck2_data::ActionExecutionKind {
        match self {
            Self::Local { .. } => buck2_data::ActionExecutionKind::Local,
            Self::Remote { .. } => buck2_data::ActionExecutionKind::Remote,
            Self::ActionCache { .. } => buck2_data::ActionExecutionKind::ActionCache,
        }
    }
}
