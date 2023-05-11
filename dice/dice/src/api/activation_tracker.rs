/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;

/// An ActivationTracker can be used to identify which keys were either reused or computed during a
/// transaction.
pub trait ActivationTracker: Send + Sync + 'static {
    /// Receives when a key was activated (computed, or reused). The caller will want to downcast
    /// the key and deps to types they care about. The caller also receives whatever the key passed
    /// to `store_evaluation_data` (if any).
    fn key_activated(
        &self,
        key: &dyn Any,
        deps: &mut dyn Iterator<Item = &dyn Any>,
        activation_data: ActivationData,
    );
}

/// Describes the kind of activation, and possibly carries data passed by the key's evaluation.
pub enum ActivationData {
    /// This key was evaluated. Evaluation data will be passed if the key's evaluation set any.
    Evaluated(Option<Box<dyn Any + Send + Sync + 'static>>),

    /// This key was reused. No data is passed.
    Reused,
}
