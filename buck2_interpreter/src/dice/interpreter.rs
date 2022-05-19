/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_common::dice::file_ops::FileChangeHandler;
use derive_more::Display;
use dice::{DiceComputations, InjectedKey};
use gazebo::prelude::*;

use crate::{dice::HasInterpreterContext, extra::InterpreterConfiguror};

#[derive(Clone, Dupe)]
struct BuildContext {
    interpreter_configuror: Arc<dyn InterpreterConfiguror>,
}

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq)]
#[display(fmt = "{:?}", self)]
struct BuildContextKey();

impl InjectedKey for BuildContextKey {
    type Value = Arc<dyn InterpreterConfiguror>;

    fn compare(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[async_trait]
impl HasInterpreterContext for DiceComputations {
    async fn get_interpreter_configuror(&self) -> Arc<dyn InterpreterConfiguror> {
        self.compute(&BuildContextKey()).await.dupe()
    }

    fn set_interpreter_context(&self, interpreter_configuror: Arc<dyn InterpreterConfiguror>) {
        self.changed_to(vec![(BuildContextKey(), interpreter_configuror)])
    }

    fn get_file_change_handler(&self) -> &dyn FileChangeHandler {
        self
    }
}
