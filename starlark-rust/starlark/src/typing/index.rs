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

use std::fmt::Debug;
use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;

use crate::typing::Ty;
use crate::typing::TyBasic;
use crate::typing::TypingOracleCtx;
use crate::typing::error::TypingNoContextOrInternalError;

/// Custom index (`[x] -> y`) typechecker.
pub trait TyCustomIndexImpl: Allocative + Debug + Send + Sync + 'static {
    /// Type check an index operation. The idea of using this is for the return type to depend on
    /// the index item type.
    fn index(
        &self,
        item: &TyBasic,
        ctx: &TypingOracleCtx,
    ) -> Result<Ty, TypingNoContextOrInternalError>;
}

/// An `Arc<dyn>` wrapper for [`TyCustomIndexImpl`].
#[derive(Debug, Allocative, Clone, Dupe)]
pub struct TyCustomIndex(pub(crate) Arc<dyn TyCustomIndexImpl>);

impl TyCustomIndex {
    /// Create new
    pub fn new<T: TyCustomIndexImpl>(ty: T) -> Self {
        Self(Arc::new(ty))
    }
}
