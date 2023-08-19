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

use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;
use std::iter;

use allocative::Allocative;

use crate::typing::Ty;
use crate::typing::TypingOracleCtx;

#[derive(Eq, PartialEq, Hash, Clone, Debug, Ord, PartialOrd, Allocative)]
pub struct TyTuple {
    pub(crate) elems: Vec<Ty>,
}

impl TyTuple {
    pub(crate) fn get(&self, i: usize) -> Option<&Ty> {
        self.elems.get(i)
    }

    pub(crate) fn item_ty(&self) -> Ty {
        Ty::unions(self.elems.clone())
    }

    pub(crate) fn intersects(this: &TyTuple, other: &TyTuple, ctx: &TypingOracleCtx) -> bool {
        this.elems.len() == other.elems.len()
            && iter::zip(&this.elems, &other.elems).all(|(x, y)| ctx.intersects(x, y))
    }
}

impl Display for TyTuple {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.elems.as_slice() {
            [x] => write!(f, "({},)", x),
            xs => display_container::fmt_container(f, "(", ")", xs),
        }
    }
}
