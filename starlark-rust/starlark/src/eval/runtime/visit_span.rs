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

use crate::{
    collections::symbol_map::Symbol,
    environment::slots::ModuleSlotId,
    eval::{
        compiler::{expr::CompareOp, span::IrSpanned},
        runtime::call_stack::FrozenFileSpan,
    },
    values::{FrozenRef, FrozenValue, FrozenValueTyped, StarlarkValue},
};

/// Visitor for code spans in the IR.
pub(crate) trait VisitSpanMut {
    fn visit_spans(&mut self, visitor: &mut impl FnMut(&mut FrozenFileSpan));
}

impl<V: VisitSpanMut> VisitSpanMut for IrSpanned<V> {
    fn visit_spans(&mut self, visitor: &mut impl FnMut(&mut FrozenFileSpan)) {
        visitor(&mut self.span);
        self.node.visit_spans(visitor);
    }
}

impl VisitSpanMut for FrozenValue {
    fn visit_spans(&mut self, _visitor: &mut impl FnMut(&mut FrozenFileSpan)) {}
}

impl VisitSpanMut for String {
    fn visit_spans(&mut self, _visitor: &mut impl FnMut(&mut FrozenFileSpan)) {}
}

impl VisitSpanMut for bool {
    fn visit_spans(&mut self, _visitor: &mut impl FnMut(&mut FrozenFileSpan)) {}
}

impl VisitSpanMut for u32 {
    fn visit_spans(&mut self, _visitor: &mut impl FnMut(&mut FrozenFileSpan)) {}
}

impl VisitSpanMut for ModuleSlotId {
    fn visit_spans(&mut self, _visitor: &mut impl FnMut(&mut FrozenFileSpan)) {}
}

impl VisitSpanMut for CompareOp {
    fn visit_spans(&mut self, _visitor: &mut impl FnMut(&mut FrozenFileSpan)) {}
}

impl<V: VisitSpanMut> VisitSpanMut for Box<V> {
    fn visit_spans(&mut self, visitor: &mut impl FnMut(&mut FrozenFileSpan)) {
        (**self).visit_spans(visitor);
    }
}

impl<T: StarlarkValue<'static>> VisitSpanMut for FrozenValueTyped<'static, T> {
    fn visit_spans(&mut self, _visitor: &mut impl FnMut(&mut FrozenFileSpan)) {}
}

impl<T> VisitSpanMut for FrozenRef<'static, T> {
    fn visit_spans(&mut self, _visitor: &mut impl FnMut(&mut FrozenFileSpan)) {}
}

impl VisitSpanMut for Symbol {
    fn visit_spans(&mut self, _visitor: &mut impl FnMut(&mut FrozenFileSpan)) {}
}

impl<A: VisitSpanMut, B: VisitSpanMut> VisitSpanMut for (A, B) {
    fn visit_spans(&mut self, visitor: &mut impl FnMut(&mut FrozenFileSpan)) {
        self.0.visit_spans(visitor);
        self.1.visit_spans(visitor);
    }
}

impl<A: VisitSpanMut, B: VisitSpanMut, C: VisitSpanMut> VisitSpanMut for (A, B, C) {
    fn visit_spans(&mut self, visitor: &mut impl FnMut(&mut FrozenFileSpan)) {
        self.0.visit_spans(visitor);
        self.1.visit_spans(visitor);
        self.2.visit_spans(visitor);
    }
}

impl<A: VisitSpanMut, B: VisitSpanMut, C: VisitSpanMut, D: VisitSpanMut> VisitSpanMut
    for (A, B, C, D)
{
    fn visit_spans(&mut self, visitor: &mut impl FnMut(&mut FrozenFileSpan)) {
        self.0.visit_spans(visitor);
        self.1.visit_spans(visitor);
        self.2.visit_spans(visitor);
        self.3.visit_spans(visitor);
    }
}

impl<V: VisitSpanMut> VisitSpanMut for Vec<V> {
    fn visit_spans(&mut self, visitor: &mut impl FnMut(&mut FrozenFileSpan)) {
        for v in self {
            v.visit_spans(visitor);
        }
    }
}

impl<V: VisitSpanMut> VisitSpanMut for Option<V> {
    fn visit_spans(&mut self, visitor: &mut impl FnMut(&mut FrozenFileSpan)) {
        if let Some(v) = self {
            v.visit_spans(visitor);
        }
    }
}
