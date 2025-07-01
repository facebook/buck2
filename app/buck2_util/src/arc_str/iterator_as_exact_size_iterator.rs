/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

pub(crate) struct IteratorAsExactSizeIterator<I>(pub(crate) I);

impl<I: Iterator> Iterator for IteratorAsExactSizeIterator<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<J: Iterator> ExactSizeIterator for IteratorAsExactSizeIterator<J> {
    fn len(&self) -> usize {
        self.0.size_hint().0
    }
}
