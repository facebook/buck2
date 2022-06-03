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

//! The range type, constructed with `range()`.

use std::{
    fmt::{self, Display},
    marker::PhantomData,
    num::NonZeroI32,
};

use gazebo::{any::ProvidesStaticType, prelude::*};

use crate as starlark;
use crate::values::{
    index::{convert_index, convert_slice_indices},
    Heap, StarlarkValue, Value, ValueError, ValueLike,
};

/// Representation of `range()` type.
#[derive(Clone, Copy, Dupe, Debug, ProvidesStaticType, NoSerialize)]
pub struct Range {
    start: i32,
    stop: i32,
    step: NonZeroI32,
}

impl Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.step.get() != 1 {
            write!(f, "range({}, {}, {})", self.start, self.stop, self.step)
        } else if self.start != 0 {
            write!(f, "range({}, {})", self.start, self.stop)
        } else {
            write!(f, "range({})", self.stop)
        }
    }
}

starlark_simple_value!(Range);

impl Range {
    /// The result of calling `type()` on a range.
    pub const TYPE: &'static str = "range";

    /// Create a new [`Range`].
    pub fn new(start: i32, stop: i32, step: NonZeroI32) -> Range {
        Range { start, stop, step }
    }

    fn equals_range(&self, other: &Range) -> anyhow::Result<bool> {
        let self_length = self.length()?;
        let other_length = other.length()?;
        if self_length == 0 || other_length == 0 {
            return Ok(self_length == other_length);
        }
        if self.start != other.start {
            return Ok(false);
        }
        if self_length == 1 || other_length == 1 {
            return Ok(self_length == other_length);
        }
        debug_assert!(self_length > 1);
        debug_assert!(other_length > 1);
        if self.step.get() == other.step.get() {
            return Ok(self_length == other_length);
        } else {
            return Ok(false);
        }
    }
}

/// Implementation of an iterator over [`Range`].
struct RangeIterator<'a>(Range, PhantomData<&'a ()>);

impl<'a> Iterator for RangeIterator<'a> {
    type Item = Value<'a>;

    fn next(&mut self) -> Option<Value<'a>> {
        if !self.0.to_bool() {
            return None;
        }

        let old_start = self.0.start;
        self.0.start = self.0.start.saturating_add(self.0.step.get());
        Some(Value::new_int(old_start))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self.0.length() {
            Ok(n) => (n as usize, Some(n as usize)),
            Err(_) => (0, None),
        }
    }
}

impl<'v> StarlarkValue<'v> for Range {
    starlark_type!(Range::TYPE);

    fn to_bool(&self) -> bool {
        (self.start < self.stop && self.step.get() > 0)
            || (self.start > self.stop && self.step.get() < 0)
    }

    fn length(&self) -> anyhow::Result<i32> {
        if self.start == self.stop {
            return Ok(0);
        }

        // If step is into opposite direction of stop, then length is zero.
        if (self.stop >= self.start) != (self.step.get() > 0) {
            return Ok(0);
        }

        // Convert range and step to `u64`
        let (dist, step) = if self.step.get() >= 0 {
            (
                self.stop.wrapping_sub(self.start) as u64,
                self.step.get() as u64,
            )
        } else {
            (
                self.start.wrapping_sub(self.stop) as u64,
                self.step.get().wrapping_neg() as u64,
            )
        };
        let i = ((dist - 1) / step + 1) as i32;
        if i >= 0 {
            Ok(i)
        } else {
            Err(ValueError::IntegerOverflow.into())
        }
    }

    fn at(&self, index: Value, _heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let index = convert_index(index, self.length()?)?;
        // Must not overflow if `length` is computed correctly
        Ok(Value::new_int(self.start + self.step.get() * index))
    }

    fn equals(&self, other: Value) -> anyhow::Result<bool> {
        if let Some(other) = other.downcast_ref::<Self>() {
            self.equals_range(&*other)
        } else {
            Ok(false)
        }
    }

    fn slice(
        &self,
        start: Option<Value>,
        stop: Option<Value>,
        stride: Option<Value>,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        let (start, stop, step) = convert_slice_indices(self.length()?, start, stop, stride)?;
        return Ok(heap.alloc(Range {
            start: self
                .start
                .checked_add(
                    start
                        .checked_mul(self.step.get())
                        .ok_or(ValueError::IntegerOverflow)?,
                )
                .ok_or(ValueError::IntegerOverflow)?,
            stop: self
                .start
                .checked_add(
                    stop.checked_mul(self.step.get())
                        .ok_or(ValueError::IntegerOverflow)?,
                )
                .ok_or(ValueError::IntegerOverflow)?,
            step: NonZeroI32::new(
                step.checked_mul(self.step.get())
                    .ok_or(ValueError::IntegerOverflow)?,
            )
            .unwrap(),
        }));
    }

    fn iterate<'a>(
        &'a self,
        _heap: &'v Heap,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Value<'v>> + 'a>>
    where
        'v: 'a,
    {
        Ok(box RangeIterator::<'v>(*self, PhantomData))
    }

    fn with_iterator(
        &self,
        _heap: &'v Heap,
        f: &mut dyn FnMut(&mut dyn Iterator<Item = Value<'v>>) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        f(&mut RangeIterator::<'v>(*self, PhantomData))
    }

    fn is_in(&self, other: Value) -> anyhow::Result<bool> {
        let other = match other.unpack_num().and_then(|n| n.as_int()) {
            Some(other) => other,
            None => {
                // Consider `"a" in range(3)`
                //
                // Should we error or return false?
                // Go Starlark errors. Python returns false.
                // Discussion at https://github.com/bazelbuild/starlark/issues/175
                return Ok(false);
            }
        };
        if !self.to_bool() {
            return Ok(false);
        }
        if self.start == other {
            return Ok(true);
        }
        if self.step.get() > 0 {
            if other < self.start || other >= self.stop {
                return Ok(false);
            }
            Ok((other.wrapping_sub(self.start) as u64) % (self.step.get() as u64) == 0)
        } else {
            if other > self.start || other <= self.stop {
                return Ok(false);
            }
            Ok(
                (self.start.wrapping_sub(other) as u64) % (self.step.get().wrapping_neg() as u64)
                    == 0,
            )
        }
    }
}

/// For tests
impl PartialEq for Range {
    fn eq(&self, other: &Range) -> bool {
        self.equals_range(other).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use std::num::NonZeroI32;

    use crate::values::{range::Range, Heap, StarlarkValue, Value};

    fn range(start: i32, stop: i32, range: i32) -> Range {
        Range {
            start,
            stop,
            step: NonZeroI32::new(range).unwrap(),
        }
    }

    fn range_start_stop(start: i32, stop: i32) -> Range {
        range(start, stop, 1)
    }

    fn range_stop(stop: i32) -> Range {
        range_start_stop(0, stop)
    }

    #[test]
    fn length_stop() {
        assert_eq!(Some(0), range_stop(0).length().ok());
        assert_eq!(Some(17), range_stop(17).length().ok());
    }

    #[test]
    fn length_start_stop() {
        assert_eq!(Some(20), range_start_stop(10, 30).length().ok());
        assert_eq!(Some(0), range_start_stop(10, -30).length().ok());
        assert_eq!(
            Some(i32::max_value()),
            range_start_stop(0, i32::max_value()).length().ok()
        );
        assert!(range_start_stop(-1, i32::max_value()).length().is_err());
    }

    #[test]
    fn length_start_stop_step() {
        assert_eq!(Some(5), range(0, 10, 2).length().ok());
        assert_eq!(Some(5), range(0, 9, 2).length().ok());
        assert_eq!(Some(0), range(0, 10, -2).length().ok());
        assert_eq!(Some(5), range(10, 0, -2).length().ok());
        assert_eq!(Some(5), range(9, 0, -2).length().ok());
        assert_eq!(Some(1), range(4, 14, 10).length().ok());
    }

    #[test]
    fn eq() {
        assert_eq!(range_stop(0), range(2, 1, 3));
    }

    #[test]
    fn test_range_exhaustive() {
        // The range implementation is fairly hairy. Lots of corner cases etc.
        // Especially around equality, length.
        // Therefore, generate ranges exhaustively over a very small range
        // and test lots of properties about them.
        let mut ranges = Vec::with_capacity(294);
        for start in -3..4 {
            for stop in -3..4 {
                for step in -3..3 {
                    let step = if step >= 0 { step + 1 } else { step };
                    ranges.push(range(start, stop, step))
                }
            }
        }
        assert_eq!(ranges.len(), 294); // Assert we don't accidentally take too long

        let heap = Heap::new();
        for x in &ranges {
            let full: Vec<Value> = x.iterate(&heap).unwrap().collect();
            assert_eq!(x.length().unwrap(), full.len() as i32);
            for (i, v) in full.iter().enumerate() {
                assert_eq!(x.at(Value::new_int(i as i32), &heap).unwrap(), *v);
            }
        }

        // Takes 294^2 steps - but completes instantly
        for x in &ranges {
            for y in &ranges {
                assert_eq!(
                    x == y,
                    Iterator::eq(x.iterate(&heap).unwrap(), y.iterate(&heap).unwrap())
                )
            }
        }
    }
}
