/*
 * Copyright 2018 The Starlark in Rust Authors.
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

//! Methods for the `set` type.

use std::mem;

use starlark_derive::starlark_module;
use starlark_map::small_set::SmallSet;
use starlark_syntax::value_error;

use crate as starlark;
use crate::environment::MethodsBuilder;
use crate::values::none::NoneType;
use crate::values::set::refs::SetMut;
use crate::values::set::refs::SetRef;
use crate::values::set::value::SetData;
use crate::values::typing::StarlarkIter;
use crate::values::Heap;
use crate::values::Value;
use crate::values::ValueOfUnchecked;

#[starlark_module]
pub(crate) fn set_methods(builder: &mut MethodsBuilder) {
    fn clear(this: Value) -> anyhow::Result<NoneType> {
        let mut this = SetMut::from_value(this)?;
        this.clear();
        Ok(NoneType)
    }

    /// Return a new set with elements from the set and all others.
    /// Unlike Python does not support variable number of arguments.
    /// ```
    /// # starlark::assert::is_true(r#"
    /// x = set([1, 2, 3])
    /// y = [3, 4, 5]
    /// x.union(y) == set([1, 2, 3, 4, 5])
    /// # "#);
    /// ```
    fn union<'v>(
        this: SetRef<'v>,
        #[starlark(require=pos)] other: ValueOfUnchecked<'v, StarlarkIter<Value<'v>>>,
        heap: &'v Heap,
    ) -> starlark::Result<SetData<'v>> {
        let it = other.get().iterate(heap)?;
        // TODO(romanp) omptimize if this is empty
        let mut data = this.content.clone();
        for elem in it {
            let hashed = elem.get_hashed()?;
            data.insert_hashed(hashed);
        }
        Ok(SetData { content: data })
    }

    /// Return a new set with elements common to the set and all others.
    /// Unlike Python does not support variable number of arguments.
    /// ```
    /// # starlark::assert::is_true(r#"
    /// x = set([1, 2, 3])
    /// y = [3, 4, 5]
    /// x.intersection(y) == set([3])
    /// # "#);
    /// ```
    fn intersection<'v>(
        this: SetRef<'v>,
        #[starlark(require=pos)] other: ValueOfUnchecked<'v, StarlarkIter<Value<'v>>>,
        heap: &'v Heap,
    ) -> starlark::Result<SetData<'v>> {
        //TODO(romanp) check if other is set
        let other_it = other.get().iterate(heap)?;
        let mut other_set = SmallSet::default();
        for elem in other_it {
            other_set.insert_hashed(elem.get_hashed()?);
        }
        let mut data = SetData::default();
        if other_set.is_empty() {
            return Ok(data);
        }

        for hashed in this.content.iter_hashed() {
            if other_set.contains_hashed(hashed) {
                data.content.insert_hashed_unique_unchecked(hashed.copied());
            }
        }
        Ok(data)
    }

    /// Returns a new set with elements in either the set or the specified iterable but not both.
    /// ```
    /// # starlark::assert::is_true(r#"
    /// x = set([1, 2, 3])
    /// y = [3, 4, 5]
    /// x.symmetric_difference(y) == set([1, 2, 4, 5])
    /// # "#);
    /// ```
    fn symmetric_difference<'v>(
        this: SetRef<'v>,
        #[starlark(require=pos)] other: ValueOfUnchecked<'v, StarlarkIter<Value<'v>>>,
        heap: &'v Heap,
    ) -> starlark::Result<SetData<'v>> {
        //TODO(romanp) check if other is set
        let other_it = other.get().iterate(heap)?;
        let mut other_set: SmallSet<Value<'v>> = SmallSet::default();
        for elem in other_it {
            other_set.insert_hashed(elem.get_hashed()?);
        }

        if other_set.is_empty() {
            return Ok(SetData {
                content: this.content.clone(),
            });
        }

        //TODO(romanp) add symmetric_difference to small set and use it here and in xor
        if this.content.is_empty() {
            return Ok(SetData { content: other_set });
        }

        let mut data = SetData::default();
        for elem in this.content.iter_hashed() {
            if !other_set.contains_hashed(elem) {
                data.add_hashed(elem.copied());
            }
        }

        for elem in other_set {
            let hashed = elem.get_hashed()?;
            if !this.content.contains_hashed(hashed.as_ref()) {
                data.add_hashed(hashed);
            }
        }
        Ok(data)
    }

    /// Add an item to the set.
    /// ```
    /// # starlark::assert::is_true(r#"
    /// x = set([1, 2, 3])
    /// x.add(4)
    /// x == set([1, 2, 3, 4])
    /// # "#);
    /// ```
    fn add<'v>(
        this: Value<'v>,
        #[starlark(require = pos)] value: Value<'v>,
    ) -> starlark::Result<NoneType> {
        let mut this = SetMut::from_value(this)?;
        let hashed = value.get_hashed()?;
        this.add_hashed(hashed);
        Ok(NoneType)
    }

    /// Remove the item from the set. It raises an error if there is no such item.
    ///
    /// `remove` fails if the key is unhashable or if the dictionary is
    /// frozen.
    /// Time complexity of this operation is *O(N)* where *N* is the number of entries in the set.
    ///
    /// ```
    /// # starlark::assert::is_true(r#"
    /// x = set([1, 2, 3])
    /// x.remove(2)
    /// x == set([1, 3])
    /// # "#)
    /// ```
    /// A subsequent call to `x.remove(2)` would yield an error because the
    /// element won't be found.
    /// ```
    /// # starlark::assert::fail(r#"
    /// x = set([1, 2, 3])
    /// x.remove(2)
    /// x.remove(2) # error: not found
    /// # "#, "not found");
    /// ```
    fn remove<'v>(
        this: Value<'v>,
        #[starlark(require = pos)] value: Value<'v>,
    ) -> starlark::Result<NoneType> {
        let mut set = SetMut::from_value(this)?;
        let hashed = value.get_hashed()?;
        if set.remove_hashed(hashed.as_ref()) {
            Ok(NoneType)
        } else {
            mem::drop(set);
            Err(value_error!("`{value}` not found in `{this}`"))
        }
    }

    /// Remove the item from the set. It does nothing if there is no such item.
    ///
    /// `discard` fails if the key is unhashable or if the dictionary is
    /// frozen.
    /// Time complexity of this operation is *O(N)* where *N* is the number of entries in the set.
    ///
    /// ```
    /// # starlark::assert::is_true(r#"
    /// x = set([1, 2, 3])
    /// x.discard(2)
    /// x == set([1, 3])
    /// # "#)
    /// ```
    /// A subsequent call to `x.discard(2)` would do nothing.
    /// ```
    /// # starlark::assert::is_true(r#"
    /// x = set([1, 2, 3])
    /// x.discard(2)
    /// x.discard(2)
    /// x == set([1, 3])
    /// # "#);
    /// ```
    fn discard<'v>(
        this: Value<'v>,
        #[starlark(require = pos)] value: Value<'v>,
    ) -> starlark::Result<NoneType> {
        let mut set = SetMut::from_value(this)?;
        let hashed = value.get_hashed()?;
        set.remove_hashed(hashed.as_ref());
        Ok(NoneType)
    }

    /// Removes and returns the first element of a set.
    ///
    /// `S.pop()` removes and returns the first element of the set S.
    ///
    /// `pop` fails if the set is empty, or if the set is frozen or has active iterators.
    /// Time complexity of this operation is *O(N)* where *N* is the number of entries in the set.
    ///
    /// ```
    /// # starlark::assert::is_true(r#"
    /// x = set([1, 2, 3])
    /// # (
    /// x.pop() == 1
    /// # and
    /// x.pop() == 2
    /// # and
    /// x == set([3])
    /// # )"#);
    /// ```
    fn pop<'v>(this: Value<'v>) -> starlark::Result<Value<'v>> {
        let mut set = SetMut::from_value(this)?;
        let first = set.iter_hashed().next();
        match first {
            Some(x) => {
                set.remove_hashed(x.as_ref());
                Ok(x.into_key())
            }
            None => Err(value_error!("pop from an empty set")),
        }
    }
}
#[cfg(test)]
mod tests {
    use crate::assert;

    #[test]
    fn test_empty() {
        assert::is_true("s = set(); len(s) == 0")
    }

    #[test]
    fn test_single() {
        assert::is_true("s = set([0, 1]); len(s) == 2")
    }

    #[test]
    fn test_eq() {
        assert::is_true("set([1, 2, 3]) == set([3, 2, 1])")
    }

    #[test]
    fn test_clear() {
        assert::is_true("s = set([1, 2, 3]); s.clear(); s == set()")
    }

    #[test]
    fn test_type() {
        assert::eq("type(set([1, 2, 3]))", "'set'")
    }

    #[test]
    fn test_iter() {
        assert::is_true("list([elem for elem in set([1, 2, 3])]) ==  [1, 2, 3]")
    }

    #[test]
    fn test_bool_true() {
        assert::is_true("bool(set([1, 2, 3]))")
    }

    #[test]
    fn test_bool_false() {
        assert::is_false("bool(set())")
    }

    #[test]
    fn test_union() {
        assert::eq(
            "set([1, 2, 3]).union(set([3, 4, 5]))",
            "set([1, 2, 3, 4, 5])",
        )
    }

    #[test]
    fn test_union_empty() {
        assert::eq("set([1, 2, 3]).union(set([]))", "set([1, 2, 3])")
    }

    #[test]
    fn test_union_iter() {
        assert::eq("set([1, 2, 3]).union([3, 4])", "set([1, 2, 3, 4])")
    }

    #[test]
    fn test_union_ordering_mixed() {
        assert::eq("list(set([1, 3, 5]).union(set([4, 3])))", "[1, 3, 5, 4]");
    }

    #[test]
    fn test_intersection() {
        assert::eq("set([1, 2, 3]).intersection(set([3, 4, 5]))", "set([3])")
    }

    #[test]
    fn test_intersection_empty() {
        assert::eq("set([1, 2, 3]).intersection(set([]))", "set([])")
    }

    #[test]
    fn test_intersection_iter() {
        assert::eq("set([1, 2, 3]).intersection([3, 4])", "set([3])")
    }

    #[test]
    fn test_intersection_order() {
        assert::eq("list(set([1, 2, 3]).intersection([4, 3, 1]))", "[1, 3]")
    }

    #[test]
    fn test_symmetric_difference() {
        assert::eq(
            "set([1, 2, 3]).symmetric_difference(set([3, 4, 5]))",
            "set([1, 2, 4, 5])",
        )
    }

    #[test]
    fn test_symmetric_difference_empty() {
        assert::eq(
            "set([1, 2, 3]).symmetric_difference(set([]))",
            "set([1, 2, 3])",
        )
    }

    #[test]
    fn test_symmetric_difference_iter() {
        assert::eq(
            "set([1, 2, 3]).symmetric_difference([3, 4])",
            "set([1, 2, 4])",
        )
    }

    #[test]
    fn test_symmetric_difference_ord() {
        assert::eq(
            "list(set([1, 2, 3, 7]).symmetric_difference(set([4, 3, 1])))",
            "[2, 7, 4]",
        )
    }

    #[test]
    fn test_add() {
        assert::eq(r#"x = set([1, 2, 3]);x.add(0);x"#, "set([0, 1, 2, 3])")
    }

    #[test]
    fn test_add_empty() {
        assert::eq(r#"x = set([]);x.add(0);x"#, "set([0])")
    }

    #[test]
    fn test_add_existing() {
        assert::eq(r#"x = set([0]);x.add(0);x"#, "set([0])")
    }

    #[test]
    fn test_add_order() {
        assert::eq(r#"x = set([1, 2, 3]);x.add(2);list(x)"#, "[1, 2, 3]");
        assert::eq(r#"x = set([1, 2, 3]);x.add(0);list(x)"#, "[1, 2, 3, 0]")
    }

    #[test]
    fn test_remove() {
        assert::eq("x = set([0, 1]);x.remove(1);x", "set([0])")
    }

    #[test]
    fn test_remove_empty() {
        assert::fail("set([]).remove(0)", "`0` not found in `set([])`");
    }

    #[test]
    fn test_remove_not_existing() {
        assert::fail("set([1]).remove(0)", "`0` not found in `set([1])`");
    }

    #[test]
    fn test_discard() {
        assert::eq("x = set([0, 1]);x.discard(1);x", "set([0])")
    }

    #[test]
    fn test_discard_multiple_times() {
        assert::eq("x = set([0, 1]); x.discard(0); x.discard(0); x", "set([1])");
    }

    #[test]
    fn test_pop() {
        assert::is_true("x = set([1, 0]); (x.pop() == 1 and x.pop() == 0 and x == set())");
    }

    #[test]
    fn test_pop_empty() {
        assert::fail("x = set([]); x.pop()", "pop from an empty set");
    }
}
