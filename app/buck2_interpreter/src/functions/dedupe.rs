/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;

use starlark::environment::GlobalsBuilder;
use starlark::values::list::AllocList;
use starlark::values::Heap;
use starlark::values::Value;

#[starlark_module]
pub fn dedupe(builder: &mut GlobalsBuilder) {
    /// Remove duplicates in a list. Uses identity of value (pointer),
    /// rather than by equality. In many cases you should use a transitive set instead.
    fn dedupe<'v>(
        #[starlark(require = pos)] val: Value<'v>,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        let mut seen = HashSet::new();
        let mut res = Vec::new();
        for v in val.iterate(heap)? {
            let p = v.identity();
            if !seen.contains(&p) {
                seen.insert(p);
                res.push(v);
            }
        }
        Ok(heap.alloc(AllocList(res)))
    }
}

#[cfg(test)]
mod tests {

    use starlark::assert::Assert;

    use crate::functions::dedupe::dedupe;

    #[test]
    fn test_dedupe() {
        let mut a = Assert::new();
        a.globals_add(dedupe);
        a.pass(
            r#"
assert_eq(dedupe([1,2,3]), [1,2,3])
assert_eq(dedupe([1,2,3,2,1]), [1,2,3])
a = [1]
b = [1]
assert_eq(dedupe([a,b,a]), [a,b])
            "#,
        );
    }
}
