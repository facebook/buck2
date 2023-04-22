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

use itertools::Itertools;
use starlark_derive::starlark_module;

use crate as starlark;
use crate::environment::GlobalsBuilder;
use crate::eval::Evaluator;
use crate::values::none::NoneType;
use crate::values::regex::StarlarkRegex;
use crate::values::Value;

#[starlark_module]
pub fn filter(builder: &mut GlobalsBuilder) {
    /// Apply a predicate to each element of the iterable, returning those that match.
    /// As a special case if the function is `None` then removes all the `None` values.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// filter(bool, [0, 1, False, True]) == [1, True]
    /// filter(lambda x: x > 2, [1, 2, 3, 4]) == [3, 4]
    /// filter(None, [True, None, False]) == [True, False]
    /// # "#);
    /// ```
    #[starlark(return_type = "[\"\"]")]
    fn filter<'v>(
        #[starlark(require = pos)] func: Value<'v>,
        #[starlark(require = pos, type = "iter(\"\")")] seq: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let mut res = Vec::new();

        for v in seq.iterate(eval.heap())? {
            if func.is_none() {
                if !v.is_none() {
                    res.push(v);
                }
            } else if func.invoke_pos(&[v], eval)?.to_bool() {
                res.push(v);
            }
        }
        Ok(eval.heap().alloc_list(&res))
    }
}

#[starlark_module]
pub fn map(builder: &mut GlobalsBuilder) {
    /// Apply a function to each element of the iterable, returning the results.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// map(abs, [7, -5, -6]) == [7, 5, 6]
    /// map(lambda x: x * 2, [1, 2, 3, 4]) == [2, 4, 6, 8]
    /// # "#);
    /// ```
    #[starlark(return_type = "[\"\"]")]
    fn map<'v>(
        #[starlark(require = pos)] func: Value<'v>,
        #[starlark(require = pos, type = "iter(\"\")")] seq: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let it = seq.iterate(eval.heap())?;
        let mut res = Vec::with_capacity(it.size_hint().0);
        for v in it {
            res.push(func.invoke_pos(&[v], eval)?);
        }
        Ok(eval.heap().alloc_list(&res))
    }
}

#[starlark_module]
pub fn debug(builder: &mut GlobalsBuilder) {
    /// Print the value with full debug formatting. The result may not be stable over time.
    /// Intended for debugging purposes and guaranteed to produce verbose output not suitable for user display.
    fn debug(#[starlark(require = pos)] val: Value) -> anyhow::Result<String> {
        Ok(format!("{:?}", val))
    }
}

#[starlark_module]
pub fn regex(builder: &mut GlobalsBuilder) {
    /// Creates a regex which can be used for matching.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// experimental_regex("^[a-z]*$").match("test") == True
    /// experimental_regex("^[a-z]*$").match("1234") == False
    /// # "#);
    /// ```
    fn experimental_regex<'v>(
        #[starlark(require = pos)] regex: &str,
    ) -> anyhow::Result<StarlarkRegex> {
        StarlarkRegex::new(regex)
    }
}

struct PrintWrapper<'a, 'b>(&'a Vec<Value<'b>>);
impl fmt::Display for PrintWrapper<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, v) in self.0.iter().enumerate() {
            if i != 0 {
                f.write_str(" ")?;
            }
            fmt::Display::fmt(v, f)?;
        }
        Ok(())
    }
}

/// Invoked from `print` or `pprint` to print a value.
pub trait PrintHandler {
    /// If this function returns error, evaluation fails with this error.
    fn println(&self, text: &str) -> anyhow::Result<()>;
}

pub(crate) struct StderrPrintHandler;

impl PrintHandler for StderrPrintHandler {
    fn println(&self, text: &str) -> anyhow::Result<()> {
        eprintln!("{}", text);
        Ok(())
    }
}

#[starlark_module]
pub fn print(builder: &mut GlobalsBuilder) {
    /// Print some values to the output.
    fn print(#[starlark(args)] args: Vec<Value>, eval: &mut Evaluator) -> anyhow::Result<NoneType> {
        // In practice most users should want to put the print somewhere else, but this does for now
        // Unfortunately, we can't use PrintWrapper because strings to_str() and Display are different.
        eval.print_handler
            .println(&args.iter().map(|x| x.to_str()).join(" "))?;
        Ok(NoneType)
    }
}

#[starlark_module]
pub fn pprint(builder: &mut GlobalsBuilder) {
    fn pprint(
        #[starlark(args)] args: Vec<Value>,
        eval: &mut Evaluator,
    ) -> anyhow::Result<NoneType> {
        // In practice most users may want to put the print somewhere else, but this does for now
        eval.print_handler
            .println(&format!("{:#}", PrintWrapper(&args)))?;
        Ok(NoneType)
    }
}

#[starlark_module]
pub fn abs(builder: &mut GlobalsBuilder) {
    /// Take the absolute value of an int.
    //
    /// ```
    /// # starlark::assert::all_true(r#"
    /// abs(0)   == 0
    /// abs(-10) == 10
    /// abs(10)  == 10
    /// # "#);
    /// ```
    fn abs(#[starlark(require = pos)] x: i32) -> anyhow::Result<i32> {
        Ok(x.abs())
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::rc::Rc;

    use dupe::Dupe;

    use crate::assert;
    use crate::assert::Assert;
    use crate::stdlib::PrintHandler;

    #[test]
    fn test_filter() {
        assert::pass(
            r#"
def contains_hello(s):
    if "hello" in s:
        return True
    return False

def positive(i):
    return i > 0

assert_eq([], filter(positive, []))
assert_eq([1, 2, 3], filter(positive, [1, 2, 3]))
assert_eq([], filter(positive, [-1, -2, -3]))
assert_eq([1, 2, 3], filter(positive, [-1, 1, 2, -2, -3, 3]))
assert_eq(["hello world!"], filter(contains_hello, ["hello world!", "goodbye"]))
"#,
        );
    }

    #[test]
    fn test_map() {
        assert::pass(
            r#"
def double(x):
    return x + x

assert_eq([], map(int, []))
assert_eq([1,2,3], map(int, ["1","2","3"]))
assert_eq(["0","1","2"], map(str, range(3)))
assert_eq(["11",8], map(double, ["1",4]))
"#,
        );
    }

    #[test]
    fn test_debug() {
        assert::pass(
            r#"assert_eq(
                debug([1,2]),
                "Value(ListGen(ListData { content: Cell { value: ValueTyped(Value(Array { len: 2, capacity: 2, iter_count: 0, content: [Value(1), Value(2)] })) } }))"
                )"#,
        );
    }

    #[test]
    fn test_print() {
        let s = Rc::new(RefCell::new(String::new()));
        let s_copy = s.dupe();
        struct PrintHandlerImpl {
            s: Rc<RefCell<String>>,
        }
        impl PrintHandler for PrintHandlerImpl {
            fn println(&self, s: &str) -> anyhow::Result<()> {
                *self.s.borrow_mut() = s.to_owned();
                Ok(())
            }
        }
        let print_handler = PrintHandlerImpl { s: s.dupe() };
        let mut a = Assert::new();
        a.set_print_handler(&print_handler);
        a.pass("print('hw')");
        assert_eq!("hw", s_copy.borrow().as_str());
    }
}
