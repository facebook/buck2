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

//! Provides utilities to implement `Display` (or `repr`) for Starlark values.
//!
//! For "normal" display, produces output like (with `prefix="prefix[", suffix="]"`):
//!
//! `prefix[]`
//! `prefix[1]`
//! `prefix[1, 2]`
//!
//! For "alternate" display, produces output like:
//!
//! `prefix[]`
//! `prefix[ 1 ]`
//! ```ignore
//! prefix[
//!   1,
//!   2
//! ]
//! ```
//!
//! This doesn't propagate the flags on the Formatter other than alternate.
// TODO(cjhopman): Starlark values don't really do anything with the rest of the flags so
// propagating them hasn't been necessary, but it would be easy enough to implement if we wanted to.

use std::fmt::Display;
use std::fmt::Write;
use std::fmt::{self};

const INDENT: &str = "  ";

/// Used to indent a displayed item for alternate display. This helps us pretty-print deep data structures.
fn subwriter<T: Display>(indent: &'static str, f: &mut fmt::Formatter, v: T) -> fmt::Result {
    if f.alternate() {
        write!(indenter::indented(f).with_str(indent), "{:#}", &v)
    } else {
        Display::fmt(&v, f)
    }
}

/// The low-level helper for displaying containers. For simple containers, it may be more convenient to use `display_container` or `display_keyed_container`.
pub struct ContainerDisplayHelper<'a, 'b> {
    f: &'a mut fmt::Formatter<'b>,
    /// The additional separator to be added after each item (except the last).
    separator: &'static str,
    /// Extra output to be added after the prefix and before the suffix. Only non-empty for the single item case.
    outer: &'static str,
    /// The indent used for each item in the multiple items case (where each item is placed on its own line).
    indent: &'static str,
    /// A count of items, used for correctly adding the separator.
    seen_items: usize,
}

impl<'a, 'b> ContainerDisplayHelper<'a, 'b> {
    /// Begins displaying a container. The provided num_items will be used to select which formatting to use for alternate display.
    pub fn begin(
        f: &'a mut fmt::Formatter<'b>,
        prefix: &str,
        num_items: usize,
    ) -> Result<Self, fmt::Error> {
        let (separator, outer, indent) = match (f.alternate(), num_items) {
            // We want to be formatted as `{prefix}item1, item2{suffix}`, like `[1, 2]` for lists.
            (false, _) => (", ", "", ""),
            // We want to be formatted as `{prefix}{suffix}`, like `[]` for lists.
            (true, 0) => ("", "", ""),
            // We want to be formatted as `{prefix} item {suffix}`, like `[ item ]` for lists
            (true, 1) => ("", " ", ""),
            // We want to be formatted as `{prefix}\n  item1,\n  item2\n{suffix}`, for lists like:
            // ```
            // [
            //    item1,
            //    item2
            // ]
            // ```
            _ => (",\n", "\n", INDENT),
        };
        f.write_str(prefix)?;
        f.write_str(outer)?;

        Ok(Self {
            f,
            separator,
            outer,
            indent,
            seen_items: 0,
        })
    }

    /// Displays an item.
    pub fn item<T: Display>(&mut self, v: T) -> fmt::Result {
        if self.seen_items != 0 {
            self.f.write_str(self.separator)?;
        }
        self.seen_items += 1;
        subwriter(self.indent, self.f, &v)
    }

    /// Displays a keyed item (will be displayed as `<key><separator><value>`)
    pub fn keyed_item<K: Display, V: Display>(
        &mut self,
        k: K,
        separator: &str,
        v: V,
    ) -> fmt::Result {
        struct Wrapper<'a, K: Display, V: Display>(K, V, &'a str);
        impl<'a, K: Display, V: Display> fmt::Display for Wrapper<'a, K, V> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::Display::fmt(&self.0, f)?;
                f.write_str(self.2)?;
                fmt::Display::fmt(&self.1, f)
            }
        }
        self.item(Wrapper(k, v, separator))
    }

    /// Ends displaying a container.
    pub fn end(self, suffix: &str) -> fmt::Result {
        self.f.write_str(self.outer)?;
        self.f.write_str(suffix)
    }
}

/// Helper for display implementation of starlark container-y types (like list, tuple).
///
/// When displaying a container that produces an ExactSizeIterator, this is more convenient
/// than using `ContainerDisplayHelper` directly.
pub fn display_container<T: Display, Iter: ExactSizeIterator<Item = T>>(
    f: &mut fmt::Formatter,
    prefix: &str,
    suffix: &str,
    items: Iter,
) -> fmt::Result {
    let mut helper = ContainerDisplayHelper::begin(f, prefix, items.len())?;
    for v in items {
        helper.item(v)?;
    }
    helper.end(suffix)
}

/// Helper for display implementation of starlark keyed container-y types (like dict, struct).
///
/// When displaying a keyed container that produces an ExactSizeIterator, this is more convenient
/// than using `ContainerDisplayHelper` directly.
pub fn display_keyed_container<K: Display, V: Display, Iter: ExactSizeIterator<Item = (K, V)>>(
    f: &mut fmt::Formatter,
    prefix: &str,
    suffix: &str,
    separator: &str,
    items: Iter,
) -> fmt::Result {
    let mut helper = ContainerDisplayHelper::begin(f, prefix, items.len())?;
    for (k, v) in items {
        helper.keyed_item(k, separator, v)?;
    }
    helper.end(suffix)
}

#[cfg(test)]
mod tests {
    use std::fmt;

    use indexmap::indexmap;
    use indexmap::IndexMap;
    use indoc::indoc;

    use super::*;

    #[test]
    fn test_container() {
        struct Wrapped(Vec<u32>);
        impl fmt::Display for Wrapped {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                display_container(f, "prefix[", "]", self.0.iter())
            }
        }

        assert_eq!("prefix[]", format!("{:}", Wrapped(vec![])));
        assert_eq!("prefix[1]", format!("{:}", Wrapped(vec![1])));
        assert_eq!("prefix[1, 2]", format!("{:}", Wrapped(vec![1, 2])));
        assert_eq!("prefix[1, 2, 3]", format!("{:}", Wrapped(vec![1, 2, 3])));

        assert_eq!("prefix[]", format!("{:#}", Wrapped(vec![])));
        assert_eq!("prefix[ 1 ]", format!("{:#}", Wrapped(vec![1])));
        assert_eq!(
            indoc!(
                "prefix[
                   1,
                   2
                 ]"
            ),
            format!("{:#}", Wrapped(vec![1, 2])),
        );
        assert_eq!(
            indoc!(
                "prefix[
                   1,
                   2,
                   3
                 ]"
            ),
            format!("{:#}", Wrapped(vec![1, 2, 3])),
        );
    }

    #[test]
    fn test_keyed_container() {
        struct Wrapped(IndexMap<u32, &'static str>);
        impl fmt::Display for Wrapped {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                display_keyed_container(
                    f,
                    "prefix[",
                    "]",
                    ": ",
                    // just wrap with `"` to make it clearer in the output
                    self.0.iter().map(|(k, v)| (k, format!("\"{}\"", v))),
                )
            }
        }

        assert_eq!("prefix[]", format!("{:}", Wrapped(indexmap! {})));
        assert_eq!(
            "prefix[1: \"1\"]",
            format!("{:}", Wrapped(indexmap! {1 => "1"}))
        );
        assert_eq!(
            "prefix[1: \"1\", 2: \"2\"]",
            format!("{:}", Wrapped(indexmap! {1 => "1", 2 => "2"})),
        );
        assert_eq!(
            "prefix[1: \"1\", 2: \"2\", 3: \"3\"]",
            format!("{:}", Wrapped(indexmap! {1 => "1", 2 => "2", 3 => "3"})),
        );

        assert_eq!("prefix[]", format!("{:#}", Wrapped(indexmap! {})));
        assert_eq!(
            "prefix[ 1: \"1\" ]",
            format!("{:#}", Wrapped(indexmap! {1 => "1"}))
        );
        assert_eq!(
            indoc!(
                "prefix[
                   1: \"1\",
                   2: \"2\"
                 ]"
            ),
            format!("{:#}", Wrapped(indexmap! {1 => "1", 2 => "2"})),
        );
        assert_eq!(
            indoc!(
                "prefix[
                   1: \"1\",
                   2: \"2\",
                   3: \"3\"
                 ]"
            ),
            format!("{:#}", Wrapped(indexmap! {1 => "1", 2 => "2", 3 => "3"})),
        );
    }

    #[test]
    fn test_helper() {
        struct Tester;
        impl fmt::Display for Tester {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let mut helper = ContainerDisplayHelper::begin(f, "prefix{", 3)?;
                helper.item("1")?;
                helper.keyed_item("x", "=", 2)?;
                helper.item(3)?;
                helper.end("}")
            }
        }

        assert_eq!("prefix{1, x=2, 3}", format!("{}", Tester));
        assert_eq!(
            indoc!(
                "prefix{
                  1,
                  x=2,
                  3
                }"
            ),
            format!("{:#}", Tester)
        );
    }
}
