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

//! Utilities to implement `Display` (or `repr`) for Starlark values.

use std::fmt::{self, Display, Write};

const INDENT: &str = "  ";

fn subwriter<T: Display>(indent: &'static str, f: &mut fmt::Formatter, v: &T) -> fmt::Result {
    if f.alternate() {
        write!(indenter::indented(f).with_str(indent), "{:#}", v)
    } else {
        Display::fmt(v, f)
    }
}

/// Helper for display implementation of starlark container-y types.
///
/// For "normal" display, produces output like (with `prefix="prefix[", suffix="]"`):
///
/// `prefix[]`
/// `prefix[1]`
/// `prefix[1, 2]`
///
/// For "alternate" display, produces output like:
///
/// `prefix[]`
/// `prefix[ 1 ]`
/// ```ignore
/// prefix[
///   1,
///   2
/// ]
/// ```
///
/// This doesn't propagate the flags on the Formatter other than alternate.
// TODO(cjhopman): Starlark values don't really do anything with the rest of the flags so
// propagating them hasn't been necessary, but it would be easy enough to implement if we wanted to.
pub fn display_container<T: Display, Iter: ExactSizeIterator<Item = T>>(
    f: &mut fmt::Formatter,
    prefix: &str,
    suffix: &str,
    items: Iter,
) -> fmt::Result {
    let (separator, outer, indent) = match (f.alternate(), items.len()) {
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
    for (i, value) in items.into_iter().enumerate() {
        if i != 0 {
            f.write_str(separator)?;
        }
        subwriter(indent, f, &value)?;
    }
    f.write_str(outer)?;
    f.write_str(suffix)
}

/// Helper for display implementation of starlark keyed container-y types (like dict, struct).
///
/// Similar to [display_container] where each key-value pair is printed as `<key><separator><value>`
pub fn display_keyed_container<K: Display, V: Display, Iter: ExactSizeIterator<Item = (K, V)>>(
    f: &mut fmt::Formatter,
    prefix: &str,
    suffix: &str,
    separator: &str,
    items: Iter,
) -> fmt::Result {
    struct Wrapper<'a, K: Display, V: Display>(K, V, &'a str);
    impl<'a, K: Display, V: Display> fmt::Display for Wrapper<'a, K, V> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            fmt::Display::fmt(&self.0, f)?;
            f.write_str(self.2)?;
            fmt::Display::fmt(&self.1, f)
        }
    }
    display_container(
        f,
        prefix,
        suffix,
        items.map(|(k, v)| Wrapper(k, v, separator)),
    )
}

#[cfg(test)]
mod tests {
    use std::fmt;

    use indexmap::{indexmap, IndexMap};
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
}
