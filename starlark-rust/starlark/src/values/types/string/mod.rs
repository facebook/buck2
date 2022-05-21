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

//! The string type. All strings must be valid UTF8.

use std::{
    cmp,
    cmp::Ordering,
    fmt,
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
    ops::{Add, Deref, Sub},
    slice, str,
    sync::atomic,
};

use gazebo::{any::AnyLifetime, prelude::*};
use serde::Serialize;

use crate::{
    collections::{Hashed, StarlarkHashValue, StarlarkHasher},
    environment::{Methods, MethodsStatic},
    private::Private,
    values::{
        index::apply_slice,
        string::repr::string_repr,
        types::{none::NoneOr, string::fast_string::StrIndices},
        Heap, StarlarkValue, UnpackValue, Value, ValueError,
    },
};

mod alloc_unpack;
pub(crate) mod fast_string;
pub(crate) mod interpolation;
pub(crate) mod iter;
mod repr;
pub(crate) mod simd;

/// Index of a char in a string.
/// This is different from string byte offset.
#[derive(Eq, PartialEq, Ord, PartialOrd, Copy, Clone, Dupe, Debug)]
pub(crate) struct CharIndex(pub(crate) usize);

impl Sub for CharIndex {
    type Output = CharIndex;

    fn sub(self, rhs: CharIndex) -> CharIndex {
        CharIndex(self.0 - rhs.0)
    }
}

impl Add for CharIndex {
    type Output = CharIndex;

    fn add(self, rhs: CharIndex) -> CharIndex {
        CharIndex(self.0 + rhs.0)
    }
}

/// The result of calling `type()` on strings.
pub const STRING_TYPE: &str = "string";

#[repr(C)] // We want the body to come after len
#[derive(AnyLifetime)]
pub(crate) struct StarlarkStrN<const N: usize> {
    // Lazily-initialized cached hash code.
    pub(crate) hash: atomic::AtomicU32,
    // Length in bytes.
    pub(crate) len: u32,
    // Followed by an unsized block, meaning this type is unsized.
    // But we can't mark it as such since we really want &StarlarkStr to
    // take up only one word.
    pub(crate) body: [u8; N],
}

/// A pointer to this type represents a Starlark string.
/// Use of this type is discouraged and not considered stable.
#[derive(AnyLifetime)]
#[repr(C)]
pub struct StarlarkStr {
    str: StarlarkStrN<0>,
}

impl Deref for StarlarkStr {
    type Target = str;

    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl PartialEq for StarlarkStr {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for StarlarkStr {}

impl PartialOrd for StarlarkStr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.as_str().partial_cmp(other.as_str())
    }
}

impl Ord for StarlarkStr {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl Debug for StarlarkStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.as_str(), f)
    }
}

impl StarlarkStr {
    /// Unsafe because if you do `unpack` on this it will blow up
    pub(crate) const unsafe fn new(len: usize) -> Self {
        assert!(len as u32 as usize == len, "len overflow");
        StarlarkStr {
            str: StarlarkStrN {
                hash: atomic::AtomicU32::new(0),
                len: len as u32,
                body: [],
            },
        }
    }

    /// Get a Rust string refence from this Starlark string.
    pub fn as_str(&self) -> &str {
        unsafe {
            let slice = slice::from_raw_parts(self.str.body.as_ptr(), self.str.len as usize);
            str::from_utf8_unchecked(slice)
        }
    }

    /// Get cached hash value or compute if it is not cached yet.
    pub fn get_hash(&self) -> StarlarkHashValue {
        // Note relaxed load and store are practically non-locking memory operations.
        let hash = self.str.hash.load(atomic::Ordering::Relaxed);
        if hash != 0 {
            StarlarkHashValue::new_unchecked(hash)
        } else {
            let mut s = StarlarkHasher::new();
            hash_string_value(self.as_str(), &mut s);
            let hash = s.finish_small();
            // If hash is zero, we are unlucky, but it is highly improbable.
            self.str.hash.store(hash.get(), atomic::Ordering::Relaxed);
            hash
        }
    }

    /// Rust string reference along with its hash value.
    pub fn as_str_hashed(&self) -> Hashed<&str> {
        Hashed::new_unchecked(self.get_hash(), self.as_str())
    }

    /// String length, in bytes.
    pub fn len(&self) -> usize {
        self.str.len as usize
    }

    /// Is this string empty?
    pub fn is_empty(&self) -> bool {
        self.str.len == 0
    }

    pub(crate) fn offset_of_content() -> usize {
        memoffset::offset_of!(StarlarkStrN<0>, body)
    }
}

/// How to hash a string in a way that is compatible with Value
#[inline]
pub(crate) fn hash_string_value<H: Hasher>(x: &str, state: &mut H) {
    x.hash(state)
}

impl Display for StarlarkStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // We could either accumulate straight into the buffer (can't preallocate, virtual call on each character)
        // or accumulate into a String buffer first. Not sure which is faster, but string buffer lets us
        // share code with collect_repr more easily.
        let mut buffer = String::new();
        string_repr(self.as_str(), &mut buffer);
        f.write_str(&buffer)
    }
}

pub(crate) fn str_methods() -> Option<&'static Methods> {
    static RES: MethodsStatic = MethodsStatic::new();
    RES.methods(crate::stdlib::string::string_methods)
}

// We don't actually use `str` on the heap, but it's helpful to have an instance
// for people who want to get access to repr/json "like string"
impl<'v> StarlarkValue<'v> for str {
    starlark_type!(STRING_TYPE);

    fn get_methods(&self) -> Option<&'static Methods> {
        str_methods()
    }

    fn collect_repr(&self, buffer: &mut String) {
        // String repr() is quite hot, so optimise it
        string_repr(self, buffer)
    }

    fn to_bool(&self) -> bool {
        !self.is_empty()
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        let mut s = StarlarkHasher::new();
        hash_string_value(self, &mut s);
        let hash = s.finish_small();
        hasher.write_u32(hash.get());
        Ok(())
    }

    fn equals(&self, other: Value) -> anyhow::Result<bool> {
        if let Some(other) = other.unpack_str() {
            Ok(self == other)
        } else {
            Ok(false)
        }
    }

    fn compare(&self, other: Value) -> anyhow::Result<Ordering> {
        if let Some(other) = other.unpack_str() {
            Ok(self.cmp(other))
        } else {
            ValueError::unsupported_with(self, "cmp()", other)
        }
    }

    fn at(&self, index: Value, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        // This method is disturbingly hot. Use the logic from `convert_index`,
        // but modified to be UTF8 string friendly.
        let i = i32::unpack_param(index)?;
        if i >= 0 {
            match fast_string::at(self, CharIndex(i as usize)) {
                None => Err(ValueError::IndexOutOfBound(i).into()),
                Some(c) => Ok(heap.alloc(c)),
            }
        } else {
            let len_chars = fast_string::len(self);
            let ind = CharIndex((-i) as usize); // Index from the end, minimum of 1
            if ind > len_chars {
                Err(ValueError::IndexOutOfBound(i).into())
            } else if len_chars.0 == self.len() {
                // We are a 7bit ASCII string, so take the fast-path
                Ok(heap.alloc(self.as_bytes()[(len_chars - ind).0] as char))
            } else {
                Ok(heap.alloc(fast_string::at(self, len_chars - ind).unwrap()))
            }
        }
    }

    fn length(&self) -> anyhow::Result<i32> {
        Ok(fast_string::len(self).0 as i32)
    }

    fn is_in(&self, other: Value) -> anyhow::Result<bool> {
        let s = <&str>::unpack_param(other)?;
        Ok(fast_string::contains(self, s))
    }

    fn slice(
        &self,
        start: Option<Value>,
        stop: Option<Value>,
        stride: Option<Value>,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        let s = self;
        if matches!(stride, Some(stride) if stride.unpack_int() != Some(1)) {
            // The stride case is super rare and super complex, so let's do something inefficient but safe
            let xs = s.chars().collect::<Vec<_>>();
            let xs = apply_slice(&xs, start, stop, stride)?;
            return Ok(heap.alloc(xs.into_iter().collect::<String>()));
        }

        #[inline(always)]
        fn start_stop_to_none_or(v: Option<Value>) -> anyhow::Result<NoneOr<i32>> {
            match v {
                None => Ok(NoneOr::None),
                Some(v) => Ok(NoneOr::Other(v.to_int()?)),
            }
        }

        let (start, stop) = (start_stop_to_none_or(start)?, start_stop_to_none_or(stop)?);

        match fast_string::convert_str_indices(self, start, stop) {
            Some(StrIndices { haystack, .. }) => Ok(heap.alloc_str(haystack).to_value()),
            None => Ok(heap.alloc_str("").to_value()),
        }
    }

    fn add(&self, other: Value<'v>, heap: &'v Heap) -> Option<anyhow::Result<Value<'v>>> {
        if let Some(other_str) = other.unpack_str() {
            if self.is_empty() {
                Some(Ok(other))
            } else {
                Some(Ok(heap.alloc_str_concat(self, other_str).to_value()))
            }
        } else {
            None
        }
    }

    fn mul(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let l = i32::unpack_param(other)?;
        let mut result = String::with_capacity(self.len() * cmp::max(0, l) as usize);
        for _i in 0..l {
            result.push_str(self)
        }
        Ok(heap.alloc(result))
    }

    fn percent(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(heap.alloc(interpolation::percent(self, other)?))
    }
}

impl<'v> StarlarkValue<'v> for StarlarkStr {
    starlark_type!(STRING_TYPE);

    fn is_special(_: Private) -> bool
    where
        Self: Sized,
    {
        true
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        // Don't defer to str because we cache the Hash in StarlarkStr
        hasher.write_u32(self.get_hash().get());
        Ok(())
    }

    fn extra_memory(&self) -> usize {
        // We don't include the extra_memory for the size because it is
        // allocated inline in the Starlark heap (which knows about it),
        // not on the malloc heap.
        0
    }

    fn get_methods(&self) -> Option<&'static Methods> {
        self.as_str().get_methods()
    }

    fn collect_repr(&self, collector: &mut String) {
        self.as_str().collect_repr(collector)
    }

    fn to_bool(&self) -> bool {
        self.as_str().to_bool()
    }

    fn equals(&self, other: Value) -> anyhow::Result<bool> {
        self.as_str().equals(other)
    }

    fn compare(&self, other: Value) -> anyhow::Result<Ordering> {
        self.as_str().compare(other)
    }

    fn at(&self, index: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        self.as_str().at(index, heap)
    }

    fn length(&self) -> anyhow::Result<i32> {
        self.as_str().length()
    }

    fn is_in(&self, other: Value) -> anyhow::Result<bool> {
        self.as_str().is_in(other)
    }

    fn slice(
        &self,
        start: Option<Value<'v>>,
        stop: Option<Value<'v>>,
        stride: Option<Value<'v>>,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        self.as_str().slice(start, stop, stride, heap)
    }

    fn add(&self, other: Value<'v>, heap: &'v Heap) -> Option<anyhow::Result<Value<'v>>> {
        self.as_str().add(other, heap)
    }

    fn mul(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        self.as_str().mul(other, heap)
    }

    fn percent(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        self.as_str().percent(other, heap)
    }
}

impl Serialize for StarlarkStr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        assert,
        values::{index::apply_slice, Heap, Value},
    };

    #[test]
    fn test_string_corruption() {
        assert::fail("'U4V6'[93]", "out of bound");
        assert::fail("''[2]", "out of bound");
    }

    #[test]
    fn test_escape_characters() {
        // Test cases from the Starlark spec
        assert_eq!(
            assert::pass(r#"'\a\b\f\n\r\t\v'"#).unpack_str().unwrap(),
            "\x07\x08\x0C\x0A\x0D\x09\x0B"
        );
        assert_eq!(assert::pass(r#"'\0'"#).unpack_str().unwrap(), "\x00");
        assert_eq!(assert::pass(r#"'\12'"#).unpack_str().unwrap(), "\n");
        assert_eq!(assert::pass(r#"'\101-\132'"#).unpack_str().unwrap(), "A-Z");
        // 9 is not an octal digit, so it terminates early
        assert_eq!(assert::pass(r#"'\119'"#).unpack_str().unwrap(), "\t9");
        assert_eq!(assert::pass(r#"'\117'"#).unpack_str().unwrap(), "O");
        assert_eq!(assert::pass(r#"'\u0041'"#).unpack_str().unwrap(), "A");
        assert_eq!(assert::pass(r#"'\u0414'"#).unpack_str().unwrap(), "Д");
        assert_eq!(assert::pass(r#"'\u754c'"#).unpack_str().unwrap(), "界");
        assert_eq!(assert::pass(r#"'\U0001F600'"#).unpack_str().unwrap(), "😀");
    }

    const EXAMPLES: &[&str] = &[
        "",
        "short",
        "longer string which is all ASCII!#",
        "🤗",
        "mix of prefix ASCII and 🤗 some emjoi",
        "🤗 and the emjoi can go first",
        "😥🍊🍉🫐🥥🥬🥒🥑🍈🍋",
        "© and other characters Ŕ",
        "ça va bien merci",
        "Диана is a name in Russia",
    ];

    #[test]
    fn test_string_hash() {
        let heap = Heap::new();
        for x in EXAMPLES {
            assert_eq!(
                heap.alloc_str(x).get_hashed().hash(),
                heap.alloc(*x).get_hashed().unwrap().hash()
            );
        }
    }

    // If hash was zero, we'd need to mask the value in the hash cache.
    #[test]
    fn test_zero_length_string_hash_is_not_zero() {
        let heap = Heap::new();
        assert_ne!(0, heap.alloc("").get_hash().unwrap().get());
    }

    #[test]
    fn test_string_len() {
        assert::all_true(
            r#"
len("😿") == 1
"#,
        );
    }

    #[test]
    fn test_arithmetic_on_string() {
        assert::all_true(
            r#"
"abc" + "def" == "abcdef"
"abc" * 3 == "abcabcabc"
"#,
        );
    }

    #[test]
    fn test_slice_string() {
        let heap = Heap::new();
        for example in EXAMPLES {
            let s = heap.alloc_str(example).to_value();
            for i in -5..=6 {
                for j in -5..=6 {
                    let start = if i == 6 {
                        None
                    } else {
                        Some(Value::new_int(i))
                    };
                    let stop = if j == 6 {
                        None
                    } else {
                        Some(Value::new_int(j))
                    };
                    // Compare list slicing (comparatively simple) to string slicing (complex unicode)
                    let res1 = apply_slice(&example.chars().collect::<Vec<_>>(), start, stop, None)
                        .unwrap()
                        .iter()
                        .collect::<String>();
                    let res2 = s
                        .slice(start, stop, None, &heap)
                        .unwrap()
                        .unpack_str()
                        .unwrap();
                    assert_eq!(
                        &res1,
                        res2,
                        "{:?}[{}:{}]",
                        example,
                        start.map_or("".to_owned(), |x| x.to_string()),
                        stop.map_or("".to_owned(), |x| x.to_string())
                    );
                }
            }
        }

        assert::all_true(
            r#"
"abc"[1:] == "bc" # Remove the first element
"abc"[:-1] == "ab" # Remove the last element
"abc"[1:-1] == "b" # Remove the first and the last element
"banana"[1::2] == "aaa" # Select one element out of 2, skipping the first
"banana"[4::-2] == "nnb" # Select one element out of 2 in reverse order, starting at index 4
"242"[ -0:-2:-1] == "" # From https://github.com/facebookexperimental/starlark-rust/issues/35
"#,
        );
    }

    #[test]
    fn test_string_is_in() {
        assert::all_true(
            r#"
("a" in "abc") == True
("b" in "abc") == True
("bc" in "abc") == True
("bd" in "abc") == False
("z" in "abc") == False
"#,
        );
    }

    #[test]
    fn test_successive_add() {
        // we hope these get optimised away with adjacent plus optimisation
        assert::eq("x = 'c'\n'a' + 'b' + x + 'd' + 'e'", "'abcde'");
    }

    #[test]
    fn test_string_index() -> anyhow::Result<()> {
        fn test_str(str: &str) -> anyhow::Result<()> {
            let chars = str.chars().collect::<Vec<char>>();
            let heap = Heap::new();
            let val = heap.alloc(str);
            let len = chars.len() as i32;
            assert_eq!(val.length()?, len);
            for (i, char) in chars.iter().enumerate() {
                let char_str = char.to_string();
                assert_eq!(
                    val.at(Value::new_int(i as i32), &heap)?.unpack_str(),
                    Some(char_str.as_str())
                );
                assert_eq!(
                    val.at(Value::new_int(-len + (i as i32)), &heap)?
                        .unpack_str(),
                    Some(char_str.as_str())
                );
            }
            assert!(val.at(Value::new_int(len), &heap).is_err());
            assert!(val.at(Value::new_int(-(len + 1)), &heap).is_err());
            Ok(())
        }

        for x in EXAMPLES {
            // We use all trailing substrings of the test, for better coverage (especially around smart prefix algorithms)
            let mut it = x.chars();
            loop {
                test_str(it.as_str())?;
                if it.next().is_none() {
                    break;
                }
            }
        }
        Ok(())
    }
}
