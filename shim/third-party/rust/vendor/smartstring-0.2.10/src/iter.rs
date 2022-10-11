use crate::{bounds_for, boxed::BoxedString, inline::InlineString, SmartString, SmartStringMode};
use alloc::string::Drain as StringDrain;
use core::{
    fmt::{Debug, Error, Formatter},
    iter::FusedIterator,
    ops::RangeBounds,
    str::Chars,
};

/// A draining iterator for a [`SmartString`].
pub struct Drain<'a, Mode>(DrainCast<'a, Mode>)
where
    Mode: SmartStringMode;

enum DrainCast<'a, Mode>
where
    Mode: SmartStringMode,
{
    Boxed {
        string: *mut SmartString<Mode>,
        iter: Option<StringDrain<'a>>,
    },
    Inline {
        string: *mut InlineString,
        start: usize,
        end: usize,
        iter: Chars<'a>,
    },
}

impl<'a, Mode> Drain<'a, Mode>
where
    Mode: SmartStringMode,
{
    pub(crate) fn new<R>(string: &'a mut SmartString<Mode>, range: R) -> Self
    where
        R: RangeBounds<usize>,
    {
        let string_ptr: *mut _ = string;
        Drain(match string.cast_mut() {
            crate::casts::StringCastMut::Boxed(boxed) => DrainCast::Boxed {
                string: string_ptr,
                iter: Some(boxed.string_mut().drain(range)),
            },
            crate::casts::StringCastMut::Inline(inline) => {
                let len = inline.len();
                let (start, end) = bounds_for(&range, len);
                let string_ptr: *mut _ = inline;
                let iter = inline.as_str()[start..end].chars();
                DrainCast::Inline {
                    string: string_ptr,
                    start,
                    end,
                    iter,
                }
            }
        })
    }
}

impl<'a, Mode> Drop for Drain<'a, Mode>
where
    Mode: SmartStringMode,
{
    fn drop(&mut self) {
        match self.0 {
            DrainCast::Boxed {
                string,
                ref mut iter,
            } => unsafe {
                iter.take();
                (*string).try_demote();
            },
            DrainCast::Inline {
                string, start, end, ..
            } => {
                unsafe { (*string).remove_bytes(start, end) };
            }
        }
    }
}

impl<'a, Mode> Iterator for Drain<'a, Mode>
where
    Mode: SmartStringMode,
{
    type Item = char;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            DrainCast::Boxed {
                iter: Some(iter), ..
            } => iter.next(),
            DrainCast::Boxed { iter: None, .. } => unreachable!(),
            DrainCast::Inline { iter, .. } => iter.next(),
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        match &self.0 {
            DrainCast::Boxed {
                iter: Some(iter), ..
            } => iter.size_hint(),
            DrainCast::Boxed { iter: None, .. } => unreachable!(),
            DrainCast::Inline { iter, .. } => iter.size_hint(),
        }
    }

    #[inline]
    fn last(mut self) -> Option<Self::Item> {
        match &mut self.0 {
            DrainCast::Boxed {
                iter: Some(iter), ..
            } => iter.next_back(),
            DrainCast::Boxed { iter: None, .. } => unreachable!(),
            DrainCast::Inline { iter, .. } => iter.next_back(),
        }
    }
}

impl<'a, Mode> DoubleEndedIterator for Drain<'a, Mode>
where
    Mode: SmartStringMode,
{
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            DrainCast::Boxed {
                iter: Some(iter), ..
            } => iter.next_back(),
            DrainCast::Boxed { iter: None, .. } => unreachable!(),
            DrainCast::Inline { iter, .. } => iter.next_back(),
        }
    }
}

impl<'a, Mode> FusedIterator for Drain<'a, Mode> where Mode: SmartStringMode {}

impl<'a, Mode> Debug for Drain<'a, Mode>
where
    Mode: SmartStringMode,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        f.pad("Drain { ... }")
    }
}
