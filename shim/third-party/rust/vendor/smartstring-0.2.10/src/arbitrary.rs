use crate::{SmartString, SmartStringMode};
use alloc::string::String;
use arbitrary::{Arbitrary, Result, Unstructured};

impl<'a, Mode: SmartStringMode> Arbitrary<'a> for SmartString<Mode>
where
    Mode: 'static,
{
    fn arbitrary(u: &mut Unstructured<'_>) -> Result<Self> {
        String::arbitrary(u).map(Self::from)
    }

    fn arbitrary_take_rest(u: Unstructured<'_>) -> Result<Self> {
        String::arbitrary_take_rest(u).map(Self::from)
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        String::size_hint(depth)
    }
}
