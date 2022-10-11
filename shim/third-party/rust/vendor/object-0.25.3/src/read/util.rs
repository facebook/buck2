use core::convert::TryInto;

use crate::pod::Bytes;

#[allow(dead_code)]
#[inline]
pub(crate) fn align(offset: usize, size: usize) -> usize {
    (offset + (size - 1)) & !(size - 1)
}

#[allow(dead_code)]
pub(crate) fn data_range(
    data: &[u8],
    data_address: u64,
    range_address: u64,
    size: u64,
) -> Option<&[u8]> {
    let offset = range_address.checked_sub(data_address)?;
    data.get(offset.try_into().ok()?..)?
        .get(..size.try_into().ok()?)
}

/// A table of zero-terminated strings.
///
/// This is used for most file formats.
#[derive(Debug, Default, Clone, Copy)]
pub struct StringTable<'data> {
    data: Bytes<'data>,
}

impl<'data> StringTable<'data> {
    /// Interpret the given data as a string table.
    pub fn new(data: &'data [u8]) -> Self {
        StringTable { data: Bytes(data) }
    }

    /// Return the string at the given offset.
    pub fn get(&self, offset: u32) -> Result<&'data [u8], ()> {
        self.data.read_string_at(offset as usize)
    }
}
