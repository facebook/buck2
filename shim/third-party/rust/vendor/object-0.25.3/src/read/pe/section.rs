use core::marker::PhantomData;
use core::{cmp, iter, result, slice, str};

use crate::endian::LittleEndian as LE;
use crate::pe;
use crate::read::{
    self, CompressedData, CompressedFileRange, ObjectSection, ObjectSegment, ReadError, ReadRef,
    Relocation, Result, SectionFlags, SectionIndex, SectionKind,
};

use super::{ImageNtHeaders, PeFile, SectionTable};

/// An iterator over the loadable sections of a `PeFile32`.
pub type PeSegmentIterator32<'data, 'file, R = &'data [u8]> =
    PeSegmentIterator<'data, 'file, pe::ImageNtHeaders32, R>;
/// An iterator over the loadable sections of a `PeFile64`.
pub type PeSegmentIterator64<'data, 'file, R = &'data [u8]> =
    PeSegmentIterator<'data, 'file, pe::ImageNtHeaders64, R>;

/// An iterator over the loadable sections of a `PeFile`.
#[derive(Debug)]
pub struct PeSegmentIterator<'data, 'file, Pe, R = &'data [u8]>
where
    Pe: ImageNtHeaders,
    R: ReadRef<'data>,
{
    pub(super) file: &'file PeFile<'data, Pe, R>,
    pub(super) iter: slice::Iter<'file, pe::ImageSectionHeader>,
}

impl<'data, 'file, Pe, R> Iterator for PeSegmentIterator<'data, 'file, Pe, R>
where
    Pe: ImageNtHeaders,
    R: ReadRef<'data>,
{
    type Item = PeSegment<'data, 'file, Pe, R>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|section| PeSegment {
            file: self.file,
            section,
        })
    }
}

/// A loadable section of a `PeFile32`.
pub type PeSegment32<'data, 'file, R = &'data [u8]> =
    PeSegment<'data, 'file, pe::ImageNtHeaders32, R>;
/// A loadable section of a `PeFile64`.
pub type PeSegment64<'data, 'file, R = &'data [u8]> =
    PeSegment<'data, 'file, pe::ImageNtHeaders64, R>;

/// A loadable section of a `PeFile`.
#[derive(Debug)]
pub struct PeSegment<'data, 'file, Pe, R = &'data [u8]>
where
    Pe: ImageNtHeaders,
    R: ReadRef<'data>,
{
    file: &'file PeFile<'data, Pe, R>,
    section: &'file pe::ImageSectionHeader,
}

impl<'data, 'file, Pe, R> PeSegment<'data, 'file, Pe, R>
where
    Pe: ImageNtHeaders,
    R: ReadRef<'data>,
{
    fn bytes(&self) -> Result<&'data [u8]> {
        self.section
            .pe_data(self.file.data)
            .read_error("Invalid PE section offset or size")
    }
}

impl<'data, 'file, Pe, R> read::private::Sealed for PeSegment<'data, 'file, Pe, R>
where
    Pe: ImageNtHeaders,
    R: ReadRef<'data>,
{
}

impl<'data, 'file, Pe, R> ObjectSegment<'data> for PeSegment<'data, 'file, Pe, R>
where
    Pe: ImageNtHeaders,
    R: ReadRef<'data>,
{
    #[inline]
    fn address(&self) -> u64 {
        u64::from(self.section.virtual_address.get(LE)).wrapping_add(self.file.common.image_base)
    }

    #[inline]
    fn size(&self) -> u64 {
        u64::from(self.section.virtual_size.get(LE))
    }

    #[inline]
    fn align(&self) -> u64 {
        self.file.section_alignment()
    }

    #[inline]
    fn file_range(&self) -> (u64, u64) {
        let (offset, size) = self.section.pe_file_range();
        (u64::from(offset), u64::from(size))
    }

    fn data(&self) -> Result<&'data [u8]> {
        self.bytes()
    }

    fn data_range(&self, address: u64, size: u64) -> Result<Option<&'data [u8]>> {
        Ok(read::util::data_range(
            self.bytes()?,
            self.address(),
            address,
            size,
        ))
    }

    #[inline]
    fn name(&self) -> Result<Option<&str>> {
        let name = self.section.name(self.file.common.symbols.strings())?;
        Ok(Some(
            str::from_utf8(name)
                .ok()
                .read_error("Non UTF-8 PE section name")?,
        ))
    }
}

/// An iterator over the sections of a `PeFile32`.
pub type PeSectionIterator32<'data, 'file, R = &'data [u8]> =
    PeSectionIterator<'data, 'file, pe::ImageNtHeaders32, R>;
/// An iterator over the sections of a `PeFile64`.
pub type PeSectionIterator64<'data, 'file, R = &'data [u8]> =
    PeSectionIterator<'data, 'file, pe::ImageNtHeaders64, R>;

/// An iterator over the sections of a `PeFile`.
#[derive(Debug)]
pub struct PeSectionIterator<'data, 'file, Pe, R = &'data [u8]>
where
    'data: 'file,
    Pe: ImageNtHeaders,
    R: ReadRef<'data>,
{
    pub(super) file: &'file PeFile<'data, Pe, R>,
    pub(super) iter: iter::Enumerate<slice::Iter<'file, pe::ImageSectionHeader>>,
}

impl<'data, 'file, Pe, R> Iterator for PeSectionIterator<'data, 'file, Pe, R>
where
    Pe: ImageNtHeaders,
    R: ReadRef<'data>,
{
    type Item = PeSection<'data, 'file, Pe, R>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(index, section)| PeSection {
            file: self.file,
            index: SectionIndex(index + 1),
            section,
        })
    }
}

/// A section of a `PeFile32`.
pub type PeSection32<'data, 'file, R = &'data [u8]> =
    PeSection<'data, 'file, pe::ImageNtHeaders32, R>;
/// A section of a `PeFile64`.
pub type PeSection64<'data, 'file, R = &'data [u8]> =
    PeSection<'data, 'file, pe::ImageNtHeaders64, R>;

/// A section of a `PeFile`.
#[derive(Debug)]
pub struct PeSection<'data, 'file, Pe, R = &'data [u8]>
where
    'data: 'file,
    Pe: ImageNtHeaders,
    R: ReadRef<'data>,
{
    pub(super) file: &'file PeFile<'data, Pe, R>,
    pub(super) index: SectionIndex,
    pub(super) section: &'file pe::ImageSectionHeader,
}

impl<'data, 'file, Pe, R> PeSection<'data, 'file, Pe, R>
where
    Pe: ImageNtHeaders,
    R: ReadRef<'data>,
{
    fn bytes(&self) -> Result<&'data [u8]> {
        self.section
            .pe_data(self.file.data)
            .read_error("Invalid PE section offset or size")
    }
}

impl<'data, 'file, Pe, R> read::private::Sealed for PeSection<'data, 'file, Pe, R>
where
    Pe: ImageNtHeaders,
    R: ReadRef<'data>,
{
}

impl<'data, 'file, Pe, R> ObjectSection<'data> for PeSection<'data, 'file, Pe, R>
where
    Pe: ImageNtHeaders,
    R: ReadRef<'data>,
{
    type RelocationIterator = PeRelocationIterator<'data, 'file, R>;

    #[inline]
    fn index(&self) -> SectionIndex {
        self.index
    }

    #[inline]
    fn address(&self) -> u64 {
        u64::from(self.section.virtual_address.get(LE)).wrapping_add(self.file.common.image_base)
    }

    #[inline]
    fn size(&self) -> u64 {
        u64::from(self.section.virtual_size.get(LE))
    }

    #[inline]
    fn align(&self) -> u64 {
        self.file.section_alignment()
    }

    #[inline]
    fn file_range(&self) -> Option<(u64, u64)> {
        let (offset, size) = self.section.pe_file_range();
        if size == 0 {
            None
        } else {
            Some((u64::from(offset), u64::from(size)))
        }
    }

    fn data(&self) -> Result<&'data [u8]> {
        self.bytes()
    }

    fn data_range(&self, address: u64, size: u64) -> Result<Option<&'data [u8]>> {
        Ok(read::util::data_range(
            self.bytes()?,
            self.address(),
            address,
            size,
        ))
    }

    #[inline]
    fn compressed_file_range(&self) -> Result<CompressedFileRange> {
        Ok(CompressedFileRange::none(self.file_range()))
    }

    #[inline]
    fn compressed_data(&self) -> Result<CompressedData<'data>> {
        self.data().map(CompressedData::none)
    }

    #[inline]
    fn name(&self) -> Result<&str> {
        let name = self.section.name(self.file.common.symbols.strings())?;
        str::from_utf8(name)
            .ok()
            .read_error("Non UTF-8 PE section name")
    }

    #[inline]
    fn segment_name(&self) -> Result<Option<&str>> {
        Ok(None)
    }

    #[inline]
    fn kind(&self) -> SectionKind {
        self.section.kind()
    }

    fn relocations(&self) -> PeRelocationIterator<'data, 'file, R> {
        PeRelocationIterator(PhantomData)
    }

    fn flags(&self) -> SectionFlags {
        SectionFlags::Coff {
            characteristics: self.section.characteristics.get(LE),
        }
    }
}

impl<'data> SectionTable<'data> {
    /// Return the data at the given virtual address in a PE file.
    pub fn pe_data_at<R: ReadRef<'data>>(&self, data: R, va: u32) -> Option<&'data [u8]> {
        self.iter()
            .filter_map(|section| section.pe_data_at(data, va))
            .next()
    }
}

impl pe::ImageSectionHeader {
    /// Return the offset and size of the section in a PE file.
    ///
    /// Returns `None` for sections that have no data in the file.
    pub fn pe_file_range(&self) -> (u32, u32) {
        // Pointer and size will be zero for uninitialized data; we don't need to validate this.
        let offset = self.pointer_to_raw_data.get(LE);
        let size = cmp::min(self.virtual_size.get(LE), self.size_of_raw_data.get(LE));
        (offset, size)
    }

    /// Return the section data in a PE file.
    pub fn pe_data<'data, R: ReadRef<'data>>(&self, data: R) -> result::Result<&'data [u8], ()> {
        let (offset, size) = self.pe_file_range();
        data.read_bytes_at(offset.into(), size.into())
    }

    /// Return the data at the given virtual address if this section contains it.
    pub fn pe_data_at<'data, R: ReadRef<'data>>(&self, data: R, va: u32) -> Option<&'data [u8]> {
        let section_va = self.virtual_address.get(LE);
        let offset = va.checked_sub(section_va)?;
        let section_data = self.pe_data(data).ok()?;
        section_data.get(offset as usize..)
    }
}

/// An iterator over the relocations in an `PeSection`.
#[derive(Debug)]
pub struct PeRelocationIterator<'data, 'file, R = &'data [u8]>(
    PhantomData<(&'data (), &'file (), R)>,
);

impl<'data, 'file, R> Iterator for PeRelocationIterator<'data, 'file, R> {
    type Item = (u64, Relocation);

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}
