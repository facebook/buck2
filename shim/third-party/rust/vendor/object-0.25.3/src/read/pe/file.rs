use alloc::vec::Vec;
use core::fmt::Debug;
use core::{mem, str};

use core::convert::TryInto;

use crate::read::coff::{CoffCommon, CoffSymbol, CoffSymbolIterator, CoffSymbolTable, SymbolTable};
use crate::read::{
    self, Architecture, ComdatKind, Error, Export, FileFlags, Import, NoDynamicRelocationIterator,
    Object, ObjectComdat, ReadError, ReadRef, Result, SectionIndex, SymbolIndex,
};
use crate::{
    pe, ByteString, Bytes, CodeView, LittleEndian as LE, Pod, U16Bytes, U32Bytes, U32, U64,
};

use super::{PeSection, PeSectionIterator, PeSegment, PeSegmentIterator, SectionTable};

/// A PE32 (32-bit) image file.
pub type PeFile32<'data, R = &'data [u8]> = PeFile<'data, pe::ImageNtHeaders32, R>;
/// A PE32+ (64-bit) image file.
pub type PeFile64<'data, R = &'data [u8]> = PeFile<'data, pe::ImageNtHeaders64, R>;

/// A PE object file.
#[derive(Debug)]
pub struct PeFile<'data, Pe, R = &'data [u8]>
where
    Pe: ImageNtHeaders,
    R: ReadRef<'data>,
{
    pub(super) dos_header: &'data pe::ImageDosHeader,
    pub(super) nt_headers: &'data Pe,
    pub(super) data_directories: &'data [pe::ImageDataDirectory],
    pub(super) common: CoffCommon<'data>,
    pub(super) data: R,
}

impl<'data, Pe, R> PeFile<'data, Pe, R>
where
    Pe: ImageNtHeaders,
    R: ReadRef<'data>,
{
    /// Parse the raw PE file data.
    pub fn parse(data: R) -> Result<Self> {
        let dos_header = pe::ImageDosHeader::parse(data)?;
        let mut offset = dos_header.nt_headers_offset().into();
        let (nt_headers, data_directories) = Pe::parse(data, &mut offset)?;
        let sections = nt_headers.sections(data, offset)?;
        let symbols = nt_headers.symbols(data)?;
        let image_base = nt_headers.optional_header().image_base();

        Ok(PeFile {
            dos_header,
            nt_headers,
            data_directories,
            common: CoffCommon {
                sections,
                symbols,
                image_base,
            },
            data,
        })
    }

    pub(super) fn section_alignment(&self) -> u64 {
        u64::from(self.nt_headers.optional_header().section_alignment())
    }

    /// Return the DOS header of this file
    pub fn dos_header(&self) -> &'data pe::ImageDosHeader {
        self.dos_header
    }

    /// Return the NT Headers of this file
    pub fn nt_headers(&self) -> &'data Pe {
        self.nt_headers
    }

    fn data_directory(&self, id: usize) -> Option<&'data pe::ImageDataDirectory> {
        self.data_directories
            .get(id)
            .filter(|d| d.size.get(LE) != 0)
    }

    fn data_at(&self, va: u32) -> Option<Bytes<'data>> {
        self.common.sections.pe_data_at(self.data, va).map(Bytes)
    }
}

impl<'data, Pe, R> read::private::Sealed for PeFile<'data, Pe, R>
where
    Pe: ImageNtHeaders,
    R: ReadRef<'data>,
{
}

impl<'data, 'file, Pe, R> Object<'data, 'file> for PeFile<'data, Pe, R>
where
    'data: 'file,
    Pe: ImageNtHeaders,
    R: 'file + ReadRef<'data>,
{
    type Segment = PeSegment<'data, 'file, Pe, R>;
    type SegmentIterator = PeSegmentIterator<'data, 'file, Pe, R>;
    type Section = PeSection<'data, 'file, Pe, R>;
    type SectionIterator = PeSectionIterator<'data, 'file, Pe, R>;
    type Comdat = PeComdat<'data, 'file, Pe, R>;
    type ComdatIterator = PeComdatIterator<'data, 'file, Pe, R>;
    type Symbol = CoffSymbol<'data, 'file>;
    type SymbolIterator = CoffSymbolIterator<'data, 'file>;
    type SymbolTable = CoffSymbolTable<'data, 'file>;
    type DynamicRelocationIterator = NoDynamicRelocationIterator;

    fn architecture(&self) -> Architecture {
        match self.nt_headers.file_header().machine.get(LE) {
            pe::IMAGE_FILE_MACHINE_ARMNT => Architecture::Arm,
            pe::IMAGE_FILE_MACHINE_ARM64 => Architecture::Aarch64,
            pe::IMAGE_FILE_MACHINE_I386 => Architecture::I386,
            pe::IMAGE_FILE_MACHINE_AMD64 => Architecture::X86_64,
            _ => Architecture::Unknown,
        }
    }

    #[inline]
    fn is_little_endian(&self) -> bool {
        // Only little endian is supported.
        true
    }

    #[inline]
    fn is_64(&self) -> bool {
        self.nt_headers.is_type_64()
    }

    fn segments(&'file self) -> PeSegmentIterator<'data, 'file, Pe, R> {
        PeSegmentIterator {
            file: self,
            iter: self.common.sections.iter(),
        }
    }

    fn section_by_name(&'file self, section_name: &str) -> Option<PeSection<'data, 'file, Pe, R>> {
        self.common
            .sections
            .section_by_name(self.common.symbols.strings(), section_name.as_bytes())
            .map(|(index, section)| PeSection {
                file: self,
                index: SectionIndex(index),
                section,
            })
    }

    fn section_by_index(
        &'file self,
        index: SectionIndex,
    ) -> Result<PeSection<'data, 'file, Pe, R>> {
        let section = self.common.sections.section(index.0)?;
        Ok(PeSection {
            file: self,
            index,
            section,
        })
    }

    fn sections(&'file self) -> PeSectionIterator<'data, 'file, Pe, R> {
        PeSectionIterator {
            file: self,
            iter: self.common.sections.iter().enumerate(),
        }
    }

    fn comdats(&'file self) -> PeComdatIterator<'data, 'file, Pe, R> {
        PeComdatIterator { file: self }
    }

    fn symbol_by_index(&'file self, index: SymbolIndex) -> Result<CoffSymbol<'data, 'file>> {
        let symbol = self.common.symbols.symbol(index.0)?;
        Ok(CoffSymbol {
            file: &self.common,
            index,
            symbol,
        })
    }

    fn symbols(&'file self) -> CoffSymbolIterator<'data, 'file> {
        CoffSymbolIterator {
            file: &self.common,
            index: 0,
        }
    }

    fn symbol_table(&'file self) -> Option<CoffSymbolTable<'data, 'file>> {
        Some(CoffSymbolTable { file: &self.common })
    }

    fn dynamic_symbols(&'file self) -> CoffSymbolIterator<'data, 'file> {
        CoffSymbolIterator {
            file: &self.common,
            // Hack: don't return any.
            index: self.common.symbols.len(),
        }
    }

    fn dynamic_symbol_table(&'file self) -> Option<CoffSymbolTable<'data, 'file>> {
        None
    }

    fn dynamic_relocations(&'file self) -> Option<NoDynamicRelocationIterator> {
        None
    }

    fn imports(&self) -> Result<Vec<Import<'data>>> {
        let data_dir = match self.data_directory(pe::IMAGE_DIRECTORY_ENTRY_IMPORT) {
            Some(data_dir) => data_dir,
            None => return Ok(Vec::new()),
        };
        let mut import_descriptors = data_dir.data(self.data, &self.common.sections).map(Bytes)?;
        let mut imports = Vec::new();
        loop {
            let import_desc = import_descriptors
                .read::<pe::ImageImportDescriptor>()
                .read_error("Missing PE null import descriptor")?;
            if import_desc.original_first_thunk.get(LE) == 0 {
                break;
            }

            let library = self
                .data_at(import_desc.name.get(LE))
                .read_error("Invalid PE import descriptor name")?
                .read_string()
                .read_error("Invalid PE import descriptor name")?;

            let thunk_va = import_desc.original_first_thunk.get(LE);
            let mut thunk_data = self
                .data_at(thunk_va)
                .read_error("Invalid PE import thunk address")?;
            loop {
                let hint_name = if self.is_64() {
                    let thunk = thunk_data
                        .read::<U64<_>>()
                        .read_error("Missing PE null import thunk")?
                        .get(LE);
                    if thunk == 0 {
                        break;
                    }
                    if thunk & pe::IMAGE_ORDINAL_FLAG64 != 0 {
                        // TODO: handle import by ordinal
                        continue;
                    }
                    thunk as u32
                } else {
                    let thunk = thunk_data
                        .read::<U32<_>>()
                        .read_error("Missing PE null import thunk")?
                        .get(LE);
                    if thunk == 0 {
                        break;
                    }
                    if thunk & pe::IMAGE_ORDINAL_FLAG32 != 0 {
                        // TODO: handle import by ordinal
                        continue;
                    }
                    thunk
                };
                let name = self
                    .data_at(hint_name)
                    .read_error("Invalid PE import thunk name")?
                    .read_string_at(2)
                    .read_error("Invalid PE import thunk name")?;

                imports.push(Import {
                    name: ByteString(name),
                    library: ByteString(library),
                });
            }
        }
        Ok(imports)
    }

    fn exports(&self) -> Result<Vec<Export<'data>>> {
        let data_dir = match self.data_directory(pe::IMAGE_DIRECTORY_ENTRY_EXPORT) {
            Some(data_dir) => data_dir,
            None => return Ok(Vec::new()),
        };
        let export_va = data_dir.virtual_address.get(LE);
        let export_size = data_dir.size.get(LE);
        let export_data = data_dir.data(self.data, &self.common.sections).map(Bytes)?;
        let export_dir = export_data
            .read_at::<pe::ImageExportDirectory>(0)
            .read_error("Invalid PE export dir size")?;
        let addresses = export_data
            .read_slice_at::<U32Bytes<_>>(
                export_dir
                    .address_of_functions
                    .get(LE)
                    .wrapping_sub(export_va) as usize,
                export_dir.number_of_functions.get(LE) as usize,
            )
            .read_error("Invalid PE export address table")?;
        let number = export_dir.number_of_names.get(LE) as usize;
        let names = export_data
            .read_slice_at::<U32Bytes<_>>(
                export_dir.address_of_names.get(LE).wrapping_sub(export_va) as usize,
                number,
            )
            .read_error("Invalid PE export name table")?;
        let ordinals = export_data
            .read_slice_at::<U16Bytes<_>>(
                export_dir
                    .address_of_name_ordinals
                    .get(LE)
                    .wrapping_sub(export_va) as usize,
                number,
            )
            .read_error("Invalid PE export ordinal table")?;

        let mut exports = Vec::new();
        for (name, ordinal) in names.iter().zip(ordinals.iter()) {
            let name = export_data
                .read_string_at(name.get(LE).wrapping_sub(export_va) as usize)
                .read_error("Invalid PE export name entry")?;
            let address = addresses
                .get(ordinal.get(LE) as usize)
                .read_error("Invalid PE export ordinal entry")?
                .get(LE);
            // Check for export address (vs forwarder address).
            if address < export_va || (address - export_va) >= export_size {
                exports.push(Export {
                    name: ByteString(name),
                    address: self.common.image_base.wrapping_add(address.into()),
                })
            }
        }
        Ok(exports)
    }

    fn pdb_info(&self) -> Result<Option<CodeView>> {
        let data_dir = match self.data_directory(pe::IMAGE_DIRECTORY_ENTRY_DEBUG) {
            Some(data_dir) => data_dir,
            None => return Ok(None),
        };
        let debug_data = data_dir.data(self.data, &self.common.sections).map(Bytes)?;
        let debug_dir = debug_data
            .read_at::<pe::ImageDebugDirectory>(0)
            .read_error("Invalid PE debug dir size")?;

        if debug_dir.typ.get(LE) != pe::IMAGE_DEBUG_TYPE_CODEVIEW {
            return Ok(None);
        }

        let info = self
            .data
            .read_slice_at::<u8>(
                debug_dir.pointer_to_raw_data.get(LE) as u64,
                debug_dir.size_of_data.get(LE) as usize,
            )
            .read_error("Invalid CodeView Info address")?;

        let mut info = Bytes(info);

        let sig = info
            .read_bytes(4)
            .read_error("Invalid CodeView signature")?;
        if sig.0 != b"RSDS" {
            return Ok(None);
        }

        let guid: [u8; 16] = info
            .read_bytes(16)
            .read_error("Invalid CodeView GUID")?
            .0
            .try_into()
            .unwrap();

        let age = info.read::<U32<LE>>().read_error("Invalid CodeView Age")?;

        let path = info
            .read_string()
            .read_error("Invalid CodeView file path")?;

        Ok(Some(CodeView {
            path: ByteString(path),
            guid,
            age: age.get(LE),
        }))
    }

    fn has_debug_symbols(&self) -> bool {
        self.section_by_name(".debug_info").is_some()
    }

    fn relative_address_base(&self) -> u64 {
        self.common.image_base
    }

    fn entry(&self) -> u64 {
        u64::from(self.nt_headers.optional_header().address_of_entry_point())
            .wrapping_add(self.common.image_base)
    }

    fn flags(&self) -> FileFlags {
        FileFlags::Coff {
            characteristics: self.nt_headers.file_header().characteristics.get(LE),
        }
    }
}

/// An iterator over the COMDAT section groups of a `PeFile32`.
pub type PeComdatIterator32<'data, 'file, R = &'data [u8]> =
    PeComdatIterator<'data, 'file, pe::ImageNtHeaders32, R>;
/// An iterator over the COMDAT section groups of a `PeFile64`.
pub type PeComdatIterator64<'data, 'file, R = &'data [u8]> =
    PeComdatIterator<'data, 'file, pe::ImageNtHeaders64, R>;

/// An iterator over the COMDAT section groups of a `PeFile`.
#[derive(Debug)]
pub struct PeComdatIterator<'data, 'file, Pe, R = &'data [u8]>
where
    Pe: ImageNtHeaders,
    R: ReadRef<'data>,
{
    file: &'file PeFile<'data, Pe, R>,
}

impl<'data, 'file, Pe, R> Iterator for PeComdatIterator<'data, 'file, Pe, R>
where
    Pe: ImageNtHeaders,
    R: ReadRef<'data>,
{
    type Item = PeComdat<'data, 'file, Pe, R>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}

/// A COMDAT section group of a `PeFile32`.
pub type PeComdat32<'data, 'file, R = &'data [u8]> =
    PeComdat<'data, 'file, pe::ImageNtHeaders32, R>;
/// A COMDAT section group of a `PeFile64`.
pub type PeComdat64<'data, 'file, R = &'data [u8]> =
    PeComdat<'data, 'file, pe::ImageNtHeaders64, R>;

/// A COMDAT section group of a `PeFile`.
#[derive(Debug)]
pub struct PeComdat<'data, 'file, Pe, R = &'data [u8]>
where
    Pe: ImageNtHeaders,
    R: ReadRef<'data>,
{
    file: &'file PeFile<'data, Pe, R>,
}

impl<'data, 'file, Pe, R> read::private::Sealed for PeComdat<'data, 'file, Pe, R>
where
    Pe: ImageNtHeaders,
    R: ReadRef<'data>,
{
}

impl<'data, 'file, Pe, R> ObjectComdat<'data> for PeComdat<'data, 'file, Pe, R>
where
    Pe: ImageNtHeaders,
    R: ReadRef<'data>,
{
    type SectionIterator = PeComdatSectionIterator<'data, 'file, Pe, R>;

    #[inline]
    fn kind(&self) -> ComdatKind {
        unreachable!();
    }

    #[inline]
    fn symbol(&self) -> SymbolIndex {
        unreachable!();
    }

    #[inline]
    fn name(&self) -> Result<&str> {
        unreachable!();
    }

    #[inline]
    fn sections(&self) -> Self::SectionIterator {
        unreachable!();
    }
}

/// An iterator over the sections in a COMDAT section group of a `PeFile32`.
pub type PeComdatSectionIterator32<'data, 'file, R = &'data [u8]> =
    PeComdatSectionIterator<'data, 'file, pe::ImageNtHeaders32, R>;
/// An iterator over the sections in a COMDAT section group of a `PeFile64`.
pub type PeComdatSectionIterator64<'data, 'file, R = &'data [u8]> =
    PeComdatSectionIterator<'data, 'file, pe::ImageNtHeaders64, R>;

/// An iterator over the sections in a COMDAT section group of a `PeFile`.
#[derive(Debug)]
pub struct PeComdatSectionIterator<'data, 'file, Pe, R = &'data [u8]>
where
    Pe: ImageNtHeaders,
    R: ReadRef<'data>,
{
    file: &'file PeFile<'data, Pe, R>,
}

impl<'data, 'file, Pe, R> Iterator for PeComdatSectionIterator<'data, 'file, Pe, R>
where
    Pe: ImageNtHeaders,
    R: ReadRef<'data>,
{
    type Item = SectionIndex;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}

impl pe::ImageDosHeader {
    /// Read the DOS header.
    ///
    /// Also checks that the `e_magic` field in the header is valid.
    pub fn parse<'data, R: ReadRef<'data>>(data: R) -> read::Result<&'data Self> {
        // DOS header comes first.
        let dos_header = data
            .read_at::<pe::ImageDosHeader>(0)
            .read_error("Invalid DOS header size or alignment")?;
        if dos_header.e_magic.get(LE) != pe::IMAGE_DOS_SIGNATURE {
            return Err(Error("Invalid DOS magic"));
        }
        Ok(dos_header)
    }

    /// Return the file offset of the nt_headers.
    #[inline]
    pub fn nt_headers_offset(&self) -> u32 {
        self.e_lfanew.get(LE)
    }
}

/// Find the optional header and read the `optional_header.magic`.
///
/// It can be useful to know this magic value before trying to
/// fully parse the NT headers.
pub fn optional_header_magic<'data, R: ReadRef<'data>>(data: R) -> Result<u16> {
    let dos_header = pe::ImageDosHeader::parse(data)?;
    // NT headers are at an offset specified in the DOS header.
    let offset = dos_header.nt_headers_offset().into();
    // It doesn't matter which NT header type is used for the purpose
    // of reading the optional header magic.
    let nt_headers = data
        .read_at::<pe::ImageNtHeaders32>(offset)
        .read_error("Invalid NT headers offset, size, or alignment")?;
    if nt_headers.signature() != pe::IMAGE_NT_SIGNATURE {
        return Err(Error("Invalid PE magic"));
    }
    Ok(nt_headers.optional_header().magic())
}

/// A trait for generic access to `ImageNtHeaders32` and `ImageNtHeaders64`.
#[allow(missing_docs)]
pub trait ImageNtHeaders: Debug + Pod {
    type ImageOptionalHeader: ImageOptionalHeader;

    /// Return true if this type is a 64-bit header.
    ///
    /// This is a property of the type, not a value in the header data.
    fn is_type_64(&self) -> bool;

    /// Return true if the magic field in the optional header is valid.
    fn is_valid_optional_magic(&self) -> bool;

    /// Return the signature
    fn signature(&self) -> u32;

    /// Return the file header.
    fn file_header(&self) -> &pe::ImageFileHeader;

    /// Return the optional header.
    fn optional_header(&self) -> &Self::ImageOptionalHeader;

    // Provided methods.

    /// Read the NT headers, including the data directories.
    ///
    /// `data` must be for the entire file.
    ///
    /// `offset` must be headers offset, which can be obtained from `ImageDosHeader::nt_headers_offset`.
    /// It is updated to point after the optional header, which is where the section headers are located.
    ///
    /// Also checks that the `signature` and `magic` fields in the headers are valid.
    fn parse<'data, R: ReadRef<'data>>(
        data: R,
        offset: &mut u64,
    ) -> read::Result<(&'data Self, &'data [pe::ImageDataDirectory])> {
        // Note that this does not include the data directories in the optional header.
        let nt_headers = data
            .read::<Self>(offset)
            .read_error("Invalid PE headers offset or size")?;
        if nt_headers.signature() != pe::IMAGE_NT_SIGNATURE {
            return Err(Error("Invalid PE magic"));
        }
        if !nt_headers.is_valid_optional_magic() {
            return Err(Error("Invalid PE optional header magic"));
        }

        // Read the rest of the optional header, and then read the data directories from that.
        let optional_data_size =
            u64::from(nt_headers.file_header().size_of_optional_header.get(LE))
                .checked_sub(mem::size_of::<Self::ImageOptionalHeader>() as u64)
                .read_error("PE optional header size is too small")?;
        let mut optional_data = data
            .read_bytes(offset, optional_data_size)
            .read_error("Invalid PE optional header size")
            .map(Bytes)?;
        let data_directories = optional_data
            .read_slice(nt_headers.optional_header().number_of_rva_and_sizes() as usize)
            .read_error("Invalid PE number of RVA and sizes")?;

        Ok((nt_headers, data_directories))
    }

    /// Read the section table.
    ///
    /// `data` must be for the entire file.
    /// `offset` must be after the optional file header.
    #[inline]
    fn sections<'data, R: ReadRef<'data>>(
        &self,
        data: R,
        offset: u64,
    ) -> read::Result<SectionTable<'data>> {
        SectionTable::parse(self.file_header(), data, offset)
    }

    /// Read the symbol table and string table.
    ///
    /// `data` must be the entire file data.
    #[inline]
    fn symbols<'data, R: ReadRef<'data>>(&self, data: R) -> read::Result<SymbolTable<'data>> {
        SymbolTable::parse(self.file_header(), data)
    }
}

/// A trait for generic access to `ImageOptionalHeader32` and `ImageOptionalHeader64`.
#[allow(missing_docs)]
pub trait ImageOptionalHeader: Debug + Pod {
    // Standard fields.
    fn magic(&self) -> u16;
    fn major_linker_version(&self) -> u8;
    fn minor_linker_version(&self) -> u8;
    fn size_of_code(&self) -> u32;
    fn size_of_initialized_data(&self) -> u32;
    fn size_of_uninitialized_data(&self) -> u32;
    fn address_of_entry_point(&self) -> u32;
    fn base_of_code(&self) -> u32;

    // NT additional fields.
    fn image_base(&self) -> u64;
    fn section_alignment(&self) -> u32;
    fn file_alignment(&self) -> u32;
    fn major_operating_system_version(&self) -> u16;
    fn minor_operating_system_version(&self) -> u16;
    fn major_image_version(&self) -> u16;
    fn minor_image_version(&self) -> u16;
    fn major_subsystem_version(&self) -> u16;
    fn minor_subsystem_version(&self) -> u16;
    fn win32_version_value(&self) -> u32;
    fn size_of_image(&self) -> u32;
    fn size_of_headers(&self) -> u32;
    fn check_sum(&self) -> u32;
    fn subsystem(&self) -> u16;
    fn dll_characteristics(&self) -> u16;
    fn size_of_stack_reserve(&self) -> u64;
    fn size_of_stack_commit(&self) -> u64;
    fn size_of_heap_reserve(&self) -> u64;
    fn size_of_heap_commit(&self) -> u64;
    fn loader_flags(&self) -> u32;
    fn number_of_rva_and_sizes(&self) -> u32;
}

impl ImageNtHeaders for pe::ImageNtHeaders32 {
    type ImageOptionalHeader = pe::ImageOptionalHeader32;

    #[inline]
    fn is_type_64(&self) -> bool {
        false
    }

    #[inline]
    fn is_valid_optional_magic(&self) -> bool {
        self.optional_header.magic.get(LE) == pe::IMAGE_NT_OPTIONAL_HDR32_MAGIC
    }

    #[inline]
    fn signature(&self) -> u32 {
        self.signature.get(LE)
    }

    #[inline]
    fn file_header(&self) -> &pe::ImageFileHeader {
        &self.file_header
    }

    #[inline]
    fn optional_header(&self) -> &Self::ImageOptionalHeader {
        &self.optional_header
    }
}

impl ImageOptionalHeader for pe::ImageOptionalHeader32 {
    #[inline]
    fn magic(&self) -> u16 {
        self.magic.get(LE)
    }

    #[inline]
    fn major_linker_version(&self) -> u8 {
        self.major_linker_version
    }

    #[inline]
    fn minor_linker_version(&self) -> u8 {
        self.minor_linker_version
    }

    #[inline]
    fn size_of_code(&self) -> u32 {
        self.size_of_code.get(LE)
    }

    #[inline]
    fn size_of_initialized_data(&self) -> u32 {
        self.size_of_initialized_data.get(LE)
    }

    #[inline]
    fn size_of_uninitialized_data(&self) -> u32 {
        self.size_of_uninitialized_data.get(LE)
    }

    #[inline]
    fn address_of_entry_point(&self) -> u32 {
        self.address_of_entry_point.get(LE)
    }

    #[inline]
    fn base_of_code(&self) -> u32 {
        self.base_of_code.get(LE)
    }

    #[inline]
    fn image_base(&self) -> u64 {
        self.image_base.get(LE).into()
    }

    #[inline]
    fn section_alignment(&self) -> u32 {
        self.section_alignment.get(LE)
    }

    #[inline]
    fn file_alignment(&self) -> u32 {
        self.file_alignment.get(LE)
    }

    #[inline]
    fn major_operating_system_version(&self) -> u16 {
        self.major_operating_system_version.get(LE)
    }

    #[inline]
    fn minor_operating_system_version(&self) -> u16 {
        self.minor_operating_system_version.get(LE)
    }

    #[inline]
    fn major_image_version(&self) -> u16 {
        self.major_image_version.get(LE)
    }

    #[inline]
    fn minor_image_version(&self) -> u16 {
        self.minor_image_version.get(LE)
    }

    #[inline]
    fn major_subsystem_version(&self) -> u16 {
        self.major_subsystem_version.get(LE)
    }

    #[inline]
    fn minor_subsystem_version(&self) -> u16 {
        self.minor_subsystem_version.get(LE)
    }

    #[inline]
    fn win32_version_value(&self) -> u32 {
        self.win32_version_value.get(LE)
    }

    #[inline]
    fn size_of_image(&self) -> u32 {
        self.size_of_image.get(LE)
    }

    #[inline]
    fn size_of_headers(&self) -> u32 {
        self.size_of_headers.get(LE)
    }

    #[inline]
    fn check_sum(&self) -> u32 {
        self.check_sum.get(LE)
    }

    #[inline]
    fn subsystem(&self) -> u16 {
        self.subsystem.get(LE)
    }

    #[inline]
    fn dll_characteristics(&self) -> u16 {
        self.dll_characteristics.get(LE)
    }

    #[inline]
    fn size_of_stack_reserve(&self) -> u64 {
        self.size_of_stack_reserve.get(LE).into()
    }

    #[inline]
    fn size_of_stack_commit(&self) -> u64 {
        self.size_of_stack_commit.get(LE).into()
    }

    #[inline]
    fn size_of_heap_reserve(&self) -> u64 {
        self.size_of_heap_reserve.get(LE).into()
    }

    #[inline]
    fn size_of_heap_commit(&self) -> u64 {
        self.size_of_heap_commit.get(LE).into()
    }

    #[inline]
    fn loader_flags(&self) -> u32 {
        self.loader_flags.get(LE)
    }

    #[inline]
    fn number_of_rva_and_sizes(&self) -> u32 {
        self.number_of_rva_and_sizes.get(LE)
    }
}

impl ImageNtHeaders for pe::ImageNtHeaders64 {
    type ImageOptionalHeader = pe::ImageOptionalHeader64;

    #[inline]
    fn is_type_64(&self) -> bool {
        true
    }

    #[inline]
    fn is_valid_optional_magic(&self) -> bool {
        self.optional_header.magic.get(LE) == pe::IMAGE_NT_OPTIONAL_HDR64_MAGIC
    }

    #[inline]
    fn signature(&self) -> u32 {
        self.signature.get(LE)
    }

    #[inline]
    fn file_header(&self) -> &pe::ImageFileHeader {
        &self.file_header
    }

    #[inline]
    fn optional_header(&self) -> &Self::ImageOptionalHeader {
        &self.optional_header
    }
}

impl ImageOptionalHeader for pe::ImageOptionalHeader64 {
    #[inline]
    fn magic(&self) -> u16 {
        self.magic.get(LE)
    }

    #[inline]
    fn major_linker_version(&self) -> u8 {
        self.major_linker_version
    }

    #[inline]
    fn minor_linker_version(&self) -> u8 {
        self.minor_linker_version
    }

    #[inline]
    fn size_of_code(&self) -> u32 {
        self.size_of_code.get(LE)
    }

    #[inline]
    fn size_of_initialized_data(&self) -> u32 {
        self.size_of_initialized_data.get(LE)
    }

    #[inline]
    fn size_of_uninitialized_data(&self) -> u32 {
        self.size_of_uninitialized_data.get(LE)
    }

    #[inline]
    fn address_of_entry_point(&self) -> u32 {
        self.address_of_entry_point.get(LE)
    }

    #[inline]
    fn base_of_code(&self) -> u32 {
        self.base_of_code.get(LE)
    }

    #[inline]
    fn image_base(&self) -> u64 {
        self.image_base.get(LE)
    }

    #[inline]
    fn section_alignment(&self) -> u32 {
        self.section_alignment.get(LE)
    }

    #[inline]
    fn file_alignment(&self) -> u32 {
        self.file_alignment.get(LE)
    }

    #[inline]
    fn major_operating_system_version(&self) -> u16 {
        self.major_operating_system_version.get(LE)
    }

    #[inline]
    fn minor_operating_system_version(&self) -> u16 {
        self.minor_operating_system_version.get(LE)
    }

    #[inline]
    fn major_image_version(&self) -> u16 {
        self.major_image_version.get(LE)
    }

    #[inline]
    fn minor_image_version(&self) -> u16 {
        self.minor_image_version.get(LE)
    }

    #[inline]
    fn major_subsystem_version(&self) -> u16 {
        self.major_subsystem_version.get(LE)
    }

    #[inline]
    fn minor_subsystem_version(&self) -> u16 {
        self.minor_subsystem_version.get(LE)
    }

    #[inline]
    fn win32_version_value(&self) -> u32 {
        self.win32_version_value.get(LE)
    }

    #[inline]
    fn size_of_image(&self) -> u32 {
        self.size_of_image.get(LE)
    }

    #[inline]
    fn size_of_headers(&self) -> u32 {
        self.size_of_headers.get(LE)
    }

    #[inline]
    fn check_sum(&self) -> u32 {
        self.check_sum.get(LE)
    }

    #[inline]
    fn subsystem(&self) -> u16 {
        self.subsystem.get(LE)
    }

    #[inline]
    fn dll_characteristics(&self) -> u16 {
        self.dll_characteristics.get(LE)
    }

    #[inline]
    fn size_of_stack_reserve(&self) -> u64 {
        self.size_of_stack_reserve.get(LE)
    }

    #[inline]
    fn size_of_stack_commit(&self) -> u64 {
        self.size_of_stack_commit.get(LE)
    }

    #[inline]
    fn size_of_heap_reserve(&self) -> u64 {
        self.size_of_heap_reserve.get(LE)
    }

    #[inline]
    fn size_of_heap_commit(&self) -> u64 {
        self.size_of_heap_commit.get(LE)
    }

    #[inline]
    fn loader_flags(&self) -> u32 {
        self.loader_flags.get(LE)
    }

    #[inline]
    fn number_of_rva_and_sizes(&self) -> u32 {
        self.number_of_rva_and_sizes.get(LE)
    }
}

impl pe::ImageDataDirectory {
    /// Get the data referenced by this directory entry.
    pub fn data<'data, R: ReadRef<'data>>(
        &self,
        data: R,
        sections: &SectionTable<'data>,
    ) -> Result<&'data [u8]> {
        sections
            .pe_data_at(data, self.virtual_address.get(LE))
            .read_error("Invalid data dir virtual address or size")?
            .get(..self.size.get(LE) as usize)
            .read_error("Invalid data dir size")
    }
}
