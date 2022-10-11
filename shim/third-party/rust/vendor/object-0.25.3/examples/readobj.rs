//! Example that uses the lower level read API.

use object::read::archive::ArchiveFile;
use object::read::macho::{FatArch, FatHeader};
use object::Endianness;
use std::convert::TryInto;
use std::io::Write;
use std::{env, fmt, fs, io, process, str};

fn main() {
    let arg_len = env::args().len();
    if arg_len <= 1 {
        eprintln!("Usage: {} <file> ...", env::args().next().unwrap());
        process::exit(1);
    }

    for file_path in env::args().skip(1) {
        if arg_len > 2 {
            println!();
            println!("{}:", file_path);
        }

        let file = match fs::File::open(&file_path) {
            Ok(file) => file,
            Err(err) => {
                println!("Failed to open file '{}': {}", file_path, err);
                continue;
            }
        };
        let file = match unsafe { memmap2::Mmap::map(&file) } {
            Ok(mmap) => mmap,
            Err(err) => {
                println!("Failed to map file '{}': {}", file_path, err);
                continue;
            }
        };

        let stdout = io::stdout();
        let mut printer = Printer::new(stdout.lock());
        print_object(&mut printer, &*file);
    }
}

struct Printer<W: Write> {
    w: W,
    indent: usize,
}

impl<W: Write> Printer<W> {
    fn new(w: W) -> Self {
        Self { w, indent: 0 }
    }

    fn blank(&mut self) {
        writeln!(self.w).unwrap();
    }

    fn print_indent(&mut self) {
        if self.indent != 0 {
            write!(self.w, "{:-1$}", " ", self.indent * 4).unwrap();
        }
    }

    fn print_string(&mut self, s: &[u8]) {
        if let Ok(s) = str::from_utf8(s) {
            write!(self.w, "\"{}\"", s).unwrap();
        } else {
            write!(self.w, "{:X?}", s).unwrap();
        }
    }

    fn indent<F: FnOnce(&mut Self)>(&mut self, f: F) {
        self.indent += 1;
        f(self);
        self.indent -= 1;
    }

    fn group<F: FnOnce(&mut Self)>(&mut self, name: &str, f: F) {
        self.print_indent();
        writeln!(self.w, "{} {{", name).unwrap();
        self.indent(f);
        self.print_indent();
        writeln!(self.w, "}}").unwrap();
    }

    fn field_name(&mut self, name: &str) {
        self.print_indent();
        if !name.is_empty() {
            write!(self.w, "{}: ", name).unwrap();
        }
    }

    fn field<T: fmt::Display>(&mut self, name: &str, value: T) {
        self.field_name(name);
        writeln!(self.w, "{}", value).unwrap();
    }

    fn field_hex<T: fmt::UpperHex>(&mut self, name: &str, value: T) {
        self.field_name(name);
        writeln!(self.w, "0x{:X}", value).unwrap();
    }

    fn field_bytes(&mut self, name: &str, value: &[u8]) {
        self.field_name(name);
        writeln!(self.w, "{:X?}", value).unwrap();
    }

    fn field_string<T: fmt::UpperHex>(&mut self, name: &str, value: T, s: Option<&[u8]>) {
        if let Some(s) = s {
            self.field_name(name);
            self.print_string(s);
            writeln!(self.w, " (0x{:X})", value).unwrap();
        } else {
            self.field_hex(name, value);
        }
    }

    fn field_inline_string(&mut self, name: &str, s: &[u8]) {
        self.field_name(name);
        self.print_string(s);
        writeln!(self.w).unwrap();
    }

    fn field_enum<T: Eq + fmt::UpperHex>(&mut self, name: &str, value: T, flags: &[Flag<T>]) {
        for flag in flags {
            if value == flag.value {
                self.field_name(name);
                writeln!(self.w, "{} (0x{:X})", flag.name, value).unwrap();
                return;
            }
        }
        self.field_hex(name, value);
    }

    fn field_enums<T: Eq + fmt::UpperHex>(&mut self, name: &str, value: T, enums: &[&[Flag<T>]]) {
        for flags in enums {
            for flag in *flags {
                if value == flag.value {
                    self.field_name(name);
                    writeln!(self.w, "{} (0x{:X})", flag.name, value).unwrap();
                    return;
                }
            }
        }
        self.field_hex(name, value);
    }

    fn flags<T: Into<u64>, U: Copy + Into<u64>>(&mut self, value: T, mask: U, flags: &[Flag<U>]) {
        let value = value.into();
        let mask = mask.into();
        self.indent(|p| {
            if mask != 0 {
                for flag in flags {
                    if value & mask == flag.value.into() {
                        p.print_indent();
                        writeln!(p.w, "{} (0x{:X})", flag.name, flag.value.into()).unwrap();
                        return;
                    }
                }
                p.print_indent();
                writeln!(p.w, "<unknown> (0x{:X})", value & mask).unwrap();
            } else {
                for flag in flags {
                    if value & flag.value.into() == flag.value.into() {
                        p.print_indent();
                        writeln!(p.w, "{} (0x{:X})", flag.name, flag.value.into()).unwrap();
                    }
                }
                // TODO: display unknown flags (need to display all flags at once for this)
            }
        });
    }
}

struct Flag<T> {
    value: T,
    name: &'static str,
}

macro_rules! flags {
    ($($name:ident),+ $(,)?) => ( [ $(Flag { value: $name, name: stringify!($name), }),+ ] )
}

fn print_object(p: &mut Printer<impl Write>, data: &[u8]) {
    let kind = match object::FileKind::parse(data) {
        Ok(file) => file,
        Err(err) => {
            println!("Failed to parse file: {}", err);
            return;
        }
    };
    match kind {
        object::FileKind::Archive => print_archive(p, data),
        object::FileKind::Coff => pe::print_coff(p, data),
        object::FileKind::DyldCache => macho::print_dyld_cache(p, data),
        object::FileKind::Elf32 => elf::print_elf32(p, data),
        object::FileKind::Elf64 => elf::print_elf64(p, data),
        object::FileKind::MachO32 => macho::print_macho32(p, data, 0),
        object::FileKind::MachO64 => macho::print_macho64(p, data, 0),
        object::FileKind::MachOFat32 => macho::print_macho_fat32(p, data),
        object::FileKind::MachOFat64 => macho::print_macho_fat64(p, data),
        object::FileKind::Pe32 => pe::print_pe32(p, data),
        object::FileKind::Pe64 => pe::print_pe64(p, data),
        // TODO
        _ => {}
    }
}

fn print_object_at(p: &mut Printer<impl Write>, data: &[u8], offset: u64) {
    let kind = match object::FileKind::parse_at(data, offset) {
        Ok(file) => file,
        Err(err) => {
            println!("Failed to parse file: {}", err);
            return;
        }
    };
    match kind {
        object::FileKind::MachO32 => macho::print_macho32(p, data, offset),
        object::FileKind::MachO64 => macho::print_macho64(p, data, offset),
        // TODO
        _ => {}
    }
}

fn print_archive(p: &mut Printer<impl Write>, data: &[u8]) {
    if let Ok(archive) = ArchiveFile::parse(data) {
        p.field("Format", format!("Archive ({:?})", archive.kind()));
        for member in archive.members() {
            if let Ok(member) = member {
                p.blank();
                p.field("Member", String::from_utf8_lossy(member.name()));
                if let Ok(data) = member.data(data) {
                    print_object(p, data);
                }
            }
        }
    }
}

mod elf {
    use super::*;
    use object::elf::*;
    use object::read::elf::*;

    pub(super) fn print_elf32(p: &mut Printer<impl Write>, data: &[u8]) {
        if let Ok(elf) = FileHeader32::<Endianness>::parse(data) {
            println!("Format: ELF 32-bit");
            print_elf(p, elf, data);
        }
    }

    pub(super) fn print_elf64(p: &mut Printer<impl Write>, data: &[u8]) {
        if let Ok(elf) = FileHeader64::<Endianness>::parse(data) {
            println!("Format: ELF 64-bit");
            print_elf(p, elf, data);
        }
    }

    fn print_elf<Elf: FileHeader<Endian = Endianness>>(
        p: &mut Printer<impl Write>,
        elf: &Elf,
        data: &[u8],
    ) {
        if let Ok(endian) = elf.endian() {
            print_file_header(p, endian, elf);
            if let Ok(segments) = elf.program_headers(endian, data) {
                print_program_headers(p, endian, data, elf, segments);
            }
            if let Ok(sections) = elf.sections(endian, data) {
                print_section_headers(p, endian, data, elf, &sections);
            }
        }
    }

    fn print_file_header<Elf: FileHeader>(
        p: &mut Printer<impl Write>,
        endian: Elf::Endian,
        elf: &Elf,
    ) {
        p.group("FileHeader", |p| {
            p.group("Ident", |p| print_ident(p, elf.e_ident()));
            p.field_enum("Type", elf.e_type(endian), &FLAGS_ET);
            p.field_enum("Machine", elf.e_machine(endian), &FLAGS_EM);
            let version = elf.e_version(endian);
            if version < 256 {
                p.field_enum("Version", version as u8, &FLAGS_EV);
            } else {
                p.field_hex("Version", version);
            }
            p.field_enum("Type", elf.e_type(endian), &FLAGS_ET);
            p.field_hex("Entry", elf.e_entry(endian).into());
            p.field_hex("ProgramHeaderOffset", elf.e_phoff(endian).into());
            p.field_hex("SectionHeaderOffset", elf.e_shoff(endian).into());
            let flags = elf.e_flags(endian);
            p.field_hex("Flags", flags);
            match elf.e_machine(endian) {
                EM_SPARC => p.flags(flags, 0, &FLAGS_EF_SPARC),
                EM_SPARCV9 => p.flags(flags, 0, &FLAGS_EF_SPARCV9),
                EM_MIPS => {
                    p.flags(flags, 0, &FLAGS_EF_MIPS);
                    p.flags(flags, EF_MIPS_ARCH, &FLAGS_EF_MIPS_ARCH);
                }
                EM_PARISC => {
                    p.flags(flags, 0, &FLAGS_EF_PARISC);
                    p.flags(flags, EF_PARISC_ARCH, &FLAGS_EF_PARISC_ARCH);
                }
                EM_ALPHA => p.flags(flags, 0, &FLAGS_EF_ALPHA),
                EM_PPC => p.flags(flags, 0, &FLAGS_EF_PPC),
                EM_PPC64 => p.flags(flags, 0, &FLAGS_EF_PPC64),
                EM_ARM => {
                    p.flags(flags, 0, &FLAGS_EF_ARM);
                    p.flags(flags, EF_ARM_EABIMASK, &FLAGS_EF_ARM_EABI);
                }
                EM_CSKY => p.flags(flags, EF_CSKY_ABIMASK, &FLAGS_EF_CSKY_ABI),
                EM_IA_64 => p.flags(flags, 0, &FLAGS_EF_IA_64),
                EM_SH => p.flags(flags, EF_SH_MACH_MASK, &FLAGS_EF_SH_MACH),
                EM_S390 => p.flags(flags, 0, &FLAGS_EF_S390),
                EM_RISCV => {
                    p.flags(flags, 0, &FLAGS_EF_RISCV);
                    p.flags(flags, EF_RISCV_FLOAT_ABI, &FLAGS_EF_RISCV_FLOAT_ABI);
                }
                _ => {}
            };
            p.field_hex("HeaderSize", elf.e_ehsize(endian));
            p.field_hex("ProgramHeaderEntrySize", elf.e_phentsize(endian));
            p.field("ProgramHeaderCount", elf.e_phnum(endian));
            p.field_hex("SectionHeaderEntrySize", elf.e_shentsize(endian));
            p.field("SectionHeaderCount", elf.e_shnum(endian));
            p.field("SectionHeaderStringTableIndex", elf.e_shstrndx(endian));
        });
    }

    fn print_ident(p: &mut Printer<impl Write>, ident: &Ident) {
        p.field("Magic", format!("{:X?}", ident.magic));
        p.field_enum("Class", ident.class, &FLAGS_EI_CLASS);
        p.field_enum("Data", ident.data, &FLAGS_EI_DATA);
        p.field_enum("Version", ident.version, &FLAGS_EV);
        p.field_enum("OsAbi", ident.os_abi, &FLAGS_EI_OSABI);
        p.field_hex("AbiVersion", ident.abi_version);
        p.field("Unused", format!("{:X?}", ident.padding));
    }

    fn print_program_headers<Elf: FileHeader>(
        p: &mut Printer<impl Write>,
        endian: Elf::Endian,
        data: &[u8],
        elf: &Elf,
        segments: &[Elf::ProgramHeader],
    ) {
        for segment in segments {
            p.group("ProgramHeader", |p| {
                let proc = match elf.e_machine(endian) {
                    EM_MIPS => FLAGS_PT_MIPS,
                    EM_PARISC => FLAGS_PT_PARISC,
                    EM_ARM => FLAGS_PT_ARM,
                    EM_IA_64 => FLAGS_PT_IA_64,
                    _ => &[],
                };
                let os = match elf.e_ident().os_abi {
                    ELFOSABI_HPUX => FLAGS_PT_HP,
                    _ => &[],
                };
                p.field_enums("Type", segment.p_type(endian), &[FLAGS_PT, proc, os]);

                p.field_hex("Offset", segment.p_offset(endian).into());
                p.field_hex("VirtualAddress", segment.p_vaddr(endian).into());
                p.field_hex("PhysicalAddress", segment.p_paddr(endian).into());
                p.field_hex("FileSize", segment.p_filesz(endian).into());
                p.field_hex("MemorySize", segment.p_memsz(endian).into());

                let flags = segment.p_flags(endian);
                p.field_hex("Flags", flags);
                p.flags(flags, 0, FLAGS_PF);
                match elf.e_ident().os_abi {
                    ELFOSABI_HPUX => p.flags(flags, 0, FLAGS_PF_HP),
                    _ => {}
                };
                match elf.e_machine(endian) {
                    EM_MIPS => p.flags(flags, 0, FLAGS_PF_MIPS),
                    EM_PARISC => p.flags(flags, 0, FLAGS_PF_PARISC),
                    EM_ARM => p.flags(flags, 0, FLAGS_PF_ARM),
                    EM_IA_64 => p.flags(flags, 0, FLAGS_PF_IA_64),
                    _ => {}
                };

                p.field_hex("Align", segment.p_align(endian).into());

                match segment.p_type(endian) {
                    PT_NOTE => print_segment_notes(p, endian, data, elf, segment),
                    PT_DYNAMIC => print_segment_dynamic(p, endian, data, elf, segments, segment),
                    // TODO:
                    //PT_INTERP =>
                    //PT_SHLIB =>
                    //PT_PHDR =>
                    //PT_TLS =>
                    //PT_GNU_EH_FRAME =>
                    //PT_GNU_STACK =>
                    //PT_GNU_RELRO =>
                    _ => {}
                }
            });
        }
    }

    fn print_segment_notes<Elf: FileHeader>(
        p: &mut Printer<impl Write>,
        endian: Elf::Endian,
        data: &[u8],
        _elf: &Elf,
        segment: &Elf::ProgramHeader,
    ) {
        if let Ok(Some(notes)) = segment.notes(endian, data) {
            print_notes(p, endian, notes);
        }
    }

    fn print_segment_dynamic<Elf: FileHeader>(
        p: &mut Printer<impl Write>,
        endian: Elf::Endian,
        data: &[u8],
        elf: &Elf,
        segments: &[Elf::ProgramHeader],
        segment: &Elf::ProgramHeader,
    ) {
        if let Ok(Some(dynamic)) = segment.dynamic(endian, data) {
            // TODO: add a helper API for this and the other mandatory tags?
            let mut strtab = 0;
            let mut strsz = 0;
            for d in dynamic {
                let tag = d.d_tag(endian).into();
                if tag == DT_STRTAB.into() {
                    strtab = d.d_val(endian).into();
                } else if tag == DT_STRSZ.into() {
                    strsz = d.d_val(endian).into();
                }
            }
            let mut dynstr = object::StringTable::default();
            for s in segments {
                if let Ok(Some(data)) = s.data_range(endian, data, strtab, strsz) {
                    dynstr = object::StringTable::new(data);
                    break;
                }
            }

            let proc = match elf.e_machine(endian) {
                EM_SPARC => FLAGS_DT_SPARC,
                EM_MIPS => FLAGS_DT_MIPS,
                EM_ALPHA => FLAGS_DT_ALPHA,
                EM_PPC => FLAGS_DT_PPC,
                EM_PPC64 => FLAGS_DT_PPC64,
                EM_IA_64 => FLAGS_DT_IA_64,
                EM_ALTERA_NIOS2 => FLAGS_DT_NIOS2,
                _ => &[],
            };
            for d in dynamic {
                let tag = d.d_tag(endian).into();
                let val = d.d_val(endian).into();
                p.group("Dynamic", |p| {
                    if let Ok(tag) = tag.try_into() {
                        p.field_enums("Tag", tag, &[FLAGS_DT, proc]);
                        if tag == DT_NEEDED {
                            p.field_string(
                                "Value",
                                val,
                                val.try_into().ok().and_then(|val| dynstr.get(val).ok()),
                            );
                        } else {
                            p.field_hex("Value", val);
                            if tag == DT_FLAGS {
                                p.flags(val, 0, FLAGS_DF);
                            } else if tag == DT_FLAGS_1 {
                                p.flags(val, 0, FLAGS_DF_1);
                            }
                        }
                    } else {
                        p.field_hex("Tag", tag);
                        p.field_hex("Value", val);
                    }
                });
                if tag == DT_NULL.into() {
                    break;
                }
            }
        }
    }

    fn print_section_headers<Elf: FileHeader>(
        p: &mut Printer<impl Write>,
        endian: Elf::Endian,
        data: &[u8],
        elf: &Elf,
        sections: &SectionTable<Elf>,
    ) {
        for (index, section) in sections.iter().enumerate() {
            p.group("SectionHeader", |p| {
                p.field("Index", index);
                p.field_string(
                    "Name",
                    section.sh_name(endian),
                    sections.section_name(endian, section).ok(),
                );

                let proc = match elf.e_machine(endian) {
                    EM_MIPS => FLAGS_SHT_MIPS,
                    EM_PARISC => FLAGS_SHT_PARISC,
                    EM_ALPHA => FLAGS_SHT_ALPHA,
                    EM_ARM => FLAGS_SHT_ARM,
                    EM_CSKY => FLAGS_SHT_CSKY,
                    EM_IA_64 => FLAGS_SHT_IA_64,
                    EM_X86_64 => FLAGS_SHT_X86_64,
                    _ => &[],
                };
                p.field_enums("Type", section.sh_type(endian), &[FLAGS_SHT, proc]);

                let flags = section.sh_flags(endian).into();
                p.field_hex("Flags", flags);
                p.flags(flags, 0, FLAGS_SHF);
                match elf.e_machine(endian) {
                    EM_MIPS => p.flags(flags, 0, FLAGS_SHF_MIPS),
                    EM_PARISC => p.flags(flags, 0, FLAGS_SHF_PARISC),
                    EM_ALPHA => p.flags(flags, 0, FLAGS_SHF_ALPHA),
                    EM_ARM => p.flags(flags, 0, FLAGS_SHF_ARM),
                    EM_IA_64 => p.flags(flags, 0, FLAGS_SHF_IA_64),
                    _ => {}
                }

                p.field_hex("Address", section.sh_addr(endian).into());
                p.field_hex("Offset", section.sh_offset(endian).into());
                p.field_hex("Size", section.sh_size(endian).into());
                p.field("Link", section.sh_link(endian));
                p.field("Info", section.sh_info(endian));
                p.field_hex("AddressAlign", section.sh_addralign(endian).into());
                p.field_hex("EntrySize", section.sh_entsize(endian).into());

                match section.sh_type(endian) {
                    SHT_SYMTAB | SHT_DYNSYM => {
                        print_section_symbols(p, endian, data, elf, sections, index, section)
                    }
                    SHT_REL => print_section_rel(p, endian, data, elf, sections, section),
                    SHT_RELA => print_section_rela(p, endian, data, elf, sections, section),
                    SHT_NOTE => print_section_notes(p, endian, data, elf, section),
                    SHT_GROUP => print_section_group(p, endian, data, elf, sections, section),
                    // TODO:
                    //SHT_HASH =>
                    //SHT_DYNAMIC =>
                    //SHT_SHLIB =>
                    //SHT_INIT_ARRAY =>
                    //SHT_FINI_ARRAY =>
                    //SHT_PREINIT_ARRAY =>
                    _ => {}
                }
            });
        }
    }

    fn print_section_symbols<Elf: FileHeader>(
        p: &mut Printer<impl Write>,
        endian: Elf::Endian,
        data: &[u8],
        elf: &Elf,
        sections: &SectionTable<Elf>,
        section_index: usize,
        section: &Elf::SectionHeader,
    ) {
        if let Ok(Some(symbols)) = section.symbols(endian, data, sections, section_index) {
            let os_stt = match elf.e_ident().os_abi {
                ELFOSABI_GNU => FLAGS_STT_GNU,
                ELFOSABI_HPUX => FLAGS_STT_HP,
                _ => &[],
            };
            let proc_stt = match elf.e_machine(endian) {
                EM_SPARC => FLAGS_STT_SPARC,
                EM_PARISC => FLAGS_STT_PARISC,
                EM_ARM => FLAGS_STT_ARM,
                _ => &[],
            };
            let os_stb = match elf.e_ident().os_abi {
                ELFOSABI_GNU => FLAGS_STB_GNU,
                _ => &[],
            };
            let proc_stb = match elf.e_machine(endian) {
                EM_MIPS => FLAGS_STB_MIPS,
                _ => &[],
            };
            let proc_shn = match elf.e_machine(endian) {
                EM_MIPS => FLAGS_SHN_MIPS,
                EM_PARISC => FLAGS_SHN_PARISC,
                _ => &[],
            };
            for (index, symbol) in symbols.iter().enumerate() {
                p.group("Symbol", |p| {
                    p.field("Index", index);
                    p.field_string(
                        "Name",
                        symbol.st_name(endian),
                        symbol.name(endian, symbols.strings()).ok(),
                    );
                    p.field_hex("Value", symbol.st_value(endian).into());
                    p.field_hex("Size", symbol.st_size(endian).into());
                    p.field_enums("Type", symbol.st_type(), &[FLAGS_STT, os_stt, proc_stt]);
                    p.field_enums("Bind", symbol.st_bind(), &[FLAGS_STB, os_stb, proc_stb]);

                    let other = symbol.st_other();
                    if other & !0x3 == 0 {
                        p.field_enum("Other", other, FLAGS_STV);
                    } else {
                        p.field_hex("Other", other);
                        p.flags(other, 0x3, FLAGS_STV);
                        match elf.e_machine(endian) {
                            EM_MIPS => p.flags(other, 0, FLAGS_STO_MIPS),
                            EM_ALPHA => p.flags(other, 0, FLAGS_STO_ALPHA),
                            EM_PPC64 => p.field_hex(
                                "Local",
                                (other & STO_PPC64_LOCAL_MASK) >> STO_PPC64_LOCAL_BIT,
                            ),
                            _ => {}
                        }
                    }

                    let shndx = symbol.st_shndx(endian);
                    if shndx == SHN_UNDEF || shndx >= SHN_LORESERVE {
                        p.field_enums("SectionIndex", shndx, &[FLAGS_SHN, proc_shn]);
                    } else {
                        p.field("SectionIndex", shndx);
                    }
                    if let Some(shndx) = symbols.shndx(index) {
                        p.field("ExtendedSectionIndex", shndx);
                    }
                });
            }
        }
    }

    fn print_section_rel<Elf: FileHeader>(
        p: &mut Printer<impl Write>,
        endian: Elf::Endian,
        data: &[u8],
        elf: &Elf,
        sections: &SectionTable<Elf>,
        section: &Elf::SectionHeader,
    ) {
        if let Ok(Some(relocations)) = section.rel(endian, data) {
            let symbols = section.relocation_symbols(endian, data, sections).ok();
            let proc = rel_flag_type(endian, elf);
            for relocation in relocations {
                p.group("Relocation", |p| {
                    p.field_hex("Offset", relocation.r_offset(endian).into());
                    p.field_enum("Type", relocation.r_type(endian), proc);
                    let sym = relocation.r_sym(endian);
                    p.field_string("Symbol", sym, rel_symbol(endian, symbols, sym as usize));
                });
            }
        }
    }

    fn print_section_rela<Elf: FileHeader>(
        p: &mut Printer<impl Write>,
        endian: Elf::Endian,
        data: &[u8],
        elf: &Elf,
        sections: &SectionTable<Elf>,
        section: &Elf::SectionHeader,
    ) {
        if let Ok(Some(relocations)) = section.rela(endian, data) {
            let symbols = section.relocation_symbols(endian, data, sections).ok();
            let proc = rel_flag_type(endian, elf);
            for relocation in relocations {
                p.group("Relocation", |p| {
                    p.field_hex("Offset", relocation.r_offset(endian).into());
                    p.field_enum(
                        "Type",
                        relocation.r_type(endian, elf.is_mips64el(endian)),
                        proc,
                    );
                    let sym = relocation.r_sym(endian, elf.is_mips64el(endian));
                    p.field_string("Symbol", sym, rel_symbol(endian, symbols, sym as usize));
                    let addend = relocation.r_addend(endian).into() as u64;
                    if addend != 0 {
                        p.field_hex("Addend", addend);
                    }
                });
            }
        }
    }

    fn rel_symbol<'data, Elf: FileHeader>(
        endian: Elf::Endian,
        symbols: Option<SymbolTable<'data, Elf>>,
        sym: usize,
    ) -> Option<&'data [u8]> {
        let symbols = symbols?;
        let symbol = symbols.symbol(sym as usize).ok()?;
        symbol.name(endian, symbols.strings()).ok()
    }

    fn rel_flag_type<Elf: FileHeader>(endian: Elf::Endian, elf: &Elf) -> &'static [Flag<u32>] {
        match elf.e_machine(endian) {
            EM_68K => FLAGS_R_68K,
            EM_386 => FLAGS_R_386,
            EM_SPARC => FLAGS_R_SPARC,
            EM_MIPS => FLAGS_R_MIPS,
            EM_PARISC => FLAGS_R_PARISC,
            EM_ALPHA => FLAGS_R_ALPHA,
            EM_PPC => FLAGS_R_PPC,
            EM_PPC64 => FLAGS_R_PPC64,
            EM_AARCH64 => FLAGS_R_AARCH64,
            EM_ARM => FLAGS_R_ARM,
            EM_CSKY => FLAGS_R_CKCORE,
            EM_IA_64 => FLAGS_R_IA64,
            EM_SH => FLAGS_R_SH,
            EM_S390 => FLAGS_R_390,
            EM_CRIS => FLAGS_R_CRIS,
            EM_X86_64 => FLAGS_R_X86_64,
            EM_MN10300 => FLAGS_R_MN10300,
            EM_M32R => FLAGS_R_M32R,
            EM_MICROBLAZE => FLAGS_R_MICROBLAZE,
            EM_ALTERA_NIOS2 => FLAGS_R_NIOS2,
            EM_TILEPRO => FLAGS_R_TILEPRO,
            EM_TILEGX => FLAGS_R_TILEGX,
            EM_RISCV => FLAGS_R_RISCV,
            EM_BPF => FLAGS_R_BPF,
            EM_METAG => FLAGS_R_METAG,
            EM_NDS32 => FLAGS_R_NDS32,
            _ => &[],
        }
    }

    fn print_section_notes<Elf: FileHeader>(
        p: &mut Printer<impl Write>,
        endian: Elf::Endian,
        data: &[u8],
        _elf: &Elf,
        section: &Elf::SectionHeader,
    ) {
        if let Ok(Some(notes)) = section.notes(endian, data) {
            print_notes(p, endian, notes);
        }
    }

    fn print_section_group<Elf: FileHeader>(
        p: &mut Printer<impl Write>,
        endian: Elf::Endian,
        data: &[u8],
        _elf: &Elf,
        sections: &SectionTable<Elf>,
        section: &Elf::SectionHeader,
    ) {
        if let Ok(Some((flag, members))) = section.group(endian, data) {
            p.field_enum("GroupFlag", flag, FLAGS_GRP);
            p.group("GroupSections", |p| {
                for member in members {
                    let index = member.get(endian);
                    p.print_indent();
                    if let Ok(section) = sections.section(index as usize) {
                        if let Ok(name) = sections.section_name(endian, section) {
                            p.print_string(name);
                            writeln!(p.w, " ({})", index).unwrap();
                        } else {
                            writeln!(p.w, "{}", index).unwrap();
                        }
                    } else {
                        writeln!(p.w, "{}", index).unwrap();
                    }
                }
            });
        }
    }

    fn print_notes<Elf: FileHeader>(
        p: &mut Printer<impl Write>,
        endian: Elf::Endian,
        mut notes: NoteIterator<Elf>,
    ) {
        while let Ok(Some(note)) = notes.next() {
            p.group("Note", |p| {
                let name = note.name();
                p.field_string("Name", note.n_namesz(endian), Some(name));
                let flags = if name == ELF_NOTE_CORE || name == ELF_NOTE_LINUX {
                    FLAGS_NT_CORE
                } else if name == ELF_NOTE_SOLARIS {
                    FLAGS_NT_SOLARIS
                } else if name == ELF_NOTE_GNU {
                    FLAGS_NT_GNU
                } else {
                    // TODO: NT_VERSION
                    &[]
                };
                p.field_enum("Type", note.n_type(endian), flags);
                // TODO: interpret desc
                p.field_bytes("Desc", note.desc());
            });
        }
    }

    static FLAGS_EI_CLASS: &[Flag<u8>] = &flags!(ELFCLASSNONE, ELFCLASS32, ELFCLASS64);
    static FLAGS_EI_DATA: &[Flag<u8>] = &flags!(ELFDATANONE, ELFDATA2LSB, ELFDATA2MSB);
    static FLAGS_EV: &[Flag<u8>] = &flags!(EV_NONE, EV_CURRENT);
    static FLAGS_EI_OSABI: &[Flag<u8>] = &flags!(
        ELFOSABI_SYSV,
        ELFOSABI_HPUX,
        ELFOSABI_NETBSD,
        ELFOSABI_GNU,
        ELFOSABI_SOLARIS,
        ELFOSABI_AIX,
        ELFOSABI_IRIX,
        ELFOSABI_FREEBSD,
        ELFOSABI_TRU64,
        ELFOSABI_MODESTO,
        ELFOSABI_OPENBSD,
        ELFOSABI_ARM_AEABI,
        ELFOSABI_ARM,
        ELFOSABI_STANDALONE,
    );
    static FLAGS_ET: &[Flag<u16>] = &flags!(ET_NONE, ET_REL, ET_EXEC, ET_DYN, ET_CORE);
    static FLAGS_EM: &[Flag<u16>] = &flags!(
        EM_NONE,
        EM_M32,
        EM_SPARC,
        EM_386,
        EM_68K,
        EM_88K,
        EM_IAMCU,
        EM_860,
        EM_MIPS,
        EM_S370,
        EM_MIPS_RS3_LE,
        EM_PARISC,
        EM_VPP500,
        EM_SPARC32PLUS,
        EM_960,
        EM_PPC,
        EM_PPC64,
        EM_S390,
        EM_SPU,
        EM_V800,
        EM_FR20,
        EM_RH32,
        EM_RCE,
        EM_ARM,
        EM_FAKE_ALPHA,
        EM_SH,
        EM_SPARCV9,
        EM_TRICORE,
        EM_ARC,
        EM_H8_300,
        EM_H8_300H,
        EM_H8S,
        EM_H8_500,
        EM_IA_64,
        EM_MIPS_X,
        EM_COLDFIRE,
        EM_68HC12,
        EM_MMA,
        EM_PCP,
        EM_NCPU,
        EM_NDR1,
        EM_STARCORE,
        EM_ME16,
        EM_ST100,
        EM_TINYJ,
        EM_X86_64,
        EM_PDSP,
        EM_PDP10,
        EM_PDP11,
        EM_FX66,
        EM_ST9PLUS,
        EM_ST7,
        EM_68HC16,
        EM_68HC11,
        EM_68HC08,
        EM_68HC05,
        EM_SVX,
        EM_ST19,
        EM_VAX,
        EM_CRIS,
        EM_JAVELIN,
        EM_FIREPATH,
        EM_ZSP,
        EM_MMIX,
        EM_HUANY,
        EM_PRISM,
        EM_AVR,
        EM_FR30,
        EM_D10V,
        EM_D30V,
        EM_V850,
        EM_M32R,
        EM_MN10300,
        EM_MN10200,
        EM_PJ,
        EM_OPENRISC,
        EM_ARC_COMPACT,
        EM_XTENSA,
        EM_VIDEOCORE,
        EM_TMM_GPP,
        EM_NS32K,
        EM_TPC,
        EM_SNP1K,
        EM_ST200,
        EM_IP2K,
        EM_MAX,
        EM_CR,
        EM_F2MC16,
        EM_MSP430,
        EM_BLACKFIN,
        EM_SE_C33,
        EM_SEP,
        EM_ARCA,
        EM_UNICORE,
        EM_EXCESS,
        EM_DXP,
        EM_ALTERA_NIOS2,
        EM_CRX,
        EM_XGATE,
        EM_C166,
        EM_M16C,
        EM_DSPIC30F,
        EM_CE,
        EM_M32C,
        EM_TSK3000,
        EM_RS08,
        EM_SHARC,
        EM_ECOG2,
        EM_SCORE7,
        EM_DSP24,
        EM_VIDEOCORE3,
        EM_LATTICEMICO32,
        EM_SE_C17,
        EM_TI_C6000,
        EM_TI_C2000,
        EM_TI_C5500,
        EM_TI_ARP32,
        EM_TI_PRU,
        EM_MMDSP_PLUS,
        EM_CYPRESS_M8C,
        EM_R32C,
        EM_TRIMEDIA,
        EM_HEXAGON,
        EM_8051,
        EM_STXP7X,
        EM_NDS32,
        EM_ECOG1X,
        EM_MAXQ30,
        EM_XIMO16,
        EM_MANIK,
        EM_CRAYNV2,
        EM_RX,
        EM_METAG,
        EM_MCST_ELBRUS,
        EM_ECOG16,
        EM_CR16,
        EM_ETPU,
        EM_SLE9X,
        EM_L10M,
        EM_K10M,
        EM_AARCH64,
        EM_AVR32,
        EM_STM8,
        EM_TILE64,
        EM_TILEPRO,
        EM_MICROBLAZE,
        EM_CUDA,
        EM_TILEGX,
        EM_CLOUDSHIELD,
        EM_COREA_1ST,
        EM_COREA_2ND,
        EM_ARC_COMPACT2,
        EM_OPEN8,
        EM_RL78,
        EM_VIDEOCORE5,
        EM_78KOR,
        EM_56800EX,
        EM_BA1,
        EM_BA2,
        EM_XCORE,
        EM_MCHP_PIC,
        EM_KM32,
        EM_KMX32,
        EM_EMX16,
        EM_EMX8,
        EM_KVARC,
        EM_CDP,
        EM_COGE,
        EM_COOL,
        EM_NORC,
        EM_CSR_KALIMBA,
        EM_Z80,
        EM_VISIUM,
        EM_FT32,
        EM_MOXIE,
        EM_AMDGPU,
        EM_RISCV,
        EM_BPF,
        EM_CSKY,
        EM_ALPHA,
    );
    static FLAGS_EF_SPARC: &[Flag<u32>] = &flags!(
        EF_SPARC_LEDATA,
        EF_SPARC_EXT_MASK,
        EF_SPARC_32PLUS,
        EF_SPARC_SUN_US1,
        EF_SPARC_HAL_R1,
        EF_SPARC_SUN_US3,
    );
    static FLAGS_EF_SPARCV9: &[Flag<u32>] = &flags!(
        EF_SPARCV9_MM,
        EF_SPARCV9_TSO,
        EF_SPARCV9_PSO,
        EF_SPARCV9_RMO,
    );
    static FLAGS_EF_MIPS: &[Flag<u32>] = &flags!(
        EF_MIPS_NOREORDER,
        EF_MIPS_PIC,
        EF_MIPS_CPIC,
        EF_MIPS_XGOT,
        EF_MIPS_64BIT_WHIRL,
        EF_MIPS_ABI2,
        EF_MIPS_ABI_ON32,
        EF_MIPS_FP64,
        EF_MIPS_NAN2008,
    );
    static FLAGS_EF_MIPS_ARCH: &[Flag<u32>] = &flags!(
        EF_MIPS_ARCH_1,
        EF_MIPS_ARCH_2,
        EF_MIPS_ARCH_3,
        EF_MIPS_ARCH_4,
        EF_MIPS_ARCH_5,
        EF_MIPS_ARCH_32,
        EF_MIPS_ARCH_64,
        EF_MIPS_ARCH_32R2,
        EF_MIPS_ARCH_64R2,
    );
    static FLAGS_EF_PARISC: &[Flag<u32>] = &flags!(
        EF_PARISC_TRAPNIL,
        EF_PARISC_EXT,
        EF_PARISC_LSB,
        EF_PARISC_WIDE,
        EF_PARISC_NO_KABP,
        EF_PARISC_LAZYSWAP,
    );
    static FLAGS_EF_PARISC_ARCH: &[Flag<u32>] =
        &flags!(EFA_PARISC_1_0, EFA_PARISC_1_1, EFA_PARISC_2_0);
    static FLAGS_EF_ALPHA: &[Flag<u32>] = &flags!(EF_ALPHA_32BIT, EF_ALPHA_CANRELAX);
    static FLAGS_EF_PPC: &[Flag<u32>] =
        &flags!(EF_PPC_EMB, EF_PPC_RELOCATABLE, EF_PPC_RELOCATABLE_LIB);
    static FLAGS_EF_PPC64: &[Flag<u32>] = &flags!(EF_PPC64_ABI);
    static FLAGS_EF_ARM: &[Flag<u32>] = &flags!(
        EF_ARM_RELEXEC,
        EF_ARM_HASENTRY,
        EF_ARM_INTERWORK,
        EF_ARM_APCS_26,
        EF_ARM_APCS_FLOAT,
        EF_ARM_PIC,
        EF_ARM_ALIGN8,
        EF_ARM_NEW_ABI,
        EF_ARM_OLD_ABI,
        EF_ARM_SOFT_FLOAT,
        EF_ARM_VFP_FLOAT,
        EF_ARM_MAVERICK_FLOAT,
        EF_ARM_BE8,
        EF_ARM_LE8,
    );
    static FLAGS_EF_ARM_EABI: &[Flag<u32>] = &flags!(
        EF_ARM_EABI_UNKNOWN,
        EF_ARM_EABI_VER1,
        EF_ARM_EABI_VER2,
        EF_ARM_EABI_VER3,
        EF_ARM_EABI_VER4,
        EF_ARM_EABI_VER5,
    );
    static FLAGS_EF_CSKY_ABI: &[Flag<u32>] = &flags!(EF_CSKY_ABIV1, EF_CSKY_ABIV2);
    static FLAGS_EF_IA_64: &[Flag<u32>] = &flags!(EF_IA_64_ABI64);
    static FLAGS_EF_SH_MACH: &[Flag<u32>] = &flags!(
        EF_SH_UNKNOWN,
        EF_SH1,
        EF_SH2,
        EF_SH3,
        EF_SH_DSP,
        EF_SH3_DSP,
        EF_SH4AL_DSP,
        EF_SH3E,
        EF_SH4,
        EF_SH2E,
        EF_SH4A,
        EF_SH2A,
        EF_SH4_NOFPU,
        EF_SH4A_NOFPU,
        EF_SH4_NOMMU_NOFPU,
        EF_SH2A_NOFPU,
        EF_SH3_NOMMU,
        EF_SH2A_SH4_NOFPU,
        EF_SH2A_SH3_NOFPU,
        EF_SH2A_SH4,
        EF_SH2A_SH3E,
    );
    static FLAGS_EF_S390: &[Flag<u32>] = &flags!(EF_S390_HIGH_GPRS);
    static FLAGS_EF_RISCV: &[Flag<u32>] = &flags!(EF_RISCV_RVC);
    static FLAGS_EF_RISCV_FLOAT_ABI: &[Flag<u32>] = &flags!(
        EF_RISCV_FLOAT_ABI_SOFT,
        EF_RISCV_FLOAT_ABI_SINGLE,
        EF_RISCV_FLOAT_ABI_DOUBLE,
        EF_RISCV_FLOAT_ABI_QUAD,
    );
    static FLAGS_PT: &[Flag<u32>] = &flags!(
        PT_NULL,
        PT_LOAD,
        PT_DYNAMIC,
        PT_INTERP,
        PT_NOTE,
        PT_SHLIB,
        PT_PHDR,
        PT_TLS,
        PT_LOOS,
        PT_GNU_EH_FRAME,
        PT_GNU_STACK,
        PT_GNU_RELRO,
    );
    static FLAGS_PT_HP: &[Flag<u32>] = &flags!(
        PT_HP_TLS,
        PT_HP_CORE_NONE,
        PT_HP_CORE_VERSION,
        PT_HP_CORE_KERNEL,
        PT_HP_CORE_COMM,
        PT_HP_CORE_PROC,
        PT_HP_CORE_LOADABLE,
        PT_HP_CORE_STACK,
        PT_HP_CORE_SHM,
        PT_HP_CORE_MMF,
        PT_HP_PARALLEL,
        PT_HP_FASTBIND,
        PT_HP_OPT_ANNOT,
        PT_HP_HSL_ANNOT,
        PT_HP_STACK,
    );
    static FLAGS_PT_MIPS: &[Flag<u32>] = &flags!(
        PT_MIPS_REGINFO,
        PT_MIPS_RTPROC,
        PT_MIPS_OPTIONS,
        PT_MIPS_ABIFLAGS,
    );
    static FLAGS_PT_PARISC: &[Flag<u32>] = &flags!(PT_PARISC_ARCHEXT, PT_PARISC_UNWIND);
    static FLAGS_PT_ARM: &[Flag<u32>] = &flags!(PT_ARM_EXIDX);
    static FLAGS_PT_IA_64: &[Flag<u32>] = &flags!(PT_IA_64_ARCHEXT, PT_IA_64_UNWIND);
    static FLAGS_PF: &[Flag<u32>] = &flags!(PF_X, PF_W, PF_R);
    static FLAGS_PF_HP: &[Flag<u32>] = &flags!(
        PF_HP_PAGE_SIZE,
        PF_HP_FAR_SHARED,
        PF_HP_NEAR_SHARED,
        PF_HP_CODE,
        PF_HP_MODIFY,
        PF_HP_LAZYSWAP,
        PF_HP_SBP,
    );
    static FLAGS_PF_MIPS: &[Flag<u32>] = &flags!(PF_MIPS_LOCAL);
    static FLAGS_PF_PARISC: &[Flag<u32>] = &flags!(PF_PARISC_SBP);
    static FLAGS_PF_ARM: &[Flag<u32>] = &flags!(PF_ARM_SB, PF_ARM_PI, PF_ARM_ABS);
    static FLAGS_PF_IA_64: &[Flag<u32>] = &flags!(PF_IA_64_NORECOV);
    static FLAGS_SHT: &[Flag<u32>] = &flags!(
        SHT_NULL,
        SHT_PROGBITS,
        SHT_SYMTAB,
        SHT_STRTAB,
        SHT_RELA,
        SHT_HASH,
        SHT_DYNAMIC,
        SHT_NOTE,
        SHT_NOBITS,
        SHT_REL,
        SHT_SHLIB,
        SHT_DYNSYM,
        SHT_INIT_ARRAY,
        SHT_FINI_ARRAY,
        SHT_PREINIT_ARRAY,
        SHT_GROUP,
        SHT_SYMTAB_SHNDX,
    );
    static FLAGS_SHT_MIPS: &[Flag<u32>] = &flags!(
        SHT_MIPS_LIBLIST,
        SHT_MIPS_MSYM,
        SHT_MIPS_CONFLICT,
        SHT_MIPS_GPTAB,
        SHT_MIPS_UCODE,
        SHT_MIPS_DEBUG,
        SHT_MIPS_REGINFO,
        SHT_MIPS_PACKAGE,
        SHT_MIPS_PACKSYM,
        SHT_MIPS_RELD,
        SHT_MIPS_IFACE,
        SHT_MIPS_CONTENT,
        SHT_MIPS_OPTIONS,
        SHT_MIPS_SHDR,
        SHT_MIPS_FDESC,
        SHT_MIPS_EXTSYM,
        SHT_MIPS_DENSE,
        SHT_MIPS_PDESC,
        SHT_MIPS_LOCSYM,
        SHT_MIPS_AUXSYM,
        SHT_MIPS_OPTSYM,
        SHT_MIPS_LOCSTR,
        SHT_MIPS_LINE,
        SHT_MIPS_RFDESC,
        SHT_MIPS_DELTASYM,
        SHT_MIPS_DELTAINST,
        SHT_MIPS_DELTACLASS,
        SHT_MIPS_DWARF,
        SHT_MIPS_DELTADECL,
        SHT_MIPS_SYMBOL_LIB,
        SHT_MIPS_EVENTS,
        SHT_MIPS_TRANSLATE,
        SHT_MIPS_PIXIE,
        SHT_MIPS_XLATE,
        SHT_MIPS_XLATE_DEBUG,
        SHT_MIPS_WHIRL,
        SHT_MIPS_EH_REGION,
        SHT_MIPS_XLATE_OLD,
        SHT_MIPS_PDR_EXCEPTION,
    );
    static FLAGS_SHT_PARISC: &[Flag<u32>] =
        &flags!(SHT_PARISC_EXT, SHT_PARISC_UNWIND, SHT_PARISC_DOC);
    static FLAGS_SHT_ALPHA: &[Flag<u32>] = &flags!(SHT_ALPHA_DEBUG, SHT_ALPHA_REGINFO);
    static FLAGS_SHT_ARM: &[Flag<u32>] =
        &flags!(SHT_ARM_EXIDX, SHT_ARM_PREEMPTMAP, SHT_ARM_ATTRIBUTES);
    static FLAGS_SHT_CSKY: &[Flag<u32>] = &flags!(SHT_CSKY_ATTRIBUTES);
    static FLAGS_SHT_IA_64: &[Flag<u32>] = &flags!(SHT_IA_64_EXT, SHT_IA_64_UNWIND);
    static FLAGS_SHT_X86_64: &[Flag<u32>] = &flags!(SHT_X86_64_UNWIND);
    static FLAGS_SHF: &[Flag<u32>] = &flags!(
        SHF_WRITE,
        SHF_ALLOC,
        SHF_EXECINSTR,
        SHF_MERGE,
        SHF_STRINGS,
        SHF_INFO_LINK,
        SHF_LINK_ORDER,
        SHF_OS_NONCONFORMING,
        SHF_GROUP,
        SHF_TLS,
        SHF_COMPRESSED,
    );
    static FLAGS_SHF_MIPS: &[Flag<u32>] = &flags!(
        SHF_MIPS_GPREL,
        SHF_MIPS_MERGE,
        SHF_MIPS_ADDR,
        SHF_MIPS_STRINGS,
        SHF_MIPS_NOSTRIP,
        SHF_MIPS_LOCAL,
        SHF_MIPS_NAMES,
        SHF_MIPS_NODUPE,
    );
    static FLAGS_SHF_PARISC: &[Flag<u32>] =
        &flags!(SHF_PARISC_SHORT, SHF_PARISC_HUGE, SHF_PARISC_SBP);
    static FLAGS_SHF_ALPHA: &[Flag<u32>] = &flags!(SHF_ALPHA_GPREL);
    static FLAGS_SHF_ARM: &[Flag<u32>] = &flags!(SHF_ARM_ENTRYSECT, SHF_ARM_COMDEF);
    static FLAGS_SHF_IA_64: &[Flag<u32>] = &flags!(SHF_IA_64_SHORT, SHF_IA_64_NORECOV);
    static FLAGS_STT: &[Flag<u8>] = &flags!(
        STT_NOTYPE,
        STT_OBJECT,
        STT_FUNC,
        STT_SECTION,
        STT_FILE,
        STT_COMMON,
        STT_TLS,
    );
    static FLAGS_STT_GNU: &[Flag<u8>] = &flags!(STT_GNU_IFUNC);
    static FLAGS_STT_HP: &[Flag<u8>] = &flags!(STT_HP_OPAQUE, STT_HP_STUB);
    static FLAGS_STT_SPARC: &[Flag<u8>] = &flags!(STT_SPARC_REGISTER);
    static FLAGS_STT_PARISC: &[Flag<u8>] = &flags!(STT_PARISC_MILLICODE);
    static FLAGS_STT_ARM: &[Flag<u8>] = &flags!(STT_ARM_TFUNC, STT_ARM_16BIT);
    static FLAGS_STB: &[Flag<u8>] = &flags!(STB_LOCAL, STB_GLOBAL, STB_WEAK);
    static FLAGS_STB_GNU: &[Flag<u8>] = &flags!(STB_GNU_UNIQUE);
    static FLAGS_STB_MIPS: &[Flag<u8>] = &flags!(STB_MIPS_SPLIT_COMMON);
    static FLAGS_STV: &[Flag<u8>] = &flags!(STV_DEFAULT, STV_INTERNAL, STV_HIDDEN, STV_PROTECTED);
    static FLAGS_STO_MIPS: &[Flag<u8>] = &flags!(STO_MIPS_PLT);
    static FLAGS_STO_ALPHA: &[Flag<u8>] = &flags!(STO_ALPHA_NOPV, STO_ALPHA_STD_GPLOAD);
    static FLAGS_SHN: &[Flag<u16>] = &flags!(SHN_UNDEF, SHN_ABS, SHN_COMMON, SHN_XINDEX);
    static FLAGS_SHN_MIPS: &[Flag<u16>] = &flags!(
        SHN_MIPS_ACOMMON,
        SHN_MIPS_TEXT,
        SHN_MIPS_DATA,
        SHN_MIPS_SCOMMON,
        SHN_MIPS_SUNDEFINED,
    );
    static FLAGS_SHN_PARISC: &[Flag<u16>] = &flags!(SHN_PARISC_ANSI_COMMON, SHN_PARISC_HUGE_COMMON);
    static FLAGS_R_68K: &[Flag<u32>] = &flags!(
        R_68K_NONE,
        R_68K_32,
        R_68K_16,
        R_68K_8,
        R_68K_PC32,
        R_68K_PC16,
        R_68K_PC8,
        R_68K_GOT32,
        R_68K_GOT16,
        R_68K_GOT8,
        R_68K_GOT32O,
        R_68K_GOT16O,
        R_68K_GOT8O,
        R_68K_PLT32,
        R_68K_PLT16,
        R_68K_PLT8,
        R_68K_PLT32O,
        R_68K_PLT16O,
        R_68K_PLT8O,
        R_68K_COPY,
        R_68K_GLOB_DAT,
        R_68K_JMP_SLOT,
        R_68K_RELATIVE,
        R_68K_TLS_GD32,
        R_68K_TLS_GD16,
        R_68K_TLS_GD8,
        R_68K_TLS_LDM32,
        R_68K_TLS_LDM16,
        R_68K_TLS_LDM8,
        R_68K_TLS_LDO32,
        R_68K_TLS_LDO16,
        R_68K_TLS_LDO8,
        R_68K_TLS_IE32,
        R_68K_TLS_IE16,
        R_68K_TLS_IE8,
        R_68K_TLS_LE32,
        R_68K_TLS_LE16,
        R_68K_TLS_LE8,
        R_68K_TLS_DTPMOD32,
        R_68K_TLS_DTPREL32,
        R_68K_TLS_TPREL32,
    );
    static FLAGS_R_386: &[Flag<u32>] = &flags!(
        R_386_NONE,
        R_386_32,
        R_386_PC32,
        R_386_GOT32,
        R_386_PLT32,
        R_386_COPY,
        R_386_GLOB_DAT,
        R_386_JMP_SLOT,
        R_386_RELATIVE,
        R_386_GOTOFF,
        R_386_GOTPC,
        R_386_32PLT,
        R_386_TLS_TPOFF,
        R_386_TLS_IE,
        R_386_TLS_GOTIE,
        R_386_TLS_LE,
        R_386_TLS_GD,
        R_386_TLS_LDM,
        R_386_16,
        R_386_PC16,
        R_386_8,
        R_386_PC8,
        R_386_TLS_GD_32,
        R_386_TLS_GD_PUSH,
        R_386_TLS_GD_CALL,
        R_386_TLS_GD_POP,
        R_386_TLS_LDM_32,
        R_386_TLS_LDM_PUSH,
        R_386_TLS_LDM_CALL,
        R_386_TLS_LDM_POP,
        R_386_TLS_LDO_32,
        R_386_TLS_IE_32,
        R_386_TLS_LE_32,
        R_386_TLS_DTPMOD32,
        R_386_TLS_DTPOFF32,
        R_386_TLS_TPOFF32,
        R_386_SIZE32,
        R_386_TLS_GOTDESC,
        R_386_TLS_DESC_CALL,
        R_386_TLS_DESC,
        R_386_IRELATIVE,
        R_386_GOT32X,
    );
    static FLAGS_R_SPARC: &[Flag<u32>] = &flags!(
        R_SPARC_NONE,
        R_SPARC_8,
        R_SPARC_16,
        R_SPARC_32,
        R_SPARC_DISP8,
        R_SPARC_DISP16,
        R_SPARC_DISP32,
        R_SPARC_WDISP30,
        R_SPARC_WDISP22,
        R_SPARC_HI22,
        R_SPARC_22,
        R_SPARC_13,
        R_SPARC_LO10,
        R_SPARC_GOT10,
        R_SPARC_GOT13,
        R_SPARC_GOT22,
        R_SPARC_PC10,
        R_SPARC_PC22,
        R_SPARC_WPLT30,
        R_SPARC_COPY,
        R_SPARC_GLOB_DAT,
        R_SPARC_JMP_SLOT,
        R_SPARC_RELATIVE,
        R_SPARC_UA32,
        R_SPARC_PLT32,
        R_SPARC_HIPLT22,
        R_SPARC_LOPLT10,
        R_SPARC_PCPLT32,
        R_SPARC_PCPLT22,
        R_SPARC_PCPLT10,
        R_SPARC_10,
        R_SPARC_11,
        R_SPARC_64,
        R_SPARC_OLO10,
        R_SPARC_HH22,
        R_SPARC_HM10,
        R_SPARC_LM22,
        R_SPARC_PC_HH22,
        R_SPARC_PC_HM10,
        R_SPARC_PC_LM22,
        R_SPARC_WDISP16,
        R_SPARC_WDISP19,
        R_SPARC_GLOB_JMP,
        R_SPARC_7,
        R_SPARC_5,
        R_SPARC_6,
        R_SPARC_DISP64,
        R_SPARC_PLT64,
        R_SPARC_HIX22,
        R_SPARC_LOX10,
        R_SPARC_H44,
        R_SPARC_M44,
        R_SPARC_L44,
        R_SPARC_REGISTER,
        R_SPARC_UA64,
        R_SPARC_UA16,
        R_SPARC_TLS_GD_HI22,
        R_SPARC_TLS_GD_LO10,
        R_SPARC_TLS_GD_ADD,
        R_SPARC_TLS_GD_CALL,
        R_SPARC_TLS_LDM_HI22,
        R_SPARC_TLS_LDM_LO10,
        R_SPARC_TLS_LDM_ADD,
        R_SPARC_TLS_LDM_CALL,
        R_SPARC_TLS_LDO_HIX22,
        R_SPARC_TLS_LDO_LOX10,
        R_SPARC_TLS_LDO_ADD,
        R_SPARC_TLS_IE_HI22,
        R_SPARC_TLS_IE_LO10,
        R_SPARC_TLS_IE_LD,
        R_SPARC_TLS_IE_LDX,
        R_SPARC_TLS_IE_ADD,
        R_SPARC_TLS_LE_HIX22,
        R_SPARC_TLS_LE_LOX10,
        R_SPARC_TLS_DTPMOD32,
        R_SPARC_TLS_DTPMOD64,
        R_SPARC_TLS_DTPOFF32,
        R_SPARC_TLS_DTPOFF64,
        R_SPARC_TLS_TPOFF32,
        R_SPARC_TLS_TPOFF64,
        R_SPARC_GOTDATA_HIX22,
        R_SPARC_GOTDATA_LOX10,
        R_SPARC_GOTDATA_OP_HIX22,
        R_SPARC_GOTDATA_OP_LOX10,
        R_SPARC_GOTDATA_OP,
        R_SPARC_H34,
        R_SPARC_SIZE32,
        R_SPARC_SIZE64,
        R_SPARC_WDISP10,
        R_SPARC_JMP_IREL,
        R_SPARC_IRELATIVE,
        R_SPARC_GNU_VTINHERIT,
        R_SPARC_GNU_VTENTRY,
        R_SPARC_REV32,
    );
    static FLAGS_R_MIPS: &[Flag<u32>] = &flags!(
        R_MIPS_NONE,
        R_MIPS_16,
        R_MIPS_32,
        R_MIPS_REL32,
        R_MIPS_26,
        R_MIPS_HI16,
        R_MIPS_LO16,
        R_MIPS_GPREL16,
        R_MIPS_LITERAL,
        R_MIPS_GOT16,
        R_MIPS_PC16,
        R_MIPS_CALL16,
        R_MIPS_GPREL32,
        R_MIPS_SHIFT5,
        R_MIPS_SHIFT6,
        R_MIPS_64,
        R_MIPS_GOT_DISP,
        R_MIPS_GOT_PAGE,
        R_MIPS_GOT_OFST,
        R_MIPS_GOT_HI16,
        R_MIPS_GOT_LO16,
        R_MIPS_SUB,
        R_MIPS_INSERT_A,
        R_MIPS_INSERT_B,
        R_MIPS_DELETE,
        R_MIPS_HIGHER,
        R_MIPS_HIGHEST,
        R_MIPS_CALL_HI16,
        R_MIPS_CALL_LO16,
        R_MIPS_SCN_DISP,
        R_MIPS_REL16,
        R_MIPS_ADD_IMMEDIATE,
        R_MIPS_PJUMP,
        R_MIPS_RELGOT,
        R_MIPS_JALR,
        R_MIPS_TLS_DTPMOD32,
        R_MIPS_TLS_DTPREL32,
        R_MIPS_TLS_DTPMOD64,
        R_MIPS_TLS_DTPREL64,
        R_MIPS_TLS_GD,
        R_MIPS_TLS_LDM,
        R_MIPS_TLS_DTPREL_HI16,
        R_MIPS_TLS_DTPREL_LO16,
        R_MIPS_TLS_GOTTPREL,
        R_MIPS_TLS_TPREL32,
        R_MIPS_TLS_TPREL64,
        R_MIPS_TLS_TPREL_HI16,
        R_MIPS_TLS_TPREL_LO16,
        R_MIPS_GLOB_DAT,
        R_MIPS_COPY,
        R_MIPS_JUMP_SLOT,
    );
    static FLAGS_R_PARISC: &[Flag<u32>] = &flags!(
        R_PARISC_NONE,
        R_PARISC_DIR32,
        R_PARISC_DIR21L,
        R_PARISC_DIR17R,
        R_PARISC_DIR17F,
        R_PARISC_DIR14R,
        R_PARISC_PCREL32,
        R_PARISC_PCREL21L,
        R_PARISC_PCREL17R,
        R_PARISC_PCREL17F,
        R_PARISC_PCREL14R,
        R_PARISC_DPREL21L,
        R_PARISC_DPREL14R,
        R_PARISC_GPREL21L,
        R_PARISC_GPREL14R,
        R_PARISC_LTOFF21L,
        R_PARISC_LTOFF14R,
        R_PARISC_SECREL32,
        R_PARISC_SEGBASE,
        R_PARISC_SEGREL32,
        R_PARISC_PLTOFF21L,
        R_PARISC_PLTOFF14R,
        R_PARISC_LTOFF_FPTR32,
        R_PARISC_LTOFF_FPTR21L,
        R_PARISC_LTOFF_FPTR14R,
        R_PARISC_FPTR64,
        R_PARISC_PLABEL32,
        R_PARISC_PLABEL21L,
        R_PARISC_PLABEL14R,
        R_PARISC_PCREL64,
        R_PARISC_PCREL22F,
        R_PARISC_PCREL14WR,
        R_PARISC_PCREL14DR,
        R_PARISC_PCREL16F,
        R_PARISC_PCREL16WF,
        R_PARISC_PCREL16DF,
        R_PARISC_DIR64,
        R_PARISC_DIR14WR,
        R_PARISC_DIR14DR,
        R_PARISC_DIR16F,
        R_PARISC_DIR16WF,
        R_PARISC_DIR16DF,
        R_PARISC_GPREL64,
        R_PARISC_GPREL14WR,
        R_PARISC_GPREL14DR,
        R_PARISC_GPREL16F,
        R_PARISC_GPREL16WF,
        R_PARISC_GPREL16DF,
        R_PARISC_LTOFF64,
        R_PARISC_LTOFF14WR,
        R_PARISC_LTOFF14DR,
        R_PARISC_LTOFF16F,
        R_PARISC_LTOFF16WF,
        R_PARISC_LTOFF16DF,
        R_PARISC_SECREL64,
        R_PARISC_SEGREL64,
        R_PARISC_PLTOFF14WR,
        R_PARISC_PLTOFF14DR,
        R_PARISC_PLTOFF16F,
        R_PARISC_PLTOFF16WF,
        R_PARISC_PLTOFF16DF,
        R_PARISC_LTOFF_FPTR64,
        R_PARISC_LTOFF_FPTR14WR,
        R_PARISC_LTOFF_FPTR14DR,
        R_PARISC_LTOFF_FPTR16F,
        R_PARISC_LTOFF_FPTR16WF,
        R_PARISC_LTOFF_FPTR16DF,
        R_PARISC_COPY,
        R_PARISC_IPLT,
        R_PARISC_EPLT,
        R_PARISC_TPREL32,
        R_PARISC_TPREL21L,
        R_PARISC_TPREL14R,
        R_PARISC_LTOFF_TP21L,
        R_PARISC_LTOFF_TP14R,
        R_PARISC_LTOFF_TP14F,
        R_PARISC_TPREL64,
        R_PARISC_TPREL14WR,
        R_PARISC_TPREL14DR,
        R_PARISC_TPREL16F,
        R_PARISC_TPREL16WF,
        R_PARISC_TPREL16DF,
        R_PARISC_LTOFF_TP64,
        R_PARISC_LTOFF_TP14WR,
        R_PARISC_LTOFF_TP14DR,
        R_PARISC_LTOFF_TP16F,
        R_PARISC_LTOFF_TP16WF,
        R_PARISC_LTOFF_TP16DF,
        R_PARISC_GNU_VTENTRY,
        R_PARISC_GNU_VTINHERIT,
        R_PARISC_TLS_GD21L,
        R_PARISC_TLS_GD14R,
        R_PARISC_TLS_GDCALL,
        R_PARISC_TLS_LDM21L,
        R_PARISC_TLS_LDM14R,
        R_PARISC_TLS_LDMCALL,
        R_PARISC_TLS_LDO21L,
        R_PARISC_TLS_LDO14R,
        R_PARISC_TLS_DTPMOD32,
        R_PARISC_TLS_DTPMOD64,
        R_PARISC_TLS_DTPOFF32,
        R_PARISC_TLS_DTPOFF64,
        R_PARISC_TLS_LE21L,
        R_PARISC_TLS_LE14R,
        R_PARISC_TLS_IE21L,
        R_PARISC_TLS_IE14R,
        R_PARISC_TLS_TPREL32,
        R_PARISC_TLS_TPREL64,
    );
    static FLAGS_R_ALPHA: &[Flag<u32>] = &flags!(
        R_ALPHA_NONE,
        R_ALPHA_REFLONG,
        R_ALPHA_REFQUAD,
        R_ALPHA_GPREL32,
        R_ALPHA_LITERAL,
        R_ALPHA_LITUSE,
        R_ALPHA_GPDISP,
        R_ALPHA_BRADDR,
        R_ALPHA_HINT,
        R_ALPHA_SREL16,
        R_ALPHA_SREL32,
        R_ALPHA_SREL64,
        R_ALPHA_GPRELHIGH,
        R_ALPHA_GPRELLOW,
        R_ALPHA_GPREL16,
        R_ALPHA_COPY,
        R_ALPHA_GLOB_DAT,
        R_ALPHA_JMP_SLOT,
        R_ALPHA_RELATIVE,
        R_ALPHA_TLS_GD_HI,
        R_ALPHA_TLSGD,
        R_ALPHA_TLS_LDM,
        R_ALPHA_DTPMOD64,
        R_ALPHA_GOTDTPREL,
        R_ALPHA_DTPREL64,
        R_ALPHA_DTPRELHI,
        R_ALPHA_DTPRELLO,
        R_ALPHA_DTPREL16,
        R_ALPHA_GOTTPREL,
        R_ALPHA_TPREL64,
        R_ALPHA_TPRELHI,
        R_ALPHA_TPRELLO,
        R_ALPHA_TPREL16,
    );
    static FLAGS_R_PPC: &[Flag<u32>] = &flags!(
        R_PPC_NONE,
        R_PPC_ADDR32,
        R_PPC_ADDR24,
        R_PPC_ADDR16,
        R_PPC_ADDR16_LO,
        R_PPC_ADDR16_HI,
        R_PPC_ADDR16_HA,
        R_PPC_ADDR14,
        R_PPC_ADDR14_BRTAKEN,
        R_PPC_ADDR14_BRNTAKEN,
        R_PPC_REL24,
        R_PPC_REL14,
        R_PPC_REL14_BRTAKEN,
        R_PPC_REL14_BRNTAKEN,
        R_PPC_GOT16,
        R_PPC_GOT16_LO,
        R_PPC_GOT16_HI,
        R_PPC_GOT16_HA,
        R_PPC_PLTREL24,
        R_PPC_COPY,
        R_PPC_GLOB_DAT,
        R_PPC_JMP_SLOT,
        R_PPC_RELATIVE,
        R_PPC_LOCAL24PC,
        R_PPC_UADDR32,
        R_PPC_UADDR16,
        R_PPC_REL32,
        R_PPC_PLT32,
        R_PPC_PLTREL32,
        R_PPC_PLT16_LO,
        R_PPC_PLT16_HI,
        R_PPC_PLT16_HA,
        R_PPC_SDAREL16,
        R_PPC_SECTOFF,
        R_PPC_SECTOFF_LO,
        R_PPC_SECTOFF_HI,
        R_PPC_SECTOFF_HA,
        R_PPC_TLS,
        R_PPC_DTPMOD32,
        R_PPC_TPREL16,
        R_PPC_TPREL16_LO,
        R_PPC_TPREL16_HI,
        R_PPC_TPREL16_HA,
        R_PPC_TPREL32,
        R_PPC_DTPREL16,
        R_PPC_DTPREL16_LO,
        R_PPC_DTPREL16_HI,
        R_PPC_DTPREL16_HA,
        R_PPC_DTPREL32,
        R_PPC_GOT_TLSGD16,
        R_PPC_GOT_TLSGD16_LO,
        R_PPC_GOT_TLSGD16_HI,
        R_PPC_GOT_TLSGD16_HA,
        R_PPC_GOT_TLSLD16,
        R_PPC_GOT_TLSLD16_LO,
        R_PPC_GOT_TLSLD16_HI,
        R_PPC_GOT_TLSLD16_HA,
        R_PPC_GOT_TPREL16,
        R_PPC_GOT_TPREL16_LO,
        R_PPC_GOT_TPREL16_HI,
        R_PPC_GOT_TPREL16_HA,
        R_PPC_GOT_DTPREL16,
        R_PPC_GOT_DTPREL16_LO,
        R_PPC_GOT_DTPREL16_HI,
        R_PPC_GOT_DTPREL16_HA,
        R_PPC_TLSGD,
        R_PPC_TLSLD,
        R_PPC_EMB_NADDR32,
        R_PPC_EMB_NADDR16,
        R_PPC_EMB_NADDR16_LO,
        R_PPC_EMB_NADDR16_HI,
        R_PPC_EMB_NADDR16_HA,
        R_PPC_EMB_SDAI16,
        R_PPC_EMB_SDA2I16,
        R_PPC_EMB_SDA2REL,
        R_PPC_EMB_SDA21,
        R_PPC_EMB_MRKREF,
        R_PPC_EMB_RELSEC16,
        R_PPC_EMB_RELST_LO,
        R_PPC_EMB_RELST_HI,
        R_PPC_EMB_RELST_HA,
        R_PPC_EMB_BIT_FLD,
        R_PPC_EMB_RELSDA,
        R_PPC_DIAB_SDA21_LO,
        R_PPC_DIAB_SDA21_HI,
        R_PPC_DIAB_SDA21_HA,
        R_PPC_DIAB_RELSDA_LO,
        R_PPC_DIAB_RELSDA_HI,
        R_PPC_DIAB_RELSDA_HA,
        R_PPC_IRELATIVE,
        R_PPC_REL16,
        R_PPC_REL16_LO,
        R_PPC_REL16_HI,
        R_PPC_REL16_HA,
        R_PPC_TOC16,
    );
    static FLAGS_R_PPC64: &[Flag<u32>] = &flags!(
        R_PPC64_NONE,
        R_PPC64_ADDR32,
        R_PPC64_ADDR24,
        R_PPC64_ADDR16,
        R_PPC64_ADDR16_LO,
        R_PPC64_ADDR16_HI,
        R_PPC64_ADDR16_HA,
        R_PPC64_ADDR14,
        R_PPC64_ADDR14_BRTAKEN,
        R_PPC64_ADDR14_BRNTAKEN,
        R_PPC64_REL24,
        R_PPC64_REL14,
        R_PPC64_REL14_BRTAKEN,
        R_PPC64_REL14_BRNTAKEN,
        R_PPC64_GOT16,
        R_PPC64_GOT16_LO,
        R_PPC64_GOT16_HI,
        R_PPC64_GOT16_HA,
        R_PPC64_COPY,
        R_PPC64_GLOB_DAT,
        R_PPC64_JMP_SLOT,
        R_PPC64_RELATIVE,
        R_PPC64_UADDR32,
        R_PPC64_UADDR16,
        R_PPC64_REL32,
        R_PPC64_PLT32,
        R_PPC64_PLTREL32,
        R_PPC64_PLT16_LO,
        R_PPC64_PLT16_HI,
        R_PPC64_PLT16_HA,
        R_PPC64_SECTOFF,
        R_PPC64_SECTOFF_LO,
        R_PPC64_SECTOFF_HI,
        R_PPC64_SECTOFF_HA,
        R_PPC64_ADDR30,
        R_PPC64_ADDR64,
        R_PPC64_ADDR16_HIGHER,
        R_PPC64_ADDR16_HIGHERA,
        R_PPC64_ADDR16_HIGHEST,
        R_PPC64_ADDR16_HIGHESTA,
        R_PPC64_UADDR64,
        R_PPC64_REL64,
        R_PPC64_PLT64,
        R_PPC64_PLTREL64,
        R_PPC64_TOC16,
        R_PPC64_TOC16_LO,
        R_PPC64_TOC16_HI,
        R_PPC64_TOC16_HA,
        R_PPC64_TOC,
        R_PPC64_PLTGOT16,
        R_PPC64_PLTGOT16_LO,
        R_PPC64_PLTGOT16_HI,
        R_PPC64_PLTGOT16_HA,
        R_PPC64_ADDR16_DS,
        R_PPC64_ADDR16_LO_DS,
        R_PPC64_GOT16_DS,
        R_PPC64_GOT16_LO_DS,
        R_PPC64_PLT16_LO_DS,
        R_PPC64_SECTOFF_DS,
        R_PPC64_SECTOFF_LO_DS,
        R_PPC64_TOC16_DS,
        R_PPC64_TOC16_LO_DS,
        R_PPC64_PLTGOT16_DS,
        R_PPC64_PLTGOT16_LO_DS,
        R_PPC64_TLS,
        R_PPC64_DTPMOD64,
        R_PPC64_TPREL16,
        R_PPC64_TPREL16_LO,
        R_PPC64_TPREL16_HI,
        R_PPC64_TPREL16_HA,
        R_PPC64_TPREL64,
        R_PPC64_DTPREL16,
        R_PPC64_DTPREL16_LO,
        R_PPC64_DTPREL16_HI,
        R_PPC64_DTPREL16_HA,
        R_PPC64_DTPREL64,
        R_PPC64_GOT_TLSGD16,
        R_PPC64_GOT_TLSGD16_LO,
        R_PPC64_GOT_TLSGD16_HI,
        R_PPC64_GOT_TLSGD16_HA,
        R_PPC64_GOT_TLSLD16,
        R_PPC64_GOT_TLSLD16_LO,
        R_PPC64_GOT_TLSLD16_HI,
        R_PPC64_GOT_TLSLD16_HA,
        R_PPC64_GOT_TPREL16_DS,
        R_PPC64_GOT_TPREL16_LO_DS,
        R_PPC64_GOT_TPREL16_HI,
        R_PPC64_GOT_TPREL16_HA,
        R_PPC64_GOT_DTPREL16_DS,
        R_PPC64_GOT_DTPREL16_LO_DS,
        R_PPC64_GOT_DTPREL16_HI,
        R_PPC64_GOT_DTPREL16_HA,
        R_PPC64_TPREL16_DS,
        R_PPC64_TPREL16_LO_DS,
        R_PPC64_TPREL16_HIGHER,
        R_PPC64_TPREL16_HIGHERA,
        R_PPC64_TPREL16_HIGHEST,
        R_PPC64_TPREL16_HIGHESTA,
        R_PPC64_DTPREL16_DS,
        R_PPC64_DTPREL16_LO_DS,
        R_PPC64_DTPREL16_HIGHER,
        R_PPC64_DTPREL16_HIGHERA,
        R_PPC64_DTPREL16_HIGHEST,
        R_PPC64_DTPREL16_HIGHESTA,
        R_PPC64_TLSGD,
        R_PPC64_TLSLD,
        R_PPC64_TOCSAVE,
        R_PPC64_ADDR16_HIGH,
        R_PPC64_ADDR16_HIGHA,
        R_PPC64_TPREL16_HIGH,
        R_PPC64_TPREL16_HIGHA,
        R_PPC64_DTPREL16_HIGH,
        R_PPC64_DTPREL16_HIGHA,
        R_PPC64_JMP_IREL,
        R_PPC64_IRELATIVE,
        R_PPC64_REL16,
        R_PPC64_REL16_LO,
        R_PPC64_REL16_HI,
        R_PPC64_REL16_HA,
    );
    static FLAGS_R_AARCH64: &[Flag<u32>] = &flags!(
        R_AARCH64_NONE,
        R_AARCH64_P32_ABS32,
        R_AARCH64_P32_COPY,
        R_AARCH64_P32_GLOB_DAT,
        R_AARCH64_P32_JUMP_SLOT,
        R_AARCH64_P32_RELATIVE,
        R_AARCH64_P32_TLS_DTPMOD,
        R_AARCH64_P32_TLS_DTPREL,
        R_AARCH64_P32_TLS_TPREL,
        R_AARCH64_P32_TLSDESC,
        R_AARCH64_P32_IRELATIVE,
        R_AARCH64_ABS64,
        R_AARCH64_ABS32,
        R_AARCH64_ABS16,
        R_AARCH64_PREL64,
        R_AARCH64_PREL32,
        R_AARCH64_PREL16,
        R_AARCH64_MOVW_UABS_G0,
        R_AARCH64_MOVW_UABS_G0_NC,
        R_AARCH64_MOVW_UABS_G1,
        R_AARCH64_MOVW_UABS_G1_NC,
        R_AARCH64_MOVW_UABS_G2,
        R_AARCH64_MOVW_UABS_G2_NC,
        R_AARCH64_MOVW_UABS_G3,
        R_AARCH64_MOVW_SABS_G0,
        R_AARCH64_MOVW_SABS_G1,
        R_AARCH64_MOVW_SABS_G2,
        R_AARCH64_LD_PREL_LO19,
        R_AARCH64_ADR_PREL_LO21,
        R_AARCH64_ADR_PREL_PG_HI21,
        R_AARCH64_ADR_PREL_PG_HI21_NC,
        R_AARCH64_ADD_ABS_LO12_NC,
        R_AARCH64_LDST8_ABS_LO12_NC,
        R_AARCH64_TSTBR14,
        R_AARCH64_CONDBR19,
        R_AARCH64_JUMP26,
        R_AARCH64_CALL26,
        R_AARCH64_LDST16_ABS_LO12_NC,
        R_AARCH64_LDST32_ABS_LO12_NC,
        R_AARCH64_LDST64_ABS_LO12_NC,
        R_AARCH64_MOVW_PREL_G0,
        R_AARCH64_MOVW_PREL_G0_NC,
        R_AARCH64_MOVW_PREL_G1,
        R_AARCH64_MOVW_PREL_G1_NC,
        R_AARCH64_MOVW_PREL_G2,
        R_AARCH64_MOVW_PREL_G2_NC,
        R_AARCH64_MOVW_PREL_G3,
        R_AARCH64_LDST128_ABS_LO12_NC,
        R_AARCH64_MOVW_GOTOFF_G0,
        R_AARCH64_MOVW_GOTOFF_G0_NC,
        R_AARCH64_MOVW_GOTOFF_G1,
        R_AARCH64_MOVW_GOTOFF_G1_NC,
        R_AARCH64_MOVW_GOTOFF_G2,
        R_AARCH64_MOVW_GOTOFF_G2_NC,
        R_AARCH64_MOVW_GOTOFF_G3,
        R_AARCH64_GOTREL64,
        R_AARCH64_GOTREL32,
        R_AARCH64_GOT_LD_PREL19,
        R_AARCH64_LD64_GOTOFF_LO15,
        R_AARCH64_ADR_GOT_PAGE,
        R_AARCH64_LD64_GOT_LO12_NC,
        R_AARCH64_LD64_GOTPAGE_LO15,
        R_AARCH64_TLSGD_ADR_PREL21,
        R_AARCH64_TLSGD_ADR_PAGE21,
        R_AARCH64_TLSGD_ADD_LO12_NC,
        R_AARCH64_TLSGD_MOVW_G1,
        R_AARCH64_TLSGD_MOVW_G0_NC,
        R_AARCH64_TLSLD_ADR_PREL21,
        R_AARCH64_TLSLD_ADR_PAGE21,
        R_AARCH64_TLSLD_ADD_LO12_NC,
        R_AARCH64_TLSLD_MOVW_G1,
        R_AARCH64_TLSLD_MOVW_G0_NC,
        R_AARCH64_TLSLD_LD_PREL19,
        R_AARCH64_TLSLD_MOVW_DTPREL_G2,
        R_AARCH64_TLSLD_MOVW_DTPREL_G1,
        R_AARCH64_TLSLD_MOVW_DTPREL_G1_NC,
        R_AARCH64_TLSLD_MOVW_DTPREL_G0,
        R_AARCH64_TLSLD_MOVW_DTPREL_G0_NC,
        R_AARCH64_TLSLD_ADD_DTPREL_HI12,
        R_AARCH64_TLSLD_ADD_DTPREL_LO12,
        R_AARCH64_TLSLD_ADD_DTPREL_LO12_NC,
        R_AARCH64_TLSLD_LDST8_DTPREL_LO12,
        R_AARCH64_TLSLD_LDST8_DTPREL_LO12_NC,
        R_AARCH64_TLSLD_LDST16_DTPREL_LO12,
        R_AARCH64_TLSLD_LDST16_DTPREL_LO12_NC,
        R_AARCH64_TLSLD_LDST32_DTPREL_LO12,
        R_AARCH64_TLSLD_LDST32_DTPREL_LO12_NC,
        R_AARCH64_TLSLD_LDST64_DTPREL_LO12,
        R_AARCH64_TLSLD_LDST64_DTPREL_LO12_NC,
        R_AARCH64_TLSIE_MOVW_GOTTPREL_G1,
        R_AARCH64_TLSIE_MOVW_GOTTPREL_G0_NC,
        R_AARCH64_TLSIE_ADR_GOTTPREL_PAGE21,
        R_AARCH64_TLSIE_LD64_GOTTPREL_LO12_NC,
        R_AARCH64_TLSIE_LD_GOTTPREL_PREL19,
        R_AARCH64_TLSLE_MOVW_TPREL_G2,
        R_AARCH64_TLSLE_MOVW_TPREL_G1,
        R_AARCH64_TLSLE_MOVW_TPREL_G1_NC,
        R_AARCH64_TLSLE_MOVW_TPREL_G0,
        R_AARCH64_TLSLE_MOVW_TPREL_G0_NC,
        R_AARCH64_TLSLE_ADD_TPREL_HI12,
        R_AARCH64_TLSLE_ADD_TPREL_LO12,
        R_AARCH64_TLSLE_ADD_TPREL_LO12_NC,
        R_AARCH64_TLSLE_LDST8_TPREL_LO12,
        R_AARCH64_TLSLE_LDST8_TPREL_LO12_NC,
        R_AARCH64_TLSLE_LDST16_TPREL_LO12,
        R_AARCH64_TLSLE_LDST16_TPREL_LO12_NC,
        R_AARCH64_TLSLE_LDST32_TPREL_LO12,
        R_AARCH64_TLSLE_LDST32_TPREL_LO12_NC,
        R_AARCH64_TLSLE_LDST64_TPREL_LO12,
        R_AARCH64_TLSLE_LDST64_TPREL_LO12_NC,
        R_AARCH64_TLSDESC_LD_PREL19,
        R_AARCH64_TLSDESC_ADR_PREL21,
        R_AARCH64_TLSDESC_ADR_PAGE21,
        R_AARCH64_TLSDESC_LD64_LO12,
        R_AARCH64_TLSDESC_ADD_LO12,
        R_AARCH64_TLSDESC_OFF_G1,
        R_AARCH64_TLSDESC_OFF_G0_NC,
        R_AARCH64_TLSDESC_LDR,
        R_AARCH64_TLSDESC_ADD,
        R_AARCH64_TLSDESC_CALL,
        R_AARCH64_TLSLE_LDST128_TPREL_LO12,
        R_AARCH64_TLSLE_LDST128_TPREL_LO12_NC,
        R_AARCH64_TLSLD_LDST128_DTPREL_LO12,
        R_AARCH64_TLSLD_LDST128_DTPREL_LO12_NC,
        R_AARCH64_COPY,
        R_AARCH64_GLOB_DAT,
        R_AARCH64_JUMP_SLOT,
        R_AARCH64_RELATIVE,
        R_AARCH64_TLS_DTPMOD,
        R_AARCH64_TLS_DTPREL,
        R_AARCH64_TLS_TPREL,
        R_AARCH64_TLSDESC,
        R_AARCH64_IRELATIVE,
    );
    static FLAGS_R_ARM: &[Flag<u32>] = &flags!(
        R_ARM_NONE,
        R_ARM_PC24,
        R_ARM_ABS32,
        R_ARM_REL32,
        R_ARM_PC13,
        R_ARM_ABS16,
        R_ARM_ABS12,
        R_ARM_THM_ABS5,
        R_ARM_ABS8,
        R_ARM_SBREL32,
        R_ARM_THM_PC22,
        R_ARM_THM_PC8,
        R_ARM_AMP_VCALL9,
        R_ARM_SWI24,
        R_ARM_TLS_DESC,
        R_ARM_THM_SWI8,
        R_ARM_XPC25,
        R_ARM_THM_XPC22,
        R_ARM_TLS_DTPMOD32,
        R_ARM_TLS_DTPOFF32,
        R_ARM_TLS_TPOFF32,
        R_ARM_COPY,
        R_ARM_GLOB_DAT,
        R_ARM_JUMP_SLOT,
        R_ARM_RELATIVE,
        R_ARM_GOTOFF,
        R_ARM_GOTPC,
        R_ARM_GOT32,
        R_ARM_PLT32,
        R_ARM_CALL,
        R_ARM_JUMP24,
        R_ARM_THM_JUMP24,
        R_ARM_BASE_ABS,
        R_ARM_ALU_PCREL_7_0,
        R_ARM_ALU_PCREL_15_8,
        R_ARM_ALU_PCREL_23_15,
        R_ARM_LDR_SBREL_11_0,
        R_ARM_ALU_SBREL_19_12,
        R_ARM_ALU_SBREL_27_20,
        R_ARM_TARGET1,
        R_ARM_SBREL31,
        R_ARM_V4BX,
        R_ARM_TARGET2,
        R_ARM_PREL31,
        R_ARM_MOVW_ABS_NC,
        R_ARM_MOVT_ABS,
        R_ARM_MOVW_PREL_NC,
        R_ARM_MOVT_PREL,
        R_ARM_THM_MOVW_ABS_NC,
        R_ARM_THM_MOVT_ABS,
        R_ARM_THM_MOVW_PREL_NC,
        R_ARM_THM_MOVT_PREL,
        R_ARM_THM_JUMP19,
        R_ARM_THM_JUMP6,
        R_ARM_THM_ALU_PREL_11_0,
        R_ARM_THM_PC12,
        R_ARM_ABS32_NOI,
        R_ARM_REL32_NOI,
        R_ARM_ALU_PC_G0_NC,
        R_ARM_ALU_PC_G0,
        R_ARM_ALU_PC_G1_NC,
        R_ARM_ALU_PC_G1,
        R_ARM_ALU_PC_G2,
        R_ARM_LDR_PC_G1,
        R_ARM_LDR_PC_G2,
        R_ARM_LDRS_PC_G0,
        R_ARM_LDRS_PC_G1,
        R_ARM_LDRS_PC_G2,
        R_ARM_LDC_PC_G0,
        R_ARM_LDC_PC_G1,
        R_ARM_LDC_PC_G2,
        R_ARM_ALU_SB_G0_NC,
        R_ARM_ALU_SB_G0,
        R_ARM_ALU_SB_G1_NC,
        R_ARM_ALU_SB_G1,
        R_ARM_ALU_SB_G2,
        R_ARM_LDR_SB_G0,
        R_ARM_LDR_SB_G1,
        R_ARM_LDR_SB_G2,
        R_ARM_LDRS_SB_G0,
        R_ARM_LDRS_SB_G1,
        R_ARM_LDRS_SB_G2,
        R_ARM_LDC_SB_G0,
        R_ARM_LDC_SB_G1,
        R_ARM_LDC_SB_G2,
        R_ARM_MOVW_BREL_NC,
        R_ARM_MOVT_BREL,
        R_ARM_MOVW_BREL,
        R_ARM_THM_MOVW_BREL_NC,
        R_ARM_THM_MOVT_BREL,
        R_ARM_THM_MOVW_BREL,
        R_ARM_TLS_GOTDESC,
        R_ARM_TLS_CALL,
        R_ARM_TLS_DESCSEQ,
        R_ARM_THM_TLS_CALL,
        R_ARM_PLT32_ABS,
        R_ARM_GOT_ABS,
        R_ARM_GOT_PREL,
        R_ARM_GOT_BREL12,
        R_ARM_GOTOFF12,
        R_ARM_GOTRELAX,
        R_ARM_GNU_VTENTRY,
        R_ARM_GNU_VTINHERIT,
        R_ARM_THM_PC11,
        R_ARM_THM_PC9,
        R_ARM_TLS_GD32,
        R_ARM_TLS_LDM32,
        R_ARM_TLS_LDO32,
        R_ARM_TLS_IE32,
        R_ARM_TLS_LE32,
        R_ARM_TLS_LDO12,
        R_ARM_TLS_LE12,
        R_ARM_TLS_IE12GP,
        R_ARM_ME_TOO,
        R_ARM_THM_TLS_DESCSEQ,
        R_ARM_THM_TLS_DESCSEQ16,
        R_ARM_THM_TLS_DESCSEQ32,
        R_ARM_THM_GOT_BREL12,
        R_ARM_IRELATIVE,
        R_ARM_RXPC25,
        R_ARM_RSBREL32,
        R_ARM_THM_RPC22,
        R_ARM_RREL32,
        R_ARM_RABS22,
        R_ARM_RPC24,
        R_ARM_RBASE,
    );
    static FLAGS_R_CKCORE: &[Flag<u32>] = &flags!(
        R_CKCORE_NONE,
        R_CKCORE_ADDR32,
        R_CKCORE_PCRELIMM8BY4,
        R_CKCORE_PCRELIMM11BY2,
        R_CKCORE_PCREL32,
        R_CKCORE_PCRELJSR_IMM11BY2,
        R_CKCORE_RELATIVE,
        R_CKCORE_COPY,
        R_CKCORE_GLOB_DAT,
        R_CKCORE_JUMP_SLOT,
        R_CKCORE_GOTOFF,
        R_CKCORE_GOTPC,
        R_CKCORE_GOT32,
        R_CKCORE_PLT32,
        R_CKCORE_ADDRGOT,
        R_CKCORE_ADDRPLT,
        R_CKCORE_PCREL_IMM26BY2,
        R_CKCORE_PCREL_IMM16BY2,
        R_CKCORE_PCREL_IMM16BY4,
        R_CKCORE_PCREL_IMM10BY2,
        R_CKCORE_PCREL_IMM10BY4,
        R_CKCORE_ADDR_HI16,
        R_CKCORE_ADDR_LO16,
        R_CKCORE_GOTPC_HI16,
        R_CKCORE_GOTPC_LO16,
        R_CKCORE_GOTOFF_HI16,
        R_CKCORE_GOTOFF_LO16,
        R_CKCORE_GOT12,
        R_CKCORE_GOT_HI16,
        R_CKCORE_GOT_LO16,
        R_CKCORE_PLT12,
        R_CKCORE_PLT_HI16,
        R_CKCORE_PLT_LO16,
        R_CKCORE_ADDRGOT_HI16,
        R_CKCORE_ADDRGOT_LO16,
        R_CKCORE_ADDRPLT_HI16,
        R_CKCORE_ADDRPLT_LO16,
        R_CKCORE_PCREL_JSR_IMM26BY2,
        R_CKCORE_TOFFSET_LO16,
        R_CKCORE_DOFFSET_LO16,
        R_CKCORE_PCREL_IMM18BY2,
        R_CKCORE_DOFFSET_IMM18,
        R_CKCORE_DOFFSET_IMM18BY2,
        R_CKCORE_DOFFSET_IMM18BY4,
        R_CKCORE_GOT_IMM18BY4,
        R_CKCORE_PLT_IMM18BY4,
        R_CKCORE_PCREL_IMM7BY4,
        R_CKCORE_TLS_LE32,
        R_CKCORE_TLS_IE32,
        R_CKCORE_TLS_GD32,
        R_CKCORE_TLS_LDM32,
        R_CKCORE_TLS_LDO32,
        R_CKCORE_TLS_DTPMOD32,
        R_CKCORE_TLS_DTPOFF32,
        R_CKCORE_TLS_TPOFF32,
    );
    static FLAGS_R_IA64: &[Flag<u32>] = &flags!(
        R_IA64_NONE,
        R_IA64_IMM14,
        R_IA64_IMM22,
        R_IA64_IMM64,
        R_IA64_DIR32MSB,
        R_IA64_DIR32LSB,
        R_IA64_DIR64MSB,
        R_IA64_DIR64LSB,
        R_IA64_GPREL22,
        R_IA64_GPREL64I,
        R_IA64_GPREL32MSB,
        R_IA64_GPREL32LSB,
        R_IA64_GPREL64MSB,
        R_IA64_GPREL64LSB,
        R_IA64_LTOFF22,
        R_IA64_LTOFF64I,
        R_IA64_PLTOFF22,
        R_IA64_PLTOFF64I,
        R_IA64_PLTOFF64MSB,
        R_IA64_PLTOFF64LSB,
        R_IA64_FPTR64I,
        R_IA64_FPTR32MSB,
        R_IA64_FPTR32LSB,
        R_IA64_FPTR64MSB,
        R_IA64_FPTR64LSB,
        R_IA64_PCREL60B,
        R_IA64_PCREL21B,
        R_IA64_PCREL21M,
        R_IA64_PCREL21F,
        R_IA64_PCREL32MSB,
        R_IA64_PCREL32LSB,
        R_IA64_PCREL64MSB,
        R_IA64_PCREL64LSB,
        R_IA64_LTOFF_FPTR22,
        R_IA64_LTOFF_FPTR64I,
        R_IA64_LTOFF_FPTR32MSB,
        R_IA64_LTOFF_FPTR32LSB,
        R_IA64_LTOFF_FPTR64MSB,
        R_IA64_LTOFF_FPTR64LSB,
        R_IA64_SEGREL32MSB,
        R_IA64_SEGREL32LSB,
        R_IA64_SEGREL64MSB,
        R_IA64_SEGREL64LSB,
        R_IA64_SECREL32MSB,
        R_IA64_SECREL32LSB,
        R_IA64_SECREL64MSB,
        R_IA64_SECREL64LSB,
        R_IA64_REL32MSB,
        R_IA64_REL32LSB,
        R_IA64_REL64MSB,
        R_IA64_REL64LSB,
        R_IA64_LTV32MSB,
        R_IA64_LTV32LSB,
        R_IA64_LTV64MSB,
        R_IA64_LTV64LSB,
        R_IA64_PCREL21BI,
        R_IA64_PCREL22,
        R_IA64_PCREL64I,
        R_IA64_IPLTMSB,
        R_IA64_IPLTLSB,
        R_IA64_COPY,
        R_IA64_SUB,
        R_IA64_LTOFF22X,
        R_IA64_LDXMOV,
        R_IA64_TPREL14,
        R_IA64_TPREL22,
        R_IA64_TPREL64I,
        R_IA64_TPREL64MSB,
        R_IA64_TPREL64LSB,
        R_IA64_LTOFF_TPREL22,
        R_IA64_DTPMOD64MSB,
        R_IA64_DTPMOD64LSB,
        R_IA64_LTOFF_DTPMOD22,
        R_IA64_DTPREL14,
        R_IA64_DTPREL22,
        R_IA64_DTPREL64I,
        R_IA64_DTPREL32MSB,
        R_IA64_DTPREL32LSB,
        R_IA64_DTPREL64MSB,
        R_IA64_DTPREL64LSB,
        R_IA64_LTOFF_DTPREL22,
    );
    static FLAGS_R_SH: &[Flag<u32>] = &flags!(
        R_SH_NONE,
        R_SH_DIR32,
        R_SH_REL32,
        R_SH_DIR8WPN,
        R_SH_IND12W,
        R_SH_DIR8WPL,
        R_SH_DIR8WPZ,
        R_SH_DIR8BP,
        R_SH_DIR8W,
        R_SH_DIR8L,
        R_SH_SWITCH16,
        R_SH_SWITCH32,
        R_SH_USES,
        R_SH_COUNT,
        R_SH_ALIGN,
        R_SH_CODE,
        R_SH_DATA,
        R_SH_LABEL,
        R_SH_SWITCH8,
        R_SH_GNU_VTINHERIT,
        R_SH_GNU_VTENTRY,
        R_SH_TLS_GD_32,
        R_SH_TLS_LD_32,
        R_SH_TLS_LDO_32,
        R_SH_TLS_IE_32,
        R_SH_TLS_LE_32,
        R_SH_TLS_DTPMOD32,
        R_SH_TLS_DTPOFF32,
        R_SH_TLS_TPOFF32,
        R_SH_GOT32,
        R_SH_PLT32,
        R_SH_COPY,
        R_SH_GLOB_DAT,
        R_SH_JMP_SLOT,
        R_SH_RELATIVE,
        R_SH_GOTOFF,
        R_SH_GOTPC,
    );
    static FLAGS_R_390: &[Flag<u32>] = &flags!(
        R_390_NONE,
        R_390_8,
        R_390_12,
        R_390_16,
        R_390_32,
        R_390_PC32,
        R_390_GOT12,
        R_390_GOT32,
        R_390_PLT32,
        R_390_COPY,
        R_390_GLOB_DAT,
        R_390_JMP_SLOT,
        R_390_RELATIVE,
        R_390_GOTOFF32,
        R_390_GOTPC,
        R_390_GOT16,
        R_390_PC16,
        R_390_PC16DBL,
        R_390_PLT16DBL,
        R_390_PC32DBL,
        R_390_PLT32DBL,
        R_390_GOTPCDBL,
        R_390_64,
        R_390_PC64,
        R_390_GOT64,
        R_390_PLT64,
        R_390_GOTENT,
        R_390_GOTOFF16,
        R_390_GOTOFF64,
        R_390_GOTPLT12,
        R_390_GOTPLT16,
        R_390_GOTPLT32,
        R_390_GOTPLT64,
        R_390_GOTPLTENT,
        R_390_PLTOFF16,
        R_390_PLTOFF32,
        R_390_PLTOFF64,
        R_390_TLS_LOAD,
        R_390_TLS_GDCALL,
        R_390_TLS_LDCALL,
        R_390_TLS_GD32,
        R_390_TLS_GD64,
        R_390_TLS_GOTIE12,
        R_390_TLS_GOTIE32,
        R_390_TLS_GOTIE64,
        R_390_TLS_LDM32,
        R_390_TLS_LDM64,
        R_390_TLS_IE32,
        R_390_TLS_IE64,
        R_390_TLS_IEENT,
        R_390_TLS_LE32,
        R_390_TLS_LE64,
        R_390_TLS_LDO32,
        R_390_TLS_LDO64,
        R_390_TLS_DTPMOD,
        R_390_TLS_DTPOFF,
        R_390_TLS_TPOFF,
        R_390_20,
        R_390_GOT20,
        R_390_GOTPLT20,
        R_390_TLS_GOTIE20,
        R_390_IRELATIVE,
    );
    static FLAGS_R_CRIS: &[Flag<u32>] = &flags!(
        R_CRIS_NONE,
        R_CRIS_8,
        R_CRIS_16,
        R_CRIS_32,
        R_CRIS_8_PCREL,
        R_CRIS_16_PCREL,
        R_CRIS_32_PCREL,
        R_CRIS_GNU_VTINHERIT,
        R_CRIS_GNU_VTENTRY,
        R_CRIS_COPY,
        R_CRIS_GLOB_DAT,
        R_CRIS_JUMP_SLOT,
        R_CRIS_RELATIVE,
        R_CRIS_16_GOT,
        R_CRIS_32_GOT,
        R_CRIS_16_GOTPLT,
        R_CRIS_32_GOTPLT,
        R_CRIS_32_GOTREL,
        R_CRIS_32_PLT_GOTREL,
        R_CRIS_32_PLT_PCREL,
    );
    static FLAGS_R_X86_64: &[Flag<u32>] = &flags!(
        R_X86_64_NONE,
        R_X86_64_64,
        R_X86_64_PC32,
        R_X86_64_GOT32,
        R_X86_64_PLT32,
        R_X86_64_COPY,
        R_X86_64_GLOB_DAT,
        R_X86_64_JUMP_SLOT,
        R_X86_64_RELATIVE,
        R_X86_64_GOTPCREL,
        R_X86_64_32,
        R_X86_64_32S,
        R_X86_64_16,
        R_X86_64_PC16,
        R_X86_64_8,
        R_X86_64_PC8,
        R_X86_64_DTPMOD64,
        R_X86_64_DTPOFF64,
        R_X86_64_TPOFF64,
        R_X86_64_TLSGD,
        R_X86_64_TLSLD,
        R_X86_64_DTPOFF32,
        R_X86_64_GOTTPOFF,
        R_X86_64_TPOFF32,
        R_X86_64_PC64,
        R_X86_64_GOTOFF64,
        R_X86_64_GOTPC32,
        R_X86_64_GOT64,
        R_X86_64_GOTPCREL64,
        R_X86_64_GOTPC64,
        R_X86_64_GOTPLT64,
        R_X86_64_PLTOFF64,
        R_X86_64_SIZE32,
        R_X86_64_SIZE64,
        R_X86_64_GOTPC32_TLSDESC,
        R_X86_64_TLSDESC_CALL,
        R_X86_64_TLSDESC,
        R_X86_64_IRELATIVE,
        R_X86_64_RELATIVE64,
        R_X86_64_GOTPCRELX,
        R_X86_64_REX_GOTPCRELX,
    );
    static FLAGS_R_MN10300: &[Flag<u32>] = &flags!(
        R_MN10300_NONE,
        R_MN10300_32,
        R_MN10300_16,
        R_MN10300_8,
        R_MN10300_PCREL32,
        R_MN10300_PCREL16,
        R_MN10300_PCREL8,
        R_MN10300_GNU_VTINHERIT,
        R_MN10300_GNU_VTENTRY,
        R_MN10300_24,
        R_MN10300_GOTPC32,
        R_MN10300_GOTPC16,
        R_MN10300_GOTOFF32,
        R_MN10300_GOTOFF24,
        R_MN10300_GOTOFF16,
        R_MN10300_PLT32,
        R_MN10300_PLT16,
        R_MN10300_GOT32,
        R_MN10300_GOT24,
        R_MN10300_GOT16,
        R_MN10300_COPY,
        R_MN10300_GLOB_DAT,
        R_MN10300_JMP_SLOT,
        R_MN10300_RELATIVE,
        R_MN10300_TLS_GD,
        R_MN10300_TLS_LD,
        R_MN10300_TLS_LDO,
        R_MN10300_TLS_GOTIE,
        R_MN10300_TLS_IE,
        R_MN10300_TLS_LE,
        R_MN10300_TLS_DTPMOD,
        R_MN10300_TLS_DTPOFF,
        R_MN10300_TLS_TPOFF,
        R_MN10300_SYM_DIFF,
        R_MN10300_ALIGN,
    );
    static FLAGS_R_M32R: &[Flag<u32>] = &flags!(
        R_M32R_NONE,
        R_M32R_16,
        R_M32R_32,
        R_M32R_24,
        R_M32R_10_PCREL,
        R_M32R_18_PCREL,
        R_M32R_26_PCREL,
        R_M32R_HI16_ULO,
        R_M32R_HI16_SLO,
        R_M32R_LO16,
        R_M32R_SDA16,
        R_M32R_GNU_VTINHERIT,
        R_M32R_GNU_VTENTRY,
        R_M32R_16_RELA,
        R_M32R_32_RELA,
        R_M32R_24_RELA,
        R_M32R_10_PCREL_RELA,
        R_M32R_18_PCREL_RELA,
        R_M32R_26_PCREL_RELA,
        R_M32R_HI16_ULO_RELA,
        R_M32R_HI16_SLO_RELA,
        R_M32R_LO16_RELA,
        R_M32R_SDA16_RELA,
        R_M32R_RELA_GNU_VTINHERIT,
        R_M32R_RELA_GNU_VTENTRY,
        R_M32R_REL32,
        R_M32R_GOT24,
        R_M32R_26_PLTREL,
        R_M32R_COPY,
        R_M32R_GLOB_DAT,
        R_M32R_JMP_SLOT,
        R_M32R_RELATIVE,
        R_M32R_GOTOFF,
        R_M32R_GOTPC24,
        R_M32R_GOT16_HI_ULO,
        R_M32R_GOT16_HI_SLO,
        R_M32R_GOT16_LO,
        R_M32R_GOTPC_HI_ULO,
        R_M32R_GOTPC_HI_SLO,
        R_M32R_GOTPC_LO,
        R_M32R_GOTOFF_HI_ULO,
        R_M32R_GOTOFF_HI_SLO,
        R_M32R_GOTOFF_LO,
        R_M32R_NUM,
    );
    static FLAGS_R_MICROBLAZE: &[Flag<u32>] = &flags!(
        R_MICROBLAZE_NONE,
        R_MICROBLAZE_32,
        R_MICROBLAZE_32_PCREL,
        R_MICROBLAZE_64_PCREL,
        R_MICROBLAZE_32_PCREL_LO,
        R_MICROBLAZE_64,
        R_MICROBLAZE_32_LO,
        R_MICROBLAZE_SRO32,
        R_MICROBLAZE_SRW32,
        R_MICROBLAZE_64_NONE,
        R_MICROBLAZE_32_SYM_OP_SYM,
        R_MICROBLAZE_GNU_VTINHERIT,
        R_MICROBLAZE_GNU_VTENTRY,
        R_MICROBLAZE_GOTPC_64,
        R_MICROBLAZE_GOT_64,
        R_MICROBLAZE_PLT_64,
        R_MICROBLAZE_REL,
        R_MICROBLAZE_JUMP_SLOT,
        R_MICROBLAZE_GLOB_DAT,
        R_MICROBLAZE_GOTOFF_64,
        R_MICROBLAZE_GOTOFF_32,
        R_MICROBLAZE_COPY,
        R_MICROBLAZE_TLS,
        R_MICROBLAZE_TLSGD,
        R_MICROBLAZE_TLSLD,
        R_MICROBLAZE_TLSDTPMOD32,
        R_MICROBLAZE_TLSDTPREL32,
        R_MICROBLAZE_TLSDTPREL64,
        R_MICROBLAZE_TLSGOTTPREL32,
        R_MICROBLAZE_TLSTPREL32,
    );
    static FLAGS_R_NIOS2: &[Flag<u32>] = &flags!(
        R_NIOS2_NONE,
        R_NIOS2_S16,
        R_NIOS2_U16,
        R_NIOS2_PCREL16,
        R_NIOS2_CALL26,
        R_NIOS2_IMM5,
        R_NIOS2_CACHE_OPX,
        R_NIOS2_IMM6,
        R_NIOS2_IMM8,
        R_NIOS2_HI16,
        R_NIOS2_LO16,
        R_NIOS2_HIADJ16,
        R_NIOS2_BFD_RELOC_32,
        R_NIOS2_BFD_RELOC_16,
        R_NIOS2_BFD_RELOC_8,
        R_NIOS2_GPREL,
        R_NIOS2_GNU_VTINHERIT,
        R_NIOS2_GNU_VTENTRY,
        R_NIOS2_UJMP,
        R_NIOS2_CJMP,
        R_NIOS2_CALLR,
        R_NIOS2_ALIGN,
        R_NIOS2_GOT16,
        R_NIOS2_CALL16,
        R_NIOS2_GOTOFF_LO,
        R_NIOS2_GOTOFF_HA,
        R_NIOS2_PCREL_LO,
        R_NIOS2_PCREL_HA,
        R_NIOS2_TLS_GD16,
        R_NIOS2_TLS_LDM16,
        R_NIOS2_TLS_LDO16,
        R_NIOS2_TLS_IE16,
        R_NIOS2_TLS_LE16,
        R_NIOS2_TLS_DTPMOD,
        R_NIOS2_TLS_DTPREL,
        R_NIOS2_TLS_TPREL,
        R_NIOS2_COPY,
        R_NIOS2_GLOB_DAT,
        R_NIOS2_JUMP_SLOT,
        R_NIOS2_RELATIVE,
        R_NIOS2_GOTOFF,
        R_NIOS2_CALL26_NOAT,
        R_NIOS2_GOT_LO,
        R_NIOS2_GOT_HA,
        R_NIOS2_CALL_LO,
        R_NIOS2_CALL_HA,
    );
    static FLAGS_R_TILEPRO: &[Flag<u32>] = &flags!(
        R_TILEPRO_NONE,
        R_TILEPRO_32,
        R_TILEPRO_16,
        R_TILEPRO_8,
        R_TILEPRO_32_PCREL,
        R_TILEPRO_16_PCREL,
        R_TILEPRO_8_PCREL,
        R_TILEPRO_LO16,
        R_TILEPRO_HI16,
        R_TILEPRO_HA16,
        R_TILEPRO_COPY,
        R_TILEPRO_GLOB_DAT,
        R_TILEPRO_JMP_SLOT,
        R_TILEPRO_RELATIVE,
        R_TILEPRO_BROFF_X1,
        R_TILEPRO_JOFFLONG_X1,
        R_TILEPRO_JOFFLONG_X1_PLT,
        R_TILEPRO_IMM8_X0,
        R_TILEPRO_IMM8_Y0,
        R_TILEPRO_IMM8_X1,
        R_TILEPRO_IMM8_Y1,
        R_TILEPRO_MT_IMM15_X1,
        R_TILEPRO_MF_IMM15_X1,
        R_TILEPRO_IMM16_X0,
        R_TILEPRO_IMM16_X1,
        R_TILEPRO_IMM16_X0_LO,
        R_TILEPRO_IMM16_X1_LO,
        R_TILEPRO_IMM16_X0_HI,
        R_TILEPRO_IMM16_X1_HI,
        R_TILEPRO_IMM16_X0_HA,
        R_TILEPRO_IMM16_X1_HA,
        R_TILEPRO_IMM16_X0_PCREL,
        R_TILEPRO_IMM16_X1_PCREL,
        R_TILEPRO_IMM16_X0_LO_PCREL,
        R_TILEPRO_IMM16_X1_LO_PCREL,
        R_TILEPRO_IMM16_X0_HI_PCREL,
        R_TILEPRO_IMM16_X1_HI_PCREL,
        R_TILEPRO_IMM16_X0_HA_PCREL,
        R_TILEPRO_IMM16_X1_HA_PCREL,
        R_TILEPRO_IMM16_X0_GOT,
        R_TILEPRO_IMM16_X1_GOT,
        R_TILEPRO_IMM16_X0_GOT_LO,
        R_TILEPRO_IMM16_X1_GOT_LO,
        R_TILEPRO_IMM16_X0_GOT_HI,
        R_TILEPRO_IMM16_X1_GOT_HI,
        R_TILEPRO_IMM16_X0_GOT_HA,
        R_TILEPRO_IMM16_X1_GOT_HA,
        R_TILEPRO_MMSTART_X0,
        R_TILEPRO_MMEND_X0,
        R_TILEPRO_MMSTART_X1,
        R_TILEPRO_MMEND_X1,
        R_TILEPRO_SHAMT_X0,
        R_TILEPRO_SHAMT_X1,
        R_TILEPRO_SHAMT_Y0,
        R_TILEPRO_SHAMT_Y1,
        R_TILEPRO_DEST_IMM8_X1,
        R_TILEPRO_TLS_GD_CALL,
        R_TILEPRO_IMM8_X0_TLS_GD_ADD,
        R_TILEPRO_IMM8_X1_TLS_GD_ADD,
        R_TILEPRO_IMM8_Y0_TLS_GD_ADD,
        R_TILEPRO_IMM8_Y1_TLS_GD_ADD,
        R_TILEPRO_TLS_IE_LOAD,
        R_TILEPRO_IMM16_X0_TLS_GD,
        R_TILEPRO_IMM16_X1_TLS_GD,
        R_TILEPRO_IMM16_X0_TLS_GD_LO,
        R_TILEPRO_IMM16_X1_TLS_GD_LO,
        R_TILEPRO_IMM16_X0_TLS_GD_HI,
        R_TILEPRO_IMM16_X1_TLS_GD_HI,
        R_TILEPRO_IMM16_X0_TLS_GD_HA,
        R_TILEPRO_IMM16_X1_TLS_GD_HA,
        R_TILEPRO_IMM16_X0_TLS_IE,
        R_TILEPRO_IMM16_X1_TLS_IE,
        R_TILEPRO_IMM16_X0_TLS_IE_LO,
        R_TILEPRO_IMM16_X1_TLS_IE_LO,
        R_TILEPRO_IMM16_X0_TLS_IE_HI,
        R_TILEPRO_IMM16_X1_TLS_IE_HI,
        R_TILEPRO_IMM16_X0_TLS_IE_HA,
        R_TILEPRO_IMM16_X1_TLS_IE_HA,
        R_TILEPRO_TLS_DTPMOD32,
        R_TILEPRO_TLS_DTPOFF32,
        R_TILEPRO_TLS_TPOFF32,
        R_TILEPRO_IMM16_X0_TLS_LE,
        R_TILEPRO_IMM16_X1_TLS_LE,
        R_TILEPRO_IMM16_X0_TLS_LE_LO,
        R_TILEPRO_IMM16_X1_TLS_LE_LO,
        R_TILEPRO_IMM16_X0_TLS_LE_HI,
        R_TILEPRO_IMM16_X1_TLS_LE_HI,
        R_TILEPRO_IMM16_X0_TLS_LE_HA,
        R_TILEPRO_IMM16_X1_TLS_LE_HA,
        R_TILEPRO_GNU_VTINHERIT,
        R_TILEPRO_GNU_VTENTRY,
    );
    static FLAGS_R_TILEGX: &[Flag<u32>] = &flags!(
        R_TILEGX_NONE,
        R_TILEGX_64,
        R_TILEGX_32,
        R_TILEGX_16,
        R_TILEGX_8,
        R_TILEGX_64_PCREL,
        R_TILEGX_32_PCREL,
        R_TILEGX_16_PCREL,
        R_TILEGX_8_PCREL,
        R_TILEGX_HW0,
        R_TILEGX_HW1,
        R_TILEGX_HW2,
        R_TILEGX_HW3,
        R_TILEGX_HW0_LAST,
        R_TILEGX_HW1_LAST,
        R_TILEGX_HW2_LAST,
        R_TILEGX_COPY,
        R_TILEGX_GLOB_DAT,
        R_TILEGX_JMP_SLOT,
        R_TILEGX_RELATIVE,
        R_TILEGX_BROFF_X1,
        R_TILEGX_JUMPOFF_X1,
        R_TILEGX_JUMPOFF_X1_PLT,
        R_TILEGX_IMM8_X0,
        R_TILEGX_IMM8_Y0,
        R_TILEGX_IMM8_X1,
        R_TILEGX_IMM8_Y1,
        R_TILEGX_DEST_IMM8_X1,
        R_TILEGX_MT_IMM14_X1,
        R_TILEGX_MF_IMM14_X1,
        R_TILEGX_MMSTART_X0,
        R_TILEGX_MMEND_X0,
        R_TILEGX_SHAMT_X0,
        R_TILEGX_SHAMT_X1,
        R_TILEGX_SHAMT_Y0,
        R_TILEGX_SHAMT_Y1,
        R_TILEGX_IMM16_X0_HW0,
        R_TILEGX_IMM16_X1_HW0,
        R_TILEGX_IMM16_X0_HW1,
        R_TILEGX_IMM16_X1_HW1,
        R_TILEGX_IMM16_X0_HW2,
        R_TILEGX_IMM16_X1_HW2,
        R_TILEGX_IMM16_X0_HW3,
        R_TILEGX_IMM16_X1_HW3,
        R_TILEGX_IMM16_X0_HW0_LAST,
        R_TILEGX_IMM16_X1_HW0_LAST,
        R_TILEGX_IMM16_X0_HW1_LAST,
        R_TILEGX_IMM16_X1_HW1_LAST,
        R_TILEGX_IMM16_X0_HW2_LAST,
        R_TILEGX_IMM16_X1_HW2_LAST,
        R_TILEGX_IMM16_X0_HW0_PCREL,
        R_TILEGX_IMM16_X1_HW0_PCREL,
        R_TILEGX_IMM16_X0_HW1_PCREL,
        R_TILEGX_IMM16_X1_HW1_PCREL,
        R_TILEGX_IMM16_X0_HW2_PCREL,
        R_TILEGX_IMM16_X1_HW2_PCREL,
        R_TILEGX_IMM16_X0_HW3_PCREL,
        R_TILEGX_IMM16_X1_HW3_PCREL,
        R_TILEGX_IMM16_X0_HW0_LAST_PCREL,
        R_TILEGX_IMM16_X1_HW0_LAST_PCREL,
        R_TILEGX_IMM16_X0_HW1_LAST_PCREL,
        R_TILEGX_IMM16_X1_HW1_LAST_PCREL,
        R_TILEGX_IMM16_X0_HW2_LAST_PCREL,
        R_TILEGX_IMM16_X1_HW2_LAST_PCREL,
        R_TILEGX_IMM16_X0_HW0_GOT,
        R_TILEGX_IMM16_X1_HW0_GOT,
        R_TILEGX_IMM16_X0_HW0_PLT_PCREL,
        R_TILEGX_IMM16_X1_HW0_PLT_PCREL,
        R_TILEGX_IMM16_X0_HW1_PLT_PCREL,
        R_TILEGX_IMM16_X1_HW1_PLT_PCREL,
        R_TILEGX_IMM16_X0_HW2_PLT_PCREL,
        R_TILEGX_IMM16_X1_HW2_PLT_PCREL,
        R_TILEGX_IMM16_X0_HW0_LAST_GOT,
        R_TILEGX_IMM16_X1_HW0_LAST_GOT,
        R_TILEGX_IMM16_X0_HW1_LAST_GOT,
        R_TILEGX_IMM16_X1_HW1_LAST_GOT,
        R_TILEGX_IMM16_X0_HW3_PLT_PCREL,
        R_TILEGX_IMM16_X1_HW3_PLT_PCREL,
        R_TILEGX_IMM16_X0_HW0_TLS_GD,
        R_TILEGX_IMM16_X1_HW0_TLS_GD,
        R_TILEGX_IMM16_X0_HW0_TLS_LE,
        R_TILEGX_IMM16_X1_HW0_TLS_LE,
        R_TILEGX_IMM16_X0_HW0_LAST_TLS_LE,
        R_TILEGX_IMM16_X1_HW0_LAST_TLS_LE,
        R_TILEGX_IMM16_X0_HW1_LAST_TLS_LE,
        R_TILEGX_IMM16_X1_HW1_LAST_TLS_LE,
        R_TILEGX_IMM16_X0_HW0_LAST_TLS_GD,
        R_TILEGX_IMM16_X1_HW0_LAST_TLS_GD,
        R_TILEGX_IMM16_X0_HW1_LAST_TLS_GD,
        R_TILEGX_IMM16_X1_HW1_LAST_TLS_GD,
        R_TILEGX_IMM16_X0_HW0_TLS_IE,
        R_TILEGX_IMM16_X1_HW0_TLS_IE,
        R_TILEGX_IMM16_X0_HW0_LAST_PLT_PCREL,
        R_TILEGX_IMM16_X1_HW0_LAST_PLT_PCREL,
        R_TILEGX_IMM16_X0_HW1_LAST_PLT_PCREL,
        R_TILEGX_IMM16_X1_HW1_LAST_PLT_PCREL,
        R_TILEGX_IMM16_X0_HW2_LAST_PLT_PCREL,
        R_TILEGX_IMM16_X1_HW2_LAST_PLT_PCREL,
        R_TILEGX_IMM16_X0_HW0_LAST_TLS_IE,
        R_TILEGX_IMM16_X1_HW0_LAST_TLS_IE,
        R_TILEGX_IMM16_X0_HW1_LAST_TLS_IE,
        R_TILEGX_IMM16_X1_HW1_LAST_TLS_IE,
        R_TILEGX_TLS_DTPMOD64,
        R_TILEGX_TLS_DTPOFF64,
        R_TILEGX_TLS_TPOFF64,
        R_TILEGX_TLS_DTPMOD32,
        R_TILEGX_TLS_DTPOFF32,
        R_TILEGX_TLS_TPOFF32,
        R_TILEGX_TLS_GD_CALL,
        R_TILEGX_IMM8_X0_TLS_GD_ADD,
        R_TILEGX_IMM8_X1_TLS_GD_ADD,
        R_TILEGX_IMM8_Y0_TLS_GD_ADD,
        R_TILEGX_IMM8_Y1_TLS_GD_ADD,
        R_TILEGX_TLS_IE_LOAD,
        R_TILEGX_IMM8_X0_TLS_ADD,
        R_TILEGX_IMM8_X1_TLS_ADD,
        R_TILEGX_IMM8_Y0_TLS_ADD,
        R_TILEGX_IMM8_Y1_TLS_ADD,
        R_TILEGX_GNU_VTINHERIT,
        R_TILEGX_GNU_VTENTRY,
    );
    static FLAGS_R_RISCV: &[Flag<u32>] = &flags!(
        R_RISCV_NONE,
        R_RISCV_32,
        R_RISCV_64,
        R_RISCV_RELATIVE,
        R_RISCV_COPY,
        R_RISCV_JUMP_SLOT,
        R_RISCV_TLS_DTPMOD32,
        R_RISCV_TLS_DTPMOD64,
        R_RISCV_TLS_DTPREL32,
        R_RISCV_TLS_DTPREL64,
        R_RISCV_TLS_TPREL32,
        R_RISCV_TLS_TPREL64,
        R_RISCV_BRANCH,
        R_RISCV_JAL,
        R_RISCV_CALL,
        R_RISCV_CALL_PLT,
        R_RISCV_GOT_HI20,
        R_RISCV_TLS_GOT_HI20,
        R_RISCV_TLS_GD_HI20,
        R_RISCV_PCREL_HI20,
        R_RISCV_PCREL_LO12_I,
        R_RISCV_PCREL_LO12_S,
        R_RISCV_HI20,
        R_RISCV_LO12_I,
        R_RISCV_LO12_S,
        R_RISCV_TPREL_HI20,
        R_RISCV_TPREL_LO12_I,
        R_RISCV_TPREL_LO12_S,
        R_RISCV_TPREL_ADD,
        R_RISCV_ADD8,
        R_RISCV_ADD16,
        R_RISCV_ADD32,
        R_RISCV_ADD64,
        R_RISCV_SUB8,
        R_RISCV_SUB16,
        R_RISCV_SUB32,
        R_RISCV_SUB64,
        R_RISCV_GNU_VTINHERIT,
        R_RISCV_GNU_VTENTRY,
        R_RISCV_ALIGN,
        R_RISCV_RVC_BRANCH,
        R_RISCV_RVC_JUMP,
        R_RISCV_RVC_LUI,
        R_RISCV_GPREL_I,
        R_RISCV_GPREL_S,
        R_RISCV_TPREL_I,
        R_RISCV_TPREL_S,
        R_RISCV_RELAX,
        R_RISCV_SUB6,
        R_RISCV_SET6,
        R_RISCV_SET8,
        R_RISCV_SET16,
        R_RISCV_SET32,
        R_RISCV_32_PCREL,
    );
    static FLAGS_R_BPF: &[Flag<u32>] = &flags!(R_BPF_NONE, R_BPF_64_64, R_BPF_64_32);
    static FLAGS_R_METAG: &[Flag<u32>] = &flags!(
        R_METAG_HIADDR16,
        R_METAG_LOADDR16,
        R_METAG_ADDR32,
        R_METAG_NONE,
        R_METAG_RELBRANCH,
        R_METAG_GETSETOFF,
        R_METAG_REG32OP1,
        R_METAG_REG32OP2,
        R_METAG_REG32OP3,
        R_METAG_REG16OP1,
        R_METAG_REG16OP2,
        R_METAG_REG16OP3,
        R_METAG_REG32OP4,
        R_METAG_HIOG,
        R_METAG_LOOG,
        R_METAG_REL8,
        R_METAG_REL16,
        R_METAG_GNU_VTINHERIT,
        R_METAG_GNU_VTENTRY,
        R_METAG_HI16_GOTOFF,
        R_METAG_LO16_GOTOFF,
        R_METAG_GETSET_GOTOFF,
        R_METAG_GETSET_GOT,
        R_METAG_HI16_GOTPC,
        R_METAG_LO16_GOTPC,
        R_METAG_HI16_PLT,
        R_METAG_LO16_PLT,
        R_METAG_RELBRANCH_PLT,
        R_METAG_GOTOFF,
        R_METAG_PLT,
        R_METAG_COPY,
        R_METAG_JMP_SLOT,
        R_METAG_RELATIVE,
        R_METAG_GLOB_DAT,
        R_METAG_TLS_GD,
        R_METAG_TLS_LDM,
        R_METAG_TLS_LDO_HI16,
        R_METAG_TLS_LDO_LO16,
        R_METAG_TLS_LDO,
        R_METAG_TLS_IE,
        R_METAG_TLS_IENONPIC,
        R_METAG_TLS_IENONPIC_HI16,
        R_METAG_TLS_IENONPIC_LO16,
        R_METAG_TLS_TPOFF,
        R_METAG_TLS_DTPMOD,
        R_METAG_TLS_DTPOFF,
        R_METAG_TLS_LE,
        R_METAG_TLS_LE_HI16,
        R_METAG_TLS_LE_LO16,
    );
    static FLAGS_R_NDS32: &[Flag<u32>] = &flags!(
        R_NDS32_NONE,
        R_NDS32_32_RELA,
        R_NDS32_COPY,
        R_NDS32_GLOB_DAT,
        R_NDS32_JMP_SLOT,
        R_NDS32_RELATIVE,
        R_NDS32_TLS_TPOFF,
        R_NDS32_TLS_DESC,
    );
    static FLAGS_NT_CORE: &[Flag<u32>] = &flags!(
        NT_PRSTATUS,
        NT_PRFPREG,
        NT_FPREGSET,
        NT_PRPSINFO,
        NT_PRXREG,
        NT_TASKSTRUCT,
        NT_PLATFORM,
        NT_AUXV,
        NT_GWINDOWS,
        NT_ASRS,
        NT_PSTATUS,
        NT_PSINFO,
        NT_PRCRED,
        NT_UTSNAME,
        NT_LWPSTATUS,
        NT_LWPSINFO,
        NT_PRFPXREG,
        NT_SIGINFO,
        NT_FILE,
        NT_PRXFPREG,
        NT_PPC_VMX,
        NT_PPC_SPE,
        NT_PPC_VSX,
        NT_PPC_TAR,
        NT_PPC_PPR,
        NT_PPC_DSCR,
        NT_PPC_EBB,
        NT_PPC_PMU,
        NT_PPC_TM_CGPR,
        NT_PPC_TM_CFPR,
        NT_PPC_TM_CVMX,
        NT_PPC_TM_CVSX,
        NT_PPC_TM_SPR,
        NT_PPC_TM_CTAR,
        NT_PPC_TM_CPPR,
        NT_PPC_TM_CDSCR,
        NT_PPC_PKEY,
        NT_386_TLS,
        NT_386_IOPERM,
        NT_X86_XSTATE,
        NT_S390_HIGH_GPRS,
        NT_S390_TIMER,
        NT_S390_TODCMP,
        NT_S390_TODPREG,
        NT_S390_CTRS,
        NT_S390_PREFIX,
        NT_S390_LAST_BREAK,
        NT_S390_SYSTEM_CALL,
        NT_S390_TDB,
        NT_S390_VXRS_LOW,
        NT_S390_VXRS_HIGH,
        NT_S390_GS_CB,
        NT_S390_GS_BC,
        NT_S390_RI_CB,
        NT_ARM_VFP,
        NT_ARM_TLS,
        NT_ARM_HW_BREAK,
        NT_ARM_HW_WATCH,
        NT_ARM_SYSTEM_CALL,
        NT_ARM_SVE,
        NT_VMCOREDD,
        NT_MIPS_DSP,
        NT_MIPS_FP_MODE,
    );
    static FLAGS_NT_SOLARIS: &[Flag<u32>] = &flags!(NT_SOLARIS_PAGESIZE_HINT);
    static FLAGS_NT_GNU: &[Flag<u32>] = &flags!(
        NT_GNU_ABI_TAG,
        NT_GNU_HWCAP,
        NT_GNU_BUILD_ID,
        NT_GNU_GOLD_VERSION,
        NT_GNU_PROPERTY_TYPE_0,
    );
    static FLAGS_GRP: &[Flag<u32>] = &flags!(GRP_COMDAT);
    static FLAGS_DT: &[Flag<u32>] = &flags!(
        DT_NULL,
        DT_NEEDED,
        DT_PLTRELSZ,
        DT_PLTGOT,
        DT_HASH,
        DT_STRTAB,
        DT_SYMTAB,
        DT_RELA,
        DT_RELASZ,
        DT_RELAENT,
        DT_STRSZ,
        DT_SYMENT,
        DT_INIT,
        DT_FINI,
        DT_SONAME,
        DT_RPATH,
        DT_SYMBOLIC,
        DT_REL,
        DT_RELSZ,
        DT_RELENT,
        DT_PLTREL,
        DT_DEBUG,
        DT_TEXTREL,
        DT_JMPREL,
        DT_BIND_NOW,
        DT_INIT_ARRAY,
        DT_FINI_ARRAY,
        DT_INIT_ARRAYSZ,
        DT_FINI_ARRAYSZ,
        DT_RUNPATH,
        DT_FLAGS,
        DT_PREINIT_ARRAY,
        DT_PREINIT_ARRAYSZ,
        DT_SYMTAB_SHNDX,
        DT_GNU_PRELINKED,
        DT_GNU_CONFLICTSZ,
        DT_GNU_LIBLISTSZ,
        DT_CHECKSUM,
        DT_PLTPADSZ,
        DT_MOVEENT,
        DT_MOVESZ,
        DT_FEATURE_1,
        DT_POSFLAG_1,
        DT_SYMINSZ,
        DT_SYMINENT,
        DT_GNU_HASH,
        DT_TLSDESC_PLT,
        DT_TLSDESC_GOT,
        DT_GNU_CONFLICT,
        DT_GNU_LIBLIST,
        DT_CONFIG,
        DT_DEPAUDIT,
        DT_AUDIT,
        DT_PLTPAD,
        DT_MOVETAB,
        DT_SYMINFO,
        DT_VERSYM,
        DT_RELACOUNT,
        DT_RELCOUNT,
        DT_FLAGS_1,
        DT_VERDEF,
        DT_VERDEFNUM,
        DT_VERNEED,
        DT_VERNEEDNUM,
        DT_AUXILIARY,
        DT_FILTER,
    );
    static FLAGS_DT_SPARC: &[Flag<u32>] = &flags!(DT_SPARC_REGISTER);
    static FLAGS_DT_MIPS: &[Flag<u32>] = &flags!(
        DT_MIPS_RLD_VERSION,
        DT_MIPS_TIME_STAMP,
        DT_MIPS_ICHECKSUM,
        DT_MIPS_IVERSION,
        DT_MIPS_FLAGS,
        DT_MIPS_BASE_ADDRESS,
        DT_MIPS_MSYM,
        DT_MIPS_CONFLICT,
        DT_MIPS_LIBLIST,
        DT_MIPS_LOCAL_GOTNO,
        DT_MIPS_CONFLICTNO,
        DT_MIPS_LIBLISTNO,
        DT_MIPS_SYMTABNO,
        DT_MIPS_UNREFEXTNO,
        DT_MIPS_GOTSYM,
        DT_MIPS_HIPAGENO,
        DT_MIPS_RLD_MAP,
        DT_MIPS_DELTA_CLASS,
        DT_MIPS_DELTA_CLASS_NO,
        DT_MIPS_DELTA_INSTANCE,
        DT_MIPS_DELTA_INSTANCE_NO,
        DT_MIPS_DELTA_RELOC,
        DT_MIPS_DELTA_RELOC_NO,
        DT_MIPS_DELTA_SYM,
        DT_MIPS_DELTA_SYM_NO,
        DT_MIPS_DELTA_CLASSSYM,
        DT_MIPS_DELTA_CLASSSYM_NO,
        DT_MIPS_CXX_FLAGS,
        DT_MIPS_PIXIE_INIT,
        DT_MIPS_SYMBOL_LIB,
        DT_MIPS_LOCALPAGE_GOTIDX,
        DT_MIPS_LOCAL_GOTIDX,
        DT_MIPS_HIDDEN_GOTIDX,
        DT_MIPS_PROTECTED_GOTIDX,
        DT_MIPS_OPTIONS,
        DT_MIPS_INTERFACE,
        DT_MIPS_DYNSTR_ALIGN,
        DT_MIPS_INTERFACE_SIZE,
        DT_MIPS_RLD_TEXT_RESOLVE_ADDR,
        DT_MIPS_PERF_SUFFIX,
        DT_MIPS_COMPACT_SIZE,
        DT_MIPS_GP_VALUE,
        DT_MIPS_AUX_DYNAMIC,
        DT_MIPS_PLTGOT,
        DT_MIPS_RWPLT,
        DT_MIPS_RLD_MAP_REL,
    );
    static FLAGS_DT_ALPHA: &[Flag<u32>] = &flags!(DT_ALPHA_PLTRO);
    static FLAGS_DT_PPC: &[Flag<u32>] = &flags!(DT_PPC_GOT, DT_PPC_OPT);
    static FLAGS_DT_PPC64: &[Flag<u32>] =
        &flags!(DT_PPC64_GLINK, DT_PPC64_OPD, DT_PPC64_OPDSZ, DT_PPC64_OPT);
    static FLAGS_DT_IA_64: &[Flag<u32>] = &flags!(DT_IA_64_PLT_RESERVE);
    static FLAGS_DT_NIOS2: &[Flag<u32>] = &flags!(DT_NIOS2_GP);
    static FLAGS_DF: &[Flag<u32>] = &flags!(
        DF_ORIGIN,
        DF_SYMBOLIC,
        DF_TEXTREL,
        DF_BIND_NOW,
        DF_STATIC_TLS,
    );
    static FLAGS_DF_1: &[Flag<u32>] = &flags!(
        DF_1_NOW,
        DF_1_GLOBAL,
        DF_1_GROUP,
        DF_1_NODELETE,
        DF_1_LOADFLTR,
        DF_1_INITFIRST,
        DF_1_NOOPEN,
        DF_1_ORIGIN,
        DF_1_DIRECT,
        DF_1_TRANS,
        DF_1_INTERPOSE,
        DF_1_NODEFLIB,
        DF_1_NODUMP,
        DF_1_CONFALT,
        DF_1_ENDFILTEE,
        DF_1_DISPRELDNE,
        DF_1_DISPRELPND,
        DF_1_NODIRECT,
        DF_1_IGNMULDEF,
        DF_1_NOKSYMS,
        DF_1_NOHDR,
        DF_1_EDITED,
        DF_1_NORELOC,
        DF_1_SYMINTPOSE,
        DF_1_GLOBAUDIT,
        DF_1_SINGLETON,
        DF_1_STUB,
        DF_1_PIE,
    );
}

mod macho {
    use super::*;
    use object::macho::*;
    use object::read::macho::*;
    use object::BigEndian;

    pub(super) fn print_dyld_cache(p: &mut Printer<impl Write>, data: &[u8]) {
        if let Ok(header) = DyldCacheHeader::<Endianness>::parse(data) {
            if let Ok((_, endian)) = header.parse_magic() {
                print_dyld_cache_header(p, endian, header);
                let mappings = header.mappings(endian, data).ok();
                if let Some(mappings) = mappings {
                    print_dyld_cache_mappings(p, endian, mappings);
                }
                if let Ok(images) = header.images(endian, data) {
                    print_dyld_cache_images(p, endian, data, mappings, images);
                }
            }
        }
    }

    pub(super) fn print_dyld_cache_header(
        p: &mut Printer<impl Write>,
        endian: Endianness,
        header: &DyldCacheHeader<Endianness>,
    ) {
        p.group("DyldCacheHeader", |p| {
            p.field_bytes("Magic", &header.magic);
            p.field_hex("MappingOffset", header.mapping_offset.get(endian));
            p.field("MappingCount", header.mapping_count.get(endian));
            p.field_hex("ImagesOffset", header.images_offset.get(endian));
            p.field("ImagesCount", header.images_count.get(endian));
            p.field_hex("DyldBaseAddress", header.dyld_base_address.get(endian));
        });
    }

    pub(super) fn print_dyld_cache_mappings(
        p: &mut Printer<impl Write>,
        endian: Endianness,
        mappings: &[DyldCacheMappingInfo<Endianness>],
    ) {
        for mapping in mappings {
            p.group("DyldCacheMappingInfo", |p| {
                p.field_hex("Address", mapping.address.get(endian));
                p.field_hex("Size", mapping.size.get(endian));
                p.field_hex("FileOffset", mapping.file_offset.get(endian));
                p.field_hex("MaxProt", mapping.max_prot.get(endian));
                p.flags(mapping.max_prot.get(endian), 0, FLAGS_VM);
                p.field_hex("InitProt", mapping.init_prot.get(endian));
                p.flags(mapping.init_prot.get(endian), 0, FLAGS_VM);
            });
        }
    }

    pub(super) fn print_dyld_cache_images(
        p: &mut Printer<impl Write>,
        endian: Endianness,
        data: &[u8],
        mappings: Option<&[DyldCacheMappingInfo<Endianness>]>,
        images: &[DyldCacheImageInfo<Endianness>],
    ) {
        for image in images {
            p.group("DyldCacheImageInfo", |p| {
                p.field_hex("Address", image.address.get(endian));
                p.field_hex("ModTime", image.mod_time.get(endian));
                p.field_hex("Inode", image.inode.get(endian));
                p.field_string(
                    "Path",
                    image.path_file_offset.get(endian),
                    image.path(endian, data).ok(),
                );
                p.field_hex("Pad", image.pad.get(endian));
            });
            if let Some(offset) =
                mappings.and_then(|mappings| image.file_offset(endian, mappings).ok())
            {
                p.blank();
                print_object_at(p, data, offset);
                p.blank();
            }
        }
    }

    pub(super) fn print_macho_fat32(p: &mut Printer<impl Write>, data: &[u8]) {
        if let Ok(arches) = FatHeader::parse_arch32(data) {
            println!("Format: Mach-O Fat 32-bit");
            print_fat_header(p, data);
            for arch in arches {
                print_fat_arch(p, arch);
            }
            for arch in arches {
                if let Ok(data) = arch.data(data) {
                    p.blank();
                    print_object(p, data);
                }
            }
        }
    }

    pub(super) fn print_macho_fat64(p: &mut Printer<impl Write>, data: &[u8]) {
        if let Ok(arches) = FatHeader::parse_arch64(data) {
            println!("Format: Mach-O Fat 64-bit");
            print_fat_header(p, data);
            for arch in arches {
                print_fat_arch(p, arch);
            }
            for arch in arches {
                if let Ok(data) = arch.data(data) {
                    p.blank();
                    print_object(p, data);
                }
            }
        }
    }

    pub(super) fn print_fat_header(p: &mut Printer<impl Write>, data: &[u8]) {
        if let Ok(header) = FatHeader::parse(data) {
            p.group("FatHeader", |p| {
                p.field_hex("Magic", header.magic.get(BigEndian));
                p.field("NumberOfFatArch", header.nfat_arch.get(BigEndian));
            });
        }
    }

    pub(super) fn print_fat_arch<Arch: FatArch>(p: &mut Printer<impl Write>, arch: &Arch) {
        p.group("FatArch", |p| {
            print_cputype(p, arch.cputype(), arch.cpusubtype());
            p.field_hex("Offset", arch.offset().into());
            p.field_hex("Size", arch.size().into());
            p.field("Align", arch.align());
        });
    }

    pub(super) fn print_macho32(p: &mut Printer<impl Write>, data: &[u8], offset: u64) {
        if let Ok(header) = MachHeader32::parse(data, offset) {
            println!("Format: Mach-O 32-bit");
            print_macho(p, header, data, offset);
        }
    }

    pub(super) fn print_macho64(p: &mut Printer<impl Write>, data: &[u8], offset: u64) {
        if let Ok(header) = MachHeader64::parse(data, offset) {
            println!("Format: Mach-O 64-bit");
            print_macho(p, header, data, offset);
        }
    }

    #[derive(Default)]
    struct MachState {
        section_index: usize,
    }

    fn print_macho<Mach: MachHeader<Endian = Endianness>>(
        p: &mut Printer<impl Write>,
        header: &Mach,
        data: &[u8],
        offset: u64,
    ) {
        if let Ok(endian) = header.endian() {
            let mut state = MachState::default();
            print_mach_header(p, endian, header);
            if let Ok(mut commands) = header.load_commands(endian, data, offset) {
                while let Ok(Some(command)) = commands.next() {
                    print_load_command(p, endian, data, header, command, &mut state);
                }
            }
        }
    }

    fn print_mach_header<Mach: MachHeader>(
        p: &mut Printer<impl Write>,
        endian: Mach::Endian,
        header: &Mach,
    ) {
        p.group("MachHeader", |p| {
            p.field_hex("Magic", header.magic().to_be());
            print_cputype(p, header.cputype(endian), header.cpusubtype(endian));
            p.field_enum("FileType", header.filetype(endian), FLAGS_MH_FILETYPE);
            p.field("NumberOfCmds", header.ncmds(endian));
            p.field_hex("SizeOfCmds", header.sizeofcmds(endian));
            p.field_enum("Flags", header.flags(endian), FLAGS_MH);
        });
    }

    fn print_load_command<Mach: MachHeader>(
        p: &mut Printer<impl Write>,
        endian: Mach::Endian,
        data: &[u8],
        header: &Mach,
        command: LoadCommandData<Mach::Endian>,
        state: &mut MachState,
    ) {
        if let Ok(variant) = command.variant() {
            match variant {
                LoadCommandVariant::Segment32(segment, section_data) => {
                    print_segment(
                        p,
                        endian,
                        data,
                        header.cputype(endian),
                        segment,
                        section_data,
                        state,
                    );
                }
                LoadCommandVariant::Segment64(segment, section_data) => {
                    print_segment(
                        p,
                        endian,
                        data,
                        header.cputype(endian),
                        segment,
                        section_data,
                        state,
                    );
                }
                LoadCommandVariant::Symtab(symtab) => {
                    print_symtab::<Mach, _>(p, endian, data, symtab);
                }
                LoadCommandVariant::Thread(x, _thread_data) => {
                    p.group("ThreadCommand", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        // TODO: thread_data
                    });
                }
                LoadCommandVariant::Dysymtab(x) => {
                    p.group("DysymtabCommand", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        // TODO: dump the tables these are all pointing to
                        p.field("IndexOfLocalSymbols", x.ilocalsym.get(endian));
                        p.field("NumberOfLocalSymbols", x.nlocalsym.get(endian));
                        p.field("IndexOfExternallyDefinedSymbols", x.iextdefsym.get(endian));
                        p.field("NumberOfExternallyDefinedSymbols", x.nextdefsym.get(endian));
                        p.field("IndexOfUndefinedSymbols", x.iundefsym.get(endian));
                        p.field("NumberOfUndefinedSymbols", x.nundefsym.get(endian));
                        p.field_hex("TocOffset", x.tocoff.get(endian));
                        p.field("NumberOfTocEntries", x.ntoc.get(endian));
                        p.field_hex("ModuleTableOffset", x.modtaboff.get(endian));
                        p.field("NumberOfModuleTableEntries", x.nmodtab.get(endian));
                        p.field_hex("ExternalRefSymbolOffset", x.extrefsymoff.get(endian));
                        p.field("NumberOfExternalRefSymbols", x.nextrefsyms.get(endian));
                        p.field_hex("IndirectSymbolOffset", x.indirectsymoff.get(endian));
                        p.field("NumberOfIndirectSymbols", x.nindirectsyms.get(endian));
                        p.field_hex("ExternalRelocationOffset", x.extreloff.get(endian));
                        p.field("NumberOfExternalRelocations", x.nextrel.get(endian));
                        p.field_hex("LocalRelocationOffset", x.locreloff.get(endian));
                        p.field("NumberOfLocalRelocations", x.nlocrel.get(endian));
                    });
                }
                LoadCommandVariant::Dylib(x) | LoadCommandVariant::IdDylib(x) => {
                    p.group("DylibCommand", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        p.group("Dylib", |p| {
                            p.field_string(
                                "Name",
                                x.dylib.name.offset.get(endian),
                                command.string(endian, x.dylib.name).ok(),
                            );
                            p.field("Timestamp", x.dylib.timestamp.get(endian));
                            p.field_hex("CurrentVersion", x.dylib.current_version.get(endian));
                            p.field_hex(
                                "CompatibilityVersion",
                                x.dylib.compatibility_version.get(endian),
                            );
                        });
                    });
                }
                LoadCommandVariant::LoadDylinker(x)
                | LoadCommandVariant::IdDylinker(x)
                | LoadCommandVariant::DyldEnvironment(x) => {
                    p.group("DylinkerCommand", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        p.field_string(
                            "Name",
                            x.name.offset.get(endian),
                            command.string(endian, x.name).ok(),
                        );
                    });
                }
                LoadCommandVariant::PreboundDylib(x) => {
                    p.group("PreboundDylibCommand", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        p.field_string(
                            "Name",
                            x.name.offset.get(endian),
                            command.string(endian, x.name).ok(),
                        );
                        p.field("NumberOfModules", x.nmodules.get(endian));
                        // TODO: display bit vector
                        p.field_hex("LinkedModules", x.linked_modules.offset.get(endian));
                    });
                }
                LoadCommandVariant::Routines32(x) => {
                    p.group("RoutinesCommand32", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        p.field_hex("InitAddress", x.init_address.get(endian));
                        p.field_hex("InitModule", x.init_module.get(endian));
                        p.field_hex("Reserved1", x.reserved1.get(endian));
                        p.field_hex("Reserved2", x.reserved2.get(endian));
                        p.field_hex("Reserved3", x.reserved3.get(endian));
                        p.field_hex("Reserved4", x.reserved4.get(endian));
                        p.field_hex("Reserved5", x.reserved5.get(endian));
                        p.field_hex("Reserved6", x.reserved6.get(endian));
                    });
                }
                LoadCommandVariant::Routines64(x) => {
                    p.group("RoutinesCommand64", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        p.field_hex("InitAddress", x.init_address.get(endian));
                        p.field_hex("InitModule", x.init_module.get(endian));
                        p.field_hex("Reserved1", x.reserved1.get(endian));
                        p.field_hex("Reserved2", x.reserved2.get(endian));
                        p.field_hex("Reserved3", x.reserved3.get(endian));
                        p.field_hex("Reserved4", x.reserved4.get(endian));
                        p.field_hex("Reserved5", x.reserved5.get(endian));
                        p.field_hex("Reserved6", x.reserved6.get(endian));
                    });
                }
                LoadCommandVariant::SubFramework(x) => {
                    p.group("SubFrameworkCommand", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        p.field_string(
                            "Umbrella",
                            x.umbrella.offset.get(endian),
                            command.string(endian, x.umbrella).ok(),
                        );
                    });
                }
                LoadCommandVariant::SubUmbrella(x) => {
                    p.group("SubUmbrellaCommand", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        p.field_string(
                            "SubUmbrella",
                            x.sub_umbrella.offset.get(endian),
                            command.string(endian, x.sub_umbrella).ok(),
                        );
                    });
                }
                LoadCommandVariant::SubClient(x) => {
                    p.group("SubClientCommand", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        p.field_string(
                            "Client",
                            x.client.offset.get(endian),
                            command.string(endian, x.client).ok(),
                        );
                    });
                }
                LoadCommandVariant::SubLibrary(x) => {
                    p.group("SubLibraryCommand", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        p.field_string(
                            "SubLibrary",
                            x.sub_library.offset.get(endian),
                            command.string(endian, x.sub_library).ok(),
                        );
                    });
                }
                LoadCommandVariant::TwolevelHints(x) => {
                    p.group("TwolevelHintsCommand", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        p.field_hex("Offset", x.offset.get(endian));
                        p.field_hex("NumberOfHints", x.nhints.get(endian));
                        // TODO: display hints
                    });
                }
                LoadCommandVariant::PrebindCksum(x) => {
                    p.group("PrebindCksumCommand", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        p.field_hex("Cksum", x.cksum.get(endian));
                    });
                }
                LoadCommandVariant::Uuid(x) => {
                    p.group("UuidCommand", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        p.field("Uuid", format!("{:X?}", x.uuid));
                    });
                }
                LoadCommandVariant::Rpath(x) => {
                    p.group("RpathCommand", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        p.field_string(
                            "Path",
                            x.path.offset.get(endian),
                            command.string(endian, x.path).ok(),
                        );
                    });
                }
                LoadCommandVariant::LinkeditData(x) => {
                    p.group("LinkeditDataCommand", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        p.field_hex("DataOffset", x.dataoff.get(endian));
                        p.field_hex("DataSize", x.datasize.get(endian));
                    });
                }
                LoadCommandVariant::EncryptionInfo32(x) => {
                    p.group("EncryptionInfoCommand32", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        p.field_hex("CryptOffset", x.cryptoff.get(endian));
                        p.field_hex("CryptSize", x.cryptsize.get(endian));
                        p.field_hex("CryptId", x.cryptid.get(endian));
                    });
                }
                LoadCommandVariant::EncryptionInfo64(x) => {
                    p.group("EncryptionInfoCommand64", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        p.field_hex("CryptOffset", x.cryptoff.get(endian));
                        p.field_hex("CryptSize", x.cryptsize.get(endian));
                        p.field_hex("CryptId", x.cryptid.get(endian));
                        p.field_hex("Pad", x.pad.get(endian));
                    });
                }
                LoadCommandVariant::DyldInfo(x) => {
                    p.group("DyldInfoCommand", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        // TODO: dump the tables these are all pointing to
                        p.field_hex("RebaseOffset", x.rebase_off.get(endian));
                        p.field_hex("RebaseSize", x.rebase_size.get(endian));
                        p.field_hex("BindOffset", x.bind_off.get(endian));
                        p.field_hex("BindSize", x.bind_size.get(endian));
                        p.field_hex("WeakBindOffset", x.weak_bind_off.get(endian));
                        p.field_hex("WeakBindSize", x.weak_bind_size.get(endian));
                        p.field_hex("LazyBindOffset", x.lazy_bind_off.get(endian));
                        p.field_hex("LazyBindSize", x.lazy_bind_size.get(endian));
                        p.field_hex("ExportOffset", x.export_off.get(endian));
                        p.field_hex("ExportSize", x.export_size.get(endian));
                    });
                }
                LoadCommandVariant::VersionMin(x) => {
                    p.group("VersionMinCommand", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        p.field_hex("Version", x.version.get(endian));
                        p.field_hex("Sdk", x.sdk.get(endian));
                    });
                }
                LoadCommandVariant::EntryPoint(x) => {
                    p.group("EntryPointCommand", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        p.field_hex("EntryOffset", x.entryoff.get(endian));
                        p.field_hex("StackSize", x.stacksize.get(endian));
                    });
                }
                LoadCommandVariant::SourceVersion(x) => {
                    p.group("SourceVersionCommand", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        p.field_hex("Version", x.version.get(endian));
                    });
                }
                LoadCommandVariant::LinkerOption(x) => {
                    p.group("LinkerOptionCommand", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        p.field_hex("Count", x.count.get(endian));
                        // TODO: dump strings
                    });
                }
                LoadCommandVariant::Note(x) => {
                    p.group("NoteCommand", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        // TODO: string?
                        p.field("DataOwner", format!("{:X?}", x.data_owner));
                        p.field_hex("Offset", x.offset.get(endian));
                        p.field_hex("Size", x.size.get(endian));
                    });
                }
                LoadCommandVariant::BuildVersion(x) => {
                    p.group("BuildVersionCommand", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        p.field_enum("Platform", x.platform.get(endian), FLAGS_PLATFORM);
                        p.field_hex("MinOs", x.minos.get(endian));
                        p.field_hex("Sdk", x.sdk.get(endian));
                        p.field_hex("NumberOfTools", x.ntools.get(endian));
                        // TODO: dump tools
                    });
                }
                LoadCommandVariant::FilesetEntry(x) => {
                    p.group("FilesetEntryCommand", |p| {
                        p.field_enum("Cmd", x.cmd.get(endian), FLAGS_LC);
                        p.field_hex("CmdSize", x.cmdsize.get(endian));
                        p.field_hex("VmAddress", x.vmaddr.get(endian));
                        p.field_hex("FileOffset", x.fileoff.get(endian));
                        p.field_string(
                            "EntryId",
                            x.entry_id.offset.get(endian),
                            command.string(endian, x.entry_id).ok(),
                        );
                        p.field_hex("Reserved", x.reserved.get(endian));
                    });
                }
                _ => {
                    p.group("LoadCommand", |p| {
                        p.field_enum("Cmd", command.cmd(), FLAGS_LC);
                        p.field_hex("CmdSize", command.cmdsize());
                    });
                }
            }
        } else {
            p.group("LoadCommand", |p| {
                p.field_enum("Cmd", command.cmd(), FLAGS_LC);
                p.field_hex("CmdSize", command.cmdsize());
            });
        }
    }

    fn print_segment<S: Segment>(
        p: &mut Printer<impl Write>,
        endian: S::Endian,
        data: &[u8],
        cputype: u32,
        segment: &S,
        section_data: &[u8],
        state: &mut MachState,
    ) {
        p.group("SegmentCommand", |p| {
            p.field_enum("Cmd", segment.cmd(endian), FLAGS_LC);
            p.field_hex("CmdSize", segment.cmdsize(endian));
            p.field_inline_string("SegmentName", segment.name());
            p.field_hex("VmAddress", segment.vmaddr(endian).into());
            p.field_hex("VmSize", segment.vmsize(endian).into());
            p.field_hex("FileOffset", segment.fileoff(endian).into());
            p.field_hex("FileSize", segment.filesize(endian).into());
            p.field_hex("MaxProt", segment.maxprot(endian));
            p.flags(segment.maxprot(endian), 0, FLAGS_VM);
            p.field_hex("InitProt", segment.initprot(endian));
            p.flags(segment.initprot(endian), 0, FLAGS_VM);
            p.field("NumberOfSections", segment.nsects(endian));
            p.field_hex("Flags", segment.flags(endian));
            p.flags(segment.flags(endian), 0, FLAGS_SG);
            if let Ok(sections) = segment.sections(endian, section_data) {
                for section in sections {
                    print_section(p, endian, data, cputype, section, state);
                }
            }
        });
    }

    fn print_section<S: Section>(
        p: &mut Printer<impl Write>,
        endian: S::Endian,
        data: &[u8],
        cputype: u32,
        section: &S,
        state: &mut MachState,
    ) {
        p.group("Section", |p| {
            p.field("Index", state.section_index);
            state.section_index += 1;
            p.field_inline_string("SectionName", section.name());
            p.field_inline_string("SegmentName", section.segment_name());
            p.field_hex("Address", section.addr(endian).into());
            p.field_hex("Size", section.size(endian).into());
            p.field_hex("Offset", section.offset(endian));
            p.field_hex("Align", section.align(endian));
            p.field_hex("RelocationOffset", section.reloff(endian));
            p.field_hex("NumberOfRelocations", section.nreloc(endian));
            let flags = section.flags(endian);
            if flags & SECTION_TYPE == flags {
                p.field_enum("Flags", flags, FLAGS_S_TYPE);
            } else {
                p.field_hex("Flags", section.flags(endian));
                p.flags(flags, SECTION_TYPE, FLAGS_S_TYPE);
                p.flags(flags, 0, FLAGS_S_ATTR);
            }
            if let Ok(relocations) = section.relocations(endian, data) {
                let proc = match cputype {
                    CPU_TYPE_X86 => FLAGS_GENERIC_RELOC,
                    CPU_TYPE_X86_64 => FLAGS_X86_64_RELOC,
                    CPU_TYPE_ARM => FLAGS_ARM_RELOC,
                    CPU_TYPE_ARM64 | CPU_TYPE_ARM64_32 => FLAGS_ARM64_RELOC,
                    CPU_TYPE_POWERPC | CPU_TYPE_POWERPC64 => FLAGS_PPC_RELOC,
                    _ => &[],
                };
                for relocation in relocations {
                    if relocation.r_scattered(endian, cputype) {
                        let info = relocation.scattered_info(endian);
                        p.group("ScatteredRelocationInfo", |p| {
                            p.field_hex("Address", info.r_address);
                            p.field("PcRel", if info.r_pcrel { "yes" } else { "no" });
                            p.field("Length", info.r_length);
                            p.field_enum("Type", info.r_type, proc);
                            p.field_hex("Value", info.r_value);
                        });
                    } else {
                        let info = relocation.info(endian);
                        p.group("RelocationInfo", |p| {
                            p.field_hex("Address", info.r_address);
                            if info.r_extern {
                                // TODO: symbol name
                                p.field("Symbol", info.r_symbolnum);
                            } else {
                                p.field("Section", info.r_symbolnum);
                            }
                            p.field("PcRel", if info.r_pcrel { "yes" } else { "no" });
                            p.field("Length", info.r_length);
                            p.field("Extern", if info.r_extern { "yes" } else { "no" });
                            p.field_enum("Type", info.r_type, proc);
                        });
                    }
                }
            }
        });
    }

    fn print_symtab<Mach: MachHeader, W: Write>(
        p: &mut Printer<W>,
        endian: Mach::Endian,
        data: &[u8],
        symtab: &SymtabCommand<Mach::Endian>,
    ) {
        p.group("SymtabCommand", |p| {
            p.field_enum("Cmd", symtab.cmd.get(endian), FLAGS_LC);
            p.field_hex("CmdSize", symtab.cmdsize.get(endian));
            p.field_hex("SymbolOffset", symtab.symoff.get(endian));
            p.field_hex("NumberOfSymbols", symtab.nsyms.get(endian));
            p.field_hex("StringOffset", symtab.stroff.get(endian));
            p.field_hex("StringSize", symtab.strsize.get(endian));
            if let Ok(symbols) = symtab.symbols::<Mach, _>(endian, data) {
                for (index, nlist) in symbols.iter().enumerate() {
                    p.group("Nlist", |p| {
                        p.field("Index", index);
                        p.field_string(
                            "String",
                            nlist.n_strx(endian),
                            nlist.name(endian, symbols.strings()).ok(),
                        );
                        let n_type = nlist.n_type();
                        if nlist.is_stab() {
                            p.field_enum("Type", n_type, FLAGS_N_STAB);
                        } else if n_type & N_TYPE == n_type {
                            // Avoid an extra line if no flags.
                            p.field_enum("Type", n_type, FLAGS_N_TYPE);
                        } else {
                            p.field_hex("Type", n_type);
                            p.flags(n_type, N_TYPE, FLAGS_N_TYPE);
                            p.flags(n_type, 0, FLAGS_N_EXT);
                        }
                        p.field("Section", nlist.n_sect());
                        let n_desc = nlist.n_desc(endian);
                        p.field_hex("Desc", n_desc);
                        if nlist.is_undefined() {
                            p.flags(n_desc, REFERENCE_TYPE, FLAGS_REFERENCE);
                        }
                        if !nlist.is_stab() {
                            p.flags(n_desc, 0, FLAGS_N_DESC);
                        }
                        p.field_hex("Value", nlist.n_value(endian).into());
                    });
                }
            }
        });
    }

    fn print_cputype(p: &mut Printer<impl Write>, cputype: u32, cpusubtype: u32) {
        let proc = match cputype {
            CPU_TYPE_ANY => FLAGS_CPU_SUBTYPE_ANY,
            CPU_TYPE_VAX => FLAGS_CPU_SUBTYPE_VAX,
            CPU_TYPE_MC680X0 => FLAGS_CPU_SUBTYPE_MC680X0,
            CPU_TYPE_X86 => FLAGS_CPU_SUBTYPE_X86,
            CPU_TYPE_X86_64 => FLAGS_CPU_SUBTYPE_X86_64,
            CPU_TYPE_MIPS => FLAGS_CPU_SUBTYPE_MIPS,
            CPU_TYPE_MC98000 => FLAGS_CPU_SUBTYPE_MC98000,
            CPU_TYPE_HPPA => FLAGS_CPU_SUBTYPE_HPPA,
            CPU_TYPE_ARM => FLAGS_CPU_SUBTYPE_ARM,
            CPU_TYPE_ARM64 => FLAGS_CPU_SUBTYPE_ARM64,
            CPU_TYPE_ARM64_32 => FLAGS_CPU_SUBTYPE_ARM64_32,
            CPU_TYPE_MC88000 => FLAGS_CPU_SUBTYPE_MC88000,
            CPU_TYPE_SPARC => FLAGS_CPU_SUBTYPE_SPARC,
            CPU_TYPE_I860 => FLAGS_CPU_SUBTYPE_I860,
            CPU_TYPE_POWERPC | CPU_TYPE_POWERPC64 => FLAGS_CPU_SUBTYPE_POWERPC,
            _ => &[],
        };
        p.field_enum("CpuType", cputype, FLAGS_CPU_TYPE);
        p.field_hex("CpuSubtype", cpusubtype);
        p.flags(cpusubtype, !CPU_SUBTYPE_MASK, proc);
        p.flags(cpusubtype, 0, FLAGS_CPU_SUBTYPE);
    }

    static FLAGS_CPU_TYPE: &[Flag<u32>] = &flags!(
        CPU_TYPE_ANY,
        CPU_TYPE_VAX,
        CPU_TYPE_MC680X0,
        CPU_TYPE_X86,
        CPU_TYPE_X86_64,
        CPU_TYPE_MIPS,
        CPU_TYPE_MC98000,
        CPU_TYPE_HPPA,
        CPU_TYPE_ARM,
        CPU_TYPE_ARM64,
        CPU_TYPE_ARM64_32,
        CPU_TYPE_MC88000,
        CPU_TYPE_SPARC,
        CPU_TYPE_I860,
        CPU_TYPE_ALPHA,
        CPU_TYPE_POWERPC,
        CPU_TYPE_POWERPC64,
    );
    static FLAGS_CPU_SUBTYPE: &[Flag<u32>] = &flags!(CPU_SUBTYPE_LIB64);
    static FLAGS_CPU_SUBTYPE_ANY: &[Flag<u32>] = &flags!(
        CPU_SUBTYPE_MULTIPLE,
        CPU_SUBTYPE_LITTLE_ENDIAN,
        CPU_SUBTYPE_BIG_ENDIAN,
    );
    static FLAGS_CPU_SUBTYPE_VAX: &[Flag<u32>] = &flags!(
        CPU_SUBTYPE_VAX_ALL,
        CPU_SUBTYPE_VAX780,
        CPU_SUBTYPE_VAX785,
        CPU_SUBTYPE_VAX750,
        CPU_SUBTYPE_VAX730,
        CPU_SUBTYPE_UVAXI,
        CPU_SUBTYPE_UVAXII,
        CPU_SUBTYPE_VAX8200,
        CPU_SUBTYPE_VAX8500,
        CPU_SUBTYPE_VAX8600,
        CPU_SUBTYPE_VAX8650,
        CPU_SUBTYPE_VAX8800,
        CPU_SUBTYPE_UVAXIII,
    );
    static FLAGS_CPU_SUBTYPE_MC680X0: &[Flag<u32>] = &flags!(
        CPU_SUBTYPE_MC680X0_ALL,
        CPU_SUBTYPE_MC68040,
        CPU_SUBTYPE_MC68030_ONLY,
    );
    static FLAGS_CPU_SUBTYPE_X86: &[Flag<u32>] = &flags!(
        CPU_SUBTYPE_I386_ALL,
        CPU_SUBTYPE_386,
        CPU_SUBTYPE_486,
        CPU_SUBTYPE_486SX,
        CPU_SUBTYPE_586,
        CPU_SUBTYPE_PENT,
        CPU_SUBTYPE_PENTPRO,
        CPU_SUBTYPE_PENTII_M3,
        CPU_SUBTYPE_PENTII_M5,
        CPU_SUBTYPE_CELERON,
        CPU_SUBTYPE_CELERON_MOBILE,
        CPU_SUBTYPE_PENTIUM_3,
        CPU_SUBTYPE_PENTIUM_3_M,
        CPU_SUBTYPE_PENTIUM_3_XEON,
        CPU_SUBTYPE_PENTIUM_M,
        CPU_SUBTYPE_PENTIUM_4,
        CPU_SUBTYPE_PENTIUM_4_M,
        CPU_SUBTYPE_ITANIUM,
        CPU_SUBTYPE_ITANIUM_2,
        CPU_SUBTYPE_XEON,
        CPU_SUBTYPE_XEON_MP,
    );
    static FLAGS_CPU_SUBTYPE_X86_64: &[Flag<u32>] = &flags!(
        CPU_SUBTYPE_X86_64_ALL,
        CPU_SUBTYPE_X86_ARCH1,
        CPU_SUBTYPE_X86_64_H,
    );
    static FLAGS_CPU_SUBTYPE_MIPS: &[Flag<u32>] = &flags!(
        CPU_SUBTYPE_MIPS_ALL,
        CPU_SUBTYPE_MIPS_R2300,
        CPU_SUBTYPE_MIPS_R2600,
        CPU_SUBTYPE_MIPS_R2800,
        CPU_SUBTYPE_MIPS_R2000A,
        CPU_SUBTYPE_MIPS_R2000,
        CPU_SUBTYPE_MIPS_R3000A,
        CPU_SUBTYPE_MIPS_R3000,
    );
    static FLAGS_CPU_SUBTYPE_MC98000: &[Flag<u32>] =
        &flags!(CPU_SUBTYPE_MC98000_ALL, CPU_SUBTYPE_MC98601);
    static FLAGS_CPU_SUBTYPE_HPPA: &[Flag<u32>] =
        &flags!(CPU_SUBTYPE_HPPA_ALL, CPU_SUBTYPE_HPPA_7100LC);
    static FLAGS_CPU_SUBTYPE_MC88000: &[Flag<u32>] = &flags!(
        CPU_SUBTYPE_MC88000_ALL,
        CPU_SUBTYPE_MC88100,
        CPU_SUBTYPE_MC88110,
    );
    static FLAGS_CPU_SUBTYPE_SPARC: &[Flag<u32>] = &flags!(CPU_SUBTYPE_SPARC_ALL);
    static FLAGS_CPU_SUBTYPE_I860: &[Flag<u32>] =
        &flags!(CPU_SUBTYPE_I860_ALL, CPU_SUBTYPE_I860_860);
    static FLAGS_CPU_SUBTYPE_POWERPC: &[Flag<u32>] = &flags!(
        CPU_SUBTYPE_POWERPC_ALL,
        CPU_SUBTYPE_POWERPC_601,
        CPU_SUBTYPE_POWERPC_602,
        CPU_SUBTYPE_POWERPC_603,
        CPU_SUBTYPE_POWERPC_603E,
        CPU_SUBTYPE_POWERPC_603EV,
        CPU_SUBTYPE_POWERPC_604,
        CPU_SUBTYPE_POWERPC_604E,
        CPU_SUBTYPE_POWERPC_620,
        CPU_SUBTYPE_POWERPC_750,
        CPU_SUBTYPE_POWERPC_7400,
        CPU_SUBTYPE_POWERPC_7450,
        CPU_SUBTYPE_POWERPC_970,
    );
    static FLAGS_CPU_SUBTYPE_ARM: &[Flag<u32>] = &flags!(
        CPU_SUBTYPE_ARM_ALL,
        CPU_SUBTYPE_ARM_V4T,
        CPU_SUBTYPE_ARM_V6,
        CPU_SUBTYPE_ARM_V5TEJ,
        CPU_SUBTYPE_ARM_XSCALE,
        CPU_SUBTYPE_ARM_V7,
        CPU_SUBTYPE_ARM_V7F,
        CPU_SUBTYPE_ARM_V7S,
        CPU_SUBTYPE_ARM_V7K,
        CPU_SUBTYPE_ARM_V8,
        CPU_SUBTYPE_ARM_V6M,
        CPU_SUBTYPE_ARM_V7M,
        CPU_SUBTYPE_ARM_V7EM,
        CPU_SUBTYPE_ARM_V8M,
    );
    static FLAGS_CPU_SUBTYPE_ARM64: &[Flag<u32>] = &flags!(
        CPU_SUBTYPE_ARM64_ALL,
        CPU_SUBTYPE_ARM64_V8,
        CPU_SUBTYPE_ARM64E,
    );
    static FLAGS_CPU_SUBTYPE_ARM64_32: &[Flag<u32>] =
        &flags!(CPU_SUBTYPE_ARM64_32_ALL, CPU_SUBTYPE_ARM64_32_V8);
    static FLAGS_MH_FILETYPE: &[Flag<u32>] = &flags!(
        MH_OBJECT,
        MH_EXECUTE,
        MH_FVMLIB,
        MH_CORE,
        MH_PRELOAD,
        MH_DYLIB,
        MH_DYLINKER,
        MH_BUNDLE,
        MH_DYLIB_STUB,
        MH_DSYM,
        MH_KEXT_BUNDLE,
        MH_FILESET,
    );
    static FLAGS_MH: &[Flag<u32>] = &flags!(
        MH_NOUNDEFS,
        MH_INCRLINK,
        MH_DYLDLINK,
        MH_BINDATLOAD,
        MH_PREBOUND,
        MH_SPLIT_SEGS,
        MH_LAZY_INIT,
        MH_TWOLEVEL,
        MH_FORCE_FLAT,
        MH_NOMULTIDEFS,
        MH_NOFIXPREBINDING,
        MH_PREBINDABLE,
        MH_ALLMODSBOUND,
        MH_SUBSECTIONS_VIA_SYMBOLS,
        MH_CANONICAL,
        MH_WEAK_DEFINES,
        MH_BINDS_TO_WEAK,
        MH_ALLOW_STACK_EXECUTION,
        MH_ROOT_SAFE,
        MH_SETUID_SAFE,
        MH_NO_REEXPORTED_DYLIBS,
        MH_PIE,
        MH_DEAD_STRIPPABLE_DYLIB,
        MH_HAS_TLV_DESCRIPTORS,
        MH_NO_HEAP_EXECUTION,
        MH_APP_EXTENSION_SAFE,
        MH_NLIST_OUTOFSYNC_WITH_DYLDINFO,
        MH_SIM_SUPPORT,
        MH_DYLIB_IN_CACHE,
    );
    static FLAGS_LC: &[Flag<u32>] = &flags!(
        LC_SEGMENT,
        LC_SYMTAB,
        LC_SYMSEG,
        LC_THREAD,
        LC_UNIXTHREAD,
        LC_LOADFVMLIB,
        LC_IDFVMLIB,
        LC_IDENT,
        LC_FVMFILE,
        LC_PREPAGE,
        LC_DYSYMTAB,
        LC_LOAD_DYLIB,
        LC_ID_DYLIB,
        LC_LOAD_DYLINKER,
        LC_ID_DYLINKER,
        LC_PREBOUND_DYLIB,
        LC_ROUTINES,
        LC_SUB_FRAMEWORK,
        LC_SUB_UMBRELLA,
        LC_SUB_CLIENT,
        LC_SUB_LIBRARY,
        LC_TWOLEVEL_HINTS,
        LC_PREBIND_CKSUM,
        LC_LOAD_WEAK_DYLIB,
        LC_SEGMENT_64,
        LC_ROUTINES_64,
        LC_UUID,
        LC_RPATH,
        LC_CODE_SIGNATURE,
        LC_SEGMENT_SPLIT_INFO,
        LC_REEXPORT_DYLIB,
        LC_LAZY_LOAD_DYLIB,
        LC_ENCRYPTION_INFO,
        LC_DYLD_INFO,
        LC_DYLD_INFO_ONLY,
        LC_LOAD_UPWARD_DYLIB,
        LC_VERSION_MIN_MACOSX,
        LC_VERSION_MIN_IPHONEOS,
        LC_FUNCTION_STARTS,
        LC_DYLD_ENVIRONMENT,
        LC_MAIN,
        LC_DATA_IN_CODE,
        LC_SOURCE_VERSION,
        LC_DYLIB_CODE_SIGN_DRS,
        LC_ENCRYPTION_INFO_64,
        LC_LINKER_OPTION,
        LC_LINKER_OPTIMIZATION_HINT,
        LC_VERSION_MIN_TVOS,
        LC_VERSION_MIN_WATCHOS,
        LC_NOTE,
        LC_BUILD_VERSION,
        LC_DYLD_EXPORTS_TRIE,
        LC_DYLD_CHAINED_FIXUPS,
        LC_FILESET_ENTRY,
    );
    static FLAGS_VM: &[Flag<u32>] = &flags!(VM_PROT_READ, VM_PROT_WRITE, VM_PROT_EXECUTE);
    static FLAGS_SG: &[Flag<u32>] = &flags!(
        SG_HIGHVM,
        SG_FVMLIB,
        SG_NORELOC,
        SG_PROTECTED_VERSION_1,
        SG_READ_ONLY,
    );
    static FLAGS_S_TYPE: &[Flag<u32>] = &flags!(
        S_REGULAR,
        S_ZEROFILL,
        S_CSTRING_LITERALS,
        S_4BYTE_LITERALS,
        S_8BYTE_LITERALS,
        S_LITERAL_POINTERS,
        S_NON_LAZY_SYMBOL_POINTERS,
        S_LAZY_SYMBOL_POINTERS,
        S_SYMBOL_STUBS,
        S_MOD_INIT_FUNC_POINTERS,
        S_MOD_TERM_FUNC_POINTERS,
        S_COALESCED,
        S_GB_ZEROFILL,
        S_INTERPOSING,
        S_16BYTE_LITERALS,
        S_DTRACE_DOF,
        S_LAZY_DYLIB_SYMBOL_POINTERS,
        S_THREAD_LOCAL_REGULAR,
        S_THREAD_LOCAL_ZEROFILL,
        S_THREAD_LOCAL_VARIABLES,
        S_THREAD_LOCAL_VARIABLE_POINTERS,
        S_THREAD_LOCAL_INIT_FUNCTION_POINTERS,
        S_INIT_FUNC_OFFSETS,
    );
    static FLAGS_S_ATTR: &[Flag<u32>] = &flags!(
        S_ATTR_PURE_INSTRUCTIONS,
        S_ATTR_NO_TOC,
        S_ATTR_STRIP_STATIC_SYMS,
        S_ATTR_NO_DEAD_STRIP,
        S_ATTR_LIVE_SUPPORT,
        S_ATTR_SELF_MODIFYING_CODE,
        S_ATTR_DEBUG,
        S_ATTR_SOME_INSTRUCTIONS,
        S_ATTR_EXT_RELOC,
        S_ATTR_LOC_RELOC,
    );
    static FLAGS_PLATFORM: &[Flag<u32>] = &flags!(
        PLATFORM_MACOS,
        PLATFORM_IOS,
        PLATFORM_TVOS,
        PLATFORM_WATCHOS,
        PLATFORM_BRIDGEOS,
        PLATFORM_MACCATALYST,
        PLATFORM_IOSSIMULATOR,
        PLATFORM_TVOSSIMULATOR,
        PLATFORM_WATCHOSSIMULATOR,
        PLATFORM_DRIVERKIT,
    );
    static FLAGS_N_EXT: &[Flag<u8>] = &flags!(N_PEXT, N_EXT);
    static FLAGS_N_TYPE: &[Flag<u8>] = &flags!(N_UNDF, N_ABS, N_SECT, N_PBUD, N_INDR);
    static FLAGS_N_STAB: &[Flag<u8>] = &flags!(
        N_GSYM, N_FNAME, N_FUN, N_STSYM, N_LCSYM, N_BNSYM, N_AST, N_OPT, N_RSYM, N_SLINE, N_ENSYM,
        N_SSYM, N_SO, N_OSO, N_LSYM, N_BINCL, N_SOL, N_PARAMS, N_VERSION, N_OLEVEL, N_PSYM,
        N_EINCL, N_ENTRY, N_LBRAC, N_EXCL, N_RBRAC, N_BCOMM, N_ECOMM, N_ECOML, N_LENG, N_PC,
    );
    static FLAGS_REFERENCE: &[Flag<u16>] = &flags!(
        REFERENCE_FLAG_UNDEFINED_NON_LAZY,
        REFERENCE_FLAG_UNDEFINED_LAZY,
        REFERENCE_FLAG_DEFINED,
        REFERENCE_FLAG_PRIVATE_DEFINED,
        REFERENCE_FLAG_PRIVATE_UNDEFINED_NON_LAZY,
        REFERENCE_FLAG_PRIVATE_UNDEFINED_LAZY,
    );
    static FLAGS_N_DESC: &[Flag<u16>] = &flags!(
        REFERENCED_DYNAMICALLY,
        N_NO_DEAD_STRIP,
        N_DESC_DISCARDED,
        N_WEAK_REF,
        N_WEAK_DEF,
        N_REF_TO_WEAK,
        N_ARM_THUMB_DEF,
        N_SYMBOL_RESOLVER,
        N_ALT_ENTRY,
    );
    static FLAGS_GENERIC_RELOC: &[Flag<u8>] = &flags!(
        GENERIC_RELOC_VANILLA,
        GENERIC_RELOC_PAIR,
        GENERIC_RELOC_SECTDIFF,
        GENERIC_RELOC_PB_LA_PTR,
        GENERIC_RELOC_LOCAL_SECTDIFF,
        GENERIC_RELOC_TLV,
    );
    static FLAGS_ARM_RELOC: &[Flag<u8>] = &flags!(
        ARM_RELOC_VANILLA,
        ARM_RELOC_PAIR,
        ARM_RELOC_SECTDIFF,
        ARM_RELOC_LOCAL_SECTDIFF,
        ARM_RELOC_PB_LA_PTR,
        ARM_RELOC_BR24,
        ARM_THUMB_RELOC_BR22,
        ARM_THUMB_32BIT_BRANCH,
        ARM_RELOC_HALF,
        ARM_RELOC_HALF_SECTDIFF,
    );
    static FLAGS_ARM64_RELOC: &[Flag<u8>] = &flags!(
        ARM64_RELOC_UNSIGNED,
        ARM64_RELOC_SUBTRACTOR,
        ARM64_RELOC_BRANCH26,
        ARM64_RELOC_PAGE21,
        ARM64_RELOC_PAGEOFF12,
        ARM64_RELOC_GOT_LOAD_PAGE21,
        ARM64_RELOC_GOT_LOAD_PAGEOFF12,
        ARM64_RELOC_POINTER_TO_GOT,
        ARM64_RELOC_TLVP_LOAD_PAGE21,
        ARM64_RELOC_TLVP_LOAD_PAGEOFF12,
        ARM64_RELOC_ADDEND,
        ARM64_RELOC_AUTHENTICATED_POINTER,
    );
    static FLAGS_PPC_RELOC: &[Flag<u8>] = &flags!(
        PPC_RELOC_VANILLA,
        PPC_RELOC_PAIR,
        PPC_RELOC_BR14,
        PPC_RELOC_BR24,
        PPC_RELOC_HI16,
        PPC_RELOC_LO16,
        PPC_RELOC_HA16,
        PPC_RELOC_LO14,
        PPC_RELOC_SECTDIFF,
        PPC_RELOC_PB_LA_PTR,
        PPC_RELOC_HI16_SECTDIFF,
        PPC_RELOC_LO16_SECTDIFF,
        PPC_RELOC_HA16_SECTDIFF,
        PPC_RELOC_JBSR,
        PPC_RELOC_LO14_SECTDIFF,
        PPC_RELOC_LOCAL_SECTDIFF,
    );
    static FLAGS_X86_64_RELOC: &[Flag<u8>] = &flags!(
        X86_64_RELOC_UNSIGNED,
        X86_64_RELOC_SIGNED,
        X86_64_RELOC_BRANCH,
        X86_64_RELOC_GOT_LOAD,
        X86_64_RELOC_GOT,
        X86_64_RELOC_SUBTRACTOR,
        X86_64_RELOC_SIGNED_1,
        X86_64_RELOC_SIGNED_2,
        X86_64_RELOC_SIGNED_4,
        X86_64_RELOC_TLV,
    );
}

mod pe {
    use super::*;
    use object::pe::*;
    use object::read::pe::*;
    use object::LittleEndian as LE;

    pub(super) fn print_coff(p: &mut Printer<impl Write>, data: &[u8]) {
        let mut offset = 0;
        if let Ok(header) = ImageFileHeader::parse(data, &mut offset) {
            println!("Format: COFF");
            print_file(p, header);
            let sections = header.sections(data, offset).ok();
            let symbols = header.symbols(data).ok();
            if let Some(ref sections) = sections {
                print_sections(p, data, header.machine.get(LE), symbols.as_ref(), &sections);
            }
            if let Some(ref symbols) = symbols {
                print_symbols(p, sections.as_ref(), &symbols);
            }
        }
    }

    pub(super) fn print_pe32(p: &mut Printer<impl Write>, data: &[u8]) {
        println!("Format: PE 32-bit");
        print_pe::<ImageNtHeaders32, _>(p, data);
    }

    pub(super) fn print_pe64(p: &mut Printer<impl Write>, data: &[u8]) {
        println!("Format: PE 64-bit");
        print_pe::<ImageNtHeaders64, _>(p, data);
    }

    fn print_pe<Pe: ImageNtHeaders, W: Write>(p: &mut Printer<W>, data: &[u8]) {
        if let Ok(dos_header) = ImageDosHeader::parse(data) {
            p.group("ImageDosHeader", |p| {
                p.field_hex("Magic", dos_header.e_magic.get(LE));
                p.field_hex("CountBytesLastPage", dos_header.e_cblp.get(LE));
                p.field_hex("CountPages", dos_header.e_cp.get(LE));
                p.field_hex("CountRelocations", dos_header.e_crlc.get(LE));
                p.field_hex("CountHeaderParagraphs", dos_header.e_cparhdr.get(LE));
                p.field_hex("MinAllocParagraphs", dos_header.e_minalloc.get(LE));
                p.field_hex("MaxAllocParagraphs", dos_header.e_maxalloc.get(LE));
                p.field_hex("StackSegment", dos_header.e_ss.get(LE));
                p.field_hex("StackPointer", dos_header.e_sp.get(LE));
                p.field_hex("Checksum", dos_header.e_csum.get(LE));
                p.field_hex("InstructionPointer", dos_header.e_ip.get(LE));
                p.field_hex("CodeSegment", dos_header.e_cs.get(LE));
                p.field_hex("AddressOfRelocations", dos_header.e_lfarlc.get(LE));
                p.field_hex("OverlayNumber", dos_header.e_ovno.get(LE));
                p.field_hex("OemId", dos_header.e_oemid.get(LE));
                p.field_hex("OemInfo", dos_header.e_oeminfo.get(LE));
                p.field_hex("AddressOfNewHeader", dos_header.e_lfanew.get(LE));
            });
            let mut offset = dos_header.nt_headers_offset().into();
            if let Ok((nt_headers, data_directories)) = Pe::parse(data, &mut offset) {
                p.group("ImageNtHeaders", |p| {
                    p.field_hex("Signature", nt_headers.signature());
                });
                let header = nt_headers.file_header();
                let sections = header.sections(data, offset).ok();
                let symbols = header.symbols(data).ok();
                print_file(p, header);
                print_optional(p, nt_headers.optional_header());
                for (index, dir) in data_directories.iter().enumerate() {
                    p.group("ImageDataDirectory", |p| {
                        p.field_enum("Index", index, FLAGS_IMAGE_DIRECTORY_ENTRY);
                        p.field_hex("VirtualAddress", dir.virtual_address.get(LE));
                        p.field_hex("Size", dir.size.get(LE));
                        if let Some(dir_data) = sections
                            .as_ref()
                            .and_then(|sections| dir.data(data, sections).ok())
                        {
                            match index {
                                IMAGE_DIRECTORY_ENTRY_EXPORT => print_export_dir(p, dir, dir_data),
                                // TODO
                                _ => {}
                            }
                        }
                    });
                }
                if let Some(ref sections) = sections {
                    print_sections(p, data, header.machine.get(LE), symbols.as_ref(), &sections);
                }
                if let Some(ref symbols) = symbols {
                    print_symbols(p, sections.as_ref(), &symbols);
                }
            }
        }
    }

    fn print_file(p: &mut Printer<impl Write>, header: &ImageFileHeader) {
        p.group("ImageFileHeader", |p| {
            p.field_enum("Machine", header.machine.get(LE), FLAGS_IMAGE_FILE_MACHINE);
            p.field("NumberOfSections", header.number_of_sections.get(LE));
            p.field("TimeDateStamp", header.time_date_stamp.get(LE));
            p.field_hex(
                "PointerToSymbolTable",
                header.pointer_to_symbol_table.get(LE),
            );
            p.field("NumberOfSymbols", header.number_of_symbols.get(LE));
            p.field_hex(
                "SizeOfOptionalHeader",
                header.size_of_optional_header.get(LE),
            );
            p.field_hex("Characteristics", header.characteristics.get(LE));
            p.flags(header.characteristics.get(LE), 0, FLAGS_IMAGE_FILE);
        });
    }

    fn print_optional(p: &mut Printer<impl Write>, header: &impl ImageOptionalHeader) {
        p.group("ImageOptionalHeader", |p| {
            p.field_hex("Magic", header.magic());
            p.field("MajorLinkerVersion", header.major_linker_version());
            p.field("MinorLinkerVersion", header.minor_linker_version());
            p.field_hex("SizeOfCode", header.size_of_code());
            p.field_hex("SizeOfInitializedData", header.size_of_initialized_data());
            p.field_hex(
                "SizeOfUninitializedData",
                header.size_of_uninitialized_data(),
            );
            p.field_hex("AddressOfEntryPoint", header.address_of_entry_point());
            p.field_hex("BaseOfCode", header.base_of_code());
            p.field_hex("ImageBase", header.image_base());
            p.field_hex("SectionAlignment", header.section_alignment());
            p.field(
                "MajorOperatingSystemVersion",
                header.major_operating_system_version(),
            );
            p.field(
                "MinorOperatingSystemVersion",
                header.minor_operating_system_version(),
            );
            p.field("MajorImageVersion", header.major_image_version());
            p.field("MinorImageVersion", header.minor_image_version());
            p.field("MajorSubsystemVersion", header.major_subsystem_version());
            p.field("MinorSubsystemVersion", header.minor_subsystem_version());
            p.field("Win32VersionValue", header.win32_version_value());
            p.field_hex("SizeOfImage", header.size_of_image());
            p.field_hex("SizeOfHeaders", header.size_of_headers());
            p.field_hex("CheckSum", header.check_sum());
            p.field_enum("Subsystem", header.subsystem(), FLAGS_IMAGE_SUBSYSTEM);
            p.field_hex("DllCharacteristics", header.dll_characteristics());
            p.flags(
                header.dll_characteristics(),
                0,
                FLAGS_IMAGE_DLLCHARACTERISTICS,
            );
            p.field_hex("SizeOfStackReserve", header.size_of_stack_reserve());
            p.field_hex("SizeOfStackCommit", header.size_of_stack_commit());
            p.field_hex("SizeOfHeapReserve", header.size_of_heap_reserve());
            p.field_hex("SizeOfHeapCommit", header.size_of_heap_commit());
            p.field_hex("LoaderFlags", header.loader_flags());
            p.field_hex("NumberOfRvaAndSizes", header.number_of_rva_and_sizes());
        });
    }

    fn print_export_dir(p: &mut Printer<impl Write>, _dir: &ImageDataDirectory, dir_data: &[u8]) {
        if let Ok((export_dir, _)) = object::from_bytes::<pe::ImageExportDirectory>(dir_data) {
            p.group("ImageExportDirectory", |p| {
                p.field_hex("Characteristics", export_dir.characteristics.get(LE));
                p.field_hex("TimeDateStamp", export_dir.time_date_stamp.get(LE));
                p.field("MajorVersion", export_dir.major_version.get(LE));
                p.field("MinorVersion", export_dir.minor_version.get(LE));
                p.field_hex("Name", export_dir.name.get(LE));
                p.field("Base", export_dir.base.get(LE));
                p.field("NumberOfFunctions", export_dir.number_of_functions.get(LE));
                p.field("NumberOfNames", export_dir.number_of_names.get(LE));
                p.field_hex(
                    "AddressOfFunctions",
                    export_dir.address_of_functions.get(LE),
                );
                p.field_hex("AddressOfNames", export_dir.address_of_names.get(LE));
                p.field_hex(
                    "AddressOfNameOrdinals",
                    export_dir.address_of_name_ordinals.get(LE),
                );
                // TODO: display tables
            });
        }
    }

    fn print_sections(
        p: &mut Printer<impl Write>,
        data: &[u8],
        machine: u16,
        symbols: Option<&SymbolTable>,
        sections: &SectionTable,
    ) {
        for (index, section) in sections.iter().enumerate() {
            p.group("ImageSectionHeader", |p| {
                p.field("Index", index + 1);
                if let Some(name) = symbols.and_then(|symbols| section.name(symbols.strings()).ok())
                {
                    p.field_inline_string("Name", name);
                } else {
                    p.field_inline_string("Name", section.raw_name());
                }
                p.field_hex("VirtualSize", section.virtual_size.get(LE));
                p.field_hex("VirtualAddress", section.virtual_address.get(LE));
                p.field_hex("SizeOfRawData", section.size_of_raw_data.get(LE));
                p.field_hex("PointerToRawData", section.pointer_to_raw_data.get(LE));
                p.field_hex(
                    "PointerToRelocations",
                    section.pointer_to_relocations.get(LE),
                );
                p.field_hex(
                    "PointerToLinenumbers",
                    section.pointer_to_linenumbers.get(LE),
                );
                p.field("NumberOfRelocations", section.number_of_relocations.get(LE));
                p.field("NumberOfLinenumbers", section.number_of_linenumbers.get(LE));
                p.field_hex("Characteristics", section.characteristics.get(LE));
                p.flags(section.characteristics.get(LE), 0, FLAGS_IMAGE_SCN);
                p.flags(
                    section.characteristics.get(LE),
                    IMAGE_SCN_ALIGN_MASK,
                    FLAGS_IMAGE_SCN_ALIGN,
                );
                if let Ok(relocations) = section.coff_relocations(data) {
                    for relocation in relocations {
                        p.group("ImageRelocation", |p| {
                            p.field_hex("VirtualAddress", relocation.virtual_address.get(LE));
                            let index = relocation.symbol_table_index.get(LE);
                            let name = symbols.and_then(|symbols| {
                                symbols
                                    .symbol(index as usize)
                                    .and_then(|symbol| symbol.name(symbols.strings()))
                                    .ok()
                            });
                            p.field_string("Symbol", index, name);
                            let proc = match machine {
                                IMAGE_FILE_MACHINE_I386 => FLAGS_IMAGE_REL_I386,
                                IMAGE_FILE_MACHINE_MIPS16
                                | IMAGE_FILE_MACHINE_MIPSFPU
                                | IMAGE_FILE_MACHINE_MIPSFPU16 => FLAGS_IMAGE_REL_MIPS,
                                IMAGE_FILE_MACHINE_ALPHA | IMAGE_FILE_MACHINE_ALPHA64 => {
                                    FLAGS_IMAGE_REL_ALPHA
                                }
                                IMAGE_FILE_MACHINE_POWERPC | IMAGE_FILE_MACHINE_POWERPCFP => {
                                    FLAGS_IMAGE_REL_PPC
                                }
                                IMAGE_FILE_MACHINE_SH3
                                | IMAGE_FILE_MACHINE_SH3DSP
                                | IMAGE_FILE_MACHINE_SH3E
                                | IMAGE_FILE_MACHINE_SH4
                                | IMAGE_FILE_MACHINE_SH5 => FLAGS_IMAGE_REL_SH,
                                IMAGE_FILE_MACHINE_ARM => FLAGS_IMAGE_REL_ARM,
                                IMAGE_FILE_MACHINE_AM33 => FLAGS_IMAGE_REL_AM,
                                IMAGE_FILE_MACHINE_ARM64 => FLAGS_IMAGE_REL_ARM64,
                                IMAGE_FILE_MACHINE_AMD64 => FLAGS_IMAGE_REL_AMD64,
                                IMAGE_FILE_MACHINE_IA64 => FLAGS_IMAGE_REL_IA64,
                                IMAGE_FILE_MACHINE_CEF => FLAGS_IMAGE_REL_CEF,
                                IMAGE_FILE_MACHINE_CEE => FLAGS_IMAGE_REL_CEE,
                                IMAGE_FILE_MACHINE_M32R => FLAGS_IMAGE_REL_M32R,
                                IMAGE_FILE_MACHINE_EBC => FLAGS_IMAGE_REL_EBC,
                                _ => &[],
                            };
                            let typ = relocation.typ.get(LE);
                            p.field_enum("Type", typ, proc);
                            match machine {
                                IMAGE_FILE_MACHINE_POWERPC | IMAGE_FILE_MACHINE_POWERPCFP => {
                                    p.flags(typ, 0, FLAGS_IMAGE_REL_PPC_BITS)
                                }
                                IMAGE_FILE_MACHINE_SH3
                                | IMAGE_FILE_MACHINE_SH3DSP
                                | IMAGE_FILE_MACHINE_SH3E
                                | IMAGE_FILE_MACHINE_SH4
                                | IMAGE_FILE_MACHINE_SH5 => {
                                    p.flags(typ, 0, FLAGS_IMAGE_REL_SH_BITS)
                                }
                                _ => {}
                            }
                        });
                    }
                }
            });
        }
    }

    fn print_symbols(
        p: &mut Printer<impl Write>,
        sections: Option<&SectionTable>,
        symbols: &SymbolTable,
    ) {
        for (index, symbol) in symbols.iter() {
            p.group("ImageSymbol", |p| {
                p.field("Index", index);
                if let Ok(name) = symbol.name(symbols.strings()) {
                    p.field_inline_string("Name", name);
                } else {
                    p.field("Name", format!("{:X?}", symbol.name));
                }
                p.field_hex("Value", symbol.value.get(LE));
                let section = symbol.section_number.get(LE);
                if section == 0 || section >= IMAGE_SYM_SECTION_MAX {
                    p.field_enum("Section", section, FLAGS_IMAGE_SYM);
                } else {
                    let section_name = sections.and_then(|sections| {
                        sections
                            .section(section.into())
                            .and_then(|section| section.name(symbols.strings()))
                            .ok()
                    });
                    p.field_string("Section", section, section_name);
                }
                p.field_hex("Type", symbol.typ.get(LE));
                p.field_enum("BaseType", symbol.base_type(), FLAGS_IMAGE_SYM_TYPE);
                p.field_enum("DerivedType", symbol.derived_type(), FLAGS_IMAGE_SYM_DTYPE);
                p.field_enum("StorageClass", symbol.storage_class, FLAGS_IMAGE_SYM_CLASS);
                p.field_hex("NumberOfAuxSymbols", symbol.number_of_aux_symbols);
                if symbol.has_aux_file_name() {
                    if let Ok(name) = symbols.aux_file_name(index, symbol.number_of_aux_symbols) {
                        p.group("ImageAuxSymbolFile", |p| {
                            p.field_inline_string("Name", name);
                        });
                    }
                } else if symbol.has_aux_function() {
                    if let Ok(aux) = symbols.aux_function(index) {
                        p.group("ImageAuxSymbolFunction", |p| {
                            p.field("TagIndex", aux.tag_index.get(LE));
                            p.field("TotalSize", aux.total_size.get(LE));
                            p.field_hex("PointerToLinenumber", aux.pointer_to_linenumber.get(LE));
                            p.field(
                                "PointerToNextFunction",
                                aux.pointer_to_next_function.get(LE),
                            );
                            p.field("Unused", format!("{:X?}", aux.unused));
                        });
                    }
                } else if symbol.has_aux_section() {
                    if let Ok(aux) = symbols.aux_section(index) {
                        p.group("ImageAuxSymbolSection", |p| {
                            p.field_hex("Length", aux.length.get(LE));
                            p.field("NumberOfRelocations", aux.number_of_relocations.get(LE));
                            p.field("NumberOfLinenumbers", aux.number_of_linenumbers.get(LE));
                            p.field_hex("CheckSum", aux.check_sum.get(LE));
                            p.field("Number", aux.number.get(LE));
                            p.field_enum("Selection", aux.selection, FLAGS_IMAGE_COMDAT_SELECT);
                            p.field_hex("Reserved", aux.reserved);
                            p.field("HighNumber", aux.high_number.get(LE));
                        });
                    }
                }
                // TODO: ImageAuxSymbolFunctionBeginEnd
                // TODO: ImageAuxSymbolWeak
            });
        }
    }

    static FLAGS_IMAGE_FILE: &[Flag<u16>] = &flags!(
        IMAGE_FILE_RELOCS_STRIPPED,
        IMAGE_FILE_EXECUTABLE_IMAGE,
        IMAGE_FILE_LINE_NUMS_STRIPPED,
        IMAGE_FILE_LOCAL_SYMS_STRIPPED,
        IMAGE_FILE_AGGRESIVE_WS_TRIM,
        IMAGE_FILE_LARGE_ADDRESS_AWARE,
        IMAGE_FILE_BYTES_REVERSED_LO,
        IMAGE_FILE_32BIT_MACHINE,
        IMAGE_FILE_DEBUG_STRIPPED,
        IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP,
        IMAGE_FILE_NET_RUN_FROM_SWAP,
        IMAGE_FILE_SYSTEM,
        IMAGE_FILE_DLL,
        IMAGE_FILE_UP_SYSTEM_ONLY,
        IMAGE_FILE_BYTES_REVERSED_HI,
    );
    static FLAGS_IMAGE_FILE_MACHINE: &[Flag<u16>] = &flags!(
        IMAGE_FILE_MACHINE_UNKNOWN,
        IMAGE_FILE_MACHINE_TARGET_HOST,
        IMAGE_FILE_MACHINE_I386,
        IMAGE_FILE_MACHINE_R3000,
        IMAGE_FILE_MACHINE_R4000,
        IMAGE_FILE_MACHINE_R10000,
        IMAGE_FILE_MACHINE_WCEMIPSV2,
        IMAGE_FILE_MACHINE_ALPHA,
        IMAGE_FILE_MACHINE_SH3,
        IMAGE_FILE_MACHINE_SH3DSP,
        IMAGE_FILE_MACHINE_SH3E,
        IMAGE_FILE_MACHINE_SH4,
        IMAGE_FILE_MACHINE_SH5,
        IMAGE_FILE_MACHINE_ARM,
        IMAGE_FILE_MACHINE_THUMB,
        IMAGE_FILE_MACHINE_ARMNT,
        IMAGE_FILE_MACHINE_AM33,
        IMAGE_FILE_MACHINE_POWERPC,
        IMAGE_FILE_MACHINE_POWERPCFP,
        IMAGE_FILE_MACHINE_IA64,
        IMAGE_FILE_MACHINE_MIPS16,
        IMAGE_FILE_MACHINE_ALPHA64,
        IMAGE_FILE_MACHINE_MIPSFPU,
        IMAGE_FILE_MACHINE_MIPSFPU16,
        IMAGE_FILE_MACHINE_AXP64,
        IMAGE_FILE_MACHINE_TRICORE,
        IMAGE_FILE_MACHINE_CEF,
        IMAGE_FILE_MACHINE_EBC,
        IMAGE_FILE_MACHINE_AMD64,
        IMAGE_FILE_MACHINE_M32R,
        IMAGE_FILE_MACHINE_ARM64,
        IMAGE_FILE_MACHINE_CEE,
    );
    static FLAGS_IMAGE_SCN: &[Flag<u32>] = &flags!(
        IMAGE_SCN_TYPE_NO_PAD,
        IMAGE_SCN_CNT_CODE,
        IMAGE_SCN_CNT_INITIALIZED_DATA,
        IMAGE_SCN_CNT_UNINITIALIZED_DATA,
        IMAGE_SCN_LNK_OTHER,
        IMAGE_SCN_LNK_INFO,
        IMAGE_SCN_LNK_REMOVE,
        IMAGE_SCN_LNK_COMDAT,
        IMAGE_SCN_NO_DEFER_SPEC_EXC,
        IMAGE_SCN_GPREL,
        IMAGE_SCN_MEM_FARDATA,
        IMAGE_SCN_MEM_PURGEABLE,
        IMAGE_SCN_MEM_16BIT,
        IMAGE_SCN_MEM_LOCKED,
        IMAGE_SCN_MEM_PRELOAD,
        IMAGE_SCN_LNK_NRELOC_OVFL,
        IMAGE_SCN_MEM_DISCARDABLE,
        IMAGE_SCN_MEM_NOT_CACHED,
        IMAGE_SCN_MEM_NOT_PAGED,
        IMAGE_SCN_MEM_SHARED,
        IMAGE_SCN_MEM_EXECUTE,
        IMAGE_SCN_MEM_READ,
        IMAGE_SCN_MEM_WRITE,
    );
    static FLAGS_IMAGE_SCN_ALIGN: &[Flag<u32>] = &flags!(
        IMAGE_SCN_ALIGN_1BYTES,
        IMAGE_SCN_ALIGN_2BYTES,
        IMAGE_SCN_ALIGN_4BYTES,
        IMAGE_SCN_ALIGN_8BYTES,
        IMAGE_SCN_ALIGN_16BYTES,
        IMAGE_SCN_ALIGN_32BYTES,
        IMAGE_SCN_ALIGN_64BYTES,
        IMAGE_SCN_ALIGN_128BYTES,
        IMAGE_SCN_ALIGN_256BYTES,
        IMAGE_SCN_ALIGN_512BYTES,
        IMAGE_SCN_ALIGN_1024BYTES,
        IMAGE_SCN_ALIGN_2048BYTES,
        IMAGE_SCN_ALIGN_4096BYTES,
        IMAGE_SCN_ALIGN_8192BYTES,
    );
    static FLAGS_IMAGE_REL_I386: &[Flag<u16>] = &flags!(
        IMAGE_REL_I386_ABSOLUTE,
        IMAGE_REL_I386_DIR16,
        IMAGE_REL_I386_REL16,
        IMAGE_REL_I386_DIR32,
        IMAGE_REL_I386_DIR32NB,
        IMAGE_REL_I386_SEG12,
        IMAGE_REL_I386_SECTION,
        IMAGE_REL_I386_SECREL,
        IMAGE_REL_I386_TOKEN,
        IMAGE_REL_I386_SECREL7,
        IMAGE_REL_I386_REL32,
    );
    static FLAGS_IMAGE_REL_MIPS: &[Flag<u16>] = &flags!(
        IMAGE_REL_MIPS_ABSOLUTE,
        IMAGE_REL_MIPS_REFHALF,
        IMAGE_REL_MIPS_REFWORD,
        IMAGE_REL_MIPS_JMPADDR,
        IMAGE_REL_MIPS_REFHI,
        IMAGE_REL_MIPS_REFLO,
        IMAGE_REL_MIPS_GPREL,
        IMAGE_REL_MIPS_LITERAL,
        IMAGE_REL_MIPS_SECTION,
        IMAGE_REL_MIPS_SECREL,
        IMAGE_REL_MIPS_SECRELLO,
        IMAGE_REL_MIPS_SECRELHI,
        IMAGE_REL_MIPS_TOKEN,
        IMAGE_REL_MIPS_JMPADDR16,
        IMAGE_REL_MIPS_REFWORDNB,
        IMAGE_REL_MIPS_PAIR,
    );
    static FLAGS_IMAGE_REL_ALPHA: &[Flag<u16>] = &flags!(
        IMAGE_REL_ALPHA_ABSOLUTE,
        IMAGE_REL_ALPHA_REFLONG,
        IMAGE_REL_ALPHA_REFQUAD,
        IMAGE_REL_ALPHA_GPREL32,
        IMAGE_REL_ALPHA_LITERAL,
        IMAGE_REL_ALPHA_LITUSE,
        IMAGE_REL_ALPHA_GPDISP,
        IMAGE_REL_ALPHA_BRADDR,
        IMAGE_REL_ALPHA_HINT,
        IMAGE_REL_ALPHA_INLINE_REFLONG,
        IMAGE_REL_ALPHA_REFHI,
        IMAGE_REL_ALPHA_REFLO,
        IMAGE_REL_ALPHA_PAIR,
        IMAGE_REL_ALPHA_MATCH,
        IMAGE_REL_ALPHA_SECTION,
        IMAGE_REL_ALPHA_SECREL,
        IMAGE_REL_ALPHA_REFLONGNB,
        IMAGE_REL_ALPHA_SECRELLO,
        IMAGE_REL_ALPHA_SECRELHI,
        IMAGE_REL_ALPHA_REFQ3,
        IMAGE_REL_ALPHA_REFQ2,
        IMAGE_REL_ALPHA_REFQ1,
        IMAGE_REL_ALPHA_GPRELLO,
        IMAGE_REL_ALPHA_GPRELHI,
    );
    static FLAGS_IMAGE_REL_PPC: &[Flag<u16>] = &flags!(
        IMAGE_REL_PPC_ABSOLUTE,
        IMAGE_REL_PPC_ADDR64,
        IMAGE_REL_PPC_ADDR32,
        IMAGE_REL_PPC_ADDR24,
        IMAGE_REL_PPC_ADDR16,
        IMAGE_REL_PPC_ADDR14,
        IMAGE_REL_PPC_REL24,
        IMAGE_REL_PPC_REL14,
        IMAGE_REL_PPC_TOCREL16,
        IMAGE_REL_PPC_TOCREL14,
        IMAGE_REL_PPC_ADDR32NB,
        IMAGE_REL_PPC_SECREL,
        IMAGE_REL_PPC_SECTION,
        IMAGE_REL_PPC_IFGLUE,
        IMAGE_REL_PPC_IMGLUE,
        IMAGE_REL_PPC_SECREL16,
        IMAGE_REL_PPC_REFHI,
        IMAGE_REL_PPC_REFLO,
        IMAGE_REL_PPC_PAIR,
        IMAGE_REL_PPC_SECRELLO,
        IMAGE_REL_PPC_SECRELHI,
        IMAGE_REL_PPC_GPREL,
        IMAGE_REL_PPC_TOKEN,
    );
    static FLAGS_IMAGE_REL_PPC_BITS: &[Flag<u16>] = &flags!(
        IMAGE_REL_PPC_NEG,
        IMAGE_REL_PPC_BRTAKEN,
        IMAGE_REL_PPC_BRNTAKEN,
        IMAGE_REL_PPC_TOCDEFN,
    );
    static FLAGS_IMAGE_REL_SH: &[Flag<u16>] = &flags!(
        IMAGE_REL_SH3_ABSOLUTE,
        IMAGE_REL_SH3_DIRECT16,
        IMAGE_REL_SH3_DIRECT32,
        IMAGE_REL_SH3_DIRECT8,
        IMAGE_REL_SH3_DIRECT8_WORD,
        IMAGE_REL_SH3_DIRECT8_LONG,
        IMAGE_REL_SH3_DIRECT4,
        IMAGE_REL_SH3_DIRECT4_WORD,
        IMAGE_REL_SH3_DIRECT4_LONG,
        IMAGE_REL_SH3_PCREL8_WORD,
        IMAGE_REL_SH3_PCREL8_LONG,
        IMAGE_REL_SH3_PCREL12_WORD,
        IMAGE_REL_SH3_STARTOF_SECTION,
        IMAGE_REL_SH3_SIZEOF_SECTION,
        IMAGE_REL_SH3_SECTION,
        IMAGE_REL_SH3_SECREL,
        IMAGE_REL_SH3_DIRECT32_NB,
        IMAGE_REL_SH3_GPREL4_LONG,
        IMAGE_REL_SH3_TOKEN,
        IMAGE_REL_SHM_PCRELPT,
        IMAGE_REL_SHM_REFLO,
        IMAGE_REL_SHM_REFHALF,
        IMAGE_REL_SHM_RELLO,
        IMAGE_REL_SHM_RELHALF,
        IMAGE_REL_SHM_PAIR,
    );
    static FLAGS_IMAGE_REL_SH_BITS: &[Flag<u16>] = &flags!(IMAGE_REL_SH_NOMODE,);
    static FLAGS_IMAGE_REL_ARM: &[Flag<u16>] = &flags!(
        IMAGE_REL_ARM_ABSOLUTE,
        IMAGE_REL_ARM_ADDR32,
        IMAGE_REL_ARM_ADDR32NB,
        IMAGE_REL_ARM_BRANCH24,
        IMAGE_REL_ARM_BRANCH11,
        IMAGE_REL_ARM_TOKEN,
        IMAGE_REL_ARM_GPREL12,
        IMAGE_REL_ARM_GPREL7,
        IMAGE_REL_ARM_BLX24,
        IMAGE_REL_ARM_BLX11,
        IMAGE_REL_ARM_SECTION,
        IMAGE_REL_ARM_SECREL,
        IMAGE_REL_ARM_MOV32A,
        IMAGE_REL_ARM_MOV32T,
        IMAGE_REL_ARM_BRANCH20T,
        IMAGE_REL_ARM_BRANCH24T,
        IMAGE_REL_ARM_BLX23T,
    );
    static FLAGS_IMAGE_REL_AM: &[Flag<u16>] = &flags!(
        IMAGE_REL_AM_ABSOLUTE,
        IMAGE_REL_AM_ADDR32,
        IMAGE_REL_AM_ADDR32NB,
        IMAGE_REL_AM_CALL32,
        IMAGE_REL_AM_FUNCINFO,
        IMAGE_REL_AM_REL32_1,
        IMAGE_REL_AM_REL32_2,
        IMAGE_REL_AM_SECREL,
        IMAGE_REL_AM_SECTION,
        IMAGE_REL_AM_TOKEN,
    );
    static FLAGS_IMAGE_REL_ARM64: &[Flag<u16>] = &flags!(
        IMAGE_REL_ARM64_ABSOLUTE,
        IMAGE_REL_ARM64_ADDR32,
        IMAGE_REL_ARM64_ADDR32NB,
        IMAGE_REL_ARM64_BRANCH26,
        IMAGE_REL_ARM64_PAGEBASE_REL21,
        IMAGE_REL_ARM64_REL21,
        IMAGE_REL_ARM64_PAGEOFFSET_12A,
        IMAGE_REL_ARM64_PAGEOFFSET_12L,
        IMAGE_REL_ARM64_SECREL,
        IMAGE_REL_ARM64_SECREL_LOW12A,
        IMAGE_REL_ARM64_SECREL_HIGH12A,
        IMAGE_REL_ARM64_SECREL_LOW12L,
        IMAGE_REL_ARM64_TOKEN,
        IMAGE_REL_ARM64_SECTION,
        IMAGE_REL_ARM64_ADDR64,
        IMAGE_REL_ARM64_BRANCH19,
    );
    static FLAGS_IMAGE_REL_AMD64: &[Flag<u16>] = &flags!(
        IMAGE_REL_AMD64_ABSOLUTE,
        IMAGE_REL_AMD64_ADDR64,
        IMAGE_REL_AMD64_ADDR32,
        IMAGE_REL_AMD64_ADDR32NB,
        IMAGE_REL_AMD64_REL32,
        IMAGE_REL_AMD64_REL32_1,
        IMAGE_REL_AMD64_REL32_2,
        IMAGE_REL_AMD64_REL32_3,
        IMAGE_REL_AMD64_REL32_4,
        IMAGE_REL_AMD64_REL32_5,
        IMAGE_REL_AMD64_SECTION,
        IMAGE_REL_AMD64_SECREL,
        IMAGE_REL_AMD64_SECREL7,
        IMAGE_REL_AMD64_TOKEN,
        IMAGE_REL_AMD64_SREL32,
        IMAGE_REL_AMD64_PAIR,
        IMAGE_REL_AMD64_SSPAN32,
        IMAGE_REL_AMD64_EHANDLER,
        IMAGE_REL_AMD64_IMPORT_BR,
        IMAGE_REL_AMD64_IMPORT_CALL,
        IMAGE_REL_AMD64_CFG_BR,
        IMAGE_REL_AMD64_CFG_BR_REX,
        IMAGE_REL_AMD64_CFG_CALL,
        IMAGE_REL_AMD64_INDIR_BR,
        IMAGE_REL_AMD64_INDIR_BR_REX,
        IMAGE_REL_AMD64_INDIR_CALL,
        IMAGE_REL_AMD64_INDIR_BR_SWITCHTABLE_FIRST,
        IMAGE_REL_AMD64_INDIR_BR_SWITCHTABLE_LAST,
    );
    static FLAGS_IMAGE_REL_IA64: &[Flag<u16>] = &flags!(
        IMAGE_REL_IA64_ABSOLUTE,
        IMAGE_REL_IA64_IMM14,
        IMAGE_REL_IA64_IMM22,
        IMAGE_REL_IA64_IMM64,
        IMAGE_REL_IA64_DIR32,
        IMAGE_REL_IA64_DIR64,
        IMAGE_REL_IA64_PCREL21B,
        IMAGE_REL_IA64_PCREL21M,
        IMAGE_REL_IA64_PCREL21F,
        IMAGE_REL_IA64_GPREL22,
        IMAGE_REL_IA64_LTOFF22,
        IMAGE_REL_IA64_SECTION,
        IMAGE_REL_IA64_SECREL22,
        IMAGE_REL_IA64_SECREL64I,
        IMAGE_REL_IA64_SECREL32,
        IMAGE_REL_IA64_DIR32NB,
        IMAGE_REL_IA64_SREL14,
        IMAGE_REL_IA64_SREL22,
        IMAGE_REL_IA64_SREL32,
        IMAGE_REL_IA64_UREL32,
        IMAGE_REL_IA64_PCREL60X,
        IMAGE_REL_IA64_PCREL60B,
        IMAGE_REL_IA64_PCREL60F,
        IMAGE_REL_IA64_PCREL60I,
        IMAGE_REL_IA64_PCREL60M,
        IMAGE_REL_IA64_IMMGPREL64,
        IMAGE_REL_IA64_TOKEN,
        IMAGE_REL_IA64_GPREL32,
        IMAGE_REL_IA64_ADDEND,
    );
    static FLAGS_IMAGE_REL_CEF: &[Flag<u16>] = &flags!(
        IMAGE_REL_CEF_ABSOLUTE,
        IMAGE_REL_CEF_ADDR32,
        IMAGE_REL_CEF_ADDR64,
        IMAGE_REL_CEF_ADDR32NB,
        IMAGE_REL_CEF_SECTION,
        IMAGE_REL_CEF_SECREL,
        IMAGE_REL_CEF_TOKEN,
    );
    static FLAGS_IMAGE_REL_CEE: &[Flag<u16>] = &flags!(
        IMAGE_REL_CEE_ABSOLUTE,
        IMAGE_REL_CEE_ADDR32,
        IMAGE_REL_CEE_ADDR64,
        IMAGE_REL_CEE_ADDR32NB,
        IMAGE_REL_CEE_SECTION,
        IMAGE_REL_CEE_SECREL,
        IMAGE_REL_CEE_TOKEN,
    );
    static FLAGS_IMAGE_REL_M32R: &[Flag<u16>] = &flags!(
        IMAGE_REL_M32R_ABSOLUTE,
        IMAGE_REL_M32R_ADDR32,
        IMAGE_REL_M32R_ADDR32NB,
        IMAGE_REL_M32R_ADDR24,
        IMAGE_REL_M32R_GPREL16,
        IMAGE_REL_M32R_PCREL24,
        IMAGE_REL_M32R_PCREL16,
        IMAGE_REL_M32R_PCREL8,
        IMAGE_REL_M32R_REFHALF,
        IMAGE_REL_M32R_REFHI,
        IMAGE_REL_M32R_REFLO,
        IMAGE_REL_M32R_PAIR,
        IMAGE_REL_M32R_SECTION,
        IMAGE_REL_M32R_SECREL32,
        IMAGE_REL_M32R_TOKEN,
    );
    static FLAGS_IMAGE_REL_EBC: &[Flag<u16>] = &flags!(
        IMAGE_REL_EBC_ABSOLUTE,
        IMAGE_REL_EBC_ADDR32NB,
        IMAGE_REL_EBC_REL32,
        IMAGE_REL_EBC_SECTION,
        IMAGE_REL_EBC_SECREL,
    );
    static FLAGS_IMAGE_SYM: &[Flag<u16>] =
        &flags!(IMAGE_SYM_UNDEFINED, IMAGE_SYM_ABSOLUTE, IMAGE_SYM_DEBUG,);
    static FLAGS_IMAGE_SYM_TYPE: &[Flag<u16>] = &flags!(
        IMAGE_SYM_TYPE_NULL,
        IMAGE_SYM_TYPE_VOID,
        IMAGE_SYM_TYPE_CHAR,
        IMAGE_SYM_TYPE_SHORT,
        IMAGE_SYM_TYPE_INT,
        IMAGE_SYM_TYPE_LONG,
        IMAGE_SYM_TYPE_FLOAT,
        IMAGE_SYM_TYPE_DOUBLE,
        IMAGE_SYM_TYPE_STRUCT,
        IMAGE_SYM_TYPE_UNION,
        IMAGE_SYM_TYPE_ENUM,
        IMAGE_SYM_TYPE_MOE,
        IMAGE_SYM_TYPE_BYTE,
        IMAGE_SYM_TYPE_WORD,
        IMAGE_SYM_TYPE_UINT,
        IMAGE_SYM_TYPE_DWORD,
        IMAGE_SYM_TYPE_PCODE,
    );
    static FLAGS_IMAGE_SYM_DTYPE: &[Flag<u16>] = &flags!(
        IMAGE_SYM_DTYPE_NULL,
        IMAGE_SYM_DTYPE_POINTER,
        IMAGE_SYM_DTYPE_FUNCTION,
        IMAGE_SYM_DTYPE_ARRAY,
    );
    static FLAGS_IMAGE_SYM_CLASS: &[Flag<u8>] = &flags!(
        IMAGE_SYM_CLASS_END_OF_FUNCTION,
        IMAGE_SYM_CLASS_NULL,
        IMAGE_SYM_CLASS_AUTOMATIC,
        IMAGE_SYM_CLASS_EXTERNAL,
        IMAGE_SYM_CLASS_STATIC,
        IMAGE_SYM_CLASS_REGISTER,
        IMAGE_SYM_CLASS_EXTERNAL_DEF,
        IMAGE_SYM_CLASS_LABEL,
        IMAGE_SYM_CLASS_UNDEFINED_LABEL,
        IMAGE_SYM_CLASS_MEMBER_OF_STRUCT,
        IMAGE_SYM_CLASS_ARGUMENT,
        IMAGE_SYM_CLASS_STRUCT_TAG,
        IMAGE_SYM_CLASS_MEMBER_OF_UNION,
        IMAGE_SYM_CLASS_UNION_TAG,
        IMAGE_SYM_CLASS_TYPE_DEFINITION,
        IMAGE_SYM_CLASS_UNDEFINED_STATIC,
        IMAGE_SYM_CLASS_ENUM_TAG,
        IMAGE_SYM_CLASS_MEMBER_OF_ENUM,
        IMAGE_SYM_CLASS_REGISTER_PARAM,
        IMAGE_SYM_CLASS_BIT_FIELD,
        IMAGE_SYM_CLASS_FAR_EXTERNAL,
        IMAGE_SYM_CLASS_BLOCK,
        IMAGE_SYM_CLASS_FUNCTION,
        IMAGE_SYM_CLASS_END_OF_STRUCT,
        IMAGE_SYM_CLASS_FILE,
        IMAGE_SYM_CLASS_SECTION,
        IMAGE_SYM_CLASS_WEAK_EXTERNAL,
        IMAGE_SYM_CLASS_CLR_TOKEN,
    );
    static FLAGS_IMAGE_COMDAT_SELECT: &[Flag<u8>] = &flags!(
        IMAGE_COMDAT_SELECT_NODUPLICATES,
        IMAGE_COMDAT_SELECT_ANY,
        IMAGE_COMDAT_SELECT_SAME_SIZE,
        IMAGE_COMDAT_SELECT_EXACT_MATCH,
        IMAGE_COMDAT_SELECT_ASSOCIATIVE,
        IMAGE_COMDAT_SELECT_LARGEST,
        IMAGE_COMDAT_SELECT_NEWEST,
    );
    static FLAGS_IMAGE_SUBSYSTEM: &[Flag<u16>] = &flags!(
        IMAGE_SUBSYSTEM_UNKNOWN,
        IMAGE_SUBSYSTEM_NATIVE,
        IMAGE_SUBSYSTEM_WINDOWS_GUI,
        IMAGE_SUBSYSTEM_WINDOWS_CUI,
        IMAGE_SUBSYSTEM_OS2_CUI,
        IMAGE_SUBSYSTEM_POSIX_CUI,
        IMAGE_SUBSYSTEM_NATIVE_WINDOWS,
        IMAGE_SUBSYSTEM_WINDOWS_CE_GUI,
        IMAGE_SUBSYSTEM_EFI_APPLICATION,
        IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER,
        IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER,
        IMAGE_SUBSYSTEM_EFI_ROM,
        IMAGE_SUBSYSTEM_XBOX,
        IMAGE_SUBSYSTEM_WINDOWS_BOOT_APPLICATION,
        IMAGE_SUBSYSTEM_XBOX_CODE_CATALOG,
    );
    static FLAGS_IMAGE_DLLCHARACTERISTICS: &[Flag<u16>] = &flags!(
        IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA,
        IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE,
        IMAGE_DLLCHARACTERISTICS_FORCE_INTEGRITY,
        IMAGE_DLLCHARACTERISTICS_NX_COMPAT,
        IMAGE_DLLCHARACTERISTICS_NO_ISOLATION,
        IMAGE_DLLCHARACTERISTICS_NO_SEH,
        IMAGE_DLLCHARACTERISTICS_NO_BIND,
        IMAGE_DLLCHARACTERISTICS_APPCONTAINER,
        IMAGE_DLLCHARACTERISTICS_WDM_DRIVER,
        IMAGE_DLLCHARACTERISTICS_GUARD_CF,
        IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE,
    );
    static FLAGS_IMAGE_DIRECTORY_ENTRY: &[Flag<usize>] = &flags!(
        IMAGE_DIRECTORY_ENTRY_EXPORT,
        IMAGE_DIRECTORY_ENTRY_IMPORT,
        IMAGE_DIRECTORY_ENTRY_RESOURCE,
        IMAGE_DIRECTORY_ENTRY_EXCEPTION,
        IMAGE_DIRECTORY_ENTRY_SECURITY,
        IMAGE_DIRECTORY_ENTRY_BASERELOC,
        IMAGE_DIRECTORY_ENTRY_DEBUG,
        IMAGE_DIRECTORY_ENTRY_ARCHITECTURE,
        IMAGE_DIRECTORY_ENTRY_GLOBALPTR,
        IMAGE_DIRECTORY_ENTRY_TLS,
        IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG,
        IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT,
        IMAGE_DIRECTORY_ENTRY_IAT,
        IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT,
        IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR,
    );
}
