use object::read::archive::ArchiveFile;
use object::read::macho::{DyldCache, FatArch, FatHeader};
use object::{Endianness, Object, ObjectComdat, ObjectSection, ObjectSymbol};
use std::{env, fs, process};

fn main() {
    let mut args = env::args();
    let cmd = args.next().unwrap();
    if args.len() == 0 {
        eprintln!("Usage: {} <file> [<member>...]", cmd);
        process::exit(1);
    }
    let file_path = args.next().unwrap();
    let mut member_names: Vec<_> = args.map(|name| (name, false)).collect();

    let file = match fs::File::open(&file_path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Failed to open file '{}': {}", file_path, err,);
            process::exit(1);
        }
    };
    let file = match unsafe { memmap2::Mmap::map(&file) } {
        Ok(mmap) => mmap,
        Err(err) => {
            eprintln!("Failed to map file '{}': {}", file_path, err,);
            process::exit(1);
        }
    };

    if let Ok(archive) = ArchiveFile::parse(&*file) {
        eprintln!("Format: Archive (kind: {:?})", archive.kind());
        for member in archive.members() {
            if let Ok(member) = member {
                if find_member(&mut member_names, member.name()) {
                    println!();
                    println!("{}:", String::from_utf8_lossy(member.name()));
                    if let Ok(data) = member.data(&*file) {
                        dump_object(data);
                    }
                }
            }
        }
    } else if let Ok(arches) = FatHeader::parse_arch32(&*file) {
        println!("Format: Mach-O Fat 32");
        for arch in arches {
            println!();
            println!("Fat Arch: {:?}", arch.architecture());
            if let Ok(data) = arch.data(&*file) {
                dump_object(data);
            }
        }
    } else if let Ok(arches) = FatHeader::parse_arch64(&*file) {
        println!("Format: Mach-O Fat 64");
        for arch in arches {
            println!();
            println!("Fat Arch: {:?}", arch.architecture());
            if let Ok(data) = arch.data(&*file) {
                dump_object(data);
            }
        }
    } else if let Ok(cache) = DyldCache::<Endianness>::parse(&*file) {
        println!("Format: dyld cache {:?}-endian", cache.endianness());
        println!("Architecture: {:?}", cache.architecture());
        for image in cache.images() {
            if let Ok(path) = image.path() {
                if find_member(&mut member_names, path.as_bytes()) {
                    println!();
                    println!("{}:", path);
                    let file = match image.parse_object() {
                        Ok(file) => file,
                        Err(err) => {
                            eprintln!("Failed to parse file: {}", err);
                            continue;
                        }
                    };
                    dump_parsed_object(&file);
                }
            }
        }
    } else {
        dump_object(&*file);
    }

    for (name, found) in member_names {
        if !found {
            eprintln!("Failed to find member '{}", name);
        }
    }
}

fn find_member(member_names: &mut [(String, bool)], name: &[u8]) -> bool {
    if member_names.is_empty() {
        return true;
    }
    match member_names.iter().position(|x| x.0.as_bytes() == name) {
        Some(i) => {
            member_names[i].1 = true;
            true
        }
        None => false,
    }
}

fn dump_object(data: &[u8]) {
    let file = match object::File::parse(data) {
        Ok(file) => file,
        Err(err) => {
            println!("Failed to parse file: {}", err);
            return;
        }
    };
    dump_parsed_object(&file);
}

fn dump_parsed_object(file: &object::File) {
    println!(
        "Format: {:?} {:?}-endian {}-bit",
        file.format(),
        file.endianness(),
        if file.is_64() { "64" } else { "32" }
    );
    println!("Architecture: {:?}", file.architecture());
    println!("Flags: {:x?}", file.flags());
    println!("Relative Address Base: {:x?}", file.relative_address_base());
    println!("Entry Address: {:x?}", file.entry());

    match file.mach_uuid() {
        Ok(Some(uuid)) => println!("Mach UUID: {:x?}", uuid),
        Ok(None) => {}
        Err(e) => println!("Failed to parse Mach UUID: {}", e),
    }
    match file.build_id() {
        Ok(Some(build_id)) => println!("Build ID: {:x?}", build_id),
        Ok(None) => {}
        Err(e) => println!("Failed to parse build ID: {}", e),
    }
    match file.gnu_debuglink() {
        Ok(Some((filename, crc))) => println!(
            "GNU debug link: {} CRC: {:08x}",
            String::from_utf8_lossy(filename),
            crc,
        ),
        Ok(None) => {}
        Err(e) => println!("Failed to parse GNU debug link: {}", e),
    }
    match file.gnu_debugaltlink() {
        Ok(Some((filename, build_id))) => println!(
            "GNU debug alt link: {}, build ID: {:x?}",
            String::from_utf8_lossy(filename),
            build_id,
        ),
        Ok(None) => {}
        Err(e) => println!("Failed to parse GNU debug alt link: {}", e),
    }
    match file.pdb_info() {
        Ok(Some(info)) => println!(
            "PDB file: {}, GUID: {:x?}, Age: {}",
            String::from_utf8_lossy(info.path()),
            info.guid(),
            info.age()
        ),
        Ok(None) => {}
        Err(e) => println!("Failed to parse PE CodeView info: {}", e),
    }

    for segment in file.segments() {
        println!("{:x?}", segment);
    }

    for section in file.sections() {
        println!("{}: {:x?}", section.index().0, section);
    }

    for comdat in file.comdats() {
        print!("{:?} Sections:", comdat);
        for section in comdat.sections() {
            print!(" {}", section.0);
        }
        println!();
    }

    println!();
    println!("Symbols");
    for symbol in file.symbols() {
        println!("{}: {:x?}", symbol.index().0, symbol);
    }

    for section in file.sections() {
        if section.relocations().next().is_some() {
            println!(
                "\n{} relocations",
                section.name().unwrap_or("<invalid name>")
            );
            for relocation in section.relocations() {
                println!("{:x?}", relocation);
            }
        }
    }

    println!();
    println!("Dynamic symbols");
    for symbol in file.dynamic_symbols() {
        println!("{}: {:x?}", symbol.index().0, symbol);
    }

    if let Some(relocations) = file.dynamic_relocations() {
        println!();
        println!("Dynamic relocations");
        for relocation in relocations {
            println!("{:x?}", relocation);
        }
    }

    let imports = file.imports().unwrap();
    if !imports.is_empty() {
        println!();
        for import in imports {
            println!("{:?}", import);
        }
    }

    let exports = file.exports().unwrap();
    if !exports.is_empty() {
        println!();
        for export in exports {
            println!("{:x?}", export);
        }
    }
}
