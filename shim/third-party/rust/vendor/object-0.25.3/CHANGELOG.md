# `object` Change Log

--------------------------------------------------------------------------------

## 0.25.3

Released 2021/06/12.

### Added

* Added `RelocationEncoding::AArch64Call`.
  [#322](https://github.com/gimli-rs/object/pull/322)

--------------------------------------------------------------------------------

## 0.25.2

Released 2021/06/04.

### Added

* Added `Architecture::X86_64_X32`.
  [#320](https://github.com/gimli-rs/object/pull/320)

--------------------------------------------------------------------------------

## 0.25.1

Released 2021/06/03.

### Changed

* write: Fix choice of `SHT_REL` or `SHT_RELA` for most architectures.
  [#318](https://github.com/gimli-rs/object/pull/318)

* write: Fix relocation encoding for MIPS64EL.
  [#318](https://github.com/gimli-rs/object/pull/318)

--------------------------------------------------------------------------------

## 0.25.0

Released 2021/06/02.

### Breaking changes

* Added `non_exhaustive` to most public enums.
  [#306](https://github.com/gimli-rs/object/pull/306)

* `MachHeader::parse` and `MachHeader::load_commands` now require a header offset.
  [#304](https://github.com/gimli-rs/object/pull/304)

* Added `ReadRef::read_bytes_at_until`.
  [#308](https://github.com/gimli-rs/object/pull/308)

* `PeFile::entry`, `PeSection::address` and `PeSegment::address` now return a
  virtual address instead of a RVA.
  [#315](https://github.com/gimli-rs/object/pull/315)

### Added

* Added `pod::from_bytes_mut`, `pod::slice_from_bytes_mut`, `pod::bytes_of_mut`,
  and `pod::bytes_of_slice_mut`.
  [#296](https://github.com/gimli-rs/object/pull/296)
  [#297](https://github.com/gimli-rs/object/pull/297)

* Added `Object::pdb_info`.
  [#298](https://github.com/gimli-rs/object/pull/298)

* Added `read::macho::DyldCache`, other associated definitions,
  and support for these in the examples.
  [#308](https://github.com/gimli-rs/object/pull/308)

* Added more architecture support.
  [#303](https://github.com/gimli-rs/object/pull/303)
  [#309](https://github.com/gimli-rs/object/pull/309)

* Derive more traits for enums.
  [#311](https://github.com/gimli-rs/object/pull/311)

* Added `Object::relative_address_base`.
  [#315](https://github.com/gimli-rs/object/pull/315)

### Changed

* Improved performance for string parsing.
  [#302](https://github.com/gimli-rs/object/pull/302)

* `objdump` example allows selecting container members.
  [#308](https://github.com/gimli-rs/object/pull/308)
