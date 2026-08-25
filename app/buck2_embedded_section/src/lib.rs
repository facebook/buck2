/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Reads data embedded in the running executable as a non-allocated ELF
//! section by the `elf_sections` attr of the rule that built it.

use std::fs::File;
use std::io::Read;
use std::io::Seek;
use std::io::SeekFrom;
use std::io::Write;

use buck2_error::BuckErrorContext;
use buck2_error::ErrorTag;
use buck2_error::buck2_error;
use object::Object;
use object::ObjectSection;
use object::ReadCache;

/// The kernel resolves this to the inode we are executing, so it still reaches
/// our own image after the binary has been deleted or replaced on disk.
const SELF_EXE: &str = "/proc/self/exe";

/// How a payload is stored in its section.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SectionEncoding {
    /// The section holds the payload verbatim.
    Raw,
    /// The section holds the payload as a single zstd frame.
    ///
    /// Raw zstd rather than Managed Compression: this must decode with no
    /// configuration or network access, and outside Meta.
    Zstd,
}

/// A section embedded in the running executable.
pub struct EmbeddedSection<'a> {
    /// Must match the key under `elf_sections` on the rule that produced this
    /// executable.
    pub name: &'a str,
    pub encoding: SectionEncoding,
}

impl EmbeddedSection<'_> {
    /// Writes the payload to `writer`.
    ///
    /// The section is streamed off disk, so this costs a buffer rather than the
    /// size of the payload.
    pub fn copy_to(&self, writer: &mut impl Write) -> buck2_error::Result<()> {
        let file = File::open(SELF_EXE)
            .buck_error_context("Error opening `/proc/self/exe` to read an embedded section")?;

        // Only the headers are read here; `ReadCache` keeps us from paging in
        // the rest of the executable to find one section.
        let cache = ReadCache::new(file);
        let (offset, len) = {
            let exe = object::File::parse(&cache).map_err(|e| {
                buck2_error!(
                    ErrorTag::Environment,
                    "Error parsing `/proc/self/exe` to read embedded section `{}`: {e}",
                    self.name
                )
            })?;
            let section = exe.section_by_name(self.name).ok_or_else(|| {
                buck2_error!(
                    ErrorTag::Environment,
                    "Embedded section `{}` not found",
                    self.name
                )
            })?;
            section.file_range().ok_or_else(|| {
                buck2_error!(
                    ErrorTag::Environment,
                    "Embedded section `{}` has no contents on disk",
                    self.name
                )
            })?
        };

        let mut file = cache.into_inner();
        file.seek(SeekFrom::Start(offset))
            .buck_error_context("Error seeking to an embedded section")?;
        let mut payload = file.take(len);

        match self.encoding {
            SectionEncoding::Raw => {
                std::io::copy(&mut payload, writer)
                    .buck_error_context("Error reading an embedded section")?;
            }
            SectionEncoding::Zstd => {
                zstd::stream::copy_decode(&mut payload, &mut *writer)
                    .buck_error_context("Error decompressing an embedded section")?;
            }
        }

        writer
            .flush()
            .buck_error_context("Error flushing an embedded section")
    }
}
