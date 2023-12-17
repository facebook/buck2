/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Write;
use std::path::PathBuf;

#[derive(Debug, PartialEq)]
pub enum PrintOutputsFormat {
    Plain,
    Simple,
    Json,
}

pub struct PrintOutputs<W> {
    out: W,
    root_path: Option<PathBuf>,
    format: PrintOutputsFormat,
    empty: bool,
}

impl<W: Write> PrintOutputs<W> {
    pub fn new(
        mut out: W,
        root_path: Option<PathBuf>,
        format: PrintOutputsFormat,
    ) -> anyhow::Result<Self> {
        if format == PrintOutputsFormat::Json {
            write!(out, "{{")?;
        }
        Ok(PrintOutputs {
            out,
            root_path,
            format,
            empty: true,
        })
    }

    pub fn output(&mut self, target: &str, path: Option<&str>) -> anyhow::Result<()> {
        let windows_path;
        let absolute_path;
        let absolute_path_lossy;
        let path = if let Some(mut path) = path {
            if cfg!(windows) {
                windows_path = path.replace('/', "\\");
                path = &windows_path;
            }
            if let Some(root_path) = &self.root_path {
                absolute_path = root_path.join(path);
                absolute_path_lossy = absolute_path.to_string_lossy();
                &absolute_path_lossy
            } else {
                path
            }
        } else {
            ""
        };

        match self.format {
            PrintOutputsFormat::Plain => {
                writeln!(self.out, "{} {}", target, path)?;
            }
            PrintOutputsFormat::Simple => {
                writeln!(self.out, "{}", path)?;
            }
            PrintOutputsFormat::Json => {
                if !self.empty {
                    write!(self.out, ",")?;
                }
                serde_json::to_writer(&mut self.out, target)?;
                write!(self.out, ":")?;
                serde_json::to_writer(&mut self.out, path)?;
                self.empty = false;
            }
        }

        Ok(())
    }

    pub fn finish(&mut self) -> anyhow::Result<()> {
        if self.format == PrintOutputsFormat::Json {
            writeln!(self.out, "}}")?;
        }
        Ok(())
    }
}
