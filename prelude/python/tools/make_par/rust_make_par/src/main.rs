/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs;
use std::path::PathBuf;

use clap::Parser;
use rust_make_par::FileProcessor;
use rust_make_par::ParInfo;
use rust_make_par::process_maybe_add_init;
use rust_make_par::process_symlink;
use rust_make_par::read_json_manifest;
use rust_make_par::read_manifest_list;
use rust_make_par::read_text_manifest;
use rust_make_par::write_inits;
use serde::Serialize;

#[derive(Parser, Serialize)]
struct Args {
    #[arg(long, value_name = "OUTDIR", required = true)]
    output_path: PathBuf,

    #[arg(long, value_name = "SOURCES", required = true)]
    sources: PathBuf,

    #[arg(long, value_name = "BYTECODE")]
    bytecode: Option<PathBuf>,

    #[arg(long, value_name = "EXTENSIONS")]
    extensions: Option<PathBuf>,

    #[arg(long, value_name = "DWPS")]
    dwps: Option<PathBuf>,

    #[arg(long, value_name = "SHARED_LIBS")]
    shared_libs: Option<PathBuf>,

    #[arg(long, value_name = "DEBUGINFO")]
    debuginfo: Option<PathBuf>,

    #[arg(long, value_name = "RESOURCES")]
    resources: Option<PathBuf>,

    #[arg(long, value_name = "EXTRAS")]
    extras: Option<PathBuf>,

    #[arg(long, value_name = "GENERATED")]
    generated: Option<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let mut info = ParInfo::new(args.output_path);
    info.create_output_dir()?;

    fs::create_dir_all(&info.output_path)?;

    let source_processors: Vec<FileProcessor> = vec![process_symlink, process_maybe_add_init];

    read_manifest_list(&args.sources, &mut info, &source_processors)?;

    if let Some(extras) = &args.extras {
        read_text_manifest(extras, &mut info, &source_processors)?;
    }
    if let Some(resources) = &args.resources {
        read_manifest_list(resources, &mut info, &source_processors)?;
    }
    if let Some(extensions) = &args.extensions {
        read_json_manifest(extensions, &mut info, &source_processors)?;
    }

    let file_processors: Vec<FileProcessor> = vec![process_symlink];

    if let Some(shared_libs) = &args.shared_libs {
        read_text_manifest(shared_libs, &mut info, &file_processors)?;
    }
    if let Some(generated) = &args.generated {
        read_text_manifest(generated, &mut info, &file_processors)?;
    }
    if let Some(bytecode) = &args.bytecode {
        read_manifest_list(bytecode, &mut info, &file_processors)?;
    }
    write_inits(&mut info)?;
    Ok(())
}
