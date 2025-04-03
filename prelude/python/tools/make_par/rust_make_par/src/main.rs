/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::fs;
use std::fs::File;
use std::io;
use std::io::BufRead;
use std::io::BufReader;
use std::path::Path;
use std::path::PathBuf;

use anyhow::Context as _;
use clap::Parser;
use serde::Serialize;
use serde_json::Value;

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

fn relpath(link: &Path, target: &Path) -> PathBuf {
    let mut from = link.components().peekable();
    let mut to = target.components().peekable();
    while let (Some(a), Some(b)) = (from.peek(), to.peek()) {
        if a != b {
            break;
        }
        from.next();
        to.next();
    }
    let mut result = PathBuf::new();
    result.extend(from.map(|_| ".."));
    result.extend(to);
    result
}

fn process_file(
    output_path: &Path,
    file_path: impl AsRef<Path>,
    module_map: &mut HashMap<String, bool>,
) -> anyhow::Result<()> {
    let mut path = file_path.as_ref();
    const SOURCE_EXTENSIONS: [&str; 5] = ["py", "so", "empty_stub", "pyd", "pyc"];
    const IGNORE_PATHS: [&str; 1] = ["runtime/lib"];

    if IGNORE_PATHS.iter().any(|&ignore| path.starts_with(ignore)) {
        return Ok(());
    }
    if let Some(file_ext) = path.extension() {
        if !SOURCE_EXTENSIONS.iter().any(|&ext| ext == file_ext) {
            return Ok(());
        }
    } else {
        // For directories we recursively traverse to find any source files
        let dir_path = output_path.join(path);
        if dir_path.is_dir() {
            for entry in fs::read_dir(dir_path)? {
                let entry = entry?;
                let entry_path = entry.path();
                let relative_path = entry_path.strip_prefix(output_path)?;
                process_file(output_path, relative_path, module_map)?;
            }
        }
        return Ok(());
    }
    if let Some(file_name) = path.file_name().and_then(|f| f.to_str()) {
        if file_name == "__init__.py" {
            if let Some(dir) = path.parent().and_then(|d| d.to_str()) {
                module_map.insert(dir.to_string(), false);
            }
        }
    }

    while let Some(parent) = path.parent() {
        if let Some(path_str) = parent.to_str() {
            module_map.entry(path_str.to_string()).or_insert(true);
        }
        path = parent;
    }
    Ok(())
}

fn symlink_files(manifest: &Path, output_path: &Path) -> anyhow::Result<()> {
    let file = File::open(manifest)
        .with_context(|| format!("Failed to open manifest: {}", manifest.display()))?;

    let reader = BufReader::new(file);

    for (line_num, line) in reader.lines().enumerate() {
        let line =
            line.with_context(|| format!("Failed to read manifest: {}", manifest.display()))?;

        let parts: Vec<&str> = line.split("::").collect();
        if parts.len() != 2 {
            return Err(anyhow::anyhow!(
                "Invalid manifest format at {}: {}",
                line_num + 1,
                line
            ));
        }

        let source = parts[0].trim();
        let dest = parts[1].trim();
        symlink_file(output_path.join(dest), PathBuf::from(source), true)?;
    }

    Ok(())
}

fn symlink_sources(
    manifest: &Path,
    output_path: &Path,
    module_map: &mut HashMap<String, bool>,
) -> anyhow::Result<()> {
    let file = File::open(manifest).with_context(|| {
        format!(
            "Failed to open sources manifest file: {}",
            manifest.display()
        )
    })?;

    let reader = BufReader::new(file);

    for (line_num, line) in reader.lines().enumerate() {
        let line =
            line.with_context(|| format!("Failed to read manifest: {}", manifest.display()))?;

        let parts: Vec<&str> = line.split("::").collect();
        if parts.len() != 2 {
            return Err(anyhow::anyhow!(
                "Invalid manifest format at {}: {}",
                line_num + 1,
                line
            ));
        };

        let source = parts[0].trim();
        let dest = parts[1].trim();
        symlink_file(output_path.join(dest), PathBuf::from(source), true)?;
        process_file(output_path, PathBuf::from(dest), module_map)?;
    }

    Ok(())
}

fn symlink_bytecode_manifest(manifest: &Path, output_path: &Path) -> anyhow::Result<()> {
    let json_str = fs::read_to_string(manifest).with_context(|| {
        format!(
            "Failed to read bytecode manifest file {}",
            manifest.display()
        )
    })?;

    let json_data: Value = serde_json::from_str(&json_str).with_context(|| {
        format!(
            "Failed to parse bytecode manifest JSON {}",
            manifest.display()
        )
    })?;

    let arr = json_data.as_array().with_context(|| {
        format!(
            "Bytecode manifest {} is not a JSON array",
            manifest.display()
        )
    })?;

    for (idx, item) in arr.iter().enumerate() {
        let item_arr = item
            .as_array()
            .with_context(|| format!("entry is not an array: {}", item))?;

        if item_arr.len() != 3 {
            return Err(anyhow::anyhow!(
                "Item {} in bytecode manifest {} does not have 3 elements",
                idx,
                manifest.display()
            ));
        }

        if let (Some(dest), Some(src)) = (item_arr[0].as_str(), item_arr[1].as_str()) {
            symlink_file(output_path.join(dest), PathBuf::from(src), true)?;
        }
    }

    Ok(())
}

fn symlink_bytecode(manifest: &Path, output_path: &Path) -> anyhow::Result<()> {
    let file = File::open(manifest)
        .with_context(|| format!("Failed to open manifest file: {}", manifest.display()))?;

    let reader = BufReader::new(file);

    for line in reader.lines() {
        let line =
            line.with_context(|| format!("Failed to read manifest: {}", manifest.display()))?;
        symlink_bytecode_manifest(&PathBuf::from(line), output_path)?;
    }

    Ok(())
}

fn xplat_symlink(src: &Path, dst: &Path) -> std::io::Result<()> {
    #[cfg(windows)]
    return std::os::windows::fs::symlink_file(src, dst);
    #[cfg(unix)]
    return std::os::unix::fs::symlink(src, dst);
    #[cfg(all(not(unix), not(windows)))]
    compile_error!("symlink is not supported by the system");
}

fn symlink_file(
    link_path: impl AsRef<Path>,
    input_path: impl AsRef<Path>,
    allow_duplicates: bool,
) -> anyhow::Result<()> {
    // Create a relative symlink from the input path to the output path
    let link_path = link_path.as_ref();
    let input_path = input_path.as_ref();

    let parent = link_path
        .parent()
        .with_context(|| format!("Could not get path root {}", link_path.display()))?;

    let target = relpath(parent, input_path);

    fs::create_dir_all(parent)?;

    match xplat_symlink(&target, link_path) {
        Ok(()) => Ok(()),
        Err(err) if err.kind() == io::ErrorKind::AlreadyExists && allow_duplicates => Ok(()),
        Err(err) => Err(anyhow::anyhow!(
            "Failed to create symlink {} -> {}: {}",
            link_path.display(),
            target.display(),
            err
        )),
    }
}

fn write_inits(output_path: &Path, module_map: &mut HashMap<String, bool>) -> anyhow::Result<()> {
    for (key, value) in module_map {
        if *value {
            let mut init_path = output_path.join(key);
            init_path.push("__init__.py");
            File::create(&init_path).with_context(|| {
                format!(
                    "Failed to create __init__.py file {}: ",
                    init_path.display()
                )
            })?;
        }
    }
    Ok(())
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    fs::create_dir_all(&args.output_path)?;

    let mut module_map = HashMap::<String, bool>::new();
    module_map.insert("".to_string(), false);
    symlink_sources(&args.sources, &args.output_path, &mut module_map)?;
    if let Some(extras) = &args.extras {
        symlink_sources(extras, &args.output_path, &mut module_map)?;
    }
    if let Some(resources) = &args.resources {
        symlink_sources(resources, &args.output_path, &mut module_map)?;
    }
    if let Some(extensions) = &args.extensions {
        symlink_sources(extensions, &args.output_path, &mut module_map)?;
    }
    if let Some(shared_libs) = &args.shared_libs {
        symlink_files(shared_libs, &args.output_path)?;
    }
    if let Some(generated) = &args.generated {
        symlink_files(generated, &args.output_path)?;
    }
    if let Some(bytecode) = &args.bytecode {
        symlink_bytecode(bytecode, &args.output_path)?;
    }
    write_inits(&args.output_path, &mut module_map)?;
    Ok(())
}
