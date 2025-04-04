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
use serde_json::Value;

pub struct ParInfo {
    pub output_path: PathBuf,
    module_map: HashMap<String, bool>,
}

impl ParInfo {
    pub fn new(output_path: PathBuf) -> ParInfo {
        ParInfo {
            output_path,
            module_map: HashMap::new(),
        }
    }
    pub fn create_output_dir(&self) -> anyhow::Result<()> {
        fs::create_dir_all(&self.output_path)
            .with_context(|| format!("Failed to create dir {}", self.output_path.display()))?;
        Ok(())
    }
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

pub fn write_inits(info: &mut ParInfo) -> anyhow::Result<()> {
    // Insert an empty string to make sure we do not generate a __init__ file at the root of the
    // par
    info.module_map.insert("".to_string(), false);
    for (key, value) in info.module_map.iter() {
        if *value {
            let mut init_path = info.output_path.join(key);
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

pub type FileProcessor = fn(&Path, &Path, &mut ParInfo) -> anyhow::Result<()>;

// A text manifest is a newline separated list of:
// path/to/source_artifact::path/in/par
pub fn read_text_manifest(
    manifest: &Path,
    info: &mut ParInfo,
    processors: &Vec<FileProcessor>,
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

        let src = PathBuf::from(parts[0].trim());
        let dest = PathBuf::from(parts[1].trim());
        for processor in processors {
            processor(&src, &dest, info).with_context(|| {
                format!(
                    "failed to process src: {}, dest: {}",
                    src.display(),
                    dest.display()
                )
            })?;
        }
    }

    Ok(())
}

// A manifest list is a newline separated list of json manifests
// path/to/json_manifest
pub fn read_manifest_list(
    manifest: &Path,
    info: &mut ParInfo,
    processors: &Vec<FileProcessor>,
) -> anyhow::Result<()> {
    let file = File::open(manifest)
        .with_context(|| format!("Failed to open manifest file: {}", manifest.display()))?;

    let reader = BufReader::new(file);

    for line in reader.lines() {
        let line =
            line.with_context(|| format!("Failed to read manifest: {}", manifest.display()))?;
        read_json_manifest(&PathBuf::from(line), info, processors)?;
    }

    Ok(())
}

// A json manifest is a json list tuples:
// [(dest, source, origin),]
// * origin is ignored
pub fn read_json_manifest(
    manifest: &PathBuf,
    info: &mut ParInfo,
    processors: &Vec<FileProcessor>,
) -> anyhow::Result<()> {
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

        if let (Some(dest_str), Some(src_str)) = (item_arr[0].as_str(), item_arr[1].as_str()) {
            let src = PathBuf::from(src_str);
            let dest = PathBuf::from(dest_str);
            for processor in processors {
                processor(&src, &dest, info).with_context(|| {
                    format!(
                        "failed to process src: {}, dest: {}",
                        src.display(),
                        dest.display()
                    )
                })?;
            }
        }
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

pub fn process_symlink(source: &Path, dest: &Path, info: &mut ParInfo) -> anyhow::Result<()> {
    // Create a relative symlink from the input path to the output path
    let input_path = source;
    let link_path = info.output_path.join(dest);

    let parent = link_path
        .parent()
        .with_context(|| format!("Could not get path root {}", link_path.display()))?;

    let target = relpath(parent, input_path);

    fs::create_dir_all(parent)?;

    match xplat_symlink(&target, &link_path) {
        Ok(()) => Ok(()),
        Err(err) if err.kind() == io::ErrorKind::AlreadyExists => Ok(()),
        Err(err) => Err(anyhow::anyhow!(
            "Failed to create symlink {} -> {}: {}",
            link_path.display(),
            target.display(),
            err
        )),
    }
}

pub fn process_maybe_add_init(
    _source: &Path,
    dest: &Path,
    info: &mut ParInfo,
) -> anyhow::Result<()> {
    let mut path = dest;

    const SOURCE_EXTENSIONS: [&str; 5] = ["py", "so", "empty_stub", "pyd", "pyc"];
    // When adding __init__ files we ignore everything included with the bundled runtime
    if path.starts_with(PathBuf::from("runtime/lib")) {
        return Ok(());
    }

    if let Some(file_ext) = path.extension() {
        if !SOURCE_EXTENSIONS.iter().any(|&ext| ext == file_ext) {
            return Ok(());
        }
    } else {
        // For directories we recursively traverse to find any source files
        let dir_path = info.output_path.join(path);
        if dir_path.is_dir() {
            for entry in fs::read_dir(dir_path)? {
                let entry = entry?;
                let entry_path = entry.path();
                let relative_path = PathBuf::from(entry_path.strip_prefix(&info.output_path)?);
                process_maybe_add_init(_source, &relative_path, info)?;
            }
        }
        return Ok(());
    }
    if let Some(file_name) = path.file_name().and_then(|f| f.to_str()) {
        if file_name == "__init__.py" {
            if let Some(dir) = path.parent().and_then(|d| d.to_str()) {
                info.module_map.insert(dir.to_string(), false);
            }
        }
    }

    while let Some(parent) = path.parent() {
        if let Some(path_str) = parent.to_str() {
            info.module_map.entry(path_str.to_string()).or_insert(true);
        }
        path = parent;
    }
    Ok(())
}
