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
use walkdir::WalkDir;

pub struct ParInfo<'a> {
    pub output_path: PathBuf,
    module_map: HashMap<String, bool>,
    source_extensions: [&'a str; 5],
}

impl<'a> ParInfo<'a> {
    pub fn new(output_path: PathBuf) -> ParInfo<'a> {
        ParInfo {
            output_path,
            module_map: HashMap::new(),
            source_extensions: ["py", "so", "empty_stub", "pyd", "pyc"],
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

fn _process_maybe_add_init(dest: &Path, info: &mut ParInfo) -> anyhow::Result<()> {
    if let Some(file_ext) = dest.extension() {
        if !info.source_extensions.iter().any(|&ext| ext == file_ext) {
            return Ok(());
        }
    }

    let mut path = dest;

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

pub fn process_maybe_add_init(
    _source: &Path,
    dest: &Path,
    info: &mut ParInfo,
) -> anyhow::Result<()> {
    // When adding __init__ files we ignore everything included with the bundled runtime
    if dest.starts_with(PathBuf::from("runtime/lib")) {
        return Ok(());
    }

    if let Some(file_ext) = dest.extension() {
        if !info.source_extensions.iter().any(|&ext| ext == file_ext) {
            return Ok(());
        }
    } else {
        // For directories we recursively traverse to find any source files
        let dir_path = info.output_path.join(dest);
        if dir_path.is_dir() {
            for entry in WalkDir::new(dir_path) {
                let entry = entry?;
                let entry_path = entry.path();
                let relative_path = PathBuf::from(entry_path.strip_prefix(&info.output_path)?);
                _process_maybe_add_init(&relative_path, info)?;
            }
        }
        return Ok(());
    }
    _process_maybe_add_init(dest, info)
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::io::Write;
    use std::path::Path;

    use tempfile::tempdir;

    use super::*;

    #[test]
    fn test_relpath() {
        // Test case 1: Same directory
        let link = Path::new("/a/b/c");
        let target = Path::new("/a/b/c/file.txt");
        let result = relpath(link, target);
        assert_eq!(result, PathBuf::from("file.txt"));

        // Test case 2: Target in subdirectory
        let link = Path::new("/a/b/c");
        let target = Path::new("/a/b/c/d/file.txt");
        let result = relpath(link, target);
        assert_eq!(result, PathBuf::from("d/file.txt"));

        // Test case 3: Target in parent directory
        let link = Path::new("/a/b/c/d");
        let target = Path::new("/a/b/file.txt");
        let result = relpath(link, target);
        assert_eq!(result, PathBuf::from("../../file.txt"));

        // Test case 4: Target in different branch
        let link = Path::new("/a/b/c");
        let target = Path::new("/a/d/e/file.txt");
        let result = relpath(link, target);
        assert_eq!(result, PathBuf::from("../../d/e/file.txt"));

        // Test case 5: Completely different paths
        let link = Path::new("/a/b/c");
        let target = Path::new("/x/y/z/file.txt");
        let result = relpath(link, target);
        assert_eq!(result, PathBuf::from("../../../x/y/z/file.txt"));
    }

    #[test]
    fn test_write_inits() {
        let temp_dir = tempdir().unwrap();
        let output_path = temp_dir.path().join("test_output");
        std::fs::create_dir_all(output_path.join("pkg1/subpkg")).unwrap();
        std::fs::create_dir_all(output_path.join("pkg2")).unwrap();

        let mut info = ParInfo::new(output_path.clone());

        // Add some module paths
        info.module_map.insert("pkg1".to_string(), true);
        info.module_map.insert("pkg1/subpkg".to_string(), true);
        info.module_map.insert("pkg2".to_string(), false); // This one shouldn't get an __init__.py

        // Write the __init__.py files
        write_inits(&mut info).unwrap();

        // Check that __init__.py files were created in the right places
        assert!(output_path.join("pkg1").join("__init__.py").exists());
        assert!(
            output_path
                .join("pkg1")
                .join("subpkg")
                .join("__init__.py")
                .exists()
        );
        assert!(!output_path.join("pkg2").join("__init__.py").exists());

        // Check that the root doesn't have an __init__.py
        assert!(!output_path.join("__init__.py").exists());
    }

    #[test]
    fn test_process_symlink() {
        let temp_dir = tempdir().unwrap();
        let output_path = temp_dir.path().join("test_output");
        let mut info = ParInfo::new(output_path.clone());

        // Create output directory
        info.create_output_dir().unwrap();

        // Create a source file
        let source_file = temp_dir.path().join("source_file.txt");
        let mut file = fs::File::create(&source_file).unwrap();
        writeln!(file, "test content").unwrap();

        // Create a symlink to the source file
        let dest_path = PathBuf::from("dest_dir/dest_file.txt");
        process_symlink(&source_file, &dest_path, &mut info).unwrap();

        // Check that the symlink was created
        let symlink_path = output_path.join("dest_dir").join("dest_file.txt");
        assert!(symlink_path.exists());
        assert!(
            fs::symlink_metadata(&symlink_path)
                .unwrap()
                .file_type()
                .is_symlink()
        );
    }

    #[test]
    fn test_process_maybe_add_init() {
        let temp_dir = tempdir().unwrap();
        let output_path = temp_dir.path().join("test_output");
        let mut info = ParInfo::new(output_path.clone());

        // Create output directory and some nested directories
        info.create_output_dir().unwrap();
        fs::create_dir_all(output_path.join("pkg1/subpkg")).unwrap();

        // Create a Python file
        let py_file_path = output_path.join("pkg1/subpkg/module.py");
        let mut file = fs::File::create(&py_file_path).unwrap();
        writeln!(file, "# Test Python file").unwrap();

        // Process the Python file
        let source_path = PathBuf::from("dummy_source");
        let dest_path = PathBuf::from("pkg1/subpkg/module.py");
        process_maybe_add_init(&source_path, &dest_path, &mut info).unwrap();

        // Check that the module map was updated correctly
        assert!(info.module_map.contains_key("pkg1"));
        assert!(info.module_map.contains_key("pkg1/subpkg"));
        assert_eq!(info.module_map.get("pkg1").unwrap(), &true);
        assert_eq!(info.module_map.get("pkg1/subpkg").unwrap(), &true);
    }

    #[test]
    fn test_process_maybe_add_init_traverse_dir() {
        let temp_dir = tempdir().unwrap();
        let output_path = temp_dir.path().join("test_output");
        let mut info = ParInfo::new(output_path.clone());

        // Create output directory and some nested directories
        info.create_output_dir().unwrap();
        fs::create_dir_all(output_path.join("pkg1/subpkg")).unwrap();

        // Create a Python file
        let py_file_path = output_path.join("pkg1/subpkg/module.py");
        let mut file = fs::File::create(&py_file_path).unwrap();
        writeln!(file, "# Test Python file").unwrap();

        // Process the Python file
        let source_path = PathBuf::from("dummy_source");
        let dest_path = PathBuf::from("pkg1");
        process_maybe_add_init(&source_path, &dest_path, &mut info).unwrap();

        // Check that the module map was updated correctly
        assert!(info.module_map.contains_key("pkg1"));
        assert!(info.module_map.contains_key("pkg1/subpkg"));
        assert_eq!(info.module_map.get("pkg1").unwrap(), &true);
        assert_eq!(info.module_map.get("pkg1/subpkg").unwrap(), &true);
    }

    #[test]
    fn test_read_text_manifest() {
        let temp_dir = tempdir().unwrap();
        let output_path = temp_dir.path().join("test_output");
        let mut info = ParInfo::new(output_path.clone());

        // Create output directory
        info.create_output_dir().unwrap();

        // Create a source file
        let source_file = temp_dir.path().join("source_file.py");
        let mut file = fs::File::create(&source_file).unwrap();
        writeln!(file, "# Test Python file").unwrap();

        // Create a manifest file
        let manifest_path = temp_dir.path().join("manifest.txt");
        let mut manifest_file = fs::File::create(&manifest_path).unwrap();
        writeln!(manifest_file, "{}::pkg1/module.py", source_file.display()).unwrap();

        // Define a simple processor function for testing
        let processor: fn(&Path, &Path, &mut ParInfo) -> anyhow::Result<()> = |_src, dest, info| {
            // Just update the module map for testing
            if let Some(parent) = dest.parent() {
                if let Some(path_str) = parent.to_str() {
                    info.module_map.insert(path_str.to_string(), true);
                }
            }
            Ok(())
        };

        // Read the manifest
        let processors = vec![processor];
        read_text_manifest(&manifest_path, &mut info, &processors).unwrap();

        // Check that the module map was updated
        assert!(info.module_map.contains_key("pkg1"));
        assert_eq!(info.module_map.get("pkg1").unwrap(), &true);
    }

    #[test]
    fn test_read_json_manifest() {
        let temp_dir = tempdir().unwrap();
        let output_path = temp_dir.path().join("test_output");
        let mut info = ParInfo::new(output_path.clone());

        // Create output directory
        info.create_output_dir().unwrap();

        // Create a source file
        let source_file = temp_dir.path().join("source_file.py");
        let mut file = fs::File::create(&source_file).unwrap();
        writeln!(file, "# Test Python file").unwrap();

        // Create a JSON manifest file
        let manifest_path = temp_dir.path().join("manifest.json");
        let mut manifest_file = fs::File::create(&manifest_path).unwrap();
        writeln!(
            manifest_file,
            r#"[["pkg1/module.py", "{}", "origin"]]"#,
            source_file.display().to_string().replace("\\", "\\\\")
        )
        .unwrap();

        // Define a simple processor function for testing
        let processor: fn(&Path, &Path, &mut ParInfo) -> anyhow::Result<()> = |_src, dest, info| {
            // Just update the module map for testing
            if let Some(parent) = dest.parent() {
                if let Some(path_str) = parent.to_str() {
                    info.module_map.insert(path_str.to_string(), true);
                }
            }
            Ok(())
        };

        // Read the manifest
        let processors = vec![processor];
        read_json_manifest(&manifest_path, &mut info, &processors).unwrap();

        // Check that the module map was updated
        assert!(info.module_map.contains_key("pkg1"));
        assert_eq!(info.module_map.get("pkg1").unwrap(), &true);
    }

    #[test]
    fn test_read_manifest_list() {
        let temp_dir = tempdir().unwrap();
        let output_path = temp_dir.path().join("test_output");
        let mut info = ParInfo::new(output_path.clone());

        // Create output directory
        info.create_output_dir().unwrap();

        // Create a source file
        let source_file = temp_dir.path().join("source_file.py");
        let mut file = fs::File::create(&source_file).unwrap();
        writeln!(file, "# Test Python file").unwrap();

        // Create a JSON manifest file
        let json_manifest_path = temp_dir.path().join("manifest.json");
        let mut json_manifest_file = fs::File::create(&json_manifest_path).unwrap();
        writeln!(
            json_manifest_file,
            r#"[["pkg1/module.py", "{}", "origin"]]"#,
            source_file.display().to_string().replace("\\", "\\\\")
        )
        .unwrap();

        // Create a manifest list file
        let manifest_list_path = temp_dir.path().join("manifest_list.txt");
        let mut manifest_list_file = fs::File::create(&manifest_list_path).unwrap();
        writeln!(manifest_list_file, "{}", json_manifest_path.display()).unwrap();

        // Define a simple processor function for testing
        let processor: fn(&Path, &Path, &mut ParInfo) -> anyhow::Result<()> = |_src, dest, info| {
            // Just update the module map for testing
            if let Some(parent) = dest.parent() {
                if let Some(path_str) = parent.to_str() {
                    info.module_map.insert(path_str.to_string(), true);
                }
            }
            Ok(())
        };

        // Read the manifest list
        let processors = vec![processor];
        read_manifest_list(&manifest_list_path, &mut info, &processors).unwrap();

        // Check that the module map was updated
        assert!(info.module_map.contains_key("pkg1"));
        assert_eq!(info.module_map.get("pkg1").unwrap(), &true);
    }
}
