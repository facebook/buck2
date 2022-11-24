/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::env;
use std::ffi::OsString;
use std::fs;
use std::io;
use std::path::Path;
use std::path::PathBuf;

fn get_env(key: &str) -> Option<OsString> {
    println!("cargo:rerun-if-env-changed={}", key);
    env::var_os(key)
}

/// Set up $PROTOC to point to the in repo binary if available.
///
/// Note: repo root is expected to be a relative or absolute path to the root of the repository.
fn maybe_set_protoc(repo_root: &str) {
    // Bail if the env var is explicitly set
    if get_env("PROTOC").is_some() {
        return;
    }

    let mut protoc = PathBuf::from(repo_root);
    protoc.push("third-party/protobuf/dotslash/protoc");
    if cfg!(windows) {
        protoc.push(".exe");
    }

    // Bail if we can't find the path to the in repo protoc, e.g. for OSS builds.
    if !protoc.exists() {
        return;
    }

    let protoc = fs::canonicalize(protoc).expect("Failed to canonicalize protoc path");
    env::set_var("PROTOC", protoc);
}

/// Set $PROTOC_INCLUDE.
fn maybe_set_protoc_include(repo_root: &str) {
    // Bail if the env var is explicitly set
    if get_env("PROTOC_INCLUDE").is_some() {
        return;
    }

    let mut protoc_include = PathBuf::from(repo_root);
    protoc_include.push("third-party/protobuf/protobuf/src");

    // Bail if we can't find the path to the in repo protoc, e.g. for OSS builds.
    if !protoc_include.exists() {
        return;
    }

    let protoc_include =
        fs::canonicalize(protoc_include).expect("Failed to canonicalize protoc include path");
    env::set_var("PROTOC_INCLUDE", protoc_include);
}

pub struct Builder {
    tonic: tonic_build::Builder,
}

pub fn configure() -> Builder {
    let tonic = tonic_build::configure();
    // We want to use optional everywhere
    let tonic = tonic.protoc_arg("--experimental_allow_proto3_optional");

    Builder { tonic }
}

impl Builder {
    pub fn type_attribute<P: AsRef<str>, A: AsRef<str>>(self, path: P, attribute: A) -> Self {
        Self {
            tonic: self.tonic.type_attribute(path, attribute),
        }
    }

    pub fn field_attribute<P: AsRef<str>, A: AsRef<str>>(self, path: P, attribute: A) -> Self {
        Self {
            tonic: self.tonic.field_attribute(path, attribute),
        }
    }

    pub fn extern_path(self, proto_path: impl AsRef<str>, rust_path: impl AsRef<str>) -> Self {
        Self {
            tonic: self.tonic.extern_path(proto_path, rust_path),
        }
    }

    pub fn setup_protoc(self, repo_root: &str) -> Self {
        // It would be great if there were on the config rather than an env variables...
        maybe_set_protoc(repo_root);
        maybe_set_protoc_include(repo_root);
        self
    }

    pub fn compile(
        self,
        protos: &[impl AsRef<Path>],
        includes: &[impl AsRef<Path>],
    ) -> io::Result<()> {
        let Self { mut tonic } = self;

        // Buck likes to set $OUT in a genrule, while Cargo likes to set $OUT_DIR.
        // If we have $OUT set only, move it into the config
        if get_env("OUT_DIR").is_none() {
            if let Some(out) = get_env("OUT") {
                tonic = tonic.out_dir(out);
            }
        }

        // Tell Cargo that if the given file changes, to rerun this build script.
        for proto_file in protos {
            println!("cargo:rerun-if-changed={}", proto_file.as_ref().display());
        }

        tonic.compile(protos, includes)
    }
}
