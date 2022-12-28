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
use std::io;
use std::path::Path;

fn get_env(key: &str) -> Option<OsString> {
    println!("cargo:rerun-if-env-changed={}", key);
    env::var_os(key)
}

#[cfg(not(buck2_build))]
fn set_var(var: &str, path: &Path) {
    assert!(path.exists(), "Path does not exist: `{}`", path.display());

    let path = dunce::canonicalize(path).expect("Failed to canonicalize path");
    eprintln!("INFO: Variable ${} set to {:?}", var, path);
    env::set_var(var, path);
}

/// Set up $PROTOC to point to the in repo binary if available.
///
/// Note: repo root is expected to be a relative or absolute path to the root of the repository.
fn maybe_set_protoc() {
    #[cfg(not(buck2_build))]
    {
        set_var("PROTOC", &protoc_bin_vendored::protoc_bin_path().unwrap());
    }
}

/// Set $PROTOC_INCLUDE.
fn maybe_set_protoc_include() {
    #[cfg(not(buck2_build))]
    {
        set_var(
            "PROTOC_INCLUDE",
            &protoc_bin_vendored::include_path().unwrap(),
        );
    }
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

    pub fn setup_protoc(self) -> Self {
        // It would be great if there were on the config rather than an env variables...
        maybe_set_protoc();
        maybe_set_protoc_include();
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
