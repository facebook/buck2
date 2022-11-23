/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(clippy::derive_partial_eq_without_eq)] // FIXME?

use prost::Message;
use prost_types::Duration;

#[derive(Clone, PartialEq, Message)]
pub struct Action {
    #[prost(message, optional, tag = "1")]
    pub command_digest: Option<Digest>,
    #[prost(message, optional, tag = "2")]
    pub input_root_digest: Option<Digest>,
    #[prost(message, optional, tag = "6")]
    pub timeout: Option<Duration>,
    #[prost(bool, tag = "7")]
    pub do_not_cache: bool,
}

#[derive(Clone, PartialEq, Message)]
pub struct Command {
    #[prost(string, repeated, tag = "1")]
    pub arguments: Vec<String>,
    #[prost(message, repeated, tag = "2")]
    pub environment_variables: Vec<EnvironmentVariable>,
    #[prost(string, repeated, tag = "3")]
    pub output_files: Vec<String>,
    #[prost(string, repeated, tag = "4")]
    pub output_directories: Vec<String>,
    #[prost(message, optional, tag = "5")]
    pub platform: Option<Platform>,
    #[prost(string, tag = "6")]
    pub working_directory: String,
}

#[derive(Clone, PartialEq, Message)]
pub struct EnvironmentVariable {
    #[prost(string, tag = "1")]
    pub name: String,
    #[prost(string, tag = "2")]
    pub value: String,
}

#[derive(Clone, PartialEq, Message)]
pub struct Platform {
    #[prost(message, repeated, tag = "1")]
    pub properties: Vec<Property>,
}

#[derive(Clone, PartialEq, Message)]
pub struct Property {
    #[prost(string, tag = "1")]
    pub name: String,
    #[prost(string, tag = "2")]
    pub value: String,
}

#[derive(Clone, PartialEq, Message)]
pub struct Directory {
    #[prost(message, repeated, tag = "1")]
    pub files: Vec<FileNode>,
    #[prost(message, repeated, tag = "2")]
    pub directories: Vec<DirectoryNode>,
    #[prost(message, repeated, tag = "3")]
    pub symlinks: Vec<SymlinkNode>,
}

#[derive(Clone, PartialEq, Message)]
pub struct FileNode {
    #[prost(string, tag = "1")]
    pub name: String,
    #[prost(message, optional, tag = "2")]
    pub digest: Option<Digest>,
    #[prost(bool, tag = "4")]
    pub is_executable: bool,
}

#[derive(Clone, PartialEq, Message)]
pub struct DirectoryNode {
    #[prost(string, tag = "1")]
    pub name: String,
    #[prost(message, optional, tag = "2")]
    pub digest: Option<Digest>,
}

#[derive(Clone, PartialEq, Message)]
pub struct SymlinkNode {
    #[prost(string, tag = "1")]
    pub name: String,

    #[prost(string, tag = "2")]
    pub target: String,
}

#[derive(Clone, PartialEq, Message)]
pub struct Digest {
    #[prost(string, tag = "1")]
    pub hash: String,
    #[prost(int64, tag = "2")]
    pub size_bytes: i64,
}

#[derive(Clone, PartialEq, Message)]
pub struct Tree {
    #[prost(message, optional, tag = "1")]
    pub root: Option<Directory>,
    #[prost(message, repeated, tag = "2")]
    pub children: Vec<Directory>,
}
