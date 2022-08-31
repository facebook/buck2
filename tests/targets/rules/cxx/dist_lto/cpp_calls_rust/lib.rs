// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

use cxx::CxxString;

#[cxx::bridge]
mod ffi {
    extern "Rust" {
        fn same_length(a: &str, b: &CxxString) -> bool;
    }
}

pub fn same_length(a: &str, b: &CxxString) -> bool {
    a.len() == b.len()
}
