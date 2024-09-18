/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg_attr(set_nostd, no_std)]

#[cfg(any(rust_linkable_symbol_content_bytes, rust_linkable_symbol_getter_bytes))]
#[repr(C)]
#[cfg_attr(rust_linkable_symbol_align_bytes = "2", repr(align(2)))]
#[cfg_attr(rust_linkable_symbol_align_bytes = "4", repr(align(4)))]
#[cfg_attr(rust_linkable_symbol_align_bytes = "8", repr(align(8)))]
struct Aligned<Bytes: ?Sized> {
    bytes: Bytes,
}

#[cfg(rust_linkable_symbol_content_str)]
#[used]
#[export_name = env!("LINKABLE_SYMBOL")]
pub static LINKABLE_SYMBOL: &str = include_str!("content");

#[cfg(rust_linkable_symbol_content_bytes)]
#[used]
#[export_name = env!("LINKABLE_SYMBOL")]
pub static LINKABLE_SYMBOL: &Aligned<[u8]> = &Aligned {
    bytes: *include_bytes!("content"),
};

#[cfg(rust_linkable_symbol_getter_str)]
pub fn get() -> &'static str {
    extern "Rust" {
        #[link_name = env!("LINKABLE_SYMBOL")]
        static LINKABLE_SYMBOL: &'static str;
    }
    unsafe { LINKABLE_SYMBOL }
}

#[cfg(rust_linkable_symbol_getter_bytes)]
pub fn get() -> &'static [u8] {
    extern "Rust" {
        #[link_name = env!("LINKABLE_SYMBOL")]
        static LINKABLE_SYMBOL: &'static Aligned<[u8]>;
    }
    unsafe { &LINKABLE_SYMBOL.bytes }
}
