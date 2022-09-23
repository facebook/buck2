extern crate fib_ffi;

use std::env::args;
use std::ffi::CString;
use std::os::raw::c_char;

fn main() {
    init_caml();
    println!("fib(10) = {}", format_result(fib(10)))
}

extern "C" {
    pub fn caml_startup(argv: *const *const c_char);
    pub fn caml_named_value(s: *const c_char) -> *const usize;
    pub fn caml_callback(closure: usize, arg: usize) -> usize;
}

fn fib(n: isize) -> isize {
    let fib = CString::new("fib").unwrap();
    let result = unsafe {
        let fib_closure = caml_named_value(fib.as_ptr());
        caml_callback(*fib_closure, isize_to_ocaml(n))
    };
    isize_from_ocaml(result)
}

fn format_result(n: isize) -> String {
    let format_result = CString::new("format_result").unwrap();
    unsafe {
        let format_result_closure = caml_named_value(format_result.as_ptr());
        let result = caml_callback(*format_result_closure, isize_to_ocaml(n)) as *const usize;
        (std::str::from_utf8(bytes_from_ocaml(&result)).unwrap()).to_owned()
    }
}

fn init_caml() {
    let argv: Vec<CString> = (args().into_iter())
        .map(|arg| CString::new(arg.as_str()).unwrap())
        .collect();
    let mut p_argv: Vec<*const c_char> = argv.iter().map(|arg| arg.as_ptr()).collect();
    p_argv.push(std::ptr::null());
    unsafe {
        caml_startup(p_argv.as_ptr());
    }
}

pub fn isize_from_ocaml(n: usize) -> isize {
    (n as isize) >> 1
}

pub fn isize_to_ocaml(n: isize) -> usize {
    (n as usize) << 1 | 1
}

pub fn bytes_from_ocaml<'a>(value: &'a *const usize) -> &'a [u8] {
    unsafe {
        let ptr: *const usize = *value;
        let header = ptr.offset(-1);

        let header_bits = *header;
        let block_size = header_bits >> 10;
        let block_size_in_bytes = block_size * std::mem::size_of::<usize>();

        let ptr = ptr as *const u8;
        let padding = *ptr.add(block_size_in_bytes - 1);
        let len = block_size_in_bytes - padding as usize - 1;

        std::slice::from_raw_parts(ptr, len)
    }
}
