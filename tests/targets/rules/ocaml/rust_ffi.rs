use rust_ffi_dep::get_one;

fn isize_to_ocaml_int(value: isize) -> usize {
    ((value as usize) << get_one()) | get_one()
}

fn ocaml_int_to_isize(value: usize) -> isize {
    (value as isize) >> get_one()
}

// usize is a standin for the `value` typedef from caml/mlvalues.h:
// https://github.com/ocaml/ocaml/blob/1dc70d32da055e803a6bc801a3e07efbaab2a9a8/runtime/caml/mlvalues.h#L60
// It's not a very safe standin, since `value` is not always
// represented with a type that's equivalent to `usize`:
// https://github.com/ocaml/ocaml/blob/3ecc6cda2089492c0166d1e811b60dbc9baa818c/runtime/caml/config.h#L139
// But it works in the environments where we build.
#[no_mangle]
extern "C" fn add(x: usize, y: usize) -> usize {
    isize_to_ocaml_int(ocaml_int_to_isize(x) + ocaml_int_to_isize(y))
}
