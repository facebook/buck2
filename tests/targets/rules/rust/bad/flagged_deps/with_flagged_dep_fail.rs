// @nolint - expect-fail test

// This should fail if `lib` is `noprelude`. We need an explicit `extern crate`.
use lib::do_something;

fn main() {
    do_something();
}
