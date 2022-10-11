extern crate output_vt100;

fn main() {
    output_vt100::init();
    println!("\x1b[31mThis text is red!\x1b[0m");
}
