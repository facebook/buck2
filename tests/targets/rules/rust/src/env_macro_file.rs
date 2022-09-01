static FILE: &str = include_str!(env!("COOL_FILE"));

fn main() {
    // fails compilation if not defined
    println!("file: {}", FILE);
}
