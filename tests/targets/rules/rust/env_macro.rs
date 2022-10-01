fn main() {
    // fails compilation if not defined
    println!("var={}", env!("COOL_VARIABLE"));
}
