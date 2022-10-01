#[cfg(feature = "buck")]
fn g() {}

#[cfg(not(feature = "buck"))]
fn g() {
    compile_error!("buck feature should be set")
}

fn f() {
    let _ = env!("BUCK");
    g();
}
