use proc_macro_lib::AnswerFn;

#[derive(AnswerFn)]
struct Struct;

fn main() {
    let answer = answer();
    assert_eq!(42, answer);
    println!("{}", answer);
}
