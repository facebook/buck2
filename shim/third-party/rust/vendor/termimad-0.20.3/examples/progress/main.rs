/*!
Run with

    cargo run --example progress

Display the different steps of a pixel precise progress bar.

Note: This example is just a draft that I'll complete later with animations, colors, etc.
*/
use termimad::*;

fn main() {
    let n = 40;
    let mut markdown = String::new();
    markdown.push_str("|-:|:-:|:-:|:-:|\n");
    markdown.push_str("|iter|part|chars|bar|\n");
    markdown.push_str("|-:|-|:-:|:-|\n");
    for i in 0..=n {
        let part = (i as f32) / (n as f32);
        let pb = ProgressBar::new(part, 5);
        let char_count = pb.to_string().chars().count();
        markdown.push_str(&format!("|{}|{:1.4}|{}|{}\n", i, part, char_count, pb));
    }
    markdown.push_str("|-\n");
    print_text(&markdown);
}
