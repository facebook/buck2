use crossterm::style::Color::*;
use termimad::*;

static MD_TABLE: &str = r#"
|:-:|:-:|-
|**feature**|**supported**|**details**|
|-:|:-:|-
| tables | yes | pipe based, with or without alignments
| italic, bold | yes | star based |
| inline code | yes | `with backquotes` (it works in tables too)
| code bloc | yes |with tabs or code fences
| syntax coloring | no |
| crossed text |  ~~not yet~~ | wait... now it works `~~like this~~`
| horizontal rule | yes | Use 3 or more dashes (`---`)
| lists | yes|* unordered lists supported
|  | |* ordered lists *not* supported
| quotes |  yes |> What a wonderful time to be alive!
| links | no | (but your terminal already handles raw URLs)
|-
"#;

fn main() {
    println!("\n");
    let mut skin = MadSkin::default();
    skin.set_headers_fg(rgb(255, 187, 0));
    skin.bold.set_fg(Yellow);
    skin.italic.set_fgbg(Magenta, rgb(30, 30, 40));
    skin.paragraph.align = Alignment::Center;
    skin.table.align = Alignment::Center;
    let (width, _) = terminal_size();
    let mut markdown = format!("Available width: *{}*", width);
    markdown.push_str(MD_TABLE);
    println!("{}", skin.term_text(&markdown));
    println!("\n");
}
