//! run this example with
//!   cargo run --example ask
//!
use {
    crossterm::{
        style::Color::*,
    },
    termimad::*,
};

fn run_app(skin: &MadSkin) -> Result<(), Error> {
    // using the struct api
    let mut q = Question::new("Do you want some beer ?");
    q.add_answer('y', "**Y**es, I *do* want some");
    q.add_answer('n', "**N**o, I drink only Wasser");
    q.set_default('y');
    let a = q.ask(skin)?;
    println!("The answer was {:?}", a);

    // using the macro
    ask!(skin, "Is everything **OK**?", {
        ('g', "Everything is **g**ood, thanks") => {
            println!("Cool!");
        }
        ('b', "Nooo... I'm **b**ad...") => {
            println!("Oh I'm so sorry...");
        }
    });

    // returning a value, using a default answer, imbricated macro calls,
    // and not just chars as keys
    let beverage = ask!(skin, "What do I serve you ?", {
        ('b', "**B**eer") => {
            ask!(skin, "Really ? We have wine and orange juice too", (2) {
                ("oj", "**o**range **j**uice") => { "orange juice" }
                ('w' , "ok for some wine") => { "wine" }
                ('b' , "I said **beer**") => { "beer" }
                ( 2  , "Make it **2** beer glasses!") => { "beer x 2" }
            })
        }
        ('w', "**W**ine") => {
            println!("An excellent choice!");
            "wine"
        }
    });
    dbg!(beverage);

    Ok(())
}

fn make_skin() -> MadSkin {
    let mut skin = MadSkin::default();
    skin.table.align = Alignment::Center;
    skin.set_headers_fg(AnsiValue(178));
    skin.bold.set_fg(Yellow);
    skin.italic.set_fg(Magenta);
    skin.scrollbar.thumb.set_fg(AnsiValue(178));
    skin.code_block.align = Alignment::Center;
    skin
}

fn main() -> Result<(), Error> {
    let skin = make_skin();
    run_app(&skin)
}

