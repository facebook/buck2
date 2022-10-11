use {
    crate::clipboard,
    anyhow::{self},
    crokey::key,
    crossterm::{
        event::{Event, KeyEvent, MouseEvent},
        queue,
        terminal::{
            Clear,
            ClearType,
        },
    },
    std::io::Write,
    termimad::*,
};

/// The view covering the whole terminal, with its widgets and current state
pub struct View {
    area: Area,
    drawable: bool, // is the area big enough
    input: InputField,
    render_skin: MadSkin,
    render_area: Area, // where the inputed markdown will be rendered
}

impl Default for View {
    /// Create the view with all its widgets
    fn default() -> Self {
        let mut render_skin = MadSkin::default();
        render_skin.set_bg(gray(2));
        render_skin.set_fg(ansi(230));
        render_skin.set_headers_fg(ansi(222));
        render_skin.bold = CompoundStyle::with_fg(ansi(194));
        let mut input = InputField::default();
        input.new_line_on(key!(enter));
        let mut view = Self {
            area: Area::uninitialized(),
            drawable: false,
            input,
            render_skin,
            render_area: Area::uninitialized(),
        };
        view.input.set_str(MD);
        view
    }
}

impl View {
    pub fn new(area: Area) -> Self {
        let mut view = Self::default();
        view.resize(area);
        view
    }
    pub fn resize(&mut self, area: Area) -> bool {
        if self.area == area {
            return false;
        }
        self.drawable = area.width >= 20 && area.height >= 15;
        if self.drawable {
            let h = (area.height - 2) / 2;
            let input_area = Area::new(1, 1, area.width - 2, h);
            self.render_area = Area::new(1, h + 2, area.width -2, area.height - h - 2);
            self.input.set_area(input_area);
        }
        self.area = area;
        true
    }
    pub fn apply_key_event(&mut self, key: KeyEvent) -> bool {
        let input = &mut self.input;
        match key {
            key!(ctrl-c) => clipboard::copy_from_input(input),
            key!(ctrl-x) => clipboard::cut_from_input(input),
            key!(ctrl-v) => clipboard::paste_into_input(input),
            _ => input.apply_key_event(key),
        }
    }
    pub fn apply_mouse_event(&mut self, mouse_event: MouseEvent, double_click: bool) -> bool {
        // To keep this example simple, there's no managment of focus, scrolling, etc.
        // See the "inputs" example for how to deal with those.
        // The line below allows mouse selection and wheel scrolling.
        self.input.apply_mouse_event(mouse_event, double_click)
    }
    pub fn apply_timed_event(&mut self, timed_event: TimedEvent) -> bool {
        match timed_event.event {
            Event::Key(key) => self.apply_key_event(key),
            Event::Mouse(me) => self.apply_mouse_event(me, timed_event.double_click),
            Event::Resize(w, h) => self.resize(Area::new(0, 0, w, h)),
        }
    }
    /// Draw the view (not flushing)
    pub fn queue_on<W: Write>(&mut self, w: &mut W) -> anyhow::Result<()> {
        queue!(w, Clear(ClearType::All))?;
        if self.drawable {
            self.input.display_on(w)?;
            let md = self.input.get_content();
            let text = FmtText::from(&self.render_skin, &md, Some(self.render_area.width as usize - 1));
            debug!("text: {:#?}", &text); // look at termimad.log if you want to check the parsed md
            let text_view = TextView::from(&self.render_area, &text);
            text_view.write_on(w)?;
        }
        Ok(())
    }
}

static MD: &str = r#"# Dynamic Markdown

Edit the markdown in this input and see it rendered below.
You can try `code`, *italic*, **bold**, bulleted lists, tables, etc.

|:-:|:-:|
|shortcut|effect|
|:-:|-|
|**ctrl-q**| quit the example application |
|**ctrl-c**| copy |
|**ctrl-x**| cut |
|**ctrl-v**| paste |
|-|-|
"#;

