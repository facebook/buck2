use {
    crossterm::{
        self,
        event::{
            Event,
            KeyCode, KeyEvent, KeyModifiers,
            MouseButton, MouseEvent, MouseEventKind,
        },
    },
    std::{
        time::Instant,
    }
};

/// a user event with happening time
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TimedEvent {

    pub time: Instant,

    pub event: crossterm::event::Event,

    /// false unless you set it yourself using the time
    /// or you get the timed event with an EventSource
    /// which computes it. Can be true only for left mouse
    /// down and left mouse up (both down and up of the second
    /// click have it true)
    pub double_click: bool,

}

impl TimedEvent {

    /// Wrap a crossterm event into a timed one, with time.
    ///
    /// Normalize \r and \n into Enter (useful for key combinations)
    ///
    /// To get a double-click you'll either need to use a termimad event-source
    /// or to do the computation yourself.
    pub fn new(mut event: Event) -> Self {
        if let Event::Key(mut key) = &mut event {
            if key.code==KeyCode::Char('\r') || key.code==KeyCode::Char('\n') {
                key.code = KeyCode::Enter;
            }
        }
        Self {
            time: Instant::now(),
            event,
            double_click: false,
        }
    }

    /// If it's a simple mouse up and not determined to be the second click of
    /// a double click, return the coordinates
    pub const fn as_click(self) -> Option<(u16, u16)> {
        if self.double_click {
            return None;
        }
        match self.event {
            Event::Mouse(MouseEvent {
                kind: MouseEventKind::Up(MouseButton::Left), column, row, modifiers: KeyModifiers::NONE,
            }) => Some((column, row)),
            _ => None,
        }
    }

    pub fn is_key(self, key: KeyEvent) -> bool {
        match self.event {
            Event::Key(k) if k == key => true,
            _ => false,
        }
    }
}


