use {
    crossterm::{
        event::{
            KeyCode,
            KeyEvent,
            KeyModifiers,
        },
    },
    std::fmt,
};

/// An escape sequence made of key events
///
/// Termimad's event source currently offers no
/// guarantee *all* escape sequences are really
/// catched. If you find a problem please contact
/// me.
/// Note also that some escape sequence never
/// come this high, being catched by crossterm
/// before.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct EscapeSequence {
    pub keys: Vec<KeyEvent>,
}

impl fmt::Display for EscapeSequence {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for key in &self.keys {
            if let KeyEvent { code: KeyCode::Char(c), modifiers: KeyModifiers::NONE } = key {
                write!(f, "{}", c)?;
            }
        }
        Ok(())
    }
}
