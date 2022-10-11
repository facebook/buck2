
use {
    crate::*,
    crossterm::{
        QueueableCommand,
        style::Print,
    },
    std::io::Write,
};

const FILLING_STRING_CHAR_LEN: usize = 1000;

/// something to fill with
pub struct Filling {
    filling_string: String,
    char_size: usize,
}

impl Filling {
    pub fn from_char(filling_char: char) -> Self {
        let char_size = String::from(filling_char).len();
        let mut filling_string = String::with_capacity(char_size * FILLING_STRING_CHAR_LEN);
        for _ in 0..FILLING_STRING_CHAR_LEN {
            filling_string.push(filling_char);
        }
        Self {
            filling_string,
            char_size,
        }
    }
    pub fn queue_unstyled<W: Write>(
        &self,
        w: &mut W,
        mut len: usize,
    ) -> Result<(), Error> {
        while len > 0 {
            let sl = len.min(FILLING_STRING_CHAR_LEN);
            w.queue(Print(&self.filling_string[0..sl * self.char_size]))?;
            len -= sl;
        }
        Ok(())
    }
    pub fn queue_styled<W: Write>(
        &self,
        w: &mut W,
        cs: &CompoundStyle,
        mut len: usize,
    ) -> Result<(), Error> {
        while len > 0 {
            let sl = len.min(FILLING_STRING_CHAR_LEN);
            cs.queue_str(w, &self.filling_string[0..sl * self.char_size])?;
            len -= sl;
        }
        Ok(())
    }
}
