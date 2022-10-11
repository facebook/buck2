//! This module contains various utilities related to
//! writing in areas of limited sizes

mod composite_fit;
mod crop_writer;
mod filling;
mod fit_error;
mod str_fit;
mod tbl_fit;
pub mod wrap;

pub use {
    crate::Error,
    composite_fit::*,
    crop_writer::*,
    filling::*,
    fit_error::*,
    str_fit::*,
    tbl_fit::*,
};
use {
    crossterm::{
        style::{Color, SetBackgroundColor},
        QueueableCommand,
    },
    minimad::once_cell::sync::Lazy,
};

pub static DEFAULT_TAB_REPLACEMENT: &str = "  ";

pub static SPACE_FILLING: Lazy<Filling> = Lazy::new(|| { Filling::from_char(' ') });

pub fn fill_bg<W>(
    w: &mut W,
    len: usize,
    bg: Color,
) -> Result<(), Error>
where
    W: std::io::Write,
{
    w.queue(SetBackgroundColor(bg))?;
    SPACE_FILLING.queue_unstyled(w, len)?;
    Ok(())
}
