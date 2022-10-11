//! # Cursor
//!
//! The `cursor` module provides functionality to work with the terminal cursor.
//!
//! This documentation does not contain a lot of examples. The reason is that it's fairly
//! obvious how to use this crate. Although, we do provide
//! [examples](https://github.com/crossterm-rs/crossterm/tree/master/examples) repository
//! to demonstrate the capabilities.
//!
//! ## Examples
//!
//! Cursor actions can be performed with commands.
//! Please have a look at [command documentation](../index.html#command-api) for a more detailed documentation.
//!
//! ```no_run
//! use std::io::{stdout, Write};
//!
//! use crossterm::{
//!     ExecutableCommand, execute, Result,
//!     cursor::{DisableBlinking, EnableBlinking, MoveTo, RestorePosition, SavePosition}
//! };
//!
//! fn main() -> Result<()> {
//!     // with macro
//!     execute!(
//!         stdout(),
//!         SavePosition,
//!         MoveTo(10, 10),
//!         EnableBlinking,
//!         DisableBlinking,
//!         RestorePosition
//!     );
//!
//!   // with function
//!   stdout()
//!     .execute(MoveTo(11,11))?
//!     .execute(RestorePosition);
//!
//!  Ok(())
//! }
//! ```
//!
//! For manual execution control check out [crossterm::queue](../macro.queue.html).

use std::fmt;

#[cfg(windows)]
use crate::Result;
use crate::{csi, impl_display, Command};

pub use sys::position;

pub(crate) mod sys;

/// A command that moves the terminal cursor to the given position (column, row).
///
/// # Notes
///
/// * Top left cell is represented as `0,0`.
/// * Commands must be executed/queued for execution otherwise they do nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MoveTo(pub u16, pub u16);

impl Command for MoveTo {
    fn write_ansi(&self, f: &mut impl fmt::Write) -> fmt::Result {
        write!(f, csi!("{};{}H"), self.1 + 1, self.0 + 1)
    }

    #[cfg(windows)]
    fn execute_winapi(&self) -> Result<()> {
        sys::move_to(self.0, self.1)
    }
}

/// A command that moves the terminal cursor down the given number of lines,
/// and moves it to the first column.
///
/// # Notes
///
/// Commands must be executed/queued for execution otherwise they do nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MoveToNextLine(pub u16);

impl Command for MoveToNextLine {
    fn write_ansi(&self, f: &mut impl fmt::Write) -> fmt::Result {
        if self.0 != 0 {
            write!(f, csi!("{}E"), self.0)?;
        }
        Ok(())
    }

    #[cfg(windows)]
    fn execute_winapi(&self) -> Result<()> {
        if self.0 != 0 {
            sys::move_to_next_line(self.0)?;
        }
        Ok(())
    }
}

/// A command that moves the terminal cursor up the given number of lines,
/// and moves it to the first column.
///
/// # Notes
///
/// Commands must be executed/queued for execution otherwise they do nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MoveToPreviousLine(pub u16);

impl Command for MoveToPreviousLine {
    fn write_ansi(&self, f: &mut impl fmt::Write) -> fmt::Result {
        if self.0 != 0 {
            write!(f, csi!("{}F"), self.0)?;
        }
        Ok(())
    }

    #[cfg(windows)]
    fn execute_winapi(&self) -> Result<()> {
        if self.0 != 0 {
            sys::move_to_previous_line(self.0)?;
        }
        Ok(())
    }
}

/// A command that moves the terminal cursor to the given column on the current row.
///
/// # Notes
///
/// Commands must be executed/queued for execution otherwise they do nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MoveToColumn(pub u16);

impl Command for MoveToColumn {
    fn write_ansi(&self, f: &mut impl fmt::Write) -> fmt::Result {
        if self.0 != 0 {
            write!(f, csi!("{}G"), self.0)?;
        }
        Ok(())
    }

    #[cfg(windows)]
    fn execute_winapi(&self) -> Result<()> {
        sys::move_to_column(self.0)
    }
}

/// A command that moves the terminal cursor to the given row on the current column.
///
/// # Notes
///
/// Commands must be executed/queued for execution otherwise they do nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MoveToRow(pub u16);

impl Command for MoveToRow {
    fn write_ansi(&self, f: &mut impl fmt::Write) -> fmt::Result {
        if self.0 != 0 {
            write!(f, csi!("{}d"), self.0)?
        }
        Ok(())
    }

    #[cfg(windows)]
    fn execute_winapi(&self) -> Result<()> {
        sys::move_to_row(self.0)
    }
}

/// A command that moves the terminal cursor a given number of rows up.
///
/// # Notes
///
/// Commands must be executed/queued for execution otherwise they do nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MoveUp(pub u16);

impl Command for MoveUp {
    fn write_ansi(&self, f: &mut impl fmt::Write) -> fmt::Result {
        if self.0 != 0 {
            write!(f, csi!("{}A"), self.0)?;
        }
        Ok(())
    }

    #[cfg(windows)]
    fn execute_winapi(&self) -> Result<()> {
        sys::move_up(self.0)
    }
}

/// A command that moves the terminal cursor a given number of columns to the right.
///
/// # Notes
///
/// Commands must be executed/queued for execution otherwise they do nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MoveRight(pub u16);

impl Command for MoveRight {
    fn write_ansi(&self, f: &mut impl fmt::Write) -> fmt::Result {
        if self.0 != 0 {
            write!(f, csi!("{}C"), self.0)?;
        }
        Ok(())
    }

    #[cfg(windows)]
    fn execute_winapi(&self) -> Result<()> {
        sys::move_right(self.0)
    }
}

/// A command that moves the terminal cursor a given number of rows down.
///
/// # Notes
///
/// Commands must be executed/queued for execution otherwise they do nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MoveDown(pub u16);

impl Command for MoveDown {
    fn write_ansi(&self, f: &mut impl fmt::Write) -> fmt::Result {
        if self.0 != 0 {
            write!(f, csi!("{}B"), self.0)?;
        }
        Ok(())
    }

    #[cfg(windows)]
    fn execute_winapi(&self) -> Result<()> {
        sys::move_down(self.0)
    }
}

/// A command that moves the terminal cursor a given number of columns to the left.
///
/// # Notes
///
/// Commands must be executed/queued for execution otherwise they do nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MoveLeft(pub u16);

impl Command for MoveLeft {
    fn write_ansi(&self, f: &mut impl fmt::Write) -> fmt::Result {
        if self.0 != 0 {
            write!(f, csi!("{}D"), self.0)?;
        }
        Ok(())
    }

    #[cfg(windows)]
    fn execute_winapi(&self) -> Result<()> {
        sys::move_left(self.0)
    }
}

/// A command that saves the current terminal cursor position.
///
/// See the [RestorePosition](./struct.RestorePosition.html) command.
///
/// # Notes
///
/// - The cursor position is stored globally.
/// - Commands must be executed/queued for execution otherwise they do nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SavePosition;

impl Command for SavePosition {
    fn write_ansi(&self, f: &mut impl fmt::Write) -> fmt::Result {
        f.write_str("\x1B7")
    }

    #[cfg(windows)]
    fn execute_winapi(&self) -> Result<()> {
        sys::save_position()
    }
}

/// A command that restores the saved terminal cursor position.
///
/// See the [SavePosition](./struct.SavePosition.html) command.
///
/// # Notes
///
/// - The cursor position is stored globally.
/// - Commands must be executed/queued for execution otherwise they do nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RestorePosition;

impl Command for RestorePosition {
    fn write_ansi(&self, f: &mut impl fmt::Write) -> fmt::Result {
        f.write_str("\x1B8")
    }

    #[cfg(windows)]
    fn execute_winapi(&self) -> Result<()> {
        sys::restore_position()
    }
}

/// A command that hides the terminal cursor.
///
/// # Notes
///
/// - Commands must be executed/queued for execution otherwise they do nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Hide;

impl Command for Hide {
    fn write_ansi(&self, f: &mut impl fmt::Write) -> fmt::Result {
        f.write_str(csi!("?25l"))
    }

    #[cfg(windows)]
    fn execute_winapi(&self) -> Result<()> {
        sys::show_cursor(false)
    }
}

/// A command that shows the terminal cursor.
///
/// # Notes
///
/// - Commands must be executed/queued for execution otherwise they do nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Show;

impl Command for Show {
    fn write_ansi(&self, f: &mut impl fmt::Write) -> fmt::Result {
        f.write_str(csi!("?25h"))
    }

    #[cfg(windows)]
    fn execute_winapi(&self) -> Result<()> {
        sys::show_cursor(true)
    }
}

/// A command that enables blinking of the terminal cursor.
///
/// # Notes
///
/// - Windows versions lower than Windows 10 do not support this functionality.
/// - Commands must be executed/queued for execution otherwise they do nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EnableBlinking;

impl Command for EnableBlinking {
    fn write_ansi(&self, f: &mut impl fmt::Write) -> fmt::Result {
        f.write_str(csi!("?12h"))
    }

    #[cfg(windows)]
    fn execute_winapi(&self) -> Result<()> {
        Ok(())
    }
}

/// A command that disables blinking of the terminal cursor.
///
/// # Notes
///
/// - Windows versions lower than Windows 10 do not support this functionality.
/// - Commands must be executed/queued for execution otherwise they do nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DisableBlinking;

impl Command for DisableBlinking {
    fn write_ansi(&self, f: &mut impl fmt::Write) -> fmt::Result {
        f.write_str(csi!("?12l"))
    }

    #[cfg(windows)]
    fn execute_winapi(&self) -> Result<()> {
        Ok(())
    }
}

/// All supported cursor shapes
///
/// # Note
///
/// - Used with SetCursorShape
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CursorShape {
    UnderScore,
    Line,
    Block,
}

/// A command that sets the shape of the cursor
///
/// # Note
///
/// - Commands must be executed/queued for execution otherwise they do nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SetCursorShape(pub CursorShape);

impl Command for SetCursorShape {
    fn write_ansi(&self, f: &mut impl fmt::Write) -> fmt::Result {
        use CursorShape::*;
        match self.0 {
            UnderScore => f.write_str("\x1b[3 q"),
            Line => f.write_str("\x1b[5 q"),
            Block => f.write_str("\x1b[2 q"),
        }
    }

    #[cfg(windows)]
    fn execute_winapi(&self) -> Result<()> {
        Ok(())
    }
}

impl_display!(for MoveTo);
impl_display!(for MoveToColumn);
impl_display!(for MoveToRow);
impl_display!(for MoveToNextLine);
impl_display!(for MoveToPreviousLine);
impl_display!(for MoveUp);
impl_display!(for MoveDown);
impl_display!(for MoveLeft);
impl_display!(for MoveRight);
impl_display!(for SavePosition);
impl_display!(for RestorePosition);
impl_display!(for Hide);
impl_display!(for Show);
impl_display!(for EnableBlinking);
impl_display!(for DisableBlinking);
impl_display!(for SetCursorShape);

#[cfg(test)]
mod tests {
    use std::io::{self, stdout};

    use crate::execute;

    use super::{
        position, MoveDown, MoveLeft, MoveRight, MoveTo, MoveUp, RestorePosition, SavePosition,
    };

    // Test is disabled, because it's failing on Travis
    #[test]
    #[ignore]
    fn test_move_to() {
        let (saved_x, saved_y) = position().unwrap();

        execute!(stdout(), MoveTo(saved_x + 1, saved_y + 1)).unwrap();
        assert_eq!(position().unwrap(), (saved_x + 1, saved_y + 1));

        execute!(stdout(), MoveTo(saved_x, saved_y)).unwrap();
        assert_eq!(position().unwrap(), (saved_x, saved_y));
    }

    // Test is disabled, because it's failing on Travis
    #[test]
    #[ignore]
    fn test_move_right() {
        let (saved_x, saved_y) = position().unwrap();
        execute!(io::stdout(), MoveRight(1)).unwrap();
        assert_eq!(position().unwrap(), (saved_x + 1, saved_y));
    }

    // Test is disabled, because it's failing on Travis
    #[test]
    #[ignore]
    fn test_move_left() {
        execute!(stdout(), MoveTo(2, 0), MoveLeft(2)).unwrap();
        assert_eq!(position().unwrap(), (0, 0));
    }

    // Test is disabled, because it's failing on Travis
    #[test]
    #[ignore]
    fn test_move_up() {
        execute!(stdout(), MoveTo(0, 2), MoveUp(2)).unwrap();
        assert_eq!(position().unwrap(), (0, 0));
    }

    // Test is disabled, because it's failing on Travis
    #[test]
    #[ignore]
    fn test_move_down() {
        execute!(stdout(), MoveTo(0, 0), MoveDown(2)).unwrap();

        assert_eq!(position().unwrap(), (0, 2));
    }

    // Test is disabled, because it's failing on Travis
    #[test]
    #[ignore]
    fn test_save_restore_position() {
        let (saved_x, saved_y) = position().unwrap();

        execute!(
            stdout(),
            SavePosition,
            MoveTo(saved_x + 1, saved_y + 1),
            RestorePosition
        )
        .unwrap();

        let (x, y) = position().unwrap();

        assert_eq!(x, saved_x);
        assert_eq!(y, saved_y);
    }
}
