//! # Event
//!
//! The `event` module provides the functionality to read keyboard, mouse and terminal resize events.
//!
//! * The [`read`](fn.read.html) function returns an [`Event`](enum.Event.html) immediately
//! (if available) or blocks until an [`Event`](enum.Event.html) is available.
//!
//! * The [`poll`](fn.poll.html) function allows you to check if there is or isn't an [`Event`](enum.Event.html) available
//! within the given period of time. In other words - if subsequent call to the [`read`](fn.read.html)
//! function will block or not.
//!
//! It's **not allowed** to call these functions from different threads or combine them with the
//! [`EventStream`](struct.EventStream.html). You're allowed to either:
//!
//! * use the [`read`](fn.read.html) & [`poll`](fn.poll.html) functions on any, but same, thread
//! * or the [`EventStream`](struct.EventStream.html).
//!
//! **Make sure to enable [raw mode](./terminal/index.html#raw-mode) in order for keyboard events to work properly**
//!
//! ## Mouse Events
//!
//! Mouse events are not enabled by default. You have to enable them with the
//! [`EnableMouseCapture`](struct.EnableMouseCapture.html) command. See [Command API](./index.html#command-api)
//! for more information.
//!
//! ## Examples
//!
//! Blocking read:
//!
//! ```no_run
//! use crossterm::event::{read, Event};
//!
//! fn print_events() -> crossterm::Result<()> {
//!     loop {
//!         // `read()` blocks until an `Event` is available
//!         match read()? {
//!             Event::Key(event) => println!("{:?}", event),
//!             Event::Mouse(event) => println!("{:?}", event),
//!             Event::Resize(width, height) => println!("New size {}x{}", width, height),
//!         }
//!     }
//!     Ok(())
//! }
//! ```
//!
//! Non-blocking read:
//!
//! ```no_run
//! use std::time::Duration;
//!
//! use crossterm::event::{poll, read, Event};
//!
//! fn print_events() -> crossterm::Result<()> {
//!     loop {
//!         // `poll()` waits for an `Event` for a given time period
//!         if poll(Duration::from_millis(500))? {
//!             // It's guaranteed that the `read()` won't block when the `poll()`
//!             // function returns `true`
//!             match read()? {
//!                 Event::Key(event) => println!("{:?}", event),
//!                 Event::Mouse(event) => println!("{:?}", event),
//!                 Event::Resize(width, height) => println!("New size {}x{}", width, height),
//!             }
//!         } else {
//!             // Timeout expired and no `Event` is available
//!         }
//!     }
//!     Ok(())
//! }
//! ```
//!
//! Check the [examples](https://github.com/crossterm-rs/crossterm/tree/master/examples) folder for more of
//! them (`event-*`).

use std::fmt;
use std::hash::{Hash, Hasher};
use std::time::Duration;

use bitflags::bitflags;
use parking_lot::{MappedMutexGuard, Mutex, MutexGuard};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::{csi, Command, Result};
use filter::{EventFilter, Filter};
use read::InternalEventReader;
#[cfg(feature = "event-stream")]
pub use stream::EventStream;
use timeout::PollTimeout;

pub(crate) mod filter;
mod read;
mod source;
#[cfg(feature = "event-stream")]
mod stream;
pub(crate) mod sys;
mod timeout;

/// Static instance of `InternalEventReader`.
/// This needs to be static because there can be one event reader.
static INTERNAL_EVENT_READER: Mutex<Option<InternalEventReader>> = parking_lot::const_mutex(None);

fn lock_internal_event_reader() -> MappedMutexGuard<'static, InternalEventReader> {
    MutexGuard::map(INTERNAL_EVENT_READER.lock(), |reader| {
        reader.get_or_insert_with(InternalEventReader::default)
    })
}
fn try_lock_internal_event_reader_for(
    duration: Duration,
) -> Option<MappedMutexGuard<'static, InternalEventReader>> {
    Some(MutexGuard::map(
        INTERNAL_EVENT_READER.try_lock_for(duration)?,
        |reader| reader.get_or_insert_with(InternalEventReader::default),
    ))
}

/// Checks if there is an [`Event`](enum.Event.html) available.
///
/// Returns `Ok(true)` if an [`Event`](enum.Event.html) is available otherwise it returns `Ok(false)`.
///
/// `Ok(true)` guarantees that subsequent call to the [`read`](fn.read.html) function
/// wont block.
///
/// # Arguments
///
/// * `timeout` - maximum waiting time for event availability
///
/// # Examples
///
/// Return immediately:
///
/// ```no_run
/// use std::time::Duration;
///
/// use crossterm::{event::poll, Result};
///
/// fn is_event_available() -> Result<bool> {
///     // Zero duration says that the `poll` function must return immediately
///     // with an `Event` availability information
///     poll(Duration::from_secs(0))
/// }
/// ```
///
/// Wait up to 100ms:
///
/// ```no_run
/// use std::time::Duration;
///
/// use crossterm::{event::poll, Result};
///
/// fn is_event_available() -> Result<bool> {
///     // Wait for an `Event` availability for 100ms. It returns immediately
///     // if an `Event` is/becomes available.
///     poll(Duration::from_millis(100))
/// }
/// ```
pub fn poll(timeout: Duration) -> Result<bool> {
    poll_internal(Some(timeout), &EventFilter)
}

/// Reads a single [`Event`](enum.Event.html).
///
/// This function blocks until an [`Event`](enum.Event.html) is available. Combine it with the
/// [`poll`](fn.poll.html) function to get non-blocking reads.
///
/// # Examples
///
/// Blocking read:
///
/// ```no_run
/// use crossterm::{event::read, Result};
///
/// fn print_events() -> Result<bool> {
///     loop {
///         // Blocks until an `Event` is available
///         println!("{:?}", read()?);
///     }
/// }
/// ```
///
/// Non-blocking read:
///
/// ```no_run
/// use std::time::Duration;
///
/// use crossterm::{event::{read, poll}, Result};
///
/// fn print_events() -> Result<bool> {
///     loop {
///         if poll(Duration::from_millis(100))? {
///             // It's guaranteed that `read` wont block, because `poll` returned
///             // `Ok(true)`.
///             println!("{:?}", read()?);
///         } else {
///             // Timeout expired, no `Event` is available
///         }
///     }
/// }
/// ```
pub fn read() -> Result<Event> {
    match read_internal(&EventFilter)? {
        InternalEvent::Event(event) => Ok(event),
        #[cfg(unix)]
        _ => unreachable!(),
    }
}

/// Polls to check if there are any `InternalEvent`s that can be read within the given duration.
pub(crate) fn poll_internal<F>(timeout: Option<Duration>, filter: &F) -> Result<bool>
where
    F: Filter,
{
    let (mut reader, timeout) = if let Some(timeout) = timeout {
        let poll_timeout = PollTimeout::new(Some(timeout));
        if let Some(reader) = try_lock_internal_event_reader_for(timeout) {
            (reader, poll_timeout.leftover())
        } else {
            return Ok(false);
        }
    } else {
        (lock_internal_event_reader(), None)
    };
    reader.poll(timeout, filter)
}

/// Reads a single `InternalEvent`.
pub(crate) fn read_internal<F>(filter: &F) -> Result<InternalEvent>
where
    F: Filter,
{
    let mut reader = lock_internal_event_reader();
    reader.read(filter)
}

/// A command that enables mouse event capturing.
///
/// Mouse events can be captured with [read](./fn.read.html)/[poll](./fn.poll.html).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EnableMouseCapture;

impl Command for EnableMouseCapture {
    fn write_ansi(&self, f: &mut impl fmt::Write) -> fmt::Result {
        f.write_str(concat!(
            // Normal tracking: Send mouse X & Y on button press and release
            csi!("?1000h"),
            // Button-event tracking: Report button motion events (dragging)
            csi!("?1002h"),
            // Any-event tracking: Report all motion events
            csi!("?1003h"),
            // RXVT mouse mode: Allows mouse coordinates of >223
            csi!("?1015h"),
            // SGR mouse mode: Allows mouse coordinates of >223, preferred over RXVT mode
            csi!("?1006h"),
        ))
    }

    #[cfg(windows)]
    fn execute_winapi(&self) -> Result<()> {
        sys::windows::enable_mouse_capture()
    }

    #[cfg(windows)]
    fn is_ansi_code_supported(&self) -> bool {
        false
    }
}

/// A command that disables mouse event capturing.
///
/// Mouse events can be captured with [read](./fn.read.html)/[poll](./fn.poll.html).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DisableMouseCapture;

impl Command for DisableMouseCapture {
    fn write_ansi(&self, f: &mut impl fmt::Write) -> fmt::Result {
        f.write_str(concat!(
            // The inverse commands of EnableMouseCapture, in reverse order.
            csi!("?1006l"),
            csi!("?1015l"),
            csi!("?1003l"),
            csi!("?1002l"),
            csi!("?1000l"),
        ))
    }

    #[cfg(windows)]
    fn execute_winapi(&self) -> Result<()> {
        sys::windows::disable_mouse_capture()
    }

    #[cfg(windows)]
    fn is_ansi_code_supported(&self) -> bool {
        false
    }
}

/// Represents an event.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, PartialOrd, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Event {
    /// A single key event with additional pressed modifiers.
    Key(KeyEvent),
    /// A single mouse event with additional pressed modifiers.
    Mouse(MouseEvent),
    /// An resize event with new dimensions after resize (columns, rows).
    /// **Note** that resize events can be occur in batches.
    Resize(u16, u16),
}

/// Represents a mouse event.
///
/// # Platform-specific Notes
///
/// ## Mouse Buttons
///
/// Some platforms/terminals do not report mouse button for the
/// `MouseEventKind::Up` and `MouseEventKind::Drag` events. `MouseButton::Left`
/// is returned if we don't know which button was used.
///
/// ## Key Modifiers
///
/// Some platforms/terminals does not report all key modifiers
/// combinations for all mouse event types. For example - macOS reports
/// `Ctrl` + left mouse button click as a right mouse button click.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, PartialOrd, PartialEq, Eq, Clone, Copy, Hash)]
pub struct MouseEvent {
    /// The kind of mouse event that was caused.
    pub kind: MouseEventKind,
    /// The column that the event occurred on.
    pub column: u16,
    /// The row that the event occurred on.
    pub row: u16,
    /// The key modifiers active when the event occurred.
    pub modifiers: KeyModifiers,
}

/// A mouse event kind.
///
/// # Platform-specific Notes
///
/// ## Mouse Buttons
///
/// Some platforms/terminals do not report mouse button for the
/// `MouseEventKind::Up` and `MouseEventKind::Drag` events. `MouseButton::Left`
/// is returned if we don't know which button was used.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, PartialOrd, PartialEq, Eq, Clone, Copy, Hash)]
pub enum MouseEventKind {
    /// Pressed mouse button. Contains the button that was pressed.
    Down(MouseButton),
    /// Released mouse button. Contains the button that was released.
    Up(MouseButton),
    /// Moved the mouse cursor while pressing the contained mouse button.
    Drag(MouseButton),
    /// Moved the mouse cursor while not pressing a mouse button.
    Moved,
    /// Scrolled mouse wheel downwards (towards the user).
    ScrollDown,
    /// Scrolled mouse wheel upwards (away from the user).
    ScrollUp,
}

/// Represents a mouse button.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, PartialOrd, PartialEq, Eq, Clone, Copy, Hash)]
pub enum MouseButton {
    /// Left mouse button.
    Left,
    /// Right mouse button.
    Right,
    /// Middle mouse button.
    Middle,
}

bitflags! {
    /// Represents key modifiers (shift, control, alt).
    #[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
    pub struct KeyModifiers: u8 {
        const SHIFT = 0b0000_0001;
        const CONTROL = 0b0000_0010;
        const ALT = 0b0000_0100;
        const NONE = 0b0000_0000;
    }
}

/// Represents a key event.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, PartialOrd, Clone, Copy)]
pub struct KeyEvent {
    /// The key itself.
    pub code: KeyCode,
    /// Additional key modifiers.
    pub modifiers: KeyModifiers,
}

impl KeyEvent {
    pub const fn new(code: KeyCode, modifiers: KeyModifiers) -> KeyEvent {
        KeyEvent { code, modifiers }
    }

    // modifies the KeyEvent,
    // so that KeyModifiers::SHIFT is present iff
    // an uppercase char is present.
    fn normalize_case(mut self) -> KeyEvent {
        let c = match self.code {
            KeyCode::Char(c) => c,
            _ => return self,
        };

        if c.is_ascii_uppercase() {
            self.modifiers.insert(KeyModifiers::SHIFT);
        } else if self.modifiers.contains(KeyModifiers::SHIFT) {
            self.code = KeyCode::Char(c.to_ascii_uppercase())
        }
        self
    }
}

impl From<KeyCode> for KeyEvent {
    fn from(code: KeyCode) -> Self {
        KeyEvent {
            code,
            modifiers: KeyModifiers::empty(),
        }
    }
}

impl PartialEq for KeyEvent {
    fn eq(&self, other: &KeyEvent) -> bool {
        let KeyEvent {
            code: lhs_code,
            modifiers: lhs_modifiers,
        } = self.normalize_case();
        let KeyEvent {
            code: rhs_code,
            modifiers: rhs_modifiers,
        } = other.normalize_case();
        (lhs_code == rhs_code) && (lhs_modifiers == rhs_modifiers)
    }
}

impl Eq for KeyEvent {}

impl Hash for KeyEvent {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let KeyEvent { code, modifiers } = self.normalize_case();
        code.hash(state);
        modifiers.hash(state);
    }
}

/// Represents a key.
#[derive(Debug, PartialOrd, PartialEq, Eq, Clone, Copy, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum KeyCode {
    /// Backspace key.
    Backspace,
    /// Enter key.
    Enter,
    /// Left arrow key.
    Left,
    /// Right arrow key.
    Right,
    /// Up arrow key.
    Up,
    /// Down arrow key.
    Down,
    /// Home key.
    Home,
    /// End key.
    End,
    /// Page up key.
    PageUp,
    /// Page down key.
    PageDown,
    /// Tab key.
    Tab,
    /// Shift + Tab key.
    BackTab,
    /// Delete key.
    Delete,
    /// Insert key.
    Insert,
    /// F key.
    ///
    /// `KeyCode::F(1)` represents F1 key, etc.
    F(u8),
    /// A character.
    ///
    /// `KeyCode::Char('c')` represents `c` character, etc.
    Char(char),
    /// Null.
    Null,
    /// Escape key.
    Esc,
}

/// An internal event.
///
/// Encapsulates publicly available `Event` with additional internal
/// events that shouldn't be publicly available to the crate users.
#[derive(Debug, PartialOrd, PartialEq, Hash, Clone, Eq)]
pub(crate) enum InternalEvent {
    /// An event.
    Event(Event),
    /// A cursor position (`col`, `row`).
    #[cfg(unix)]
    CursorPosition(u16, u16),
}

#[cfg(test)]
mod tests {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    use super::{KeyCode, KeyEvent, KeyModifiers};

    #[test]
    fn test_equality() {
        let lowercase_d_with_shift = KeyEvent::new(KeyCode::Char('d'), KeyModifiers::SHIFT);
        let uppercase_d_with_shift = KeyEvent::new(KeyCode::Char('D'), KeyModifiers::SHIFT);
        let uppercase_d = KeyEvent::new(KeyCode::Char('D'), KeyModifiers::NONE);
        assert_eq!(lowercase_d_with_shift, uppercase_d_with_shift);
        assert_eq!(uppercase_d, uppercase_d_with_shift);
    }

    #[test]
    fn test_hash() {
        let lowercase_d_with_shift_hash = {
            let mut hasher = DefaultHasher::new();
            KeyEvent::new(KeyCode::Char('d'), KeyModifiers::SHIFT).hash(&mut hasher);
            hasher.finish()
        };
        let uppercase_d_with_shift_hash = {
            let mut hasher = DefaultHasher::new();
            KeyEvent::new(KeyCode::Char('D'), KeyModifiers::SHIFT).hash(&mut hasher);
            hasher.finish()
        };
        let uppercase_d_hash = {
            let mut hasher = DefaultHasher::new();
            KeyEvent::new(KeyCode::Char('D'), KeyModifiers::NONE).hash(&mut hasher);
            hasher.finish()
        };
        assert_eq!(lowercase_d_with_shift_hash, uppercase_d_with_shift_hash);
        assert_eq!(uppercase_d_hash, uppercase_d_with_shift_hash);
    }
}
