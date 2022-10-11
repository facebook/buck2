//! UNIX related logic for terminal manipulation.

use std::fs::File;
use std::os::unix::io::{IntoRawFd, RawFd};
use std::{io, mem, process};

use libc::{
    cfmakeraw, ioctl, tcgetattr, tcsetattr, termios as Termios, winsize, STDOUT_FILENO, TCSANOW,
    TIOCGWINSZ,
};
use parking_lot::Mutex;

use crate::error::Result;
use crate::event::sys::unix::file_descriptor::{tty_fd, FileDesc};

// Some(Termios) -> we're in the raw mode and this is the previous mode
// None -> we're not in the raw mode
static TERMINAL_MODE_PRIOR_RAW_MODE: Mutex<Option<Termios>> = parking_lot::const_mutex(None);

pub(crate) fn is_raw_mode_enabled() -> bool {
    TERMINAL_MODE_PRIOR_RAW_MODE.lock().is_some()
}

#[allow(clippy::useless_conversion)]
pub(crate) fn size() -> Result<(u16, u16)> {
    // http://rosettacode.org/wiki/Terminal_control/Dimensions#Library:_BSD_libc
    let mut size = winsize {
        ws_row: 0,
        ws_col: 0,
        ws_xpixel: 0,
        ws_ypixel: 0,
    };

    let file = File::open("/dev/tty").map(|file| (FileDesc::new(file.into_raw_fd(), true)));
    let fd = if let Ok(file) = &file {
        file.raw_fd()
    } else {
        // Fallback to libc::STDOUT_FILENO if /dev/tty is missing
        STDOUT_FILENO
    };

    if wrap_with_result(unsafe { ioctl(fd, TIOCGWINSZ.into(), &mut size) }).is_ok() {
        Ok((size.ws_col, size.ws_row))
    } else {
        tput_size().ok_or_else(|| std::io::Error::last_os_error().into())
    }
}

pub(crate) fn enable_raw_mode() -> Result<()> {
    let mut original_mode = TERMINAL_MODE_PRIOR_RAW_MODE.lock();

    if original_mode.is_some() {
        return Ok(());
    }

    let tty = tty_fd()?;
    let fd = tty.raw_fd();
    let mut ios = get_terminal_attr(fd)?;
    let original_mode_ios = ios;

    raw_terminal_attr(&mut ios);
    set_terminal_attr(fd, &ios)?;

    // Keep it last - set the original mode only if we were able to switch to the raw mode
    *original_mode = Some(original_mode_ios);

    Ok(())
}

/// Reset the raw mode.
///
/// More precisely, reset the whole termios mode to what it was before the first call
/// to [enable_raw_mode]. If you don't mess with termios outside of crossterm, it's
/// effectively disabling the raw mode and doing nothing else.
pub(crate) fn disable_raw_mode() -> Result<()> {
    let mut original_mode = TERMINAL_MODE_PRIOR_RAW_MODE.lock();

    if let Some(original_mode_ios) = original_mode.as_ref() {
        let tty = tty_fd()?;
        set_terminal_attr(tty.raw_fd(), original_mode_ios)?;
        // Keep it last - remove the original mode only if we were able to switch back
        *original_mode = None;
    }

    Ok(())
}

/// execute tput with the given argument and parse
/// the output as a u16.
///
/// The arg should be "cols" or "lines"
fn tput_value(arg: &str) -> Option<u16> {
    let output = process::Command::new("tput").arg(arg).output().ok()?;
    let value = output
        .stdout
        .into_iter()
        .filter_map(|b| char::from(b).to_digit(10))
        .fold(0, |v, n| v * 10 + n as u16);

    if value > 0 {
        Some(value)
    } else {
        None
    }
}

/// Returns the size of the screen as determined by tput.
///
/// This alternate way of computing the size is useful
/// when in a subshell.
fn tput_size() -> Option<(u16, u16)> {
    match (tput_value("cols"), tput_value("lines")) {
        (Some(w), Some(h)) => Some((w, h)),
        _ => None,
    }
}

// Transform the given mode into an raw mode (non-canonical) mode.
fn raw_terminal_attr(termios: &mut Termios) {
    unsafe { cfmakeraw(termios) }
}

fn get_terminal_attr(fd: RawFd) -> Result<Termios> {
    unsafe {
        let mut termios = mem::zeroed();
        wrap_with_result(tcgetattr(fd, &mut termios))?;
        Ok(termios)
    }
}

fn set_terminal_attr(fd: RawFd, termios: &Termios) -> Result<()> {
    wrap_with_result(unsafe { tcsetattr(fd, TCSANOW, termios) })
}

fn wrap_with_result(result: i32) -> Result<()> {
    if result == -1 {
        Err(io::Error::last_os_error())
    } else {
        Ok(())
    }
}
