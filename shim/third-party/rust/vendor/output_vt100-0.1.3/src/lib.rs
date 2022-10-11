//! # Output-VT100
//!
//! When you write terminal-based crates, sometimes you might want to use the
//! standard ANSI escaped characters, to display some colors, to display text
//! as bold, italic or whatever. However, you’ve just discovered all your
//! pretty displays that worked like a charm on Linux and Mac look terrible
//! on Windows, because the escaped characters do not work. Rather, they are
//! not activated by default. Then you discover you have to do system calls to
//! Windows directly to activate them in order to get your beautiful text back.
//! What a pain!
//! And this is where this crate comes in action! Simply add it as a dependency
//! for your own crate, and use it like this:
//! ```rust
//! extern crate output_vt100;
//!
//! fn main() {
//!     output_vt100::init();
//!     println!("\x1b[31mThis text is red!\x1b[0m");
//! }
//! ```
//! And that’s it! By calling it once, you have now activated PowerShell’s and
//! CMD’s support for ANSI’s escaped characters on your Windows builds! And
//! you can leave this line in your Unix builds too, it will simply do nothing.

use std::fmt;

#[derive(Debug)]
pub struct InitError;

impl fmt::Display for InitError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "output-vt100-rs: Could not initialize")
    }
}

impl std::error::Error for InitError {}

#[cfg(windows)]
pub fn try_init() -> Result<(), InitError> {
    use winapi::shared::minwindef::DWORD;
    use winapi::um::consoleapi::{GetConsoleMode, SetConsoleMode};
    use winapi::um::processenv::GetStdHandle;
    use winapi::um::winbase::STD_OUTPUT_HANDLE;
    use winapi::um::wincon::{DISABLE_NEWLINE_AUTO_RETURN, ENABLE_VIRTUAL_TERMINAL_PROCESSING};

    let console_out = unsafe { GetStdHandle(STD_OUTPUT_HANDLE) };

    let mut state: DWORD = 0;
    let mut ret: Result<(), _> = Ok(());
    unsafe {
        if GetConsoleMode(console_out, &mut state) == 0 {
            ret = Err(InitError);
        }
        if ret.is_ok() {
            state |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
            state &= !DISABLE_NEWLINE_AUTO_RETURN;
            if SetConsoleMode(console_out, state) == 0 {
                ret = Err(InitError);
            }
        }
    }
    return ret;
}

#[cfg(windows)]
pub fn init() {
    assert_eq!(try_init().is_ok(), true);
}

#[cfg(not(windows))]
pub fn try_init() -> Result<(), InitError> {
    Ok(())
}

#[cfg(not(windows))]
pub fn init() {}

#[cfg(test)]
mod tests {
    #[test]
    fn activate_vt100() {
        crate::init();
    }
}
