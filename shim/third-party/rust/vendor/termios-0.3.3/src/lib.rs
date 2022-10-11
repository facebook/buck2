//! The `termios` crate provides Rust bindings for the POSIX termios API that is implemented on
//! Unix operating systems. The termios API is defined in the [IEEE Std 1003.1 ("POSIX.1")
//! specification](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/termios.h.html).
//!
//! ## Getting Started
//!
//! The termios API operates on file descriptors that are associated with terminal devices, e.g.,
//! `/dev/tty*`. When used with other file descriptors, termios functions return an error. All
//! functions that are part of the POSIX standard are included in the `termios` crate. Where file
//! descriptors are expected, the type `std::os::unix::io::RawFd` is used, and integer error codes
//! are translated to `std::io::Result`.
//!
//! A major feature of the termios API is configuring a terminal device's parameters. The POSIX
//! standard defines a `termios` structure that contains the parameters and several functions for
//! manipulating the parameters. The `termios` crate defines a safe constructor that returns a
//! [`Termios`](struct.Termios.html) struct populated with the parameters of an open terminal
//! device:
//!
//! ```no_run
//! use termios::*;
//! # let fd = 1;
//! let mut termios = Termios::from_fd(fd).unwrap();
//! ```
//!
//! The [`Termios`](struct.Termios.html) struct provides access to the fields defined in the POSIX
//! standard (`c_iflag`, `c_oflag`, `c_cflag`, `c_lflag`, and `c_cc`):
//!
//! ```no_run
//! # use termios::*;
//! # let fd = 1;
//! # let mut termios = Termios::from_fd(fd).unwrap();
//! termios.c_cflag |= CREAD | CLOCAL;
//! termios.c_lflag &= !(ICANON | ECHO | ECHOE | ECHOK | ECHONL | ISIG | IEXTEN);
//! termios.c_oflag &= !OPOST;
//! termios.c_iflag &= !(INLCR | IGNCR | ICRNL | IGNBRK);
//!
//! termios.c_cc[VMIN] = 0;
//! termios.c_cc[VTIME] = 0;
//! ```
//!
//! The [`Termios`](struct.Termios.html) struct can also be manipulated using any of the standard
//! termios API functions:
//!
//! ```no_run
//! # use termios::*;
//! # let fd = 1;
//! # let mut termios = Termios::from_fd(fd).unwrap();
//! cfgetispeed(&termios);
//! cfgetospeed(&termios);
//! cfsetispeed(&mut termios, B9600).unwrap();
//! cfsetospeed(&mut termios, B9600).unwrap();
//! tcsetattr(fd, TCSANOW, &termios).unwrap();
//! ```
//!
//! ## Portability
//!
//! The `termios` crate is organized in a way to help write portable code, while also allowing
//! access to OS-specific functionality when necessary.
//!
//! The crate root contains types, constants, and function definitions that are common across Unix
//! operating systems. Most of the definitions in the crate root are from the POSIX standard;
//! however, support for the standard may differ across operating systems. A couple functions in
//! the crate root are not part of the POSIX standard, but are included in the crate root because
//! they are widely available across Unix operating systems.
//!
//! To write portable code, import the `termios` crate and use only the definitions from the crate
//! root.
//!
//! ### OS-Specific Extensions
//!
//! Each operating system may define extensions to the POSIX API. To make it clear when code
//! depends on OS-specific definitions, any non-standard definitions are exported in the
//! `termios::os` module. Programs that depend on OS-specific functionality must explicity opt-in.
//! When writing portable code that depends on OS-specific definitions, it will often be necessary
//! to use `#[cfg(...)]` attributes to support alternative implementations. The following is an
//! example of a portable function that sets the maximum speed on a `Termios` struct.
//!
//! ```no_run
//! use std::io;
//! use termios::{Termios,cfsetspeed};
//!
//! #[cfg(target_os = "linux")]
//! fn set_fastest_speed(termios: &mut Termios) -> io::Result<()> {
//!     cfsetspeed(termios, termios::os::linux::B4000000)
//! }
//!
//! #[cfg(target_os = "macos")]
//! fn set_fastest_speed(termios: &mut Termios) -> io::Result<()> {
//!     cfsetspeed(termios, termios::os::macos::B230400)
//! }
//!
//! #[cfg(target_os = "freebsd")]
//! fn set_fastest_speed(termios: &mut Termios) -> io::Result<()> {
//!     cfsetspeed(termios, termios::os::freebsd::B921600)
//! }
//!
//! #[cfg(target_os = "openbsd")]
//! fn set_fastest_speed(termios: &mut Termios) -> io::Result<()> {
//!     cfsetspeed(termios, termios::os::openbsd::B921600)
//! }
//!
//! #[cfg(target_os = "netbsd")]
//! fn set_fastest_speed(termios: &mut Termios) -> io::Result<()> {
//!     cfsetspeed(termios, termios::os::netbsd::B921600)
//! }
//!
//! #[cfg(target_os = "dragonfly")]
//! fn set_fastest_speed(termios: &mut Termios) -> io::Result<()> {
//!     cfsetspeed(termios, termios::os::dragonfly::B230400)
//! }
//!
//! #[cfg(target_os = "solaris")]
//! fn set_fastest_speed(termios: &mut Termios) -> io::Result<()> {
//!     cfsetspeed(termios, termios::os::solaris::B921600)
//! }
//!
//! #[cfg(target_os = "illumos")]
//! fn set_fastest_speed(termios: &mut Termios) -> io::Result<()> {
//!     cfsetspeed(termios, termios::os::illumos::B921600)
//! }
//!
//! # let fd = 1;
//! let mut termios = Termios::from_fd(fd).unwrap();
//! set_fastest_speed(&mut termios).unwrap();
//! ```

extern crate libc;

use std::io;
use std::mem;
use std::ops::{Deref,DerefMut};
use std::os::unix::io::RawFd;

use libc::{c_int,pid_t};

pub use ::os::target::{cc_t,speed_t,tcflag_t}; // types
pub use ::os::target::{VEOF,VEOL,VERASE,VINTR,VKILL,VMIN,VQUIT,VSTART,VSTOP,VSUSP,VTIME}; // c_cc subscripts
pub use ::os::target::{BRKINT,ICRNL,IGNBRK,IGNCR,IGNPAR,INLCR,INPCK,ISTRIP,IXANY,IXOFF,IXON,PARMRK}; // input modes
pub use ::os::target::{OPOST,ONLCR,OCRNL,ONOCR,ONLRET}; // output modes
pub use ::os::target::{B0,B50,B75,B110,B134,B150,B200,B300,B600,B1200,B1800,B2400,B4800,B9600,B19200,B38400}; // baud rate selection
pub use ::os::target::{CSIZE,CS5,CS6,CS7,CS8,CSTOPB,CREAD,PARENB,PARODD,HUPCL,CLOCAL}; // control modes
pub use ::os::target::{ECHO,ECHOE,ECHOK,ECHONL,ICANON,IEXTEN,ISIG,NOFLSH,TOSTOP}; // local modes
pub use ::os::target::{TCSANOW,TCSADRAIN,TCSAFLUSH}; // attribute selection
pub use ::os::target::{TCIFLUSH,TCIOFLUSH,TCOFLUSH,TCIOFF,TCION,TCOOFF,TCOON}; // line control

pub mod ffi;
pub mod os;


/// Unix terminal I/O control structure.
///
/// The `Termios` structure is a thin wrapper for the OS-specific `termios` struct. The only safe
/// way to obtain a `Termios` structure is to fill one from a file descriptor with
/// [`Termios::from_fd()`](#method.from_fd), after which it can be treated just like the POSIX
/// `termios` struct. It provides access to the standard fields of the `termios` struct (`c_iflag`,
/// `c_oflag`, `c_cflag`, `c_lflag`, and `c_cc`) through the `Deref` and `DerefMut` traits.
///
/// ## Example
///
/// The following is an example of how one might setup a file descriptor for a serial port:
///
/// ```no_run
/// use std::io;
/// use std::os::unix::io::RawFd;
///
/// fn setup_serial(fd: RawFd) -> io::Result<()> {
///     use termios::*;
///
///     let mut termios = try!(Termios::from_fd(fd));
///
///     termios.c_cflag |= CREAD | CLOCAL;
///     termios.c_lflag &= !(ICANON | ECHO | ECHOE | ECHOK | ECHONL | ISIG | IEXTEN);
///     termios.c_oflag &= !OPOST;
///     termios.c_iflag &= !(INLCR | IGNCR | ICRNL | IGNBRK);
///
///     termios.c_cc[VMIN] = 0;
///     termios.c_cc[VTIME] = 0;
///
///     try!(cfsetspeed(&mut termios, B9600));
///     try!(tcsetattr(fd, TCSANOW, &mut termios));
///
///     Ok(())
/// }
/// ```
#[derive(Debug,Copy,Clone,Eq,PartialEq)]
pub struct Termios {
    inner: ::os::target::termios
}

impl Termios {
    /// Creates a `Termios` structure based on the current settings of a file descriptor.
    ///
    /// `fd` must be an open file descriptor for a terminal device.
    pub fn from_fd(fd: RawFd) -> io::Result<Self> {
        let mut termios = unsafe { mem::uninitialized() };

        match tcgetattr(fd, &mut termios) {
            Ok(_) => Ok(termios),
            Err(err) => Err(err)
        }
    }

    fn inner(&self) -> &::os::target::termios {
        &self.inner
    }

    fn inner_mut(&mut self) -> &mut ::os::target::termios {
        &mut self.inner
    }
}

impl Deref for Termios {
    type Target = ::os::target::termios;

    fn deref(&self) -> &::os::target::termios {
        self.inner()
    }
}

impl DerefMut for Termios {
    fn deref_mut(&mut self) -> &mut ::os::target::termios {
        self.inner_mut()
    }
}


/// Gets the input baud rate stored in a `Termios` structure.
///
/// # Examples
///
/// ```
/// # use std::mem;
/// # use termios::{Termios,B9600,cfsetispeed,cfgetispeed};
/// # let mut termios = unsafe { mem::uninitialized() };
/// cfsetispeed(&mut termios, B9600).unwrap();
/// assert_eq!(cfgetispeed(&termios), B9600);
/// ```
pub fn cfgetispeed(termios: &Termios) -> speed_t {
    unsafe { ffi::cfgetispeed(termios.inner()) }
}

/// Gets the output baud rate stored in a `Termios` structure.
///
/// # Examples
///
/// ```
/// # use std::mem;
/// # use termios::{Termios,B9600,cfsetospeed,cfgetospeed};
/// # let mut termios = unsafe { mem::uninitialized() };
/// cfsetospeed(&mut termios, B9600).unwrap();
/// assert_eq!(cfgetospeed(&termios), B9600);
/// ```
pub fn cfgetospeed(termios: &Termios) -> speed_t {
    unsafe { ffi::cfgetospeed(termios.inner()) }
}

/// Sets the input baud rate.
///
/// This function only sets the necessary values on the given `Termios` structure. The settings are
/// applied by a subsequent call to [`tcsetattr()`](fn.tcsetattr.html).
///
/// # Parameters
///
/// * `termios` should be a mutable reference to a `Termios` structure.
/// * `speed` should be one of the baud rate constants:
///   - `B0`
///   - `B50`
///   - `B75`
///   - `B110`
///   - `B134`
///   - `B150`
///   - `B200`
///   - `B300`
///   - `B600`
///   - `B1200`
///   - `B1800`
///   - `B2400`
///   - `B4800`
///   - `B9600`
///   - `B19200`
///   - `B38400`
///   - any OS-specific baud rate defined in [`termios::os`](os/index.html).
///
/// A value of `B0` for `speed` sets the input baud rate to be the same as the output baud rate.
///
/// # Examples
///
/// ```
/// # use std::mem;
/// # use termios::{Termios,B9600,cfsetispeed,cfgetispeed};
/// # let mut termios = unsafe { mem::uninitialized() };
/// cfsetispeed(&mut termios, B9600).unwrap();
/// assert_eq!(cfgetispeed(&termios), B9600);
/// ```
pub fn cfsetispeed(termios: &mut Termios, speed: speed_t) -> io::Result<()> {
    io_result(unsafe { ffi::cfsetispeed(termios.inner_mut(), speed) })
}

/// Sets the output baud rate.
///
/// This function only sets the necessary values on the given `Termios` structure. The settings are
/// applied on a successful call to [`tcsetattr()`](fn.tcsetattr.html).
///
/// # Parameters
///
/// * `termios` should be a mutable reference to a `Termios` structure.
/// * `speed` should be one of the baud rate constants:
///   - `B0` (hang up)
///   - `B50`
///   - `B75`
///   - `B110`
///   - `B134`
///   - `B150`
///   - `B200`
///   - `B300`
///   - `B600`
///   - `B1200`
///   - `B1800`
///   - `B2400`
///   - `B4800`
///   - `B9600`
///   - `B19200`
///   - `B38400`
///   - any OS-specific baud rate defined in [`termios::os`](os/index.html).
///
/// A value of `B0` for `speed` deasserts the modem control lines when applied with
/// [`tcsetattr()`](fn.tcsetattr.html).  This normally has the effect of disconnecting the line.
///
/// # Examples
///
/// ```
/// # use std::mem;
/// # use termios::{Termios,B9600,cfsetospeed,cfgetospeed};
/// # let mut termios = unsafe { mem::uninitialized() };
/// cfsetospeed(&mut termios, B9600).unwrap();
/// assert_eq!(cfgetospeed(&termios), B9600);
/// ```
pub fn cfsetospeed(termios: &mut Termios, speed: speed_t) -> io::Result<()> {
    io_result(unsafe { ffi::cfsetospeed(termios.inner_mut(), speed) })
}

/// Sets input and output baud rates.
///
/// This function only sets the necessary values on the given `Termios` structure. The settings are
/// applied on a successful call to [`tcsetattr()`](fn.tcsetattr.html).
///
/// # Parameters
///
/// * `termios` should be a mutable reference to a `Termios` structure.
/// * `speed` should be one of the baud rate constants:
///   - `B0`
///   - `B50`
///   - `B75`
///   - `B110`
///   - `B134`
///   - `B150`
///   - `B200`
///   - `B300`
///   - `B600`
///   - `B1200`
///   - `B1800`
///   - `B2400`
///   - `B4800`
///   - `B9600`
///   - `B19200`
///   - `B38400`
///   - any OS-specific baud rate defined in [`termios::os`](os/index.html).
///
/// # Examples
///
/// ```
/// # use std::mem;
/// # use termios::{Termios,B9600,cfsetspeed,cfgetispeed,cfgetospeed};
/// # let mut termios = unsafe { mem::uninitialized() };
/// cfsetspeed(&mut termios, B9600).unwrap();
/// assert_eq!(cfgetispeed(&termios), B9600);
/// assert_eq!(cfgetospeed(&termios), B9600);
/// ```
///
/// # Portability
///
/// This function is not part of the IEEE Std 1003.1 ("POSIX.1") specification, but it is available
/// on Linux, BSD, and OS X.
pub fn cfsetspeed(termios: &mut Termios, speed: speed_t) -> io::Result<()> {
    io_result(unsafe { ffi::cfsetspeed(termios.inner_mut(), speed) })
}

/// Sets flags to disable all input and output processing.
///
/// This function only sets the necessary values on the given `Termios` structure. The settings are
/// applied on a successful call to [`tcsetattr()`](fn.tcsetattr.html).
///
/// # Portability
///
/// This function is not part of the IEEE Std 1003.1 ("POSIX.1") specification, but it is available
/// on Linux, BSD, and OS X.
pub fn cfmakeraw(termios: &mut Termios) {
    unsafe { ffi::cfmakeraw(termios.inner_mut()) };
}

/// Blocks until all output written to the file descriptor is transmitted.
///
/// # Parameters
///
/// * `fd` should be an open file descriptor associated with a terminal.
pub fn tcdrain(fd: RawFd) -> io::Result<()> {
    io_result(unsafe { ffi::tcdrain(fd) })
}

/// Suspends or restarts transmission or reception of data.
///
/// # Parameters
///
/// * `fd` should be an open file descriptor associated with a terminal.
/// * `action` should be one of the following constants:
///   - `TCOOFF` suspends output.
///   - `TCOON` restarts output.
///   - `TCIOFF` transmits a STOP character, intended to cause the remote device to stop
///     transmitting.
///   - `TCION` transmits a START character, intended to cause the remote device to resume
///     transmitting.
pub fn tcflow(fd: RawFd, action: c_int) -> io::Result<()> {
    io_result(unsafe { ffi::tcflow(fd, action) })
}

/// Discards data waiting in the terminal device's buffers.
///
/// `tcflush()` discards data that has been written to the device by an application but has not yet
/// been transmitted by the hardware or data that has been received by the hardware but has not yet
/// been read by an application.
///
/// # Parameters
///
/// * `fd` should be an open file descriptor associated with a terminal.
/// * `queue_selector` should be one of:
///   - `TCIFLUSH` to discard data received but not read.
///   - `TCOFLUSH` to discard data written but not transmitted.
///   - `TCIOFLUSH` to discard both data received but not read and data written but not
///     transmitted.
pub fn tcflush(fd: RawFd, queue_selector: c_int) -> io::Result<()> {
    io_result(unsafe { ffi::tcflush(fd, queue_selector) })
}

/// Populates a `Termios` structure with parameters associated with a terminal.
///
/// Upon successful completion, the `Termios` structure referred to by the `termios` parameter will
/// contain the parameters associated with the terminal device referred to by `fd`.
///
/// # Parameters
///
/// * `fd` should be an open file descriptor associated with a terminal.
/// * `termios` should be a mutable reference to the `Termios` structure that will hold the
///   terminal device's parameters.
pub fn tcgetattr(fd: RawFd, termios: &mut Termios) -> io::Result<()> {
    io_result(unsafe { ffi::tcgetattr(fd, termios.inner_mut()) })
}

/// Sets a terminal device's parameters.
///
/// `tcsetattr()` returns successfully if it was able to perform any of the requested actions, even
/// if other requested actions could not be performed. It will set all attributes that the
/// implementation supports and leave others unchanged. The `Termios` structure will not be updated
/// to reflect the changes that were applied.
///
/// In order to determine which parameters were applied to the terminal device, an application
/// should use [`tcgetattr()`](fn.tcgetattr.html) to obtain the latest state of the terminal
/// device. In particular, when attempting to change baud rates, [`tcgetattr()`](fn.tcgetattr.html)
/// can be used to determine which baud rates were actually selected.
///
/// If none of the requested actions could be performed, then `tcsetattr()` returns an error.
///
/// # Parameters
///
/// * `fd` should be an open file descriptor associated with a terminal.
/// * `action` should be one of the constants:
///   - `TCSANOW` applies the change immediately.
///   - `TCSADRAIN` applies the change after all output previously written to `fd` is transmitted.
///     This mode should be used when changing parameters that affect output.
///   - `TCSAFLUSH` applies the change after all output previously written to `fd` is transmitted.
///     All data received but not read is discarded before applying the change.
/// * `termios` should be a mutable reference to a `Termios` structure containing the parameters to
///   apply to the terminal device.
pub fn tcsetattr(fd: RawFd, action: c_int, termios: &Termios) -> io::Result<()> {
    io_result(unsafe { ffi::tcsetattr(fd, action, termios.inner()) })
}

/// Returns the process group ID of the controlling terminal's session.
///
/// # Parameters
///
/// * `fd` should be an open file descriptor associated with a controlling terminal.
pub fn tcgetsid(fd: RawFd) -> pid_t {
    unsafe { ffi::tcgetsid(fd) }
}

/// Transmits data to generate a break condition.
///
/// If the terminal device is using asynchronous data transmission, `tcsendbreak()` transmits a
/// continuous stream of zero bits for a specific duration.
///
/// # Parameters
///
/// * `fd` should be an open file descriptor associated with a terminal.
/// * `duration` controls the duration of the transmitted zero bits. A value of 0 causes a
///   transmission between 0.25 and 0.5 seconds. A value other than 0 causes a transmission for an
///   implementation-defined period of time.
pub fn tcsendbreak(fd: RawFd, duration: c_int) -> io::Result<()> {
    io_result(unsafe { ffi::tcsendbreak(fd, duration) })
}


#[inline]
fn io_result(result: c_int) -> io::Result<()> {
    match result {
        0 => Ok(()),
        _ => Err(io::Error::last_os_error())
    }
}
