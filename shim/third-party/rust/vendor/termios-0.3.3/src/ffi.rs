//! Unsafe FFI bindings.

use libc::{c_int,pid_t};

#[link(name = "c")]
extern "C" {
    pub fn tcgetattr(fd: c_int, termios_p: *mut ::os::target::termios) -> c_int;
    pub fn tcsetattr(fd: c_int, optional_actions: c_int, termios_p: *const ::os::target::termios) -> c_int;
    pub fn tcsendbreak(fd: c_int, duration: c_int) -> c_int;
    pub fn tcdrain(fd: c_int) -> c_int;
    pub fn tcflush(fd: c_int, queue_selector: c_int) -> c_int;
    pub fn tcflow(fd: c_int, action: c_int) -> c_int;
    #[cfg(not(any(target_os = "solaris", target_os = "illumos")))]
    pub fn cfmakeraw(termios_p: *mut ::os::target::termios);
    pub fn cfgetispeed(termios_p: *const ::os::target::termios) -> ::os::target::speed_t;
    pub fn cfgetospeed(termios_p: *const ::os::target::termios) -> ::os::target::speed_t;
    pub fn cfsetispeed(termios_p: *mut ::os::target::termios, speed: ::os::target::speed_t) -> c_int;
    pub fn cfsetospeed(termios_p: *mut ::os::target::termios, speed: ::os::target::speed_t) -> c_int;
    #[cfg(not(any(target_os = "solaris", target_os = "illumos")))]
    pub fn cfsetspeed(termios_p: *mut ::os::target::termios, speed: ::os::target::speed_t) -> c_int;
    pub fn tcgetsid(fd: c_int) -> pid_t;
}

#[cfg(any(target_os = "solaris", target_os = "illumos"))]
#[no_mangle]
pub unsafe extern "C" fn cfmakeraw(termios: *mut ::os::target::termios) {
    use ::os::target::{IMAXBEL, IGNBRK, BRKINT, PARMRK, ISTRIP, INLCR, IGNCR, ICRNL, IXON};
    use ::os::target::{OPOST, ECHO, ECHONL, ICANON, ISIG, IEXTEN, CSIZE, PARENB, CS8};
    use ::os::target::{VMIN, VTIME};

    // Equivalent of cfmakeraw() in glibc
    (*termios).c_iflag &= !(IMAXBEL | IGNBRK | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
    (*termios).c_oflag &= !OPOST;
    (*termios).c_lflag &= !(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
    (*termios).c_cflag &= !(CSIZE | PARENB);
    (*termios).c_cflag |= CS8;
    (*termios).c_cc[VMIN] = 1;
    (*termios).c_cc[VTIME] = 0;
}

#[cfg(any(target_os = "solaris", target_os = "illumos"))]
#[no_mangle]
pub unsafe extern "C" fn cfsetspeed(termios_p: *mut ::os::target::termios, speed: ::os::target::speed_t) -> c_int {
    match cfsetispeed(termios_p, speed) {
        0 => cfsetospeed(termios_p, speed),
        err => err,
    }
}
