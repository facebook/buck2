#![allow(non_camel_case_types)]

use libc::{c_int,c_uint,c_uchar};

pub type cc_t = c_uchar;
pub type speed_t = c_uint;
pub type tcflag_t = c_uint;

#[derive(Debug,Copy,Clone,Eq,PartialEq)]
#[repr(C)]
pub struct termios {
    pub c_iflag: tcflag_t,
    pub c_oflag: tcflag_t,
    pub c_cflag: tcflag_t,
    pub c_lflag: tcflag_t,
    pub c_cc: [cc_t; NCCS],
    c_ispeed: c_int,
    c_ospeed: c_int
}

pub const NCCS: usize = 20;

// c_cc characters
pub const VEOF:     usize = 0;
pub const VEOL:     usize = 1;
pub const VEOL2:    usize = 2;
pub const VERASE:   usize = 3;
pub const VWERASE:  usize = 4;
pub const VKILL:    usize = 5;
pub const VREPRINT: usize = 6;
pub const VINTR:    usize = 8;
pub const VQUIT:    usize = 9;
pub const VSUSP:    usize = 10;
pub const VSTART:   usize = 12;
pub const VSTOP:    usize = 13;
pub const VLNEXT:   usize = 14;
pub const VDISCARD: usize = 15;
pub const VMIN:     usize = 16;
pub const VTIME:    usize = 17;
pub const VSTATUS:  usize = 18;

// c_iflag bits
pub const IGNBRK:  tcflag_t = 0x00000001;
pub const BRKINT:  tcflag_t = 0x00000002;
pub const IGNPAR:  tcflag_t = 0x00000004;
pub const PARMRK:  tcflag_t = 0x00000008;
pub const INPCK:   tcflag_t = 0x00000010;
pub const ISTRIP:  tcflag_t = 0x00000020;
pub const INLCR:   tcflag_t = 0x00000040;
pub const IGNCR:   tcflag_t = 0x00000080;
pub const ICRNL:   tcflag_t = 0x00000100;
pub const IXON:    tcflag_t = 0x00000200;
pub const IXOFF:   tcflag_t = 0x00000400;
pub const IXANY:   tcflag_t = 0x00000800;
pub const IMAXBEL: tcflag_t = 0x00002000;

// c_oflag bits
pub const OPOST:  tcflag_t = 0x00000001;
pub const ONLCR:  tcflag_t = 0x00000002;
pub const OXTABS: tcflag_t = 0x00000004;
pub const ONOEOT: tcflag_t = 0x00000008;
pub const OCRNL:  tcflag_t = 0x00000010;
pub const ONOCR:  tcflag_t = 0x00000020;
pub const ONLRET: tcflag_t = 0x00000040;

// c_cflag bits
pub const CIGNORE:    tcflag_t = 0x00000001;
pub const CSIZE:      tcflag_t = 0x00000300;
pub const CS5:        tcflag_t = 0x00000000;
pub const CS6:        tcflag_t = 0x00000100;
pub const CS7:        tcflag_t = 0x00000200;
pub const CS8:        tcflag_t = 0x00000300;
pub const CSTOPB:     tcflag_t = 0x00000400;
pub const CREAD:      tcflag_t = 0x00000800;
pub const PARENB:     tcflag_t = 0x00001000;
pub const PARODD:     tcflag_t = 0x00002000;
pub const HUPCL:      tcflag_t = 0x00004000;
pub const CLOCAL:     tcflag_t = 0x00008000;
pub const CRTSCTS:    tcflag_t = 0x00010000;
pub const CRTS_IFLOW: tcflag_t = CRTSCTS;
pub const CCTS_OFLOW: tcflag_t = CRTSCTS;
pub const CDTRCTS:    tcflag_t = 0x00020000;
pub const MDMBUF:     tcflag_t = 0x00100000;
pub const CHWFLOW:    tcflag_t = MDMBUF|CRTSCTS|CDTRCTS;

// c_lflag bits
pub const ECHOKE:     tcflag_t = 0x00000001;
pub const ECHOE:      tcflag_t = 0x00000002;
pub const ECHOK:      tcflag_t = 0x00000004;
pub const ECHO:       tcflag_t = 0x00000008;
pub const ECHONL:     tcflag_t = 0x00000010;
pub const ECHOPRT:    tcflag_t = 0x00000020;
pub const ECHOCTL:    tcflag_t = 0x00000040;
pub const ISIG:       tcflag_t = 0x00000080;
pub const ICANON:     tcflag_t = 0x00000100;
pub const ALTWERASE:  tcflag_t = 0x00000200;
pub const IEXTEN:     tcflag_t = 0x00000400;
pub const EXTPROC:    tcflag_t = 0x00000800;
pub const TOSTOP:     tcflag_t = 0x00400000;
pub const FLUSHO:     tcflag_t = 0x00800000;
pub const NOKERNINFO: tcflag_t = 0x02000000;
pub const PENDIN:     tcflag_t = 0x20000000;
pub const NOFLSH:     tcflag_t = 0x80000000;

// baud rates
pub const B0:      speed_t = 0;
pub const B50:     speed_t = 50;
pub const B75:     speed_t = 75;
pub const B110:    speed_t = 110;
pub const B134:    speed_t = 134;
pub const B150:    speed_t = 150;
pub const B200:    speed_t = 200;
pub const B300:    speed_t = 300;
pub const B600:    speed_t = 600;
pub const B1200:   speed_t = 1200;
pub const B1800:   speed_t = 1800;
pub const B2400:   speed_t = 2400;
pub const B4800:   speed_t = 4800;
pub const B9600:   speed_t = 9600;
pub const B19200:  speed_t = 19200;
pub const B38400:  speed_t = 38400;
pub const B7200:   speed_t = 7200;
pub const B14400:  speed_t = 14400;
pub const B28800:  speed_t = 28800;
pub const B57600:  speed_t = 57600;
pub const B76800:  speed_t = 76800;
pub const B115200: speed_t = 115200;
pub const B230400: speed_t = 230400;
pub const B460800: speed_t = 460800;
pub const B921600: speed_t = 921600;
pub const EXTA:    speed_t = 19200;
pub const EXTB:    speed_t = 38400;

// tcflow()
pub const TCOOFF: c_int = 1;
pub const TCOON:  c_int = 2;
pub const TCIOFF: c_int = 3;
pub const TCION:  c_int = 4;

// tcflush()
pub const TCIFLUSH:  c_int = 1;
pub const TCOFLUSH:  c_int = 2;
pub const TCIOFLUSH: c_int = 3;

// tcsetattr()
pub const TCSANOW:   c_int = 0;
pub const TCSADRAIN: c_int = 1;
pub const TCSAFLUSH: c_int = 2;
pub const TCSASOFT:  c_int = 0x10;
