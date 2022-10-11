// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

// We need to know the endianness of the platform to know how to deal with pointers.
#[cfg(target_endian = "big")]
const UPSIDE_DOWN_LAND: bool = false;
#[cfg(target_endian = "little")]
const UPSIDE_DOWN_LAND: bool = true;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Discriminant {
    Boxed,
    Inline,
}

impl Discriminant {
    #[inline(always)]
    const fn from_bit(bit: bool) -> Self {
        if bit {
            Self::Inline
        } else {
            Self::Boxed
        }
    }

    #[inline(always)]
    const fn bit(self) -> u8 {
        match self {
            Self::Boxed => 0,
            Self::Inline => 1,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct Marker(u8);

impl Marker {
    #[inline(always)]
    const fn assemble(discriminant: Discriminant, data: u8) -> u8 {
        let data = data;
        if UPSIDE_DOWN_LAND {
            data << 1 | discriminant.bit()
        } else {
            discriminant.bit() << 7 | data
        }
    }

    #[inline(always)]
    pub(crate) const fn empty() -> Self {
        Self(Self::assemble(Discriminant::Inline, 0))
    }

    #[inline(always)]
    pub(crate) const unsafe fn new_inline_unsafe(data: u8) -> Self {
        Self(Self::assemble(Discriminant::Inline, data))
    }

    pub(crate) fn new_inline(data: u8) -> Self {
        debug_assert!(data < 0x80);
        unsafe { Self::new_inline_unsafe(data) }
    }

    #[inline(always)]
    pub(crate) const fn discriminant(self) -> Discriminant {
        Discriminant::from_bit(if UPSIDE_DOWN_LAND {
            self.0 & 0x01 != 0
        } else {
            self.0 & 0x80 != 0
        })
    }

    #[inline(always)]
    pub(crate) const fn data(self) -> u8 {
        if UPSIDE_DOWN_LAND {
            self.0 >> 1
        } else {
            self.0 & 0x7f
        }
    }

    #[inline(always)]
    pub(crate) fn set_data(&mut self, byte: u8) {
        debug_assert!(byte < 0x80);
        self.0 = Self::assemble(self.discriminant(), byte);
    }
}
