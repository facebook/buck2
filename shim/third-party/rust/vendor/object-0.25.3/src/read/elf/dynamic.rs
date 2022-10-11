use core::fmt::Debug;

use crate::elf;
use crate::endian;
use crate::pod::Pod;

/// A trait for generic access to `Dyn32` and `Dyn64`.
#[allow(missing_docs)]
pub trait Dyn: Debug + Pod {
    type Word: Into<u64>;
    type Endian: endian::Endian;

    fn d_tag(&self, endian: Self::Endian) -> Self::Word;
    fn d_val(&self, endian: Self::Endian) -> Self::Word;
}

impl<Endian: endian::Endian> Dyn for elf::Dyn32<Endian> {
    type Word = u32;
    type Endian = Endian;

    #[inline]
    fn d_tag(&self, endian: Self::Endian) -> Self::Word {
        self.d_tag.get(endian)
    }

    #[inline]
    fn d_val(&self, endian: Self::Endian) -> Self::Word {
        self.d_val.get(endian)
    }
}

impl<Endian: endian::Endian> Dyn for elf::Dyn64<Endian> {
    type Word = u64;
    type Endian = Endian;

    #[inline]
    fn d_tag(&self, endian: Self::Endian) -> Self::Word {
        self.d_tag.get(endian)
    }

    #[inline]
    fn d_val(&self, endian: Self::Endian) -> Self::Word {
        self.d_val.get(endian)
    }
}
