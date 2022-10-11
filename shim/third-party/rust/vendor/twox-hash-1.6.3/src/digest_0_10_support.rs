use core::hash::Hasher;

use digest_0_10::{
    generic_array::typenum::consts::{U16, U4, U8},
    FixedOutput, HashMarker, Output, OutputSizeUser, Update,
};

use crate::{xxh3, XxHash32, XxHash64};

// ----------

impl Update for XxHash32 {
    fn update(&mut self, data: &[u8]) {
        self.write(data);
    }
}

impl OutputSizeUser for XxHash32 {
    type OutputSize = U4;
}

impl FixedOutput for XxHash32 {
    fn finalize_into(self, out: &mut Output<Self>) {
        let tmp: &mut [u8; 4] = out.as_mut();
        *tmp = self.finish().to_be_bytes();
    }
}

impl HashMarker for XxHash32 {}

// ----------

impl Update for XxHash64 {
    fn update(&mut self, data: &[u8]) {
        self.write(data);
    }
}

impl OutputSizeUser for XxHash64 {
    type OutputSize = U8;
}

impl FixedOutput for XxHash64 {
    fn finalize_into(self, out: &mut Output<Self>) {
        let tmp: &mut [u8; 8] = out.as_mut();
        *tmp = self.finish().to_be_bytes();
    }
}

impl HashMarker for XxHash64 {}

// ----------

impl Update for xxh3::Hash64 {
    fn update(&mut self, data: &[u8]) {
        self.write(data);
    }
}

impl OutputSizeUser for xxh3::Hash64 {
    type OutputSize = U8;
}

impl FixedOutput for xxh3::Hash64 {
    fn finalize_into(self, out: &mut Output<Self>) {
        let tmp: &mut [u8; 8] = out.as_mut();
        *tmp = self.finish().to_be_bytes();
    }
}

impl HashMarker for xxh3::Hash64 {}

// ----------

impl Update for xxh3::Hash128 {
    fn update(&mut self, data: &[u8]) {
        self.write(data);
    }
}

impl OutputSizeUser for xxh3::Hash128 {
    type OutputSize = U16;
}

impl FixedOutput for xxh3::Hash128 {
    fn finalize_into(self, out: &mut Output<Self>) {
        let tmp: &mut [u8; 16] = out.as_mut();
        *tmp = xxh3::HasherExt::finish_ext(&self).to_be_bytes();
    }
}

impl HashMarker for xxh3::Hash128 {}
