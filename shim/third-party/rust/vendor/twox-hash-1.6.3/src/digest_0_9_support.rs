use core::hash::Hasher;

use digest_0_9::{
    generic_array::{
        typenum::consts::{U16, U4, U8},
        GenericArray,
    },
    Digest,
};

use crate::{xxh3, XxHash32, XxHash64};

impl Digest for XxHash32 {
    type OutputSize = U4;

    fn new() -> Self {
        Self::default()
    }

    fn update(&mut self, data: impl AsRef<[u8]>) {
        self.write(data.as_ref());
    }

    fn chain(mut self, data: impl AsRef<[u8]>) -> Self
    where
        Self: Sized,
    {
        self.update(data);
        self
    }

    fn finalize(self) -> GenericArray<u8, Self::OutputSize> {
        self.finish().to_be_bytes().into()
    }

    fn finalize_reset(&mut self) -> GenericArray<u8, Self::OutputSize> {
        let result = self.finalize();
        self.reset();
        result
    }

    fn reset(&mut self) {
        *self = Self::default();
    }

    fn output_size() -> usize {
        4
    }

    fn digest(data: &[u8]) -> GenericArray<u8, Self::OutputSize> {
        Self::new().chain(data).finalize()
    }
}

impl Digest for XxHash64 {
    type OutputSize = U8;

    fn new() -> Self {
        Self::default()
    }

    fn update(&mut self, data: impl AsRef<[u8]>) {
        self.write(data.as_ref());
    }

    fn chain(mut self, data: impl AsRef<[u8]>) -> Self
    where
        Self: Sized,
    {
        self.update(data);
        self
    }

    fn finalize(self) -> GenericArray<u8, Self::OutputSize> {
        self.finish().to_be_bytes().into()
    }

    fn finalize_reset(&mut self) -> GenericArray<u8, Self::OutputSize> {
        let result = self.finalize();
        self.reset();
        result
    }

    fn reset(&mut self) {
        *self = Self::default();
    }

    fn output_size() -> usize {
        8
    }

    fn digest(data: &[u8]) -> GenericArray<u8, Self::OutputSize> {
        Self::new().chain(data).finalize()
    }
}

impl Digest for xxh3::Hash64 {
    type OutputSize = U8;

    fn new() -> Self {
        Self::default()
    }

    fn update(&mut self, data: impl AsRef<[u8]>) {
        self.write(data.as_ref());
    }

    fn chain(mut self, data: impl AsRef<[u8]>) -> Self
    where
        Self: Sized,
    {
        self.update(data);
        self
    }

    fn finalize(self) -> GenericArray<u8, Self::OutputSize> {
        self.finish().to_be_bytes().into()
    }

    fn finalize_reset(&mut self) -> GenericArray<u8, Self::OutputSize> {
        let result = self.clone().finalize();
        self.reset();
        result
    }

    fn reset(&mut self) {
        *self = Self::default();
    }

    fn output_size() -> usize {
        8
    }

    fn digest(data: &[u8]) -> GenericArray<u8, Self::OutputSize> {
        Self::new().chain(data).finalize()
    }
}

impl Digest for xxh3::Hash128 {
    type OutputSize = U16;

    fn new() -> Self {
        Self::default()
    }

    fn update(&mut self, data: impl AsRef<[u8]>) {
        self.write(data.as_ref());
    }

    fn chain(mut self, data: impl AsRef<[u8]>) -> Self
    where
        Self: Sized,
    {
        self.update(data);
        self
    }

    fn finalize(self) -> GenericArray<u8, Self::OutputSize> {
        xxh3::HasherExt::finish_ext(&self).to_be_bytes().into()
    }

    fn finalize_reset(&mut self) -> GenericArray<u8, Self::OutputSize> {
        let result = self.clone().finalize();
        self.reset();
        result
    }

    fn reset(&mut self) {
        *self = Self::default();
    }

    fn output_size() -> usize {
        8
    }

    fn digest(data: &[u8]) -> GenericArray<u8, Self::OutputSize> {
        Self::new().chain(data).finalize()
    }
}
