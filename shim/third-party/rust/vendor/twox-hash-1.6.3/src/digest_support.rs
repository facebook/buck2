use core::hash::Hasher;

use digest::{
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

    fn input<B: AsRef<[u8]>>(&mut self, data: B) {
        self.write(data.as_ref());
    }

    fn chain<B: AsRef<[u8]>>(mut self, data: B) -> Self
    where
        Self: Sized,
    {
        self.input(data);
        self
    }

    fn result(self) -> GenericArray<u8, Self::OutputSize> {
        self.finish().to_be_bytes().into()
    }

    fn result_reset(&mut self) -> GenericArray<u8, Self::OutputSize> {
        let result = self.result();
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
        Self::new().chain(data).result()
    }
}

impl Digest for XxHash64 {
    type OutputSize = U8;

    fn new() -> Self {
        Self::default()
    }

    fn input<B: AsRef<[u8]>>(&mut self, data: B) {
        self.write(data.as_ref());
    }

    fn chain<B: AsRef<[u8]>>(mut self, data: B) -> Self
    where
        Self: Sized,
    {
        self.input(data);
        self
    }

    fn result(self) -> GenericArray<u8, Self::OutputSize> {
        self.finish().to_be_bytes().into()
    }

    fn result_reset(&mut self) -> GenericArray<u8, Self::OutputSize> {
        let result = self.result();
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
        Self::new().chain(data).result()
    }
}

impl Digest for xxh3::Hash64 {
    type OutputSize = U8;

    fn new() -> Self {
        Self::default()
    }

    fn input<B: AsRef<[u8]>>(&mut self, data: B) {
        self.write(data.as_ref());
    }

    fn chain<B: AsRef<[u8]>>(mut self, data: B) -> Self
    where
        Self: Sized,
    {
        self.input(data);
        self
    }

    fn result(self) -> GenericArray<u8, Self::OutputSize> {
        self.finish().to_be_bytes().into()
    }

    fn result_reset(&mut self) -> GenericArray<u8, Self::OutputSize> {
        let result = self.clone().result();
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
        Self::new().chain(data).result()
    }
}

impl Digest for xxh3::Hash128 {
    type OutputSize = U16;

    fn new() -> Self {
        Self::default()
    }

    fn input<B: AsRef<[u8]>>(&mut self, data: B) {
        self.write(data.as_ref());
    }

    fn chain<B: AsRef<[u8]>>(mut self, data: B) -> Self
    where
        Self: Sized,
    {
        self.input(data);
        self
    }

    fn result(self) -> GenericArray<u8, Self::OutputSize> {
        xxh3::HasherExt::finish_ext(&self).to_be_bytes().into()
    }

    fn result_reset(&mut self) -> GenericArray<u8, Self::OutputSize> {
        let result = self.clone().result();
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
        Self::new().chain(data).result()
    }
}
