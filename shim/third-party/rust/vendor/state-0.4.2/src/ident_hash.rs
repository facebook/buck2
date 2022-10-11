use std::hash::Hasher;

// This is a _super_ stupid hash. It just uses its input as the hash value. This
// hash is meant to be used _only_ for "prehashed" values. In particular, we use
// this so that hashing a TypeId is essentially a noop. This is because TypeIds
// are already unique u64s.
pub struct IdentHash(u64);

impl Hasher for IdentHash {
    fn finish(&self) -> u64 {
        self.0
    }

    fn write(&mut self, bytes: &[u8]) {
        for byte in bytes {
            self.write_u8(*byte);
        }
    }

    fn write_u8(&mut self, i: u8) {
        self.0 = (self.0 << 8) | (i as u64);
    }

    fn write_u64(&mut self, i: u64) {
        self.0 = i;
    }
}

impl Default for IdentHash {
    fn default() -> IdentHash {
        IdentHash(0)
    }
}
