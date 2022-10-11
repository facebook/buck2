pub mod sixty_four {
    use crate::XxHash64;
    use core::hash::BuildHasher;
    use rand::{self, Rng};

    #[derive(Clone)]
    /// Constructs a randomized seed and reuses it for multiple hasher instances.
    pub struct RandomXxHashBuilder64(u64);

    impl RandomXxHashBuilder64 {
        fn new() -> RandomXxHashBuilder64 {
            RandomXxHashBuilder64(rand::thread_rng().gen())
        }
    }

    impl Default for RandomXxHashBuilder64 {
        fn default() -> RandomXxHashBuilder64 {
            RandomXxHashBuilder64::new()
        }
    }

    impl BuildHasher for RandomXxHashBuilder64 {
        type Hasher = XxHash64;

        fn build_hasher(&self) -> XxHash64 {
            XxHash64::with_seed(self.0)
        }
    }
}

pub mod thirty_two {
    use crate::XxHash32;
    use core::hash::BuildHasher;
    use rand::{self, Rng};

    #[derive(Clone)]
    /// Constructs a randomized seed and reuses it for multiple hasher instances. See the usage warning on `XxHash32`.
    pub struct RandomXxHashBuilder32(u32);

    impl RandomXxHashBuilder32 {
        fn new() -> RandomXxHashBuilder32 {
            RandomXxHashBuilder32(rand::thread_rng().gen())
        }
    }

    impl Default for RandomXxHashBuilder32 {
        fn default() -> RandomXxHashBuilder32 {
            RandomXxHashBuilder32::new()
        }
    }

    impl BuildHasher for RandomXxHashBuilder32 {
        type Hasher = XxHash32;

        fn build_hasher(&self) -> XxHash32 {
            XxHash32::with_seed(self.0)
        }
    }
}

pub mod xxh3 {
    use crate::xxh3::{Hash128, Hash64};
    use core::hash::BuildHasher;
    use rand::{self, Rng};

    #[derive(Clone)]
    /// Constructs a randomized seed and reuses it for multiple hasher instances.
    pub struct RandomHashBuilder64(u64);

    impl RandomHashBuilder64 {
        fn new() -> RandomHashBuilder64 {
            RandomHashBuilder64(rand::thread_rng().gen())
        }
    }

    impl Default for RandomHashBuilder64 {
        fn default() -> RandomHashBuilder64 {
            RandomHashBuilder64::new()
        }
    }

    impl BuildHasher for RandomHashBuilder64 {
        type Hasher = Hash64;

        fn build_hasher(&self) -> Hash64 {
            Hash64::with_seed(self.0)
        }
    }

    #[derive(Clone)]
    /// Constructs a randomized seed and reuses it for multiple hasher instances.
    pub struct RandomHashBuilder128(u64);

    impl RandomHashBuilder128 {
        fn new() -> RandomHashBuilder128 {
            RandomHashBuilder128(rand::thread_rng().gen())
        }
    }

    impl Default for RandomHashBuilder128 {
        fn default() -> RandomHashBuilder128 {
            RandomHashBuilder128::new()
        }
    }

    impl BuildHasher for RandomHashBuilder128 {
        type Hasher = Hash128;

        fn build_hasher(&self) -> Hash128 {
            Hash128::with_seed(self.0)
        }
    }
}
