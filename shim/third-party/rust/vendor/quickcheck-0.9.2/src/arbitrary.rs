use std::char;
use std::collections::{
    BTreeMap, BTreeSet, BinaryHeap, HashMap, HashSet, LinkedList, VecDeque,
};
use std::env;
use std::ffi::OsString;
use std::hash::{BuildHasher, Hash};
use std::iter::{empty, once};
use std::net::{
    IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr, SocketAddrV4, SocketAddrV6,
};
use std::num::Wrapping;
use std::num::{
    NonZeroU128, NonZeroU16, NonZeroU32, NonZeroU64, NonZeroU8, NonZeroUsize,
};
use std::ops::{
    Bound, Range, RangeFrom, RangeFull, RangeInclusive, RangeTo,
    RangeToInclusive,
};
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use rand::seq::SliceRandom;
use rand::{self, Rng, RngCore};

/// `Gen` wraps a `rand::RngCore` with parameters to control the distribution of
/// random values.
///
/// A value with type satisfying the `Gen` trait can be constructed with the
/// `gen` function in this crate.
pub trait Gen: RngCore {
    fn size(&self) -> usize;
}

/// StdGen is the default implementation of `Gen`.
///
/// Values of type `StdGen` can be created with the `gen` function in this
/// crate.
pub struct StdGen<R> {
    rng: R,
    size: usize,
}

impl<R: RngCore> StdGen<R> {
    /// Returns a `StdGen` with the given configuration using any random number
    /// generator.
    ///
    /// The `size` parameter controls the size of random values generated. For
    /// example, it specifies the maximum length of a randomly generated vector
    /// and also will specify the maximum magnitude of a randomly generated
    /// number.
    pub fn new(rng: R, size: usize) -> StdGen<R> {
        StdGen { rng: rng, size: size }
    }
}

impl<R: RngCore> RngCore for StdGen<R> {
    fn next_u32(&mut self) -> u32 {
        self.rng.next_u32()
    }

    // some RNGs implement these more efficiently than the default, so
    // we might as well defer to them.
    fn next_u64(&mut self) -> u64 {
        self.rng.next_u64()
    }
    fn fill_bytes(&mut self, dest: &mut [u8]) {
        self.rng.fill_bytes(dest)
    }
    fn try_fill_bytes(&mut self, dest: &mut [u8]) -> Result<(), rand::Error> {
        self.rng.try_fill_bytes(dest)
    }
}

impl<R: RngCore> Gen for StdGen<R> {
    fn size(&self) -> usize {
        self.size
    }
}

/// StdThreadGen is an RNG in thread-local memory.
///
/// This is the default RNG used by quickcheck.
pub struct StdThreadGen(StdGen<rand::rngs::ThreadRng>);

impl StdThreadGen {
    /// Returns a new thread-local RNG.
    ///
    /// The `size` parameter controls the size of random values generated. For
    /// example, it specifies the maximum length of a randomly generated vector
    /// and also will specify the maximum magnitude of a randomly generated
    /// number.
    pub fn new(size: usize) -> StdThreadGen {
        StdThreadGen(StdGen { rng: rand::thread_rng(), size: size })
    }
}

impl RngCore for StdThreadGen {
    fn next_u32(&mut self) -> u32 {
        self.0.next_u32()
    }

    // some RNGs implement these more efficiently than the default, so
    // we might as well defer to them.
    fn next_u64(&mut self) -> u64 {
        self.0.next_u64()
    }
    fn fill_bytes(&mut self, dest: &mut [u8]) {
        self.0.fill_bytes(dest)
    }
    fn try_fill_bytes(&mut self, dest: &mut [u8]) -> Result<(), rand::Error> {
        self.0.try_fill_bytes(dest)
    }
}

impl Gen for StdThreadGen {
    fn size(&self) -> usize {
        self.0.size
    }
}

/// Creates a shrinker with zero elements.
pub fn empty_shrinker<A: 'static>() -> Box<dyn Iterator<Item = A>> {
    Box::new(empty())
}

/// Creates a shrinker with a single element.
pub fn single_shrinker<A: 'static>(value: A) -> Box<dyn Iterator<Item = A>> {
    Box::new(once(value))
}

/// `Arbitrary` describes types whose values can be randomly generated and
/// shrunk.
///
/// Aside from shrinking, `Arbitrary` is different from the `std::Rand` trait
/// in that it uses a `Gen` to control the distribution of random values.
///
/// As of now, all types that implement `Arbitrary` must also implement
/// `Clone`. (I'm not sure if this is a permanent restriction.)
///
/// They must also be sendable and static since every test is run in its own
/// thread using `thread::Builder::spawn`, which requires the `Send + 'static`
/// bounds.
pub trait Arbitrary: Clone + Send + 'static {
    fn arbitrary<G: Gen>(g: &mut G) -> Self;

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        empty_shrinker()
    }
}

impl Arbitrary for () {
    fn arbitrary<G: Gen>(_: &mut G) -> () {
        ()
    }
}

impl Arbitrary for bool {
    fn arbitrary<G: Gen>(g: &mut G) -> bool {
        g.gen()
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = bool>> {
        if *self {
            single_shrinker(false)
        } else {
            empty_shrinker()
        }
    }
}

impl<A: Arbitrary> Arbitrary for Option<A> {
    fn arbitrary<G: Gen>(g: &mut G) -> Option<A> {
        if g.gen() {
            None
        } else {
            Some(Arbitrary::arbitrary(g))
        }
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Option<A>>> {
        match *self {
            None => empty_shrinker(),
            Some(ref x) => {
                let chain = single_shrinker(None).chain(x.shrink().map(Some));
                Box::new(chain)
            }
        }
    }
}

impl<A: Arbitrary, B: Arbitrary> Arbitrary for Result<A, B> {
    fn arbitrary<G: Gen>(g: &mut G) -> Result<A, B> {
        if g.gen() {
            Ok(Arbitrary::arbitrary(g))
        } else {
            Err(Arbitrary::arbitrary(g))
        }
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Result<A, B>>> {
        match *self {
            Ok(ref x) => {
                let xs = x.shrink();
                let tagged = xs.map(Ok);
                Box::new(tagged)
            }
            Err(ref x) => {
                let xs = x.shrink();
                let tagged = xs.map(Err);
                Box::new(tagged)
            }
        }
    }
}

macro_rules! impl_arb_for_single_tuple {
    ($(($type_param:ident, $tuple_index:tt),)*) => {
        impl<$($type_param),*> Arbitrary for ($($type_param,)*)
            where $($type_param: Arbitrary,)*
        {
            fn arbitrary<GEN: Gen>(g: &mut GEN) -> ($($type_param,)*) {
                (
                    $(
                        $type_param::arbitrary(g),
                    )*
                )
            }

            fn shrink(&self) -> Box<dyn Iterator<Item=($($type_param,)*)>> {
                let iter = ::std::iter::empty();
                $(
                    let cloned = self.clone();
                    let iter = iter.chain(self.$tuple_index.shrink().map(move |shr_value| {
                        let mut result = cloned.clone();
                        result.$tuple_index = shr_value;
                        result
                    }));
                )*
                Box::new(iter)
            }
        }
    };
}

macro_rules! impl_arb_for_tuples {
    (@internal [$($acc:tt,)*]) => { };
    (@internal [$($acc:tt,)*] ($type_param:ident, $tuple_index:tt), $($rest:tt,)*) => {
        impl_arb_for_single_tuple!($($acc,)* ($type_param, $tuple_index),);
        impl_arb_for_tuples!(@internal [$($acc,)* ($type_param, $tuple_index),] $($rest,)*);
    };
    ($(($type_param:ident, $tuple_index:tt),)*) => {
        impl_arb_for_tuples!(@internal [] $(($type_param, $tuple_index),)*);
    };
}

impl_arb_for_tuples! {
    (A, 0),
    (B, 1),
    (C, 2),
    (D, 3),
    (E, 4),
    (F, 5),
    (G, 6),
    (H, 7),
}

impl<A: Arbitrary> Arbitrary for Vec<A> {
    fn arbitrary<G: Gen>(g: &mut G) -> Vec<A> {
        let size = {
            let s = g.size();
            g.gen_range(0, s)
        };
        (0..size).map(|_| Arbitrary::arbitrary(g)).collect()
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Vec<A>>> {
        VecShrinker::new(self.clone())
    }
}

///Iterator which returns successive attempts to shrink the vector `seed`
struct VecShrinker<A> {
    seed: Vec<A>,
    /// How much which is removed when trying with smaller vectors
    size: usize,
    /// The end of the removed elements
    offset: usize,
    /// The shrinker for the element at `offset` once shrinking of individual
    /// elements are attempted
    element_shrinker: Box<dyn Iterator<Item = A>>,
}

impl<A: Arbitrary> VecShrinker<A> {
    fn new(seed: Vec<A>) -> Box<dyn Iterator<Item = Vec<A>>> {
        let es = match seed.get(0) {
            Some(e) => e.shrink(),
            None => return empty_shrinker(),
        };
        let size = seed.len();
        Box::new(VecShrinker {
            seed: seed,
            size: size,
            offset: size,
            element_shrinker: es,
        })
    }

    /// Returns the next shrunk element if any, `offset` points to the index
    /// after the returned element after the function returns
    fn next_element(&mut self) -> Option<A> {
        loop {
            match self.element_shrinker.next() {
                Some(e) => return Some(e),
                None => match self.seed.get(self.offset) {
                    Some(e) => {
                        self.element_shrinker = e.shrink();
                        self.offset += 1;
                    }
                    None => return None,
                },
            }
        }
    }
}

impl<A> Iterator for VecShrinker<A>
where
    A: Arbitrary,
{
    type Item = Vec<A>;
    fn next(&mut self) -> Option<Vec<A>> {
        // Try with an empty vector first
        if self.size == self.seed.len() {
            self.size /= 2;
            self.offset = self.size;
            return Some(vec![]);
        }
        if self.size != 0 {
            // Generate a smaller vector by removing the elements between
            // (offset - size) and offset
            let xs1 = self.seed[..(self.offset - self.size)]
                .iter()
                .chain(&self.seed[self.offset..])
                .cloned()
                .collect();
            self.offset += self.size;
            // Try to reduce the amount removed from the vector once all
            // previous sizes tried
            if self.offset > self.seed.len() {
                self.size /= 2;
                self.offset = self.size;
            }
            Some(xs1)
        } else {
            // A smaller vector did not work so try to shrink each element of
            // the vector instead Reuse `offset` as the index determining which
            // element to shrink

            // The first element shrinker is already created so skip the first
            // offset (self.offset == 0 only on first entry to this part of the
            // iterator)
            if self.offset == 0 {
                self.offset = 1
            }

            match self.next_element() {
                Some(e) => Some(
                    self.seed[..self.offset - 1]
                        .iter()
                        .cloned()
                        .chain(Some(e).into_iter())
                        .chain(self.seed[self.offset..].iter().cloned())
                        .collect(),
                ),
                None => None,
            }
        }
    }
}

impl<K: Arbitrary + Ord, V: Arbitrary> Arbitrary for BTreeMap<K, V> {
    fn arbitrary<G: Gen>(g: &mut G) -> BTreeMap<K, V> {
        let vec: Vec<(K, V)> = Arbitrary::arbitrary(g);
        vec.into_iter().collect()
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = BTreeMap<K, V>>> {
        let vec: Vec<(K, V)> = self.clone().into_iter().collect();
        Box::new(
            vec.shrink().map(|v| v.into_iter().collect::<BTreeMap<K, V>>()),
        )
    }
}

impl<
        K: Arbitrary + Eq + Hash,
        V: Arbitrary,
        S: BuildHasher + Default + Clone + Send + 'static,
    > Arbitrary for HashMap<K, V, S>
{
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        let vec: Vec<(K, V)> = Arbitrary::arbitrary(g);
        vec.into_iter().collect()
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        let vec: Vec<(K, V)> = self.clone().into_iter().collect();
        Box::new(vec.shrink().map(|v| v.into_iter().collect::<Self>()))
    }
}

impl<T: Arbitrary + Ord> Arbitrary for BTreeSet<T> {
    fn arbitrary<G: Gen>(g: &mut G) -> BTreeSet<T> {
        let vec: Vec<T> = Arbitrary::arbitrary(g);
        vec.into_iter().collect()
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = BTreeSet<T>>> {
        let vec: Vec<T> = self.clone().into_iter().collect();
        Box::new(vec.shrink().map(|v| v.into_iter().collect::<BTreeSet<T>>()))
    }
}

impl<T: Arbitrary + Ord> Arbitrary for BinaryHeap<T> {
    fn arbitrary<G: Gen>(g: &mut G) -> BinaryHeap<T> {
        let vec: Vec<T> = Arbitrary::arbitrary(g);
        vec.into_iter().collect()
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = BinaryHeap<T>>> {
        let vec: Vec<T> = self.clone().into_iter().collect();
        Box::new(
            vec.shrink().map(|v| v.into_iter().collect::<BinaryHeap<T>>()),
        )
    }
}

impl<
        T: Arbitrary + Eq + Hash,
        S: BuildHasher + Default + Clone + Send + 'static,
    > Arbitrary for HashSet<T, S>
{
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        let vec: Vec<T> = Arbitrary::arbitrary(g);
        vec.into_iter().collect()
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        let vec: Vec<T> = self.clone().into_iter().collect();
        Box::new(vec.shrink().map(|v| v.into_iter().collect::<Self>()))
    }
}

impl<T: Arbitrary> Arbitrary for LinkedList<T> {
    fn arbitrary<G: Gen>(g: &mut G) -> LinkedList<T> {
        let vec: Vec<T> = Arbitrary::arbitrary(g);
        vec.into_iter().collect()
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = LinkedList<T>>> {
        let vec: Vec<T> = self.clone().into_iter().collect();
        Box::new(
            vec.shrink().map(|v| v.into_iter().collect::<LinkedList<T>>()),
        )
    }
}

impl<T: Arbitrary> Arbitrary for VecDeque<T> {
    fn arbitrary<G: Gen>(g: &mut G) -> VecDeque<T> {
        let vec: Vec<T> = Arbitrary::arbitrary(g);
        vec.into_iter().collect()
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = VecDeque<T>>> {
        let vec: Vec<T> = self.clone().into_iter().collect();
        Box::new(vec.shrink().map(|v| v.into_iter().collect::<VecDeque<T>>()))
    }
}

impl Arbitrary for IpAddr {
    fn arbitrary<G: Gen>(g: &mut G) -> IpAddr {
        let ipv4: bool = g.gen();
        if ipv4 {
            IpAddr::V4(Arbitrary::arbitrary(g))
        } else {
            IpAddr::V6(Arbitrary::arbitrary(g))
        }
    }
}

impl Arbitrary for Ipv4Addr {
    fn arbitrary<G: Gen>(g: &mut G) -> Ipv4Addr {
        Ipv4Addr::new(g.gen(), g.gen(), g.gen(), g.gen())
    }
}

impl Arbitrary for Ipv6Addr {
    fn arbitrary<G: Gen>(g: &mut G) -> Ipv6Addr {
        Ipv6Addr::new(
            g.gen(),
            g.gen(),
            g.gen(),
            g.gen(),
            g.gen(),
            g.gen(),
            g.gen(),
            g.gen(),
        )
    }
}

impl Arbitrary for SocketAddr {
    fn arbitrary<G: Gen>(g: &mut G) -> SocketAddr {
        SocketAddr::new(Arbitrary::arbitrary(g), g.gen())
    }
}

impl Arbitrary for SocketAddrV4 {
    fn arbitrary<G: Gen>(g: &mut G) -> SocketAddrV4 {
        SocketAddrV4::new(Arbitrary::arbitrary(g), g.gen())
    }
}

impl Arbitrary for SocketAddrV6 {
    fn arbitrary<G: Gen>(g: &mut G) -> SocketAddrV6 {
        SocketAddrV6::new(Arbitrary::arbitrary(g), g.gen(), g.gen(), g.gen())
    }
}

impl Arbitrary for PathBuf {
    fn arbitrary<G: Gen>(g: &mut G) -> PathBuf {
        // use some real directories as guesses, so we may end up with
        // actual working directories in case that is relevant.
        let here =
            env::current_dir().unwrap_or(PathBuf::from("/test/directory"));
        let temp = env::temp_dir();
        #[allow(deprecated)]
        let home = env::home_dir().unwrap_or(PathBuf::from("/home/user"));
        let choices = &[
            here,
            temp,
            home,
            PathBuf::from("."),
            PathBuf::from(".."),
            PathBuf::from("../../.."),
            PathBuf::new(),
        ];
        let mut p = choices.choose(g).unwrap().clone();
        p.extend(Vec::<OsString>::arbitrary(g).iter());
        p
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = PathBuf>> {
        let mut shrunk = vec![];
        let mut popped = self.clone();
        if popped.pop() {
            shrunk.push(popped);
        }

        // Iterating over a Path performs a small amount of normalization.
        let normalized = self.iter().collect::<PathBuf>();
        if normalized.as_os_str() != self.as_os_str() {
            shrunk.push(normalized);
        }

        // Add the canonicalized variant only if canonicalizing the path
        // actually does something, making it (hopefully) smaller. Also, ignore
        // canonicalization if canonicalization errors.
        if let Ok(canonicalized) = self.canonicalize() {
            if canonicalized.as_os_str() != self.as_os_str() {
                shrunk.push(canonicalized);
            }
        }

        Box::new(shrunk.into_iter())
    }
}

impl Arbitrary for OsString {
    fn arbitrary<G: Gen>(g: &mut G) -> OsString {
        OsString::from(String::arbitrary(g))
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = OsString>> {
        let mystring: String = self.clone().into_string().unwrap();
        Box::new(mystring.shrink().map(|s| OsString::from(s)))
    }
}

impl Arbitrary for String {
    fn arbitrary<G: Gen>(g: &mut G) -> String {
        let size = {
            let s = g.size();
            g.gen_range(0, s)
        };
        let mut s = String::with_capacity(size);
        for _ in 0..size {
            s.push(char::arbitrary(g));
        }
        s
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = String>> {
        // Shrink a string by shrinking a vector of its characters.
        let chars: Vec<char> = self.chars().collect();
        Box::new(chars.shrink().map(|x| x.into_iter().collect::<String>()))
    }
}

impl Arbitrary for char {
    fn arbitrary<G: Gen>(g: &mut G) -> char {
        let mode = g.gen_range(0, 100);
        match mode {
            0..=49 => {
                // ASCII + some control characters
                g.gen_range(0, 0xB0) as u8 as char
            }
            50..=59 => {
                // Unicode BMP characters
                loop {
                    if let Some(x) = char::from_u32(g.gen_range(0, 0x10000)) {
                        return x;
                    }
                    // ignore surrogate pairs
                }
            }
            60..=84 => {
                // Characters often used in programming languages
                [
                    ' ', ' ', ' ', '\t', '\n', '~', '`', '!', '@', '#', '$',
                    '%', '^', '&', '*', '(', ')', '_', '-', '=', '+', '[',
                    ']', '{', '}', ':', ';', '\'', '"', '\\', '|', ',', '<',
                    '>', '.', '/', '?', '0', '1', '2', '3', '4', '5', '6',
                    '7', '8', '9',
                ]
                .choose(g)
                .unwrap()
                .to_owned()
            }
            85..=89 => {
                // Tricky Unicode, part 1
                [
                    '\u{0149}', // a deprecated character
                    '\u{fff0}', // some of "Other, format" category:
                    '\u{fff1}',
                    '\u{fff2}',
                    '\u{fff3}',
                    '\u{fff4}',
                    '\u{fff5}',
                    '\u{fff6}',
                    '\u{fff7}',
                    '\u{fff8}',
                    '\u{fff9}',
                    '\u{fffA}',
                    '\u{fffB}',
                    '\u{fffC}',
                    '\u{fffD}',
                    '\u{fffE}',
                    '\u{fffF}',
                    '\u{0600}',
                    '\u{0601}',
                    '\u{0602}',
                    '\u{0603}',
                    '\u{0604}',
                    '\u{0605}',
                    '\u{061C}',
                    '\u{06DD}',
                    '\u{070F}',
                    '\u{180E}',
                    '\u{110BD}',
                    '\u{1D173}',
                    '\u{e0001}', // tag
                    '\u{e0020}', //  tag space
                    '\u{e000}',
                    '\u{e001}',
                    '\u{ef8ff}', // private use
                    '\u{f0000}',
                    '\u{ffffd}',
                    '\u{ffffe}',
                    '\u{fffff}',
                    '\u{100000}',
                    '\u{10FFFD}',
                    '\u{10FFFE}',
                    '\u{10FFFF}',
                    // "Other, surrogate" characters are so that very special
                    // that they are not even allowed in safe Rust,
                    //so omitted here
                    '\u{3000}', // ideographic space
                    '\u{1680}',
                    // other space characters are already covered by two next
                    // branches
                ]
                .choose(g)
                .unwrap()
                .to_owned()
            }
            90..=94 => {
                // Tricky unicode, part 2
                char::from_u32(g.gen_range(0x2000, 0x2070)).unwrap()
            }
            95..=99 => {
                // Completely arbitrary characters
                g.gen()
            }
            _ => unreachable!(),
        }
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = char>> {
        Box::new((*self as u32).shrink().filter_map(char::from_u32))
    }
}

macro_rules! unsigned_shrinker {
    ($ty:ty) => {
        mod shrinker {
            pub struct UnsignedShrinker {
                x: $ty,
                i: $ty,
            }

            impl UnsignedShrinker {
                pub fn new(x: $ty) -> Box<dyn Iterator<Item = $ty>> {
                    if x == 0 {
                        super::empty_shrinker()
                    } else {
                        Box::new(
                            vec![0]
                                .into_iter()
                                .chain(UnsignedShrinker { x: x, i: x / 2 }),
                        )
                    }
                }
            }

            impl Iterator for UnsignedShrinker {
                type Item = $ty;
                fn next(&mut self) -> Option<$ty> {
                    if self.x - self.i < self.x {
                        let result = Some(self.x - self.i);
                        self.i = self.i / 2;
                        result
                    } else {
                        None
                    }
                }
            }
        }
    };
}

macro_rules! unsigned_arbitrary {
    ($($ty:tt),*) => {
        $(
            impl Arbitrary for $ty {
                fn arbitrary<G: Gen>(g: &mut G) -> $ty {
                    #![allow(trivial_numeric_casts)]
                    let s = g.size() as $ty;
                    use std::cmp::{min, max};
                    g.gen_range(0, max(1, min(s, $ty::max_value())))
                }
                fn shrink(&self) -> Box<dyn Iterator<Item=$ty>> {
                    unsigned_shrinker!($ty);
                    shrinker::UnsignedShrinker::new(*self)
                }
            }
        )*
    }
}

unsigned_arbitrary! {
    usize, u8, u16, u32, u64
}

unsigned_arbitrary! {
    u128
}

macro_rules! signed_shrinker {
    ($ty:ty) => {
        mod shrinker {
            pub struct SignedShrinker {
                x: $ty,
                i: $ty,
            }

            impl SignedShrinker {
                pub fn new(x: $ty) -> Box<dyn Iterator<Item = $ty>> {
                    if x == 0 {
                        super::empty_shrinker()
                    } else {
                        let shrinker = SignedShrinker { x: x, i: x / 2 };
                        let mut items = vec![0];
                        if shrinker.i < 0 {
                            items.push(shrinker.x.abs());
                        }
                        Box::new(items.into_iter().chain(shrinker))
                    }
                }
            }

            impl Iterator for SignedShrinker {
                type Item = $ty;
                fn next(&mut self) -> Option<$ty> {
                    if (self.x - self.i).abs() < self.x.abs() {
                        let result = Some(self.x - self.i);
                        self.i = self.i / 2;
                        result
                    } else {
                        None
                    }
                }
            }
        }
    };
}

macro_rules! signed_arbitrary {
    ($($ty:tt),*) => {
        $(
            impl Arbitrary for $ty {
                fn arbitrary<G: Gen>(g: &mut G) -> $ty {
                    use std::cmp::{min,max};
                    let upper = min(g.size(), $ty::max_value() as usize);
                    let lower = if upper > $ty::max_value() as usize {
                        $ty::min_value()
                    } else {
                        -(upper as $ty)
                    };
                    g.gen_range(lower, max(1, upper as $ty))
                }
                fn shrink(&self) -> Box<dyn Iterator<Item=$ty>> {
                    signed_shrinker!($ty);
                    shrinker::SignedShrinker::new(*self)
                }
            }
        )*
    }
}

signed_arbitrary! {
    isize, i8, i16, i32, i64
}

signed_arbitrary! {
    i128
}

impl Arbitrary for f32 {
    fn arbitrary<G: Gen>(g: &mut G) -> f32 {
        let s = g.size();
        g.gen_range(-(s as f32), s as f32)
    }
    fn shrink(&self) -> Box<dyn Iterator<Item = f32>> {
        signed_shrinker!(i32);
        let it = shrinker::SignedShrinker::new(*self as i32);
        Box::new(it.map(|x| x as f32))
    }
}

macro_rules! unsigned_non_zero_shrinker {
    ($ty:tt) => {
        mod shrinker {
            pub struct UnsignedNonZeroShrinker {
                x: $ty,
                i: $ty,
            }

            impl UnsignedNonZeroShrinker {
                pub fn new(x: $ty) -> Box<dyn Iterator<Item = $ty>> {
                    debug_assert!(x > 0);

                    if x == 1 {
                        super::empty_shrinker()
                    } else {
                        Box::new(
                            std::iter::once(1).chain(
                                UnsignedNonZeroShrinker { x: x, i: x / 2 },
                            ),
                        )
                    }
                }
            }

            impl Iterator for UnsignedNonZeroShrinker {
                type Item = $ty;

                fn next(&mut self) -> Option<$ty> {
                    if self.x - self.i < self.x {
                        let result = Some(self.x - self.i);
                        self.i = self.i / 2;
                        result
                    } else {
                        None
                    }
                }
            }
        }
    };
}

macro_rules! unsigned_non_zero_arbitrary {
    ($($ty:tt => $inner:tt),*) => {
        $(
            impl Arbitrary for $ty {
                fn arbitrary<G: Gen>(g: &mut G) -> $ty {
                    #![allow(trivial_numeric_casts)]
                    let s = g.size() as $inner;
                    use std::cmp::{min, max};
                    let non_zero = g.gen_range(1, max(2, min(s, $inner::max_value())));
                    debug_assert!(non_zero != 0);
                    $ty::new(non_zero).expect("non-zero value contsturction failed")
                }

                fn shrink(&self) -> Box<dyn Iterator<Item = $ty>> {
                    unsigned_non_zero_shrinker!($inner);
                    Box::new(shrinker::UnsignedNonZeroShrinker::new(self.get())
                        .map($ty::new)
                        .map(Option::unwrap))
                }
            }
        )*
    }
}

unsigned_non_zero_arbitrary! {
    NonZeroUsize => usize,
    NonZeroU8    => u8,
    NonZeroU16   => u16,
    NonZeroU32   => u32,
    NonZeroU64   => u64,
    NonZeroU128  => u128
}

impl Arbitrary for f64 {
    fn arbitrary<G: Gen>(g: &mut G) -> f64 {
        let s = g.size();
        g.gen_range(-(s as f64), s as f64)
    }
    fn shrink(&self) -> Box<dyn Iterator<Item = f64>> {
        signed_shrinker!(i64);
        let it = shrinker::SignedShrinker::new(*self as i64);
        Box::new(it.map(|x| x as f64))
    }
}

impl<T: Arbitrary> Arbitrary for Wrapping<T> {
    fn arbitrary<G: Gen>(g: &mut G) -> Wrapping<T> {
        Wrapping(T::arbitrary(g))
    }
    fn shrink(&self) -> Box<dyn Iterator<Item = Wrapping<T>>> {
        Box::new(self.0.shrink().map(|inner| Wrapping(inner)))
    }
}

impl<T: Arbitrary> Arbitrary for Bound<T> {
    fn arbitrary<G: Gen>(g: &mut G) -> Bound<T> {
        match g.gen_range(0, 3) {
            0 => Bound::Included(T::arbitrary(g)),
            1 => Bound::Excluded(T::arbitrary(g)),
            _ => Bound::Unbounded,
        }
    }
    fn shrink(&self) -> Box<dyn Iterator<Item = Bound<T>>> {
        match *self {
            Bound::Included(ref x) => {
                Box::new(x.shrink().map(Bound::Included))
            }
            Bound::Excluded(ref x) => {
                Box::new(x.shrink().map(Bound::Excluded))
            }
            Bound::Unbounded => empty_shrinker(),
        }
    }
}

impl<T: Arbitrary + Clone + PartialOrd> Arbitrary for Range<T> {
    fn arbitrary<G: Gen>(g: &mut G) -> Range<T> {
        Arbitrary::arbitrary(g)..Arbitrary::arbitrary(g)
    }
    fn shrink(&self) -> Box<dyn Iterator<Item = Range<T>>> {
        Box::new(
            (self.start.clone(), self.end.clone()).shrink().map(|(s, e)| s..e),
        )
    }
}

impl<T: Arbitrary + Clone + PartialOrd> Arbitrary for RangeInclusive<T> {
    fn arbitrary<G: Gen>(g: &mut G) -> RangeInclusive<T> {
        Arbitrary::arbitrary(g)..=Arbitrary::arbitrary(g)
    }
    fn shrink(&self) -> Box<dyn Iterator<Item = RangeInclusive<T>>> {
        Box::new(
            (self.start().clone(), self.end().clone())
                .shrink()
                .map(|(s, e)| s..=e),
        )
    }
}

impl<T: Arbitrary + Clone + PartialOrd> Arbitrary for RangeFrom<T> {
    fn arbitrary<G: Gen>(g: &mut G) -> RangeFrom<T> {
        Arbitrary::arbitrary(g)..
    }
    fn shrink(&self) -> Box<dyn Iterator<Item = RangeFrom<T>>> {
        Box::new(self.start.clone().shrink().map(|start| start..))
    }
}

impl<T: Arbitrary + Clone + PartialOrd> Arbitrary for RangeTo<T> {
    fn arbitrary<G: Gen>(g: &mut G) -> RangeTo<T> {
        ..Arbitrary::arbitrary(g)
    }
    fn shrink(&self) -> Box<dyn Iterator<Item = RangeTo<T>>> {
        Box::new(self.end.clone().shrink().map(|end| ..end))
    }
}

impl<T: Arbitrary + Clone + PartialOrd> Arbitrary for RangeToInclusive<T> {
    fn arbitrary<G: Gen>(g: &mut G) -> RangeToInclusive<T> {
        ..=Arbitrary::arbitrary(g)
    }
    fn shrink(&self) -> Box<dyn Iterator<Item = RangeToInclusive<T>>> {
        Box::new(self.end.clone().shrink().map(|end| ..=end))
    }
}

impl Arbitrary for RangeFull {
    fn arbitrary<G: Gen>(_: &mut G) -> RangeFull {
        ..
    }
}

impl Arbitrary for Duration {
    fn arbitrary<G: Gen>(gen: &mut G) -> Self {
        let seconds = u64::arbitrary(gen);
        let nanoseconds = u32::arbitrary(gen) % 1_000_000;
        Duration::new(seconds, nanoseconds)
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        Box::new(
            (self.as_secs(), self.subsec_nanos())
                .shrink()
                .map(|(secs, nanos)| Duration::new(secs, nanos % 1_000_000)),
        )
    }
}

impl<A: Arbitrary> Arbitrary for Box<A> {
    fn arbitrary<G: Gen>(g: &mut G) -> Box<A> {
        Box::new(A::arbitrary(g))
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Box<A>>> {
        Box::new((**self).shrink().map(Box::new))
    }
}

impl<A: Arbitrary + Sync> Arbitrary for Arc<A> {
    fn arbitrary<G: Gen>(g: &mut G) -> Arc<A> {
        Arc::new(A::arbitrary(g))
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Arc<A>>> {
        Box::new((**self).shrink().map(Arc::new))
    }
}

impl Arbitrary for SystemTime {
    fn arbitrary<G: Gen>(gen: &mut G) -> Self {
        let after_epoch = bool::arbitrary(gen);
        let duration = Duration::arbitrary(gen);
        if after_epoch {
            UNIX_EPOCH + duration
        } else {
            UNIX_EPOCH - duration
        }
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        let duration = match self.duration_since(UNIX_EPOCH) {
            Ok(duration) => duration,
            Err(e) => e.duration(),
        };
        Box::new(
            duration
                .shrink()
                .flat_map(|d| vec![UNIX_EPOCH + d, UNIX_EPOCH - d]),
        )
    }
}

#[cfg(test)]
mod test {
    use super::Arbitrary;
    use rand;
    use std::collections::{
        BTreeMap, BTreeSet, BinaryHeap, HashMap, HashSet, LinkedList, VecDeque,
    };
    use std::fmt::Debug;
    use std::hash::Hash;
    use std::num::Wrapping;
    use std::path::PathBuf;

    #[test]
    fn arby_unit() {
        assert_eq!(arby::<()>(), ());
    }

    #[test]
    fn arby_int() {
        rep(&mut || {
            let n: isize = arby();
            assert!(n >= -5 && n <= 5);
        });
    }

    #[test]
    fn arby_uint() {
        rep(&mut || {
            let n: usize = arby();
            assert!(n <= 5);
        });
    }

    fn arby<A: super::Arbitrary>() -> A {
        super::Arbitrary::arbitrary(&mut gen())
    }

    fn gen() -> super::StdGen<rand::rngs::ThreadRng> {
        super::StdGen::new(rand::thread_rng(), 5)
    }

    fn rep<F>(f: &mut F)
    where
        F: FnMut() -> (),
    {
        for _ in 0..100 {
            f()
        }
    }

    // Shrink testing.
    #[test]
    fn unit() {
        eq((), vec![]);
    }

    #[test]
    fn bools() {
        eq(false, vec![]);
        eq(true, vec![false]);
    }

    #[test]
    fn options() {
        eq(None::<()>, vec![]);
        eq(Some(false), vec![None]);
        eq(Some(true), vec![None, Some(false)]);
    }

    #[test]
    fn results() {
        // Result<A, B> doesn't implement the Hash trait, so these tests
        // depends on the order of shrunk results. Ug.
        // TODO: Fix this.
        ordered_eq(Ok::<bool, ()>(true), vec![Ok(false)]);
        ordered_eq(Err::<(), bool>(true), vec![Err(false)]);
    }

    #[test]
    fn tuples() {
        eq((false, false), vec![]);
        eq((true, false), vec![(false, false)]);
        eq((true, true), vec![(false, true), (true, false)]);
    }

    #[test]
    fn triples() {
        eq((false, false, false), vec![]);
        eq((true, false, false), vec![(false, false, false)]);
        eq(
            (true, true, false),
            vec![(false, true, false), (true, false, false)],
        );
    }

    #[test]
    fn quads() {
        eq((false, false, false, false), vec![]);
        eq((true, false, false, false), vec![(false, false, false, false)]);
        eq(
            (true, true, false, false),
            vec![(false, true, false, false), (true, false, false, false)],
        );
    }

    #[test]
    fn ints() {
        // TODO: Test overflow?
        eq(5isize, vec![0, 3, 4]);
        eq(-5isize, vec![5, 0, -3, -4]);
        eq(0isize, vec![]);
    }

    #[test]
    fn ints8() {
        eq(5i8, vec![0, 3, 4]);
        eq(-5i8, vec![5, 0, -3, -4]);
        eq(0i8, vec![]);
    }

    #[test]
    fn ints16() {
        eq(5i16, vec![0, 3, 4]);
        eq(-5i16, vec![5, 0, -3, -4]);
        eq(0i16, vec![]);
    }

    #[test]
    fn ints32() {
        eq(5i32, vec![0, 3, 4]);
        eq(-5i32, vec![5, 0, -3, -4]);
        eq(0i32, vec![]);
    }

    #[test]
    fn ints64() {
        eq(5i64, vec![0, 3, 4]);
        eq(-5i64, vec![5, 0, -3, -4]);
        eq(0i64, vec![]);
    }

    #[test]
    fn ints128() {
        eq(5i128, vec![0, 3, 4]);
        eq(-5i128, vec![5, 0, -3, -4]);
        eq(0i128, vec![]);
    }

    #[test]
    fn uints() {
        eq(5usize, vec![0, 3, 4]);
        eq(0usize, vec![]);
    }

    #[test]
    fn uints8() {
        eq(5u8, vec![0, 3, 4]);
        eq(0u8, vec![]);
    }

    #[test]
    fn uints16() {
        eq(5u16, vec![0, 3, 4]);
        eq(0u16, vec![]);
    }

    #[test]
    fn uints32() {
        eq(5u32, vec![0, 3, 4]);
        eq(0u32, vec![]);
    }

    #[test]
    fn uints64() {
        eq(5u64, vec![0, 3, 4]);
        eq(0u64, vec![]);
    }

    #[test]
    fn uints128() {
        eq(5u128, vec![0, 3, 4]);
        eq(0u128, vec![]);
    }

    macro_rules! define_float_eq {
        ($ty:ty) => {
            fn eq(s: $ty, v: Vec<$ty>) {
                let shrunk: Vec<$ty> = s.shrink().collect();
                for n in v {
                    let found = shrunk.iter().any(|&i| i == n);
                    if !found {
                        panic!(format!(
                            "Element {:?} was not found \
                             in shrink results {:?}",
                            n, shrunk
                        ));
                    }
                }
            }
        };
    }

    #[test]
    fn floats32() {
        define_float_eq!(f32);

        eq(0.0, vec![]);
        eq(-0.0, vec![]);
        eq(1.0, vec![0.0]);
        eq(2.0, vec![0.0, 1.0]);
        eq(-2.0, vec![0.0, 2.0, -1.0]);
        eq(1.5, vec![0.0]);
    }

    #[test]
    fn floats64() {
        define_float_eq!(f64);

        eq(0.0, vec![]);
        eq(-0.0, vec![]);
        eq(1.0, vec![0.0]);
        eq(2.0, vec![0.0, 1.0]);
        eq(-2.0, vec![0.0, 2.0, -1.0]);
        eq(1.5, vec![0.0]);
    }

    #[test]
    fn wrapping_ints32() {
        eq(Wrapping(5i32), vec![Wrapping(0), Wrapping(3), Wrapping(4)]);
        eq(
            Wrapping(-5i32),
            vec![Wrapping(5), Wrapping(0), Wrapping(-3), Wrapping(-4)],
        );
        eq(Wrapping(0i32), vec![]);
    }

    #[test]
    fn vecs() {
        eq(
            {
                let it: Vec<isize> = vec![];
                it
            },
            vec![],
        );
        eq(
            {
                let it: Vec<Vec<isize>> = vec![vec![]];
                it
            },
            vec![vec![]],
        );
        eq(vec![1isize], vec![vec![], vec![0]]);
        eq(vec![11isize], vec![vec![], vec![0], vec![6], vec![9], vec![10]]);
        eq(
            vec![3isize, 5],
            vec![
                vec![],
                vec![5],
                vec![3],
                vec![0, 5],
                vec![2, 5],
                vec![3, 0],
                vec![3, 3],
                vec![3, 4],
            ],
        );
    }

    macro_rules! map_tests {
        ($name:ident, $ctor:expr) => {
            #[test]
            fn $name() {
                ordered_eq($ctor, vec![]);

                {
                    let mut map = $ctor;
                    map.insert(1usize, 1isize);

                    let shrinks = vec![
                        $ctor,
                        {
                            let mut m = $ctor;
                            m.insert(0, 1);
                            m
                        },
                        {
                            let mut m = $ctor;
                            m.insert(1, 0);
                            m
                        },
                    ];

                    ordered_eq(map, shrinks);
                }
            }
        };
    }

    map_tests!(btreemap, BTreeMap::<usize, isize>::new());
    map_tests!(hashmap, HashMap::<usize, isize>::new());

    macro_rules! list_tests {
        ($name:ident, $ctor:expr, $push:ident) => {
            #[test]
            fn $name() {
                ordered_eq($ctor, vec![]);

                {
                    let mut list = $ctor;
                    list.$push(2usize);

                    let shrinks = vec![
                        $ctor,
                        {
                            let mut m = $ctor;
                            m.$push(0);
                            m
                        },
                        {
                            let mut m = $ctor;
                            m.$push(1);
                            m
                        },
                    ];

                    ordered_eq(list, shrinks);
                }
            }
        };
    }

    list_tests!(btreesets, BTreeSet::<usize>::new(), insert);
    list_tests!(hashsets, HashSet::<usize>::new(), insert);
    list_tests!(linkedlists, LinkedList::<usize>::new(), push_back);
    list_tests!(vecdeques, VecDeque::<usize>::new(), push_back);

    #[test]
    fn binaryheaps() {
        ordered_eq(
            BinaryHeap::<usize>::new().into_iter().collect::<Vec<_>>(),
            vec![],
        );

        {
            let mut heap = BinaryHeap::<usize>::new();
            heap.push(2usize);

            let shrinks = vec![vec![], vec![0], vec![1]];

            ordered_eq(heap.into_iter().collect::<Vec<_>>(), shrinks);
        }
    }

    #[test]
    fn chars() {
        eq('\x00', vec![]);
    }

    // All this jazz is for testing set equality on the results of a shrinker.
    fn eq<A: Arbitrary + Eq + Debug + Hash>(s: A, v: Vec<A>) {
        let (left, right) = (shrunk(s), set(v));
        assert_eq!(left, right);
    }
    fn shrunk<A: Arbitrary + Eq + Hash>(s: A) -> HashSet<A> {
        set(s.shrink())
    }
    fn set<A: Hash + Eq, I: IntoIterator<Item = A>>(xs: I) -> HashSet<A> {
        xs.into_iter().collect()
    }

    fn ordered_eq<A: Arbitrary + Eq + Debug>(s: A, v: Vec<A>) {
        let (left, right) = (s.shrink().collect::<Vec<A>>(), v);
        assert_eq!(left, right);
    }

    #[test]
    fn bounds() {
        use std::ops::Bound::*;
        for i in -5..=5 {
            ordered_eq(Included(i), i.shrink().map(Included).collect());
            ordered_eq(Excluded(i), i.shrink().map(Excluded).collect());
        }
        eq(Unbounded::<i32>, vec![]);
    }

    #[test]
    fn ranges() {
        ordered_eq(0..0, vec![]);
        ordered_eq(1..1, vec![0..1, 1..0]);
        ordered_eq(3..5, vec![0..5, 2..5, 3..0, 3..3, 3..4]);
        ordered_eq(5..3, vec![0..3, 3..3, 4..3, 5..0, 5..2]);
        ordered_eq(3.., vec![0.., 2..]);
        ordered_eq(..3, vec![..0, ..2]);
        ordered_eq(.., vec![]);
        ordered_eq(3..=5, vec![0..=5, 2..=5, 3..=0, 3..=3, 3..=4]);
        ordered_eq(..=3, vec![..=0, ..=2]);
    }

    #[test]
    fn pathbuf() {
        ordered_eq(
            PathBuf::from("/home/foo//.././bar"),
            vec![
                PathBuf::from("/home/foo//.."),
                PathBuf::from("/home/foo/../bar"),
            ],
        );
    }
}
