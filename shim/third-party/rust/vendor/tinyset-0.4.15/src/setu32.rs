#![deny(missing_docs)]
//! This is a crate for the tiniest sets ever.

const fn num_bits<T>() -> u32 {
    std::mem::size_of::<T>() as u32 * 8
}

fn log_2(x: u32) -> u32 {
    if x == 0 {
        1
    } else {
        num_bits::<u32>() as u32 - x.leading_zeros() as u32
    }
}

#[test]
fn test_log_2() {
    assert_eq!(log_2(0), 1);
    assert_eq!(log_2(1), 1);
    assert_eq!(log_2(7), 3);
    assert_eq!(log_2(8), 4);
}

fn compute_array_bits(mx: u32) -> u32 {
    if log_2(mx) < 2 {
        return 62;
    } else if log_2(mx) > 62 {
        return 0;
    }
    let mut bits = num_bits::<u32>() - log_2(mx);
    while num_bits::<u32>() - log_2(mx) < bits {
        bits += 1;
    }
    bits
}

fn split_u32(x: u32, bits: u32) -> (u32, u32) {
    if bits > 0 {
        (x / bits, (x % bits))
    } else {
        (x, 0)
    }
}

fn unsplit_u32(k: u32, offset: u32, bits: u32) -> u32 {
    if bits > 0 {
        k * bits + offset
    } else {
        k
    }
}

/// A set of u32
pub struct SetU32(*mut S);

unsafe impl Send for SetU32 {}
unsafe impl Sync for SetU32 {}

use crate::copyset::impl_set_methods;
impl_set_methods!(SetU32);

#[repr(C)]
#[derive(Debug)]
struct S {
    b: Sbeginning,
    array: u32,
}

#[repr(C)]
#[derive(Debug)]
struct Sbeginning {
    sz: u32,
    cap: u32,
    bits: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Tiny {
    sz: u8,
    sz_spent: u8,
    bits: usize,
    last: usize,
}

fn mask(bits: usize) -> u32 {
    (1 << bits) - 1
}

impl Iterator for Tiny {
    type Item = u32;
    fn next(&mut self) -> Option<u32> {
        let bitsplits = BITSPLITS[self.sz as usize];
        if self.sz_spent < self.sz {
            let nbits = bitsplits[self.sz_spent as usize];
            let difference = self.bits & mask(nbits as usize) as usize;
            if self.sz_spent == 0 {
                self.last = difference;
            } else {
                self.last = self.last + 1 + difference
            }
            self.bits = self.bits >> nbits;
            self.sz_spent += 1;
            Some(self.last as u32)
        } else {
            None
        }
    }
    fn count(self) -> usize {
        self.sz as usize
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.sz as usize, Some(self.sz as usize))
    }
    fn min(mut self) -> Option<u32> {
        self.next()
    }
    fn max(mut self) -> Option<u32> {
        let mut mx = None;
        while let Some(x) = self.next() {
            mx = Some(x);
        }
        mx
    }
}

#[cfg(target_pointer_width = "64")]
static BITSPLITS: [&[u32]; 7] = [
    &[],
    &[31],
    &[31, 30],
    &[31, 15, 15],
    &[25, 12, 12, 12],
    &[21, 10, 10, 10, 10],
    &[21, 8, 8, 8, 8, 8],
];

#[cfg(target_pointer_width = "32")]
static BITSPLITS: [&[u32]; 7] = [
    &[],
    &[29],
    &[20, 9],
    &[15, 7, 7],
    &[12, 5, 5, 5],
    &[10, 4, 4, 4, 4],
    &[9, 4, 4, 4, 4, 4],
];

impl Tiny {
    #[cfg(test)]
    fn debug_me(self, msg: &str) {
        println!("{}: {:?} => {:?}", msg, self, self.collect::<Vec<_>>());
    }
    fn to_usize(self) -> usize {
        let fourbit = self.sz / 4;
        let szbits = self.sz * (1 - fourbit) + fourbit * (self.sz + 1);
        szbits as usize | self.bits << 3
    }
    fn from_usize(x: usize) -> Self {
        let sz = (x as u8 & 3) + (x as u8 & 4) / 4 * 3;
        Tiny {
            sz,
            bits: x >> 3,
            sz_spent: 0,
            last: 0,
        }
    }
    fn from_singleton(x: u32) -> Option<Self> {
        if log_2(x) > BITSPLITS[1][0] {
            None
        } else {
            Some(Tiny {
                sz: 1,
                bits: x as usize,
                sz_spent: 0,
                last: 0,
            })
        }
    }
    fn new(mut v: Vec<u32>) -> Option<Self> {
        if v.len() == 0 {
            return None;
        } else if v.len() > BITSPLITS.len() - 1 {
            return None;
        }
        let sz = v.len() as u8;
        v.sort();
        v.dedup();
        let mut last = 0;
        let mut offset = 0;
        let mut bits: usize = 0;
        let bitsplits = BITSPLITS[sz as usize];
        for (x, nbits) in v.into_iter().zip(bitsplits.iter().cloned()) {
            let y = if offset == 0 { x } else { x - last - 1 };
            if log_2(y) > nbits {
                return None;
            }
            bits = bits | (y as usize) << offset;
            offset += nbits;
            last = x;
        }
        Some(Tiny {
            sz,
            bits,
            sz_spent: 0,
            last: 0,
        })
    }
    fn insert(mut self, e: u32) -> Option<Self> {
        let mut e = e as usize;
        let old_bitsplits = BITSPLITS[self.sz as usize];
        if let Some(new_bitsplits) = BITSPLITS.get(self.sz as usize + 1) {
            let mut new = Tiny {
                bits: 0,
                sz: self.sz + 1,
                last: 0,
                sz_spent: 0,
            };
            let backup = self.clone();
            let mut offset = 0;
            let mut old_iter = old_bitsplits.iter().cloned();
            let mut new_iter = new_bitsplits.iter().cloned();
            while let Some(newb) = new_iter.next() {
                if let Some(oldb) = old_iter.next() {
                    let mut n = self.bits & mask(oldb as usize) as usize;
                    if e == n {
                        return Some(backup);
                    } else if log_2(n as u32) > newb {
                        if e < n {
                            return None;
                        }
                        e -= n + 1;
                        self.bits = self.bits >> oldb;
                        for oldb in old_iter {
                            let n = self.bits & mask(oldb as usize) as usize;
                            if n == e {
                                return Some(backup);
                            }
                            if e < n {
                                return None;
                            }
                            e -= n + 1;
                            self.bits = self.bits >> oldb;
                        }
                        return None;
                    } else if e < n {
                        new.bits = new.bits | (e << offset);
                        offset += newb;
                        n = n - e - 1;
                        let newb = new_iter.next().unwrap();
                        if log_2(n as u32) > newb {
                            return None;
                        }
                        new.bits = new.bits | (n << offset);
                        offset += newb;
                        self.bits = self.bits >> oldb;
                        for newb in new_iter {
                            let oldb = old_iter.next().unwrap();
                            let n = self.bits & mask(oldb as usize) as usize;
                            if log_2(n as u32) > newb {
                                return None;
                            }
                            new.bits = new.bits | (n << offset);
                            self.bits = self.bits >> oldb;
                            offset += newb;
                        }
                        return Some(new);
                    }
                    e -= n + 1;
                    new.bits = new.bits | (n << offset);
                    offset += newb;
                    self.bits = self.bits >> oldb;
                } else {
                    // the new one is last
                    if log_2(e as u32) > newb {
                        return None;
                    }
                    new.bits = new.bits | (e << offset);
                }
            }
            Some(new)
        } else {
            if self.clone().any(|x| x == e as u32) {
                Some(self)
            } else {
                None
            }
        }
    }
    fn contains(mut self, e: u32) -> bool {
        if e > std::usize::MAX as u32 {
            return false;
        }
        let mut e = e as usize;
        let bitsplits = BITSPLITS[self.sz as usize];
        for b in bitsplits.iter().cloned() {
            let n: usize = self.bits & mask(b as usize) as usize;
            if e == n {
                return true;
            } else if e < n {
                return false;
            }
            e -= n + 1;
            self.bits = self.bits >> b;
        }
        false
    }
}

#[cfg(test)]
fn test_vec(v: Vec<u32>) {
    println!("\ntesting {:?}", v);
    assert_eq!(Tiny::new(v.clone()).unwrap().collect::<Vec<_>>(), v);
}

#[test]
fn test_tiny() {
    assert_eq!(Tiny::new(vec![]), None);
    test_vec(vec![1]);
    test_vec(vec![1024]);
    test_vec(vec![1, 2]);
    test_vec(vec![1, 2, 3]);
    test_vec(vec![1, 2, 3, 4, 5]);
    test_vec(vec![1, 2, 3, 4, 5, 6]);
}

enum Internal<'a> {
    Empty,
    /// This is the case where we store up to seven values in the pointer
    /// itself.  The `Tiny` data structure is a copy of that information, which
    /// also functions as an `Iterator`.
    Stack(Tiny),
    /// This is the normal storage for tiny sets.  We use effectively a hash
    /// table.  Each u32 element stores two things, a bitmap (in the least
    /// significant bits) giving the elements in the set, and in the most
    /// significant bits a value representing the offset of the bitmap.
    Heap {
        s: &'a Sbeginning,
        a: &'a [u32],
    },
    /// This is for the case where the maximum number in our set is so large
    /// that we can't use the bitmap approach above.  Intead we store a single
    /// value to represent any zero element.  We use zeros to represent empty
    /// bins, and actual values to represent numbers, except in the case where
    /// an actual zero is present in the set, in which case we store a chosen
    /// unique value (which we change if that unique value gets added to the
    /// set).
    Big {
        s: &'a Sbeginning,
        a: &'a [u32],
    },
    // The data is stored as a bitmap.  This is used when the number of elements
    // in the set is getting close enough to the maximum value of the set.
    Dense {
        sz: u32,
        a: &'a [u32],
    },
}
enum InternalMut<'a> {
    Empty,
    Stack(Tiny),
    Heap {
        s: &'a mut Sbeginning,
        a: &'a mut [u32],
    },
    Big {
        s: &'a mut Sbeginning,
        a: &'a mut [u32],
    },
    Dense {
        sz: &'a mut u32,
        a: &'a mut [u32],
    },
}

#[derive(Debug)]
enum Iter<'a> {
    Empty,
    Stack(Tiny),
    Heap(HeapIter<'a>),
    Big(BigIter<'a>),
    Dense(DenseIter<'a>),
}

impl<'a> Iterator for Iter<'a> {
    type Item = u32;
    #[inline]
    fn next(&mut self) -> Option<u32> {
        match self {
            Iter::Empty => None,
            Iter::Stack(t) => t.next(),
            Iter::Dense(it) => it.next(),
            Iter::Big(it) => it.next(),
            Iter::Heap(it) => it.next(),
        }
    }
    #[inline]
    fn count(self) -> usize {
        match self {
            Iter::Empty => 0,
            Iter::Stack(t) => t.count(),
            Iter::Dense(it) => it.count(),
            Iter::Big(it) => it.count(),
            Iter::Heap(it) => it.count(),
        }
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Iter::Empty => (0, Some(0)),
            Iter::Stack(t) => t.size_hint(),
            Iter::Dense(it) => it.size_hint(),
            Iter::Big(it) => it.size_hint(),
            Iter::Heap(it) => it.size_hint(),
        }
    }
    #[inline]
    fn min(self) -> Option<u32> {
        match self {
            Iter::Empty => None,
            Iter::Stack(t) => t.min(),
            Iter::Dense(it) => it.min(),
            Iter::Big(it) => it.min(),
            Iter::Heap(it) => it.min(),
        }
    }
    #[inline]
    fn max(self) -> Option<u32> {
        match self {
            Iter::Empty => None,
            Iter::Stack(t) => t.max(),
            Iter::Dense(it) => it.max(),
            Iter::Big(it) => it.max(),
            Iter::Heap(it) => it.max(),
        }
    }
    #[inline]
    fn last(self) -> Option<u32> {
        match self {
            Iter::Empty => None,
            Iter::Stack(t) => t.last(),
            Iter::Dense(it) => it.last(),
            Iter::Big(it) => it.last(),
            Iter::Heap(it) => it.last(),
        }
    }
}

#[derive(Debug)]
struct HeapIter<'a> {
    sz_left: usize,
    bits: u32,
    whichbit: u32,
    array: &'a [u32],
}

impl<'a> Iterator for HeapIter<'a> {
    type Item = u32;
    #[inline]
    fn next(&mut self) -> Option<u32> {
        if self.bits > 0 {
            while let Some(&x) = self.array.first() {
                while self.whichbit < self.bits {
                    let oldbit = self.whichbit;
                    self.whichbit += 1;
                    if (x & (1 << oldbit)) != 0 {
                        self.sz_left -= 1;
                        return Some(unsplit_u32(x >> self.bits, oldbit, self.bits));
                    }
                }
                self.array = self.array.split_first().unwrap().1;
                self.whichbit = 0;
            }
        } else {
            if let Some((&first, rest)) = self.array.split_first() {
                self.array = rest;
                self.sz_left -= 1;
                return Some(first);
            }
        }
        None
    }
    #[inline]
    fn last(self) -> Option<u32> {
        self.array
            .into_iter()
            .rev()
            .cloned()
            .filter(|&x| x != 0)
            .map(|x| x >> self.bits + (x & mask(self.bits as usize)).leading_zeros() - 31)
            .next()
    }
    #[inline]
    fn count(self) -> usize {
        self.sz_left
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.sz_left, Some(self.sz_left))
    }
    #[inline]
    fn min(mut self) -> Option<u32> {
        if self.sz_left == 0 {
            None
        } else if self.whichbit == 0 {
            let x = self
                .array
                .into_iter()
                .cloned()
                .filter(|x| *x != 0)
                .min()
                .unwrap();
            Some((x >> self.bits) * self.bits + x.trailing_zeros() as u32)
        } else {
            let mut min = self.next().unwrap();
            while let Some(x) = self.next() {
                if x < min {
                    min = x;
                }
            }
            Some(min)
        }
    }
    #[inline]
    fn max(mut self) -> Option<u32> {
        if self.sz_left == 0 {
            None
        } else if self.whichbit == 0 {
            let x = self
                .array
                .into_iter()
                .cloned()
                .filter(|x| *x != 0)
                .max()
                .unwrap();
            let reference = (x >> self.bits) * self.bits;
            let m = mask(self.bits as usize);
            let extra = 31 - (x & m).leading_zeros();
            Some(reference + extra)
        } else {
            let mut max = self.next().unwrap();
            while let Some(x) = self.next() {
                if x > max {
                    max = x;
                }
            }
            Some(max)
        }
    }
}

#[derive(Debug)]
struct BigIter<'a> {
    sz_left: usize,
    bits: u32,
    a: &'a [u32],
}

impl<'a> Iterator for BigIter<'a> {
    type Item = u32;
    #[inline]
    fn next(&mut self) -> Option<u32> {
        while let Some((&x, rest)) = self.a.split_first() {
            self.a = rest;
            if x != 0 {
                self.sz_left -= 1;
                return Some(if x == self.bits { 0 } else { x });
            }
        }
        None
    }
    #[inline]
    fn last(self) -> Option<u32> {
        self.a
            .into_iter()
            .rev()
            .cloned()
            .filter(|&x| x != 0)
            .map(|x| if x == self.bits { 0 } else { x })
            .next()
    }
    #[inline]
    fn count(self) -> usize {
        self.sz_left
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.sz_left, Some(self.sz_left))
    }
    #[inline]
    fn min(self) -> Option<u32> {
        if self.sz_left == 0 {
            None
        } else {
            self.a
                .into_iter()
                .cloned()
                .filter(|x| *x != 0)
                .map(|x| if x == self.bits { 0 } else { x })
                .min()
        }
    }
}

#[derive(Debug)]
struct DenseIter<'a> {
    sz_left: usize,
    whichword: usize,
    whichbit: u32,
    a: &'a [u32],
}

impl<'a> Iterator for DenseIter<'a> {
    type Item = u32;
    #[inline]
    fn next(&mut self) -> Option<u32> {
        loop {
            if let Some(word) = self.a.get(self.whichword) {
                while self.whichbit < 32 {
                    let bit = self.whichbit;
                    self.whichbit = 1 + bit;
                    if word & (1 << bit) != 0 {
                        self.sz_left -= 1;
                        return Some(((self.whichword as u32) << 5) + bit as u32);
                    }
                }
                self.whichbit = 0;
                self.whichword += 1;
            } else {
                return None;
            }
        }
    }
    #[inline]
    fn last(self) -> Option<u32> {
        if self.sz_left == 0 {
            return None;
        }
        let zero_words = self.a.iter().rev().cloned().take_while(|&x| x == 0).count() as u32;
        let zero_bits = self.a[self.a.len() - 1 - zero_words as usize].leading_zeros() as u32;
        Some(self.a.len() as u32 * 32 - zero_bits - 1 - zero_words * 32)
    }
    #[inline]
    fn max(self) -> Option<u32> {
        self.last()
    }
    #[inline]
    fn count(self) -> usize {
        self.sz_left
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.sz_left, Some(self.sz_left))
    }
    #[inline]
    fn min(mut self) -> Option<u32> {
        self.next()
    }
}

impl crate::copyset::CopySet for SetU32 {
    type Item = u32;
    type Iter = IntoIter;
    fn ins(&mut self, e: u32) -> bool {
        self.insert(e)
    }
    fn rem(&mut self, e: u32) -> bool {
        self.remove(e)
    }
    fn con(&self, e: u32) -> bool {
        self.contains(e)
    }
    fn vec(&self) -> Vec<u32> {
        self.iter().collect()
    }
    fn ln(&self) -> usize {
        self.len()
    }
    fn it(self) -> Self::Iter {
        self.into_iter()
    }
}

/// An iterator over a set of `u32`.
#[derive(Debug)]
pub struct IntoIter {
    iter: Iter<'static>,
    _set: SetU32,
}

impl Extend<u32> for SetU32 {
    fn extend<T: IntoIterator<Item = u32>>(&mut self, iter: T) {
        for i in iter.into_iter() {
            self.insert(i);
        }
    }
}

#[cfg(feature = "compactserde")]
impl SetU32 {
    fn to_array(&self) -> Vec<u32> {
        let mut out = Vec::new();
        if self.0 as usize == 0 || self.0 as usize & 7 != 0 {
            out.push(self.0 as u32);
        } else {
            let s = unsafe { &*self.0 };
            let b = &s.b;
            let a = unsafe { std::slice::from_raw_parts(&s.array as *const u32, b.cap as usize) };
            out.push(b.sz as u32);
            out.push(b.bits as u32);
            out.extend(a);
        }
        out
    }
    fn from_array(v: &[u32]) -> SetU32 {
        if v.len() > 1 {
            let cap = v.len() - 2;
            let mut set = SetU32::with_capacity_and_bits(cap, v[1]);
            match set.internal_mut() {
                InternalMut::Empty => unreachable!(),
                InternalMut::Stack(_) => unreachable!(),
                InternalMut::Dense { sz, a } => {
                    *sz = v[0];
                    for (i, o) in v[2..].iter().zip(a.iter_mut()) {
                        *o = *i;
                    }
                }
                InternalMut::Heap { s, a } => {
                    s.sz = v[0];
                    s.bits = v[1];
                    for (i, o) in v[2..].iter().zip(a.iter_mut()) {
                        *o = *i;
                    }
                }
                InternalMut::Big { s, a } => {
                    s.sz = v[0];
                    s.bits = v[1];
                    for (i, o) in v[2..].iter().zip(a.iter_mut()) {
                        *o = *i;
                    }
                }
            }
            set
        } else {
            SetU32(v[0] as *mut S)
        }
    }
}

#[cfg(feature = "compactserde")]
#[test]
fn to_from_array() {
    use std::iter::FromIterator;

    let set = SetU32::from_iter([0]);
    let s = set.to_array();
    assert_eq!(set, SetU32::from_array(&s));

    let set = SetU32::from_iter([]);
    let s = set.to_array();
    assert_eq!(set, SetU32::from_array(&s));

    let set = SetU32::from_iter([u32::MAX, u32::MAX - 100]);
    let s = set.to_array();
    let newset = SetU32::from_array(&s);
    for n in set.iter() {
        assert!(set.contains(n));
    }
    for n in newset.iter() {
        assert!(newset.contains(n));
    }
    println!("set is {set:?}");
    println!("newset is {newset:?}");
    assert_eq!(set.len(), newset.len());
    assert_eq!(set, SetU32::from_array(&s));

    let set = SetU32::from_iter(0..10000);
    let s = set.to_array();
    assert_eq!(set, SetU32::from_array(&s));
}

#[cfg(feature = "serde")]
mod serde {
    use crate::SetU32;
    use serde::de::{Deserialize, Deserializer, SeqAccess, Visitor};
    use serde::ser::{Serialize, SerializeSeq, Serializer};

    impl Serialize for SetU32 {
        #[cfg(feature = "compactserde")]
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let a = self.to_array();
            let mut seq = serializer.serialize_seq(Some(a.len()))?;
            for e in a.into_iter() {
                seq.serialize_element(&e)?;
            }
            seq.end()
        }
        #[cfg(not(feature = "compactserde"))]
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let mut seq = serializer.serialize_seq(Some(self.len()))?;
            for e in self.iter() {
                seq.serialize_element(&e)?;
            }
            seq.end()
        }
    }

    // A Visitor is a type that holds methods that a Deserializer can drive
    // depending on what is contained in the input data.
    //
    // In the case of a map we need generic type parameters K and V to be
    // able to set the output type correctly, but don't require any state.
    // This is an example of a "zero sized type" in Rust. The PhantomData
    // keeps the compiler from complaining about unused generic type
    // parameters.
    struct SetVisitor;

    impl SetVisitor {
        fn new() -> Self {
            SetVisitor
        }
    }

    // This is the trait that Deserializers are going to be driving. There
    // is one method for each type of data that our type knows how to
    // deserialize from. There are many other methods that are not
    // implemented here, for example deserializing from integers or strings.
    // By default those methods will return an error, which makes sense
    // because we cannot deserialize a MyMap from an integer or string.
    impl<'de> Visitor<'de> for SetVisitor {
        // The type that our Visitor is going to produce.
        type Value = SetU32;

        // Format a message stating what data this Visitor expects to receive.
        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str("a set of usize")
        }

        #[cfg(feature = "compactserde")]
        fn visit_seq<M>(self, mut access: M) -> Result<Self::Value, M::Error>
        where
            M: SeqAccess<'de>,
        {
            let mut v = if let Some(cap) = access.size_hint() {
                Vec::with_capacity(cap)
            } else {
                Vec::new()
            };
            // While there are entries remaining in the input, add them
            // into our map.
            while let Some(elem) = access.next_element()? {
                v.push(elem);
            }
            Ok(SetU32::from_array(&v))
        }
        #[cfg(not(feature = "compactserde"))]
        fn visit_seq<M>(self, mut access: M) -> Result<Self::Value, M::Error>
        where
            M: SeqAccess<'de>,
        {
            let mut set = SetU32::new();

            // While there are entries remaining in the input, add them
            // into our map.
            while let Some(elem) = access.next_element()? {
                set.insert(elem);
            }

            Ok(set)
        }
    }

    // This is the trait that informs Serde how to deserialize MyMap.
    impl<'de> Deserialize<'de> for SetU32 {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            // Instantiate our Visitor and ask the Deserializer to drive
            // it over the input data, resulting in an instance of MyMap.
            deserializer.deserialize_seq(SetVisitor::new())
        }
    }

    #[test]
    fn serialize_deserialize() {
        use std::iter::FromIterator;

        let set = SetU32::from_iter([0]);
        let s = serde_json::to_string(&set).unwrap();
        assert_eq!(set, serde_json::from_str(&s).unwrap());

        let set = SetU32::from_iter([]);
        let s = serde_json::to_string(&set).unwrap();
        assert_eq!(set, serde_json::from_str(&s).unwrap());

        let set = SetU32::from_iter([u32::MAX, u32::MAX - 100]);
        let s = serde_json::to_string(&set).unwrap();
        assert_eq!(set, serde_json::from_str(&s).unwrap());

        let set = SetU32::from_iter(0..10000);
        let s = serde_json::to_string(&set).unwrap();
        assert_eq!(set, serde_json::from_str(&s).unwrap());
    }
}

impl IntoIterator for SetU32 {
    type Item = u32;
    type IntoIter = IntoIter;

    fn into_iter(self) -> IntoIter {
        let iter = unsafe { std::mem::transmute(self.private_iter()) };
        IntoIter { iter, _set: self }
    }
}
impl Iterator for IntoIter {
    type Item = u32;
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
    #[inline]
    fn last(self) -> Option<Self::Item> {
        self.iter.last()
    }
    #[inline]
    fn min(self) -> Option<Self::Item> {
        self.iter.min()
    }
    #[inline]
    fn max(self) -> Option<Self::Item> {
        self.iter.max()
    }
    #[inline]
    fn count(self) -> usize {
        self.iter.count()
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl Clone for SetU32 {
    fn clone(&self) -> Self {
        if self.0 as usize & 7 == 0 && self.0 != std::ptr::null_mut() {
            let c = self.capacity();
            unsafe {
                let ptr = std::alloc::alloc_zeroed(layout_for_capacity(c)) as *mut S;
                if ptr.is_null() {
                    // It is safe to panic here rather than calling `alloc::handle_alloc_error`
                    // because we haven't  even started creating this data structure, so
                    // `catch_unwind` can't get us into trouble.
                    panic!("memory allocation failed");
                }
                std::ptr::copy_nonoverlapping(
                    self.0 as *const u8,
                    ptr as *mut u8,
                    bytes_for_capacity(c),
                );
                SetU32(ptr)
            }
        } else {
            SetU32(self.0)
        }
    }
}

impl SetU32 {
    /// Create an empty set with capacity to hold the provided set.
    ///
    /// ```
    /// use tinyset::SetU32;
    ///
    /// let a: SetU32 = (1..300).collect();
    /// let mut b = SetU32::with_capacity_of(&a);
    ///
    /// assert_eq!(a.capacity(), b.capacity());
    /// assert_eq!(b.len(), 0);
    /// for i in a.iter() {
    ///   b.insert(i);
    /// }
    /// assert_eq!(a.capacity(), b.capacity());
    /// assert_eq!(b.len(), a.len());
    /// ```
    pub fn with_capacity_of(other: &Self) -> Self {
        if other.0 as usize & 7 == 0 && other.0 != std::ptr::null_mut() {
            let c = other.capacity();
            unsafe {
                let ptr = std::alloc::alloc_zeroed(layout_for_capacity(c)) as *mut S;
                if ptr.is_null() {
                    // It is safe to panic here rather than calling `alloc::handle_alloc_error`
                    // because we haven't  even started creating this data structure, so
                    // `catch_unwind` can't get us into trouble.
                    panic!("memory allocation failed");
                }
                (*ptr).b.cap = (*other.0).b.cap;
                (*ptr).b.bits = (*other.0).b.bits;
                SetU32(ptr)
            }
        } else {
            SetU32::new()
        }
    }
}

impl SetU32 {
    /// The number of elements in the set
    #[inline]
    pub fn len(&self) -> usize {
        match self.internal() {
            Internal::Empty => 0,
            Internal::Stack(t) => t.sz as usize,
            Internal::Heap { s, .. } => s.sz as usize,
            Internal::Big { s, .. } => s.sz as usize,
            Internal::Dense { sz, .. } => sz as usize,
        }
    }
    /// The capacity of the set
    #[inline]
    pub fn capacity(&self) -> usize {
        match self.internal() {
            Internal::Empty => 0,
            Internal::Stack(_) => 0,
            Internal::Heap { a, .. } => a.len(),
            Internal::Big { a, .. } => a.len(),
            Internal::Dense { a, .. } => a.len(),
        }
    }
    /// Print debugging information about this set.
    pub fn debug_me(&self, msg: &str) {
        match self.internal() {
            Internal::Empty => println!("empty set: {}", msg),
            Internal::Stack(t) => {
                println!("{}: stack {:?} => {:?}", msg, t, t.collect::<Vec<_>>());
                println!("    bits {:032b}", t.bits);
            }
            Internal::Heap { s, a } => {
                println!("{}: heap {:?}", msg, s);
                for (i, x) in a.iter().cloned().enumerate() {
                    println!(
                        "      {} (key {} pov {}): {:0b} ({})",
                        (x >> s.bits) * s.bits,
                        x >> s.bits,
                        p_poverty(x >> s.bits, i, a.len()),
                        x & mask(s.bits as usize),
                        x
                    );
                }
                println!("    => {:?}", self.iter().collect::<Vec<_>>());
            }
            Internal::Big { s, a } => {
                println!("{}: big {:?}\n    {:?}", msg, s, a);
                let v: Vec<_> = a.iter().cloned().map(|x| x % a.len() as u32).collect();
                println!("     >>>{:?}", v);
            }
            Internal::Dense { sz, a } => {
                println!(
                    "{}: dense {:?}\n    {:?}\n    => {:?}",
                    msg,
                    sz,
                    a,
                    self.iter().collect::<Vec<u32>>()
                );
                println!("    foo {:?}", self.iter());
            }
        }
    }
    /// Tally up how much memory is in use.
    #[inline]
    pub fn mem_used(&self) -> usize {
        match self.internal() {
            Internal::Empty => std::mem::size_of::<Self>(),
            Internal::Stack(_) => std::mem::size_of::<Self>(),
            Internal::Heap { s, .. } => std::mem::size_of::<Self>() + s.cap as usize * 4 - 4,
            Internal::Dense { a, .. } => std::mem::size_of::<Self>() + a.len() * 4 - 4,
            Internal::Big { s, .. } => std::mem::size_of::<Self>() + s.cap as usize * 4 - 4,
        }
    }
    /// This requires that we currently be a dense! It also requires
    /// that we add another element immediately without incrementing
    /// sz.
    unsafe fn dense_increase_mx(&mut self, mx: u32) -> &mut [u32] {
        let ptr = self.0;
        let cap = 1 + mx / 32 + mx / 128;

        let oldcap = (*ptr).b.cap;
        self.0 = std::alloc::realloc(
            ptr as *mut u8,
            layout_for_capacity(oldcap as usize),
            bytes_for_capacity(cap as usize),
        ) as *mut S;
        if self.0.is_null() {
            // We do not panic here, because that would leave us in a world of trouble
            // if the panic were to be caught.
            std::alloc::handle_alloc_error(layout_for_capacity(cap as usize));
        }
        (*self.0).b.cap = cap;
        (*self.0).b.sz += 1;
        match self.internal_mut() {
            InternalMut::Dense { a, .. } => {
                for i in oldcap as usize..a.len() {
                    a[i] = 0;
                }
                a
            }
            _ => unreachable!(),
        }
    }

    fn dense_with_max(mx: u32) -> SetU32 {
        let cap = 1 + mx / 32 + mx / 128;
        // This should be stored in a dense bitset.
        unsafe {
            let ptr = std::alloc::alloc_zeroed(layout_for_capacity(cap as usize)) as *mut S;
            if ptr.is_null() {
                // It is safe to panic here rather than calling `alloc::handle_alloc_error`
                // because we haven't  even started creating this data structure, so
                // `catch_unwind` can't get us into trouble.
                panic!("memory allocation failed");
            }
            let x = SetU32(ptr);
            (*x.0).b.cap = cap;
            (*x.0).b.bits = 32;
            x
        }
    }

    /// Create a set with the given capacity
    pub fn with_capacity_and_max(cap: usize, mx: u32) -> SetU32 {
        if cap as u32 > mx >> 5 {
            SetU32::dense_with_max(mx)
        } else {
            SetU32::with_capacity_and_bits(cap, compute_array_bits(mx))
        }
    }
    /// Create a set with the given capacity and bits
    pub fn with_capacity_and_bits(cap: usize, bits: u32) -> SetU32 {
        if cap > 0 {
            unsafe {
                let ptr = std::alloc::alloc_zeroed(layout_for_capacity(cap)) as *mut S;
                if ptr.is_null() {
                    // It is safe to panic here rather than calling `alloc::handle_alloc_error`
                    // because we haven't  even started creating this data structure, so
                    // `catch_unwind` can't get us into trouble.
                    panic!("memory allocation failed");
                }
                let x = SetU32(ptr);
                (*x.0).b.cap = cap as u32;
                (*x.0).b.bits = if bits == 0 {
                    let mut b = 0;
                    while b <= 32 {
                        b = crate::rand::rand32();
                    }
                    b
                } else {
                    bits
                };
                x
            }
        } else {
            SetU32(0 as *mut S)
        }
    }
    /// An empty set
    #[inline]
    pub const fn new() -> Self {
        SetU32(0 as *mut S)
    }

    /// Insert and return true if it was not present.
    pub fn insert(&mut self, e: u32) -> bool {
        match self.internal_mut() {
            InternalMut::Empty => {
                if let Some(t) = Tiny::from_singleton(e) {
                    *self = SetU32(t.to_usize() as *mut S);
                    return true;
                }
                // println!("I could not create tiny set with singleton {}", e);
                *self = Self::with_capacity_and_max(1, e);
            }
            InternalMut::Stack(t) => {
                if let Some(newt) = t.insert(e) {
                    *self = SetU32(newt.to_usize() as *mut S);
                    return newt.sz != t.sz;
                }
                let mx = t.max().unwrap();
                let mx = if e > mx { e } else { mx };
                *self = Self::with_capacity_and_max(t.sz as usize + 1, mx);
                // self.debug_me("empty array");
                for x in t {
                    self.insert(x);
                    // self.debug_me(&format!("   ...after inserting {}", x));
                }
                self.insert(e);
                // self.debug_me(&format!("   ...and inserting {}", e));
                return true;
            }
            _ => (),
        }
        match self.internal_mut() {
            InternalMut::Empty => unreachable!(),
            InternalMut::Stack(_) => unreachable!(),
            InternalMut::Dense { sz, a } => {
                let key = (e >> 5) as usize;
                if let Some(bits) = a.get_mut(key) {
                    let whichbit = 1 << (e & 31);
                    let present = *bits & whichbit != 0;
                    *bits = *bits | whichbit;
                    if !present {
                        *sz = *sz + 1;
                    }
                    !present
                } else {
                    // println!("key is {}", key);
                    if key > 64 * (*sz as usize) {
                        // It is getting sparse, so let us switch back
                        // to a non-hash table.
                        let cap = 1 + 2 * (*sz as usize);
                        let mut new = SetU32::with_capacity_and_bits(cap, 0);
                        for x in self.iter() {
                            new.insert(x);
                        }
                        new.insert(e);
                        *self = new;
                    } else {
                        unsafe {
                            self.dense_increase_mx(e)[key] = 1 << (e & 31);
                        }
                    }
                    true
                }
            }
            InternalMut::Heap { s, a } => {
                if compute_array_bits(e) < s.bits {
                    let newcap = s.cap + 1 + (crate::rand::rand32() % s.cap);
                    let mut new =
                        Self::with_capacity_and_bits(newcap as usize, compute_array_bits(e));
                    // new.debug_me("\n\nnew set");
                    for d in self.iter() {
                        new.insert(d);
                        // new.debug_me(&format!("\n -- after inserting {}", d));
                    }
                    new.insert(e);
                    // new.debug_me(&format!("\n -- after inserting {}", e));
                    *self = new;
                    return true;
                }
                let (key, offset) = split_u32(e, s.bits);
                match p_lookfor(key, a, s.bits) {
                    LookedUp::KeyFound(idx) => {
                        if a[idx] & (1 << offset) != 0 {
                            return false;
                        } else {
                            a[idx] = a[idx] | (1 << offset);
                            s.sz += 1;
                            return true;
                        }
                    }
                    LookedUp::EmptySpot(idx) => {
                        a[idx] = key << s.bits | 1 << offset;
                        s.sz += 1;
                        return true;
                    }
                    LookedUp::NeedInsert => {}
                }
                // println!("looking for space in sparse... {:?}", a);
                if a.iter()
                    .cloned()
                    .filter(|&x| x == 0) // look for empty spots
                    .enumerate() // count them
                    .any(|(n, _)| n + 1 > a.len() >> 4)
                // we have more than 1/16 empty?
                {
                    let idx = p_insert(key, a, s.bits);
                    // println!("about to insert key {} with elem {} at {}",
                    //          key, e, idx);
                    a[idx] = (key << s.bits) | (1 << offset);
                    s.sz += 1;
                    return true;
                }
                // println!("no room in the sparse set... {:?}", a);
                // We'll have to expand the set.
                let mx = a
                    .iter()
                    .cloned()
                    .map(|x| (x >> s.bits) * s.bits + s.bits)
                    .max()
                    .unwrap();
                let mx = if e > mx { e } else { mx };
                if s.cap > mx >> 6 {
                    // A dense set will save memory
                    let mut new = Self::dense_with_max(mx);
                    for x in self.iter() {
                        new.insert(x);
                    }
                    new.insert(e);
                    *self = new;
                } else {
                    // Let's keep things sparse
                    // A dense set will cost us memory
                    let newcap: u32 = s.cap + 1 + (crate::rand::rand32() % s.cap);
                    let mut new = Self::with_capacity_and_bits(newcap as usize, s.bits);
                    // new.debug_me("initial new");
                    for v in self.iter() {
                        new.insert(v);
                    }
                    new.insert(e);
                    *self = new;
                }
                true
            }
            InternalMut::Big { s, a } => {
                if e == s.bits {
                    // Pick a new number not present in the set.  We
                    // use a cryptographically secure random number
                    // generator to look for a new number not present,
                    // which feels like overkill.  But the cost of
                    // changing the "bits" is $O(N)$, so it's worth
                    // a high O(1) cost to reduce collisions.
                    let had_zero = p_remove(s.bits, a, 0);
                    loop {
                        let i: u32 = crate::rand::rand32();
                        if i > 32 && !a.iter().any(|&v| v == i) {
                            s.bits = i;
                            break;
                        }
                    }
                    if had_zero {
                        a[p_insert(s.bits, a, 0)] = s.bits;
                    }
                }
                let e = if e == 0 { s.bits } else { e };
                match p_lookfor(e, a, 0) {
                    LookedUp::KeyFound(_) => {
                        return false;
                    }
                    LookedUp::EmptySpot(idx) => {
                        a[idx] = e;
                        s.sz += 1;
                        return true;
                    }
                    LookedUp::NeedInsert => (),
                }
                // println!("looking for space in... {:?}", a);
                if a.iter()
                    .cloned()
                    .filter(|&x| x == 0) // look for empty spots
                    .enumerate() // count them
                    .any(|(n, _)| n + 1 > a.len() >> 4)
                // we have more than 1/16 empty?
                {
                    let idx = p_insert(e, a, 0);
                    // println!("about to insert at {}", p_insert(e, a, 0));
                    a[idx] = e;
                    s.sz += 1;
                    return true;
                }
                // println!("no room in the set... {:?}", a);
                let newcap: u32 = s.cap + 1 + (crate::rand::rand32() % s.cap);
                let mut new = Self::with_capacity_and_bits(newcap as usize, s.bits);
                // new.debug_me("initial new");
                match new.internal_mut() {
                    InternalMut::Empty => unreachable!(),
                    InternalMut::Stack(_) => unreachable!(),
                    InternalMut::Dense { .. } => unreachable!(),
                    InternalMut::Heap { .. } => unreachable!(),
                    InternalMut::Big { s: ns, a: na } => {
                        for v in a.iter().cloned().filter(|&x| x != 0) {
                            na[p_insert(v, na, 0)] = v;
                            // println!("  with {} gives {:?}", v, na);
                            // let v: Vec<_> = na.iter().cloned().map(|x| x % na.len() as u32).collect();
                            // println!("     >>>{:?}", v);
                        }
                        na[p_insert(e, na, 0)] = e;
                        // println!("  with {} gives {:?}", e, na);
                        ns.sz = s.sz + 1;
                        // println!("  size ends up as {}", ns.sz);
                        // new.debug_me("aftr growing");
                    }
                }
                *self = new;
                true
            }
        }
    }

    /// Remove
    pub fn remove(&mut self, e: u32) -> bool {
        match self.internal_mut() {
            InternalMut::Empty => false,
            InternalMut::Stack(t) => {
                if t.clone().any(|x| x == e) {
                    let sz = t.sz - 1;
                    if sz == 0 {
                        *self = SetU32(0 as *mut S);
                    } else {
                        *self = t.filter(|&x| x != e).collect();
                    }
                    true
                } else {
                    false
                }
            }
            InternalMut::Dense { sz, a } => {
                let key = e >> 5;
                if let Some(bits) = a.get_mut(key as usize) {
                    let whichbit = 1 << (e & 31);
                    let present = *bits & whichbit != 0;
                    *bits = *bits & !whichbit;
                    if present {
                        *sz = *sz - 1;
                    }
                    present
                } else {
                    false
                }
            }
            InternalMut::Heap { s, a } => {
                if compute_array_bits(e) < s.bits {
                    return false;
                }
                let (key, offset) = split_u32(e, s.bits);
                if let LookedUp::KeyFound(idx) = p_lookfor(key, a, s.bits) {
                    if a[idx] & (1 << offset) != 0 {
                        let newa = a[idx] & !(1 << offset);
                        s.sz -= 1;
                        if newa == key << s.bits {
                            // We've removed everything with this key,
                            // so remove the whole key!
                            p_remove(key, a, s.bits);
                        } else {
                            a[idx] = newa;
                        }
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            InternalMut::Big { s, a } => {
                if e == s.bits {
                    return false;
                }
                let e = if e == 0 { s.bits } else { e };
                let had_e = p_remove(e, a, 0);
                if had_e {
                    s.sz -= 1;
                }
                had_e
            }
        }
    }

    /// Contais
    pub fn contains(&self, e: u32) -> bool {
        match self.internal() {
            Internal::Empty => false,
            Internal::Stack(t) => t.contains(e),
            Internal::Dense { a, .. } => {
                let key = e >> 5;
                if let Some(bits) = a.get(key as usize) {
                    bits & (1 << (e & 31)) != 0
                } else {
                    false
                }
            }
            Internal::Heap { s, a } => {
                if compute_array_bits(e) < s.bits {
                    // println!("too big a thing");
                    return false;
                }
                let (key, offset) = split_u32(e, s.bits);
                if let LookedUp::KeyFound(idx) = p_lookfor(key, a, s.bits) {
                    a[idx] & (1 << offset) != 0
                } else {
                    // self.debug_me(&format!("did not find key {} from {}", key, e));
                    false
                }
            }
            Internal::Big { s, a } => {
                if e == s.bits {
                    return false;
                }
                let e = if e == 0 { s.bits } else { e };
                p_lookfor(e, a, 0).key_found()
            }
        }
    }

    /// Iterate over
    #[inline]
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = u32> + 'a + std::fmt::Debug {
        self.private_iter()
    }
    fn private_iter<'a>(&'a self) -> Iter<'a> {
        match self.internal() {
            Internal::Empty => Iter::Empty,
            Internal::Stack(t) => Iter::Stack(t),
            Internal::Heap { s, a } => Iter::Heap(HeapIter {
                sz_left: s.sz as usize,
                bits: s.bits,
                whichbit: 0,
                array: a,
            }),
            Internal::Big { s, a } => Iter::Big(BigIter {
                sz_left: s.sz as usize,
                bits: s.bits,
                a,
            }),
            Internal::Dense { a, sz } => Iter::Dense(DenseIter {
                sz_left: sz as usize,
                whichword: 0,
                whichbit: 0,
                a,
            }),
        }
    }
    /// Clears the set, returning all elements in an iterator.
    #[inline]
    pub fn drain<'a>(&'a mut self) -> impl Iterator<Item = u32> + 'a {
        std::mem::replace(self, SetU32::new()).into_iter()
    }

    fn internal<'a>(&'a self) -> Internal<'a> {
        if self.0 as usize == 0 {
            Internal::Empty
        } else if self.0 as usize & 3 != 0 {
            Internal::Stack(Tiny::from_usize(self.0 as usize))
        } else {
            let s = unsafe { &*self.0 };
            let b = &s.b;
            let array = unsafe {
                // Use the calculated offset to jump the pointer to where the array starts directly, keeping the permissions for the whole allocation intact.
                self.0.cast::<u32>().offset(
                    // get a raw pointer to the array field
                    (&s.array as *const u32)
                        // calculate the offset from `*mut S` to the array field
                        .offset_from(self.0.cast()),
                )
            };
            let a = unsafe { std::slice::from_raw_parts(array, b.cap as usize) };
            if b.bits == 0 || b.bits > 32 {
                Internal::Big { s: b, a }
            } else if b.bits == 32 {
                Internal::Dense { sz: b.sz, a }
            } else {
                Internal::Heap { s: b, a }
            }
        }
    }

    fn internal_mut<'a>(&'a mut self) -> InternalMut<'a> {
        if self.0 as usize == 0 {
            InternalMut::Empty
        } else if self.0 as usize & 3 != 0 {
            InternalMut::Stack(Tiny::from_usize(self.0 as usize))
        } else {
            let s = unsafe { &mut *self.0 };
            let b = &mut s.b;
            let array = unsafe {
                // Use the calculated offset to jump the pointer to where the array starts directly, keeping the permissions for the whole allocation intact.
                self.0.cast::<u32>().offset(
                    // get a raw pointer to the array field
                    (&mut s.array as *mut u32)
                        // calculate the offset from `*mut S` to the array field
                        .offset_from(self.0.cast()),
                )
            };
            let a = unsafe { std::slice::from_raw_parts_mut(array, b.cap as usize) };
            if b.bits == 0 || b.bits > 32 {
                InternalMut::Big { s: b, a }
            } else if b.bits == 32 {
                InternalMut::Dense { sz: &mut b.sz, a }
            } else {
                InternalMut::Heap { s: b, a }
            }
        }
    }
}

impl Default for SetU32 {
    fn default() -> Self {
        SetU32(0 as *mut S)
    }
}

impl std::iter::FromIterator<u32> for SetU32 {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = u32>,
    {
        let v: Vec<_> = iter.into_iter().collect();
        if let Some(mx) = v.iter().cloned().max() {
            if let Some(t) = Tiny::new(v.clone()) {
                SetU32(t.to_usize() as *mut S)
            } else {
                if v.len() as u32 > mx >> 4 {
                    // This should be stored in a dense bitset.
                    let mut s = SetU32::with_capacity_and_max(v.len(), mx);
                    for value in v.into_iter() {
                        s.insert(value);
                    }
                    return s;
                }
                let bits = compute_array_bits(mx);
                if bits == 0 {
                    let mut s = SetU32::with_capacity_and_bits(v.len(), bits);
                    for value in v.into_iter() {
                        s.insert(value);
                    }
                    s
                } else {
                    let mut keys: Vec<_> = v.iter().map(|&x| x / bits).collect();
                    keys.sort();
                    keys.dedup();
                    let sz = (keys.len() + 1) * 11 / 10;
                    let mut s = SetU32::with_capacity_and_bits(sz, bits);
                    for value in v.into_iter() {
                        s.insert(value);
                    }
                    s
                }
            }
        } else {
            SetU32(0 as *mut S)
        }
    }
}

#[cfg(test)]
fn test_a_collect(v: Vec<u32>) {
    let s: SetU32 = v.iter().cloned().collect();
    let vv: Vec<_> = s.iter().collect();
    let ss: SetU32 = vv.iter().cloned().collect();
    let vvv: Vec<_> = ss.iter().collect();
    assert_eq!(vv, vvv);
}

#[test]
fn test_collect() {
    test_a_collect(vec![]);
    test_a_collect(vec![0]);
    test_a_collect(vec![0, 1 << 29]);
    test_a_collect(vec![0, 1 << 30, 1 << 30]);
    test_a_collect((0..1024).collect());
}

fn bytes_for_capacity(sz: usize) -> usize {
    sz * 4 + std::mem::size_of::<S>() - 4
}
fn layout_for_capacity(sz: usize) -> std::alloc::Layout {
    unsafe { std::alloc::Layout::from_size_align_unchecked(bytes_for_capacity(sz), 4) }
}

impl Drop for SetU32 {
    fn drop(&mut self) {
        if self.0 as usize > 0 {
            // make it drop by moving it out
            let c = self.capacity();
            if c == 0 {
            } else {
                unsafe {
                    std::alloc::dealloc(self.0 as *mut u8, layout_for_capacity(c));
                }
            }
        }
    }
}

#[cfg(test)]
impl heapsize::HeapSizeOf for SetU32 {
    fn heap_size_of_children(&self) -> usize {
        match self.internal() {
            Internal::Empty => 0,
            Internal::Stack(_) => 0,
            Internal::Heap { a, .. } => std::mem::size_of::<S>() - 4 + a.len() * 4,
            Internal::Big { a, .. } => std::mem::size_of::<S>() - 4 + a.len() * 4,
            Internal::Dense { a, .. } => std::mem::size_of::<S>() - 4 + a.len() * 4,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use heapsize::HeapSizeOf;

    fn check_set(elems: &[u32]) {
        println!("\n\ncheck_set {:?}\n", elems);
        let mut s = SetU32::default();
        s.debug_me("default set");
        let mut count = 0;
        for x in elems.iter().cloned() {
            let was_here = s.contains(x);
            s.debug_me(&format!("\n\n\nabout to insert {}", x));
            let changed_something = s.insert(x);
            s.debug_me(&format!("\n\nafter inserting {}", x));
            if changed_something {
                count += 1;
                println!("    {} is new now count {}", x, count);
            }
            assert_eq!(!was_here, changed_something);
            s.debug_me(&format!("after inserting {} length is {}", x, s.len()));
            println!("what is this? count {} does it have {}?", count, x);
            assert!(s.contains(x));
            assert_eq!(s.len(), count);
            assert_eq!(s.iter().count(), count);
        }
        assert!(elems.len() >= s.len());
        assert_eq!(elems.iter().cloned().min(), s.iter().min());
        println!("set {:?} with length {}", elems, s.len());
        for x in s.iter() {
            println!("    {}", x);
        }
        s.debug_me("finally");
        assert_eq!(s.iter().count(), s.len());
        for x in s.iter() {
            println!("looking for {}", x);
            assert!(elems.contains(&x));
        }
        for x in s.iter() {
            println!("found {}", x);
            assert!(elems.contains(&x));
        }
        for x in elems.iter().cloned() {
            println!("YYYY looking for {}", x);
            assert!(s.contains(x));
        }
        for x in elems.iter().cloned() {
            println!("removing {}", x);
            s.remove(x);
            s.debug_me("  after remove");
        }
        for x in elems.iter().cloned() {
            println!("XXXX looking for {}", x);
            assert!(!s.contains(x));
        }
        s.debug_me("after everything was removed");
        assert_eq!(s.len(), 0);
        check_size(elems);
    }

    #[test]
    fn check_sets() {
        check_set(&[0, 1, 2, 3, 4, 5, 6, 7 << 27, 8, 9, 10]);
        check_set(&[]);
        check_set(&[10]);
        check_set(&[1024]);
        check_set(&[1]);
        check_set(&[0]);
        check_set(&[0, 1]);

        check_set(&[0, 1, 2, 3]);
        check_set(&[0, 1, 0, 2, 3]);

        check_set(&[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);

        check_set(&[0, 1, 2, 3, 4, 5, 6, 7 << 30, 8, 9, 10]);

        check_set(&[0, 1024]);
        check_set(&[0, 1024, 1 << 31]);
        check_set(&[0, 1024, 1 << 30]);
    }

    fn check_tiny_from_vec(slice: Vec<u32>) {
        println!("\n\n\ncheck_tiny_from_vec({:?})", slice);
        let mut t = Tiny::from_singleton(slice[0]).unwrap();
        t.debug_me("to start with");
        let mut count = 1;
        let mut included = Vec::new();
        for x in slice.iter().cloned() {
            t.debug_me(&format!("starting in on {}", x));
            assert_eq!(t.clone().any(|e| e == x), t.contains(x));
            let already = t.clone().any(|e| e == x);
            let next = t.insert(x);
            if let Some(tt) = next {
                tt.debug_me("    inserting gives");
                assert_eq!(tt.sz == t.sz, already);
                if tt.sz != t.sz {
                    count += 1;
                    included.push(x);
                }
                t = tt;
                t.debug_me("hello");
                assert!(t.clone().any(|e| e == x));
                assert!(t.insert(x).is_some()); // inserting a second time must succeeed
                t.debug_me("hello again");
                assert_eq!(t.sz, count);
                for xx in t.clone() {
                    assert!(t.contains(xx));
                    assert!(t.clone().any(|x| x == xx));
                }
            } else {
                assert!(!already);
            }
            t.debug_me("at end of loop");
        }
        for x in included.iter().cloned() {
            assert!(t.contains(x));
        }
    }
    #[test]
    fn check_specific_tinies() {
        check_tiny_from_vec(vec![49, 50, 1]);
        check_tiny_from_vec(vec![1]);
        check_tiny_from_vec(vec![1, 2]);
        check_tiny_from_vec(vec![1000000, 1000030]);
        check_tiny_from_vec(vec![2, 3, 143, 251, 1, 130, 251]);
        check_tiny_from_vec(vec![1, 130, 131, 132, 133, 251]);
    }

    use proptest::prelude::*;
    proptest! {
        #[test]
        fn copycheck_random_sets(slice in prop::collection::vec(1u32..5, 1usize..10)) {
            crate::copyset::check_set::<SetU32>(&slice);
        }
        #[test]
        fn copycheck_medium_sets(slice in prop::collection::vec(1u32..255, 1usize..100)) {
            crate::copyset::check_set::<SetU32>(&slice);
        }
        #[test]
        fn copycheck_big_sets(slice: Vec<u32>) {
            crate::copyset::check_set::<SetU32>(&slice);
        }
        #[test]
        fn check_random_sets(slice in prop::collection::vec(1u32..5, 1usize..10)) {
            check_set(&slice);
        }
        #[test]
        fn check_medium_sets(slice in prop::collection::vec(1u32..255, 1usize..100)) {
            check_set(&slice);
        }
        #[test]
        fn check_big_sets(slice: Vec<u32>) {
            check_set(&slice);
        }
        #[test]
        fn check_tiny(slice in prop::collection::vec(1u32..0xfffffff, 1usize..9)) {
            check_tiny_from_vec(slice);
        }
    }

    fn total_size_of<T: HeapSizeOf>(x: &T) -> usize {
        std::mem::size_of::<T>() + x.heap_size_of_children()
    }

    fn check_size(v: &[u32]) {
        let s: SetU32 = v.iter().cloned().collect();
        let hs: std::collections::HashSet<_> = v.iter().cloned().collect();
        // let bs: std::collections::BTreeSet<_> = v.iter().cloned().collect();
        println!("setu32: {}", total_size_of(&s));
        println!("hashst: {}", total_size_of(&hs));
        // println!("btrees: {}", total_size_of(&bs));
        assert!(total_size_of(&s) < total_size_of(&hs));
        // assert!(total_size_of(&s) < total_size_of(&bs));
    }

    #[cfg(test)]
    fn collect_size_is(v: &[u32], sz: usize) {
        let s: SetU32 = v.iter().cloned().collect();
        println!("collect_size_is {:?} == {} =? {}", v, total_size_of(&s), sz);
        println!("    num elements = {}", s.len());
        assert_eq!(total_size_of(&s), sz);
    }

    #[cfg(test)]
    fn incremental_size_le(v: &[u32], sz: usize) {
        // repeat the size tests because our expansion is random in a
        // lame attempt to foil DOS attacks.
        for _ in 1..1000 {
            let sc: SetU32 = v.iter().cloned().collect();
            if total_size_of(&sc) > sz {
                println!("collect_size {:?} = {} > {}", v, total_size_of(&sc), sz);
            }
            assert!(total_size_of(&sc) <= sz);

            let mut s = SetU32::new();
            let mut vv = Vec::new();
            for x in v.iter().cloned() {
                s.insert(x);
                vv.push(x);
                if total_size_of(&s) > sz {
                    println!("incremental_size {:?} = {} > {}", vv, total_size_of(&s), sz);
                }
                assert!(total_size_of(&s) <= sz);
            }
        }
    }

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn test_size() {
        assert_eq!(std::mem::size_of::<S>(), 16);
        collect_size_is(&[0], 8);
        incremental_size_le(&[0], 8);
        collect_size_is(&[], 8);
        incremental_size_le(&[], 8);
        collect_size_is(&[3000], 8);
        collect_size_is(&[1, 2, 3, 4, 5, 6], 8);
        incremental_size_le(&[1, 2, 3, 4, 5, 6], 8);
        collect_size_is(&[1000, 1002, 1004, 1006, 1008], 8);
        incremental_size_le(&[1000, 1002, 1004, 1006, 1008], 8);
        collect_size_is(&[255, 260, 265, 270, 275, 280], 8);
        collect_size_is(&[1000, 1002, 1004, 1006, 1008, 1009], 8);

        incremental_size_le(&(1..30).collect::<Vec<_>>(), 40);
        incremental_size_le(&(1..60).collect::<Vec<_>>(), 40);

        incremental_size_le(&(1..160).collect::<Vec<_>>(), 80);

        incremental_size_le(&(0..100).map(|x| x * 10).collect::<Vec<_>>(), 400);
    }

    #[cfg(target_pointer_width = "32")]
    #[test]
    fn test_size() {
        assert_eq!(std::mem::size_of::<S>(), 16);
        collect_size_is(&[0], 4);
        incremental_size_le(&[0], 4);
        collect_size_is(&[], 4);
        incremental_size_le(&[], 4);
        collect_size_is(&[3000], 4);
        collect_size_is(&[1, 2, 3, 4, 5, 6], 4);
        incremental_size_le(&[1, 2, 3, 4, 5, 6], 4);
        collect_size_is(&[1000, 1002, 1004, 1006, 1008], 4);
        incremental_size_le(&[1000, 1002, 1004, 1006, 1008], 4);

        collect_size_is(&[255, 260, 265, 270, 275, 280], 4);
        collect_size_is(&[255, 260, 265, 270, 275, 280, 285], 28);

        incremental_size_le(&(1..30).collect::<Vec<_>>(), 32);
        incremental_size_le(&(1..60).collect::<Vec<_>>(), 32);

        incremental_size_le(&(1..160).collect::<Vec<_>>(), 84);

        incremental_size_le(&(0..100).map(|x| x * 10).collect::<Vec<_>>(), 400);
    }

    fn check_set_primitives(elems: &[u32]) {
        let sz = elems.len();
        println!("\n\nprimitives: {:?}\n", elems);
        let mut a = vec![0; sz];
        for x in elems.iter().cloned() {
            if p_lookfor(x, &a, 0).key_found() {
                println!("we already have {}", x);
            } else {
                let i = p_insert(x, &mut a, 0);
                a[i] = x;
            }
            println!("    after inserting {} we have {:?}", x, a);
            assert!(p_lookfor(x, &a, 0).key_found());
        }
        for x in elems.iter().cloned() {
            println!("looking for {}", x);
            assert!(p_lookfor(x, &a, 0).key_found());
        }
        for x in elems.iter().cloned() {
            println!("removing {}", x);
            p_remove(x, &mut a, 0);
            println!("    after removing {} we have {:?}", x, a);
        }
        for x in elems.iter().cloned() {
            println!("XXXX looking for {}", x);
            assert!(p_lookfor(x, &a, 0).empty_spot());
        }
        println!("after everything was removed: {:?}", a);
        for x in a.iter().cloned() {
            assert_eq!(x, 0);
        }
    }

    proptest! {
        #[test]
        fn check_random_primitives(slice in prop::collection::vec(1u32..50, 1usize..100)) {
            check_set_primitives(&slice);
        }
    }
}

fn p_poverty(k: u32, idx: usize, n: usize) -> usize {
    ((idx % n) + n - (k % n as u32) as usize) % n
}

/// This inserts k into the array, and requires that there be room for
/// one more element.  Otherwise, things will be sad.
fn p_insert(k: u32, a: &mut [u32], offset: u32) -> usize {
    let n = a.len();
    for pov in 0..n {
        // The cast to u64 below avoids overflow issues when `k` is close
        // to `u32::MAX`.  It should cost nothing on 64-bit machines, and
        // shouldn't be much worse than an extra `%` on 32-bit machines.
        let ii = ((k as u64 + pov as u64) % n as u64) as usize;
        let ki = a[ii] >> offset;
        let pov_ki = p_poverty(ki, ii, n);
        if a[ii] == 0 || ki == k {
            // println!("already got a spot");
            return ii;
        } else if pov_ki < pov {
            // println!("need to steal from {} < {} at spot {}", pov_ki, pov, ii);
            // need to steal
            let stolen = ii;
            let mut displaced = a[ii];
            // println!("displaced value is {}", displaced);
            let mut pov_displaced = pov_ki;
            a[stolen] = 0;

            for j in 1..n {
                pov_displaced += 1;
                let jj = (stolen + j) % n;
                let kj = a[jj] >> offset;
                let pov_kj = p_poverty(kj, jj, n);
                if a[jj] == 0 {
                    // We finally found an unoccupied spot!
                    // println!("put the displaced at {}", jj);
                    a[jj] = displaced;
                    return stolen;
                }
                if pov_kj < pov_displaced {
                    // need to steal again!
                    std::mem::swap(&mut a[jj], &mut displaced);
                    pov_displaced = pov_kj;
                }
            }
            panic!("p_insert was called when there was no room!")
        }
    }
    unreachable!()
}

#[test]
fn test_insert() {
    let mut a = [0, 0, 0, 0];
    assert_eq!(2, p_insert(2, &mut a, 0));
    assert_eq!(&a, &[0, 0, 0, 0]);
    for i in 0..10 {
        assert_eq!(0, a[p_insert(i, &mut a, 0)]);
    }

    let mut a = [0, 0, 6, 0];
    assert_eq!(3, p_insert(2, &mut a, 0));
    assert_eq!(&a, &[0, 0, 6, 0]);
    for i in 0..10 {
        assert!([0, i].contains(&a[p_insert(i, &mut a, 0)]));
    }

    let mut a = [0, 0, 6, 3];
    assert_eq!(3, p_insert(2, &mut a, 0));
    assert_eq!(&a, &[3, 0, 6, 0]);
    for i in 0..10 {
        assert!([0, i].contains(&a[p_insert(i, &mut a, 0)]));
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum LookedUp {
    EmptySpot(usize),
    KeyFound(usize),
    NeedInsert,
}
impl LookedUp {
    fn key_found(self) -> bool {
        if let LookedUp::KeyFound(_) = self {
            true
        } else {
            false
        }
    }
    #[cfg(test)]
    fn empty_spot(self) -> bool {
        if let LookedUp::EmptySpot(_) = self {
            true
        } else {
            false
        }
    }
    #[cfg(test)]
    fn unwrap(self) -> usize {
        if let LookedUp::KeyFound(idx) = self {
            idx
        } else {
            panic!("unwrap called on {:?}", self)
        }
    }
}

fn p_lookfor(k: u32, a: &[u32], offset: u32) -> LookedUp {
    let n = a.len();
    for pov in 0..n {
        // The cast to u64 below avoids overflow issues when `k` is close
        // to `u32::MAX`.  It should cost nothing on 64-bit machines, and
        // shouldn't be much worse than an extra `%` on 32-bit machines.
        let ii = ((k as u64 + pov as u64) % n as u64) as usize;
        // println!("looking in spot ii = {} with pov={}", ii, pov);
        if a[ii] == 0 {
            // println!("got empty spot at {} for key {}", ii, k);
            return LookedUp::EmptySpot(ii);
        }
        let ki = a[ii] >> offset;
        let pov_ki = p_poverty(ki, ii, n);
        if ki == k {
            // println!("lookfor already got a spot");
            return LookedUp::KeyFound(ii);
        } else if pov_ki < pov {
            // println!("at index {} we have {} > {}", ii, pov, pov_ki);
            return LookedUp::NeedInsert;
        }
    }
    LookedUp::NeedInsert
}

#[test]
fn test_lookfor() {
    assert_eq!(LookedUp::NeedInsert, p_lookfor(5, &[3, 1, 2], 0));
    assert_eq!(LookedUp::NeedInsert, p_lookfor(5, &[3, 0, 2], 0));
    assert_eq!(LookedUp::KeyFound(3), p_lookfor(7, &[0, 0, 0, 7], 0));
}

fn p_remove(k: u32, a: &mut [u32], offset: u32) -> bool {
    let n = a.len();
    for i in 0..n {
        // The cast to u64 below avoids overflow issues when `k` is close
        // to `u32::MAX`.  It should cost nothing on 64-bit machines, and
        // shouldn't be much worse than an extra `%` on 32-bit machines.
        let ii = ((k as u64 + i as u64) % n as u64) as usize;
        // println!("    looking to remove at distance {} slot {}", i, ii);
        if a[ii] == 0 {
            return false;
        }
        let ki = a[ii] >> offset;
        let iki = (((ii + n) as u32 - (ki % n as u32)) % n as u32) as usize;
        if i > iki {
            return false;
        } else if ki == k {
            // println!("found {} at location {}", k, ii);
            a[ii] = 0;
            // Now we need to return anything that might have been
            // stolen from... to massacre my grammar.
            let mut previous = ii;
            for j in 1..n {
                let jj = (ii + j) % n;
                // println!("looking at removing offset {} at location {}", j, jj);
                let kj = a[jj] >> offset;
                let pov_kj = p_poverty(kj, jj, n);
                if a[jj] == 0 || pov_kj == 0 {
                    // We found an unoccupied spot or a perfectly
                    // happy customer, so nothing else could have been
                    // bumped.
                    return true;
                }
                // need to undo some stealing!
                a[previous] = a[jj];
                a[jj] = 0;
                previous = jj;
            }
            return true;
        }
    }
    panic!("bug: we should have had space in {:?} for {}", a, k)
}

#[cfg(test)]
fn test_insert_remove(x: u32, a: &mut [u32]) {
    println!("test_insert_remove({}, {:?})", x, a);
    let v: Vec<u32> = a.iter().cloned().collect();
    assert!(!p_remove(x, a, 0));
    assert!(!a.contains(&x)); // otherwise the test won't work right.
    assert!(!p_lookfor(x, a, 0).key_found());
    a[p_insert(x, a, 0)] = x;
    assert!(a.contains(&x));
    println!("  after insertion of {} a is {:?}", x, a);
    assert!(p_lookfor(x, a, 0).key_found());
    assert_eq!(x, a[p_lookfor(x, a, 0).unwrap()]);
    assert!(p_remove(x, a, 0));
    println!("  after remove of {} a is {:?}", x, a);
    assert_eq!(a, &v[..]);
}

#[test]
fn test_remove() {
    let mut a = [0, 0, 2];
    a[p_insert(5, &mut a, 0)] = 5;
    assert_eq!(&[5, 0, 2], &a);
    p_remove(2, &mut a, 0);
    println!("after removal {:?}", a);
    assert!(p_lookfor(5, &a, 0).key_found());
    assert_eq!(&[0, 0, 5], &a);

    test_insert_remove(7, &mut [0, 0, 0, 0]);

    test_insert_remove(2, &mut [0, 1, 5, 0]);

    test_insert_remove(5, &mut [0, 0, 2]);
}

#[test]
#[should_panic]
fn test_alloc_failure() {
    SetU32::with_capacity_and_bits(usize::MAX / 8 - 2, 0);
}
