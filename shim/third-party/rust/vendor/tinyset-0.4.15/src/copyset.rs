pub trait CopySet: Default + Clone {
    type Item: Copy + Eq + Ord + std::fmt::Display + std::fmt::Debug;
    type Iter: Iterator<Item = Self::Item>;
    fn ins(&mut self, e: Self::Item) -> bool;
    fn rem(&mut self, e: Self::Item) -> bool;
    fn con(&self, e: Self::Item) -> bool;
    fn vec(&self) -> Vec<Self::Item>;
    fn ln(&self) -> usize;
    fn it(self) -> Self::Iter;
}

macro_rules! impl_set_methods {
    ($ty: ty) => {
impl PartialEq for $ty {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for i in self.iter() {
            if !other.contains(i) {
                return false;
            }
        }
        true
    }
}
impl Eq for $ty {}

impl std::fmt::Debug for $ty {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, concat!(stringify!($ty), " {:?}"), self.iter().collect::<Vec<_>>())?;
        Ok(())
    }
}

impl<'a, 'b> std::ops::Sub<&'b $ty> for &'a $ty {
    type Output = $ty;
    #[doc = concat!(
        "Returns the difference of `self` and `rhs` as a new `", stringify!($ty), "`.

# Examples

```
let a: tinyset::",  stringify!($ty), " = (1..4).collect();
let b: tinyset::",  stringify!($ty), " = (3..6).into_iter().collect();

assert_eq!(&a - &b, (1..3).collect());
```
"
    )]
    fn sub(self, rhs: &$ty) -> $ty {
        let mut s = <$ty>::with_capacity_of(&self);
        for v in self.iter() {
            if !rhs.contains(v) {
                s.insert(v);
            }
        }
        s
    }
}

impl<'b> std::ops::Sub<&'b $ty> for $ty {
    type Output = $ty;
    #[doc = concat!(
        "Returns the difference of `self` and `rhs` as a new `", stringify!($ty), "` consuming `self`.  Should not allocate.

# Examples

```
let a: tinyset::",  stringify!($ty), " = (1..4).collect();
let b: tinyset::",  stringify!($ty), " = (3..6).into_iter().collect();

assert_eq!(a - &b, (1..3).collect());
```
"
    )]
    fn sub(mut self, rhs: &$ty) -> $ty {
        for v in rhs.iter() {
            self.remove(v);
        }
        self
    }
}

impl<'a, 'b> std::ops::BitOr<&'b $ty> for &'a $ty {
    type Output = $ty;
    #[doc = concat!(
        "Returns the union of `self` and `rhs` as a new `", stringify!($ty), "`.

# Examples

```
let a: tinyset::",  stringify!($ty), " = (1..4).collect();
let b: tinyset::",  stringify!($ty), " = (3..6).collect();

assert_eq!(&a | &b, (1..6).collect());
```
"
    )]
    fn bitor(self, rhs: & $ty) -> $ty {
        let mut s: $ty = if self.len() > rhs.len() {
            <$ty>::with_capacity_of(&self)
        } else {
            <$ty>::with_capacity_of(&rhs)
        };
        for x in self.iter() {
            s.insert(x);
        }
        for x in rhs.iter() {
            s.insert(x);
        }
        s
    }
}

impl<'b> std::ops::BitOr<&'b $ty> for $ty {
    type Output = $ty;
    #[doc = concat!(
        "Returns the union of `self` and `rhs` as a new `", stringify!($ty), "`, consuming `self`.

# Examples

```
let a: tinyset::",  stringify!($ty), " = (1..4).collect();
let b: tinyset::",  stringify!($ty), " = (3..6).collect();

assert_eq!(a | &b, (1..6).collect());
```
"
    )]
    fn bitor(mut self, rhs: & $ty) -> $ty {
        for x in rhs.iter() {
            self.insert(x);
        }
        self
    }
}

}
}

pub(crate) use impl_set_methods;

impl CopySet for std::collections::HashSet<u64> {
    type Item = u64;
    type Iter = std::collections::hash_set::IntoIter<u64>;
    fn ins(&mut self, e: u64) -> bool {
        self.insert(e)
    }
    fn rem(&mut self, e: u64) -> bool {
        self.remove(&e)
    }
    fn con(&self, e: u64) -> bool {
        self.contains(&e)
    }
    fn vec(&self) -> Vec<u64> {
        self.iter().cloned().collect()
    }
    fn ln(&self) -> usize {
        self.len()
    }
    fn it(self) -> Self::Iter {
        self.into_iter()
    }
}

impl CopySet for std::collections::HashSet<u32> {
    type Item = u32;
    type Iter = std::collections::hash_set::IntoIter<u32>;
    fn ins(&mut self, e: u32) -> bool {
        self.insert(e)
    }
    fn rem(&mut self, e: u32) -> bool {
        self.remove(&e)
    }
    fn con(&self, e: u32) -> bool {
        self.contains(&e)
    }
    fn vec(&self) -> Vec<u32> {
        self.iter().cloned().collect()
    }
    fn ln(&self) -> usize {
        self.len()
    }
    fn it(self) -> Self::Iter {
        self.into_iter()
    }
}

#[cfg(test)]
use proptest::prelude::*;
#[cfg(test)]
proptest! {
    #[test]
    fn check_random_sets(slice in prop::collection::vec(1u64..5, 1usize..10)) {
        check_set::<std::collections::HashSet<u64>>(&slice);
    }
    #[test]
    fn check_medium_sets(slice in prop::collection::vec(1u64..255, 1usize..100)) {
        check_set::<std::collections::HashSet<u64>>(&slice);
    }
    #[test]
    fn check_big_sets(slice: Vec<u64>) {
        check_set::<std::collections::HashSet<u64>>(&slice);
    }
}

#[cfg(test)]
pub fn check_set<T: CopySet>(elems: &[T::Item]) {
    println!("\n\ncheck_set {:?}\n", elems);
    let mut s = T::default();
    let mut count = 0;
    for x in elems.iter().cloned() {
        let was_here = s.con(x);
        let changed_something = s.ins(x);
        if changed_something {
            count += 1;
            println!("    {} is new now count {}", x, count);
        }
        assert_eq!(!was_here, changed_something);
        println!("what is this? count {} does it have {}?", count, x);
        assert!(s.con(x));
        assert_eq!(s.ln(), count);
        assert_eq!(s.vec().into_iter().count(), count);
    }
    assert!(elems.len() >= s.ln());
    assert_eq!(elems.iter().cloned().min(), s.vec().into_iter().min());
    println!("set {:?} with length {}", elems, s.ln());
    for x in s.vec().into_iter() {
        println!("    {}", x);
    }
    assert_eq!(s.vec().into_iter().count(), s.ln());
    for x in s.vec().into_iter() {
        println!("looking for {}", x);
        assert!(elems.contains(&x));
    }
    for x in s.vec().into_iter() {
        println!("found {}", x);
        assert!(elems.contains(&x));
    }
    for x in s.clone().it() {
        println!("found {}", x);
        assert!(elems.contains(&x));
    }
    println!("checking max");
    assert_eq!(s.clone().it().max(), elems.iter().cloned().max());
    println!("checking min");
    assert_eq!(s.clone().it().min(), elems.iter().cloned().min());
    for x in elems.iter().cloned() {
        println!("YYYY looking for {}", x);
        assert!(s.con(x));
    }
    for x in elems.iter().cloned() {
        println!("removing {}", x);
        s.rem(x);
    }
    for x in elems.iter().cloned() {
        println!("XXXX looking for {}", x);
        assert!(!s.con(x));
    }
    assert_eq!(s.ln(), 0);
}
