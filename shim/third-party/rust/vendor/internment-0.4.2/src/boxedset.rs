use core::ops::Deref;
use hashbrown::HashMap;
use std::{
    borrow::Borrow,
    hash::{BuildHasher, Hash, Hasher},
};
pub struct HashSet<P>(HashMap<P, ()>);

impl<P: Deref + Eq + Hash> Default for HashSet<P> {
    fn default() -> Self {
        HashSet::new()
    }
}

impl<P: Deref + Eq + Hash> HashSet<P> {
    pub fn new() -> Self {
        HashSet(HashMap::new())
    }
    pub fn get<'a, Q: ?Sized + Eq + Hash>(&'a self, key: &Q) -> Option<&'a P>
    where
        P::Target: Borrow<Q>,
    {
        let hash = {
            let mut hasher = self.0.hasher().build_hasher();
            key.hash(&mut hasher);
            hasher.finish()
        };
        self.0
            .raw_entry()
            .from_hash(hash, |k| <P::Target as Borrow<Q>>::borrow(k) == key)
            .as_ref()
            .map(|kv| kv.0)
    }
    pub fn _take<Q: ?Sized + Hash + Eq>(&mut self, k: &Q) -> Option<P>
    where
        P: Borrow<Q>,
    {
        self.0.remove_entry(k).map(|(a, ())| a)
        // let hash = {
        //     let mut hasher = self.0.hasher().build_hasher();
        //     key.hash(&mut hasher);
        //     hasher.finish()
        // };
        // let x = self.0.raw_entry_mut().from_hash(hash, |k| <P::Target as Borrow<Q>>::borrow(k) == key)
    }
    pub fn insert(&mut self, x: P) {
        self.0.insert(x, ());
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
}
