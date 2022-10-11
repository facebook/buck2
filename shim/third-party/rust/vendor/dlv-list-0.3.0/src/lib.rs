use std::cmp::Ordering;
use std::collections::hash_map::RandomState;
use std::collections::{HashMap, LinkedList};
use std::fmt::{self, Debug, Formatter};
use std::hash::{BuildHasher, Hash, Hasher};
use std::iter::{FromIterator, FusedIterator};
use std::marker::PhantomData;
use std::num::NonZeroUsize;
use std::{mem, ops};

/// A semi-doubly linked list implemented with a vector.
///
/// This provides many of the benefits of an actual linked list with a few tradeoffs. First, due to
/// the use of an underlying vector, an individual insert operation may be O(n) due to allocating
/// more space for the vector. However, it is amortized O(1) and it avoids the frequent allocation
/// that traditional linked lists suffer from.
///
/// Another tradeoff is that extending a traditional linked list with another list is O(1) but a
/// vector based implementation is O(n). Splicing has a similar disadvantage.
///
/// Lastly, the vector based implementation is likely to have better cache locality in general.
#[derive(Clone)]
pub struct VecList<EntryData> {
    /// The backing storage for the list. This includes both used and unused indices.`
    entries: Vec<Entry<EntryData>>,

    /// The current generation of the list. This is used to avoid the ABA problem.
    generation: u64,

    /// The index of the head of the list.
    head: Option<NonZeroUsize>,

    /// The length of the list since we cannot rely on the length of [`VecList::entries`] because
    /// it includes unused indices.
    length: usize,

    /// The index of the tail of the list.
    tail: Option<NonZeroUsize>,

    /// The index of the head of the vacant indices.
    vacant_head: Option<NonZeroUsize>,
}

impl<EntryData> VecList<EntryData> {
    /// Returns an immutable reference to the value at the back of the list, if it exists.
    ///
    /// Complexity: O(1)
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// assert_eq!(list.back(), None);
    ///
    /// list.push_back(0);
    /// list.push_back(5);
    /// assert_eq!(list.back(), Some(&5));
    /// ```
    pub fn back(&self) -> Option<&EntryData> {
        let index = self.tail()?;

        match &self.entries[index] {
            Entry::Occupied(entry) => Some(&entry.value),
            _ => None,
        }
    }

    /// Returns a mutable reference to the value at the back of the list, if it exists.
    ///
    /// Complexity: O(1)
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// assert_eq!(list.back_mut(), None);
    ///
    /// list.push_back(0);
    /// list.push_back(5);
    ///
    /// let mut back = list.back_mut().unwrap();
    /// assert_eq!(back, &mut 5);
    /// *back *= 2;
    ///
    /// assert_eq!(list.back(), Some(&10));
    /// ```
    pub fn back_mut(&mut self) -> Option<&mut EntryData> {
        let index = self.tail()?;

        match &mut self.entries[index] {
            Entry::Occupied(entry) => Some(&mut entry.value),
            _ => None,
        }
    }

    /// Returns the capacity of the list.
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let list: VecList<u32> = VecList::new();
    /// assert_eq!(list.capacity(), 0);
    ///
    /// let list: VecList<u32> = VecList::with_capacity(10);
    /// assert_eq!(list.capacity(), 10);
    /// ```
    pub fn capacity(&self) -> usize {
        self.entries.capacity()
    }

    /// Removes all values from the list and invalidates all existing indices.
    ///
    /// Complexity: O(n)
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    ///
    /// list.push_back(5);
    /// assert!(!list.is_empty());
    ///
    /// list.clear();
    /// assert!(list.is_empty());
    /// ```
    pub fn clear(&mut self) {
        self.entries.clear();
        self.generation = self.generation.wrapping_add(1);
        self.head = None;
        self.length = 0;
        self.tail = None;
        self.vacant_head = None;
    }

    /// Returns whether or not the list contains the given value.
    ///
    /// Complexity: O(n)
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// assert!(!list.contains(&0));
    ///
    /// list.push_back(0);
    /// assert!(list.contains(&0));
    /// ```
    pub fn contains(&self, value: &EntryData) -> bool
    where
        EntryData: PartialEq,
    {
        self.iter().any(|entry| entry == value)
    }

    /// Creates a draining iterator that removes all values from the list and yields them in order.
    ///
    /// All values are removed even if the iterator is only partially consumed or not consumed at
    /// all.
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// list.push_back(0);
    /// list.push_back(5);
    ///
    /// {
    ///     let mut iter = list.drain();
    ///     assert_eq!(iter.next(), Some(0));
    ///     assert_eq!(iter.next(), Some(5));
    ///     assert_eq!(iter.next(), None);
    /// }
    ///
    /// println!("{}", list.len());
    /// assert!(list.is_empty());
    /// ```
    pub fn drain(&mut self) -> Drain<EntryData> {
        Drain {
            head: self.head(),
            remaining: self.length,
            tail: self.tail(),
            list: self,
        }
    }

    /// Returns an immutable reference to the value at the front of the list, if it exists.
    ///
    /// Complexity: O(1)
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// assert_eq!(list.front(), None);
    ///
    /// list.push_front(0);
    /// list.push_front(5);
    /// assert_eq!(list.front(), Some(&5));
    /// ```
    pub fn front(&self) -> Option<&EntryData> {
        let index = self.head()?;

        match &self.entries[index] {
            Entry::Occupied(entry) => Some(&entry.value),
            _ => None,
        }
    }

    /// Returns a mutable reference to the value at the front of the list, if it exists.
    ///
    /// Complexity: O(1)
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// assert_eq!(list.front_mut(), None);
    ///
    /// list.push_front(0);
    /// list.push_front(5);
    ///
    /// let mut front = list.front_mut().unwrap();
    /// assert_eq!(front, &mut 5);
    /// *front *= 2;
    ///
    /// assert_eq!(list.front(), Some(&10));
    /// ```
    pub fn front_mut(&mut self) -> Option<&mut EntryData> {
        let index = self.head()?;

        match &mut self.entries[index] {
            Entry::Occupied(entry) => Some(&mut entry.value),
            _ => None,
        }
    }

    /// Returns an immutable reference to the value at the given index.
    ///
    /// If the index refers to an index not in the list anymore or if the index has been
    /// invalidated, then [`None`] will be returned.
    ///
    /// Complexity: O(1)
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// let index = list.push_front(0);
    /// assert_eq!(list.get(index), Some(&0));
    ///
    /// let index = list.push_front(5);
    /// assert_eq!(list.get(index), Some(&5));
    /// ```
    pub fn get(&self, index: Index<EntryData>) -> Option<&EntryData> {
        match self.entries.get(index.index)? {
            Entry::Occupied(entry) if entry.generation == index.generation => Some(&entry.value),
            _ => None,
        }
    }

    /// Returns a mutable reference to the value at the given index.
    ///
    /// If the index refers to an index not in the list anymore or if the index has been
    /// invalidated, then [`None`] will be returned.
    ///
    /// Complexity: O(1)
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// let index = list.push_front(0);
    /// let value = list.get_mut(index).unwrap();
    /// *value = 100;
    /// assert_eq!(list.get(index), Some(&100));
    /// ```
    pub fn get_mut(&mut self, index: Index<EntryData>) -> Option<&mut EntryData> {
        match self.entries.get_mut(index.index)? {
            Entry::Occupied(entry) if entry.generation == index.generation => {
                Some(&mut entry.value)
            }
            _ => None,
        }
    }

    /// Returns the index of the value next to the value at the given index.
    ///
    /// If the index refers to an index not in the list anymore or if the index has been
    /// invalidated, then [`None`] will be returned.
    ///
    /// Complexity: O(1)
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    ///
    /// let index_1 = list.push_back(0);
    /// assert_eq!(list.get_next_index(index_1), None);
    ///
    /// let index_2 = list.push_back(5);
    /// assert_eq!(list.get_next_index(index_1), Some(index_2));
    /// ```
    pub fn get_next_index(&self, index: Index<EntryData>) -> Option<Index<EntryData>> {
        match self.entries.get(index.index)? {
            Entry::Occupied(entry) if entry.generation == index.generation => {
                let next_index = entry.next?;
                let next_entry = self.entries[next_index].occupied_ref();
                Some(Index::new(next_index, next_entry.generation))
            }
            _ => None,
        }
    }

    /// Returns the index of the value previous to the value at the given index.
    ///
    /// If the index refers to an index not in the list anymore or if the index has been
    /// invalidated, then [`None`] will be returned.
    ///
    /// Complexity: O(1)
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    ///
    /// let index_1 = list.push_front(0);
    /// assert_eq!(list.get_previous_index(index_1), None);
    ///
    /// let index_2 = list.push_front(5);
    /// assert_eq!(list.get_previous_index(index_1), Some(index_2));
    /// ```
    pub fn get_previous_index(&self, index: Index<EntryData>) -> Option<Index<EntryData>> {
        match self.entries.get(index.index)? {
            Entry::Occupied(entry) if entry.generation == index.generation => {
                let previous_index = entry.previous?;
                let previous_entry = self.entries[previous_index].occupied_ref();
                Some(Index::new(previous_index, previous_entry.generation))
            }
            _ => None,
        }
    }

    /// Convenience function for returning the actual head index.
    fn head(&self) -> Option<usize> {
        self.head.map(|head| head.get() - 1)
    }

    /// Creates an indices iterator which will yield all indices of the list in order.
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// list.push_front(0);
    /// list.push_front(5);
    ///
    /// let mut indices = list.indices();
    /// let index = indices.next().unwrap();
    /// assert_eq!(list.get(index), Some(&5));
    ///
    /// let index = indices.next().unwrap();
    /// assert_eq!(list.get(index), Some(&0));
    ///
    /// assert_eq!(indices.next(), None);
    /// ```
    pub fn indices(&self) -> Indices<EntryData> {
        Indices {
            entries: &self.entries,
            head: self.head(),
            remaining: self.length,
            tail: self.tail(),
        }
    }

    /// Inserts the given value after the value at the given index.
    ///
    /// The index of the newly inserted value will be returned.
    ///
    /// Complexity: amortized O(1)
    ///
    /// # Panics
    ///
    /// Panics if the index refers to an index not in the list anymore or if the index has been
    /// invalidated. This is enforced because this function will consume the value to be inserted,
    /// and if it cannot be inserted (due to the index not being valid), then it will be lost.
    ///
    /// Also panics if the new capacity overflows `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// list.push_front(0);
    /// let index_1 = list.push_front(5);
    /// list.push_front(10);
    ///
    /// let index_2 = list.insert_after(index_1, 1000);
    /// assert_eq!(list.get_next_index(index_1), Some(index_2));
    /// ```
    pub fn insert_after(&mut self, index: Index<EntryData>, value: EntryData) -> Index<EntryData> {
        let next_index = match &mut self.entries[index.index] {
            Entry::Occupied(entry) if entry.generation == index.generation => entry.next,
            _ => panic!("expected occupied entry with correct generation"),
        };
        let new_index = self.insert_new(value, Some(index.index), next_index);
        let entry = self.entries[index.index].occupied_mut();
        entry.next = Some(new_index);

        if Some(index.index) == self.tail() {
            self.set_tail(new_index);
        }

        if let Some(next_index) = next_index {
            self.entries[next_index].occupied_mut().previous = Some(new_index);
        }

        Index::new(new_index, self.generation)
    }

    /// Inserts the given value before the value at the given index.
    ///
    /// The index of the newly inserted value will be returned.
    ///
    /// Complexity: amortized O(1)
    ///
    /// # Panics
    ///
    /// Panics if the index refers to an index not in the list anymore or if the index has been
    /// invalidated. This is enforced because this function will consume the value to be inserted,
    /// and if it cannot be inserted (due to the index not being valid), then it will be lost.
    ///
    /// Also panics if the new capacity overflows `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// list.push_front(0);
    /// let index_1 = list.push_front(5);
    /// list.push_front(10);
    ///
    /// let index_2 = list.insert_before(index_1, 1000);
    /// assert_eq!(list.get_previous_index(index_1), Some(index_2));
    /// ```
    pub fn insert_before(&mut self, index: Index<EntryData>, value: EntryData) -> Index<EntryData> {
        let previous_index = match &mut self.entries[index.index] {
            Entry::Occupied(entry) if entry.generation == index.generation => entry.previous,
            _ => panic!("expected occupied entry with correct generation"),
        };
        let new_index = self.insert_new(value, previous_index, Some(index.index));
        let entry = self.entries[index.index].occupied_mut();
        entry.previous = Some(new_index);

        if Some(index.index) == self.head() {
            self.set_head(new_index);
        }

        if let Some(previous_index) = previous_index {
            self.entries[previous_index].occupied_mut().next = Some(new_index);
        }

        Index::new(new_index, self.generation)
    }

    /// Inserts the given value into the list with the assumption that it is currently empty.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity overflows `usize`.
    fn insert_empty(&mut self, value: EntryData) -> Index<EntryData> {
        let generation = self.generation;
        let index = self.insert_new(value, None, None);
        self.set_head(index);
        self.set_tail(index);
        Index::new(index, generation)
    }

    /// Inserts the given value into the list with its expected previous and next value indices.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity overflows `usize`.
    fn insert_new(
        &mut self,
        value: EntryData,
        previous: Option<usize>,
        next: Option<usize>,
    ) -> usize {
        use self::Entry::*;

        self.length += 1;

        if self.length == usize::max_value() {
            panic!("reached maximum possible length");
        }

        match self.vacant_head() {
            Some(index) => {
                self.set_vacant_head(self.entries[index].vacant_ref().next);
                self.entries[index] =
                    Occupied(OccupiedEntry::new(self.generation, previous, next, value));
                index
            }
            None => {
                self.entries.push(Occupied(OccupiedEntry::new(
                    self.generation,
                    previous,
                    next,
                    value,
                )));
                self.entries.len() - 1
            }
        }
    }

    /// Returns whether or not the list is empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// assert!(list.is_empty());
    ///
    /// list.push_back(0);
    /// assert!(!list.is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.length == 0
    }

    /// Creates an iterator that yields immutable references to values in the list in order.
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// list.push_back(0);
    /// list.push_back(10);
    /// list.push_back(200);
    /// list.push_back(-10);
    ///
    /// let mut iter = list.iter();
    /// assert_eq!(iter.next(), Some(&0));
    /// assert_eq!(iter.next(), Some(&10));
    /// assert_eq!(iter.next(), Some(&200));
    /// assert_eq!(iter.next(), Some(&-10));
    /// assert_eq!(iter.next(), None);
    /// ```
    pub fn iter(&self) -> Iter<EntryData> {
        Iter {
            entries: &self.entries,
            head: self.head(),
            remaining: self.length,
            tail: self.tail(),
        }
    }

    /// Creates an iterator that yields mutable references to values in the list in order.
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// list.push_back(0);
    /// list.push_back(10);
    /// list.push_back(200);
    /// list.push_back(-10);
    ///
    /// let mut iter = list.iter_mut();
    /// assert_eq!(iter.next(), Some(&mut 0));
    /// assert_eq!(iter.next(), Some(&mut 10));
    /// assert_eq!(iter.next(), Some(&mut 200));
    /// assert_eq!(iter.next(), Some(&mut -10));
    /// assert_eq!(iter.next(), None);
    /// ```
    pub fn iter_mut(&mut self) -> IterMut<EntryData> {
        IterMut {
            entries: &mut self.entries as *mut _,
            head: self.head(),
            phantom: PhantomData,
            remaining: self.length,
            tail: self.tail(),
        }
    }

    /// Returns the number of values in the list.
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// assert_eq!(list.len(), 0);
    ///
    /// list.push_back(0);
    /// list.push_back(1);
    /// list.push_back(2);
    /// assert_eq!(list.len(), 3);
    /// ```
    pub fn len(&self) -> usize {
        self.length
    }

    /// Creates a new list with no initial capacity.
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// let index = list.push_back(0);
    /// assert_eq!(list.get(index), Some(&0));
    /// ```
    pub fn new() -> Self {
        VecList::default()
    }

    /// Reorganizes the existing values to ensure maximum cache locality and shrinks the list such
    /// that the capacity is exactly [`minimum_capacity`].
    ///
    /// This function can be used to actually increase the capacity of the list.
    ///
    /// Complexity: O(n)
    ///
    /// # Panics
    ///
    /// Panics if the given minimum capacity is less than the current length of the list.
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// let index_1 = list.push_back(5);
    /// let index_2 = list.push_back(10);
    /// let index_3 = list.push_front(100);
    /// list.remove(index_1);
    ///
    /// assert!(list.capacity() >= 3);
    ///
    /// let mut map = list.pack_to(list.len() + 5);
    /// assert_eq!(list.capacity(), 7);
    /// assert_eq!(map.len(), 2);
    ///
    /// let index_2 = map.remove(&index_2).unwrap();
    /// let index_3 = map.remove(&index_3).unwrap();
    ///
    /// assert_eq!(list.get(index_2), Some(&10));
    /// assert_eq!(list.get(index_3), Some(&100));
    ///
    /// let mut iter = list.iter();
    /// assert_eq!(iter.next(), Some(&100));
    /// assert_eq!(iter.next(), Some(&10));
    /// assert_eq!(iter.next(), None);
    /// ```
    pub fn pack_to(
        &mut self,
        minimum_capacity: usize,
    ) -> HashMap<Index<EntryData>, Index<EntryData>> {
        assert!(
            minimum_capacity >= self.length,
            "cannot shrink to capacity lower than current length"
        );

        let mut count = 0;
        let mut entries = Vec::with_capacity(minimum_capacity);
        let generation = create_initial_generation();
        let length = self.length;
        let mut map = HashMap::with_capacity(length);
        let mut next_index = self.head();

        while let Some(index) = next_index {
            let mut entry = self.remove_entry(index).expect("expected occupied entry");
            next_index = entry.next;
            map.insert(
                Index::new(index, entry.generation),
                Index::new(count, generation),
            );

            entry.generation = generation;
            entry.previous = if count > 0 { Some(count - 1) } else { None };
            entry.next = if count < length - 1 {
                Some(count + 1)
            } else {
                None
            };

            entries.push(Entry::Occupied(entry));
            count += 1;
        }

        self.entries = entries;
        self.generation = generation;
        self.length = length;
        self.vacant_head = None;

        if self.length > 0 {
            self.set_head(0);
            self.set_tail(length - 1);
        } else {
            self.head = None;
            self.tail = None;
        }

        map
    }

    /// Reorganizes the existing values to ensure maximum cache locality and shrinks the list such
    /// that no additional capacity exists.
    ///
    /// This is equivalent to calling [`VecList::pack_to`] with the current length.
    ///
    /// Complexity: O(n)
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// let index_1 = list.push_back(5);
    /// let index_2 = list.push_back(10);
    /// let index_3 = list.push_front(100);
    /// list.remove(index_1);
    ///
    /// assert!(list.capacity() >= 3);
    ///
    /// let mut map = list.pack_to_fit();
    /// assert_eq!(list.capacity(), 2);
    /// assert_eq!(map.len(), 2);
    ///
    /// let index_2 = map.remove(&index_2).unwrap();
    /// let index_3 = map.remove(&index_3).unwrap();
    ///
    /// assert_eq!(list.get(index_2), Some(&10));
    /// assert_eq!(list.get(index_3), Some(&100));
    ///
    /// let mut iter = list.iter();
    /// assert_eq!(iter.next(), Some(&100));
    /// assert_eq!(iter.next(), Some(&10));
    /// assert_eq!(iter.next(), None);
    /// ```
    pub fn pack_to_fit(&mut self) -> HashMap<Index<EntryData>, Index<EntryData>> {
        self.pack_to(self.length)
    }

    /// Removes and returns the value at the back of the list, if it exists.
    ///
    /// Complexity: O(1)
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// assert_eq!(list.pop_back(), None);
    ///
    /// list.push_back(0);
    /// list.push_back(1);
    /// list.push_back(2);
    /// assert_eq!(list.len(), 3);
    ///
    /// assert_eq!(list.pop_back(), Some(2));
    /// assert_eq!(list.len(), 2);
    /// ```
    pub fn pop_back(&mut self) -> Option<EntryData> {
        self.remove_entry(self.tail()?).map(|entry| entry.value)
    }

    /// Removes and returns the value at the front of the list, if it exists.
    ///
    /// Complexity: O(1)
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// assert_eq!(list.pop_front(), None);
    ///
    /// list.push_front(0);
    /// list.push_front(1);
    /// list.push_front(2);
    /// assert_eq!(list.len(), 3);
    ///
    /// assert_eq!(list.pop_front(), Some(2));
    /// assert_eq!(list.len(), 2);
    /// ```
    pub fn pop_front(&mut self) -> Option<EntryData> {
        self.remove_entry(self.head()?).map(|entry| entry.value)
    }

    /// Inserts the given value to the back of the list.
    ///
    /// The index of the newly inserted value will be returned.
    ///
    /// Complexity: amortized O(1)
    ///
    /// # Panics
    ///
    /// Panics if the new capacity overflows `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// let index = list.push_back(0);
    /// assert_eq!(list.get(index), Some(&0));
    /// ```
    pub fn push_back(&mut self, value: EntryData) -> Index<EntryData> {
        let tail_index = match self.tail() {
            Some(index) => index,
            None => return self.insert_empty(value),
        };
        let index = self.insert_new(value, Some(tail_index), None);
        self.entries[tail_index].occupied_mut().next = Some(index);
        self.set_tail(index);
        Index::new(index, self.generation)
    }

    /// Inserts the given value to the front of the list.
    ///
    /// The index of the newly inserted value will be returned.
    ///
    /// Complexity: amortized O(1)
    ///
    /// # Panics
    ///
    /// Panics if the new capacity overflows `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// let index = list.push_front(0);
    /// assert_eq!(list.get(index), Some(&0));
    /// ```
    pub fn push_front(&mut self, value: EntryData) -> Index<EntryData> {
        let head_index = match self.head() {
            Some(index) => index,
            None => return self.insert_empty(value),
        };
        let index = self.insert_new(value, None, Some(head_index));
        self.entries[head_index].occupied_mut().previous = Some(index);
        self.set_head(index);
        Index::new(index, self.generation)
    }

    /// Removes and returns the value at the given index, if it exists.
    ///
    /// If the index refers to an index not in the list anymore or if the index has been
    /// invalidated, then [`None`] will be returned and the list will be unaffected.
    ///
    /// Complexity: O(1)
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// let index = list.push_back(0);
    /// assert_eq!(list.remove(index), Some(0));
    /// assert_eq!(list.remove(index), None);
    /// ```
    pub fn remove(&mut self, index: Index<EntryData>) -> Option<EntryData> {
        let (previous_index, next_index) = match &self.entries[index.index] {
            Entry::Occupied(entry) if entry.generation == index.generation => {
                (entry.previous, entry.next)
            }
            _ => return None,
        };
        Some(
            self.remove_helper(previous_index, index.index, next_index)
                .value,
        )
    }

    /// Removes and returns the entry at the given index, if it exists.
    ///
    /// If the index refers to an index not in the list anymore or if the index has been
    /// invalidated, then [`None`] will be returned and the list will be unaffected.
    fn remove_entry(&mut self, index: usize) -> Option<OccupiedEntry<EntryData>> {
        use self::Entry::*;

        let (previous_index, next_index) = match &self.entries[index] {
            Occupied(entry) => (entry.previous, entry.next),
            Vacant(_) => return None,
        };
        Some(self.remove_helper(previous_index, index, next_index))
    }

    /// Removes and returns the entry at the given index with the entries previous and next index
    /// values.
    ///
    /// It is assumed that there is an entry at the given index.
    ///
    /// # Panics
    ///
    /// Panics if called when the list is empty. Behavior is undefined if provided indices do not
    /// follow the expected constraints.
    fn remove_helper(
        &mut self,
        previous_index: Option<usize>,
        index: usize,
        next_index: Option<usize>,
    ) -> OccupiedEntry<EntryData> {
        let head_index = self.head().expect("expected head index");
        let tail_index = self.tail().expect("expected tail index");
        let vacant_head = self.vacant_head();
        let removed_entry = mem::replace(
            &mut self.entries[index],
            Entry::Vacant(VacantEntry::new(vacant_head)),
        );

        self.generation = self.generation.wrapping_add(1);
        self.length -= 1;
        self.set_vacant_head(Some(index));

        if index == head_index && index == tail_index {
            self.head = None;
            self.tail = None;
        } else if index == head_index {
            self.entries[next_index.expect("expected next entry to exist")]
                .occupied_mut()
                .previous = None;
            self.head = next_index.map(|index| NonZeroUsize::new(index + 1).unwrap());
        } else if index == tail_index {
            self.entries[previous_index.expect("expected previous entry to exist")]
                .occupied_mut()
                .next = None;
            self.tail = previous_index.map(|index| NonZeroUsize::new(index + 1).unwrap());
        } else {
            self.entries[next_index.expect("expected next entry to exist")]
                .occupied_mut()
                .previous = previous_index;
            self.entries[previous_index.expect("expected previous entry to exist")]
                .occupied_mut()
                .next = next_index;
        }

        removed_entry.occupied()
    }

    /// Reserves capacity for the given expected size increase.
    ///
    /// The collection may reserve more space to avoid frequent reallocations. After calling this
    /// function, capacity will be greater than or equal to `self.len() + additional_capacity`.
    /// Does nothing if the current capacity is already sufficient.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity overflows `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list: VecList<u32> = VecList::new();
    /// assert_eq!(list.capacity(), 0);
    ///
    /// list.reserve(10);
    /// assert!(list.capacity() >= 10);
    /// ```
    pub fn reserve(&mut self, additional_capacity: usize) {
        self.entries.reserve(additional_capacity);
    }

    /// Removes all elements from the list not satisfying the given predicate.
    ///
    /// Complexity: O(n)
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list = VecList::new();
    /// list.push_back(0);
    /// list.push_back(-1);
    /// list.push_back(1);
    /// list.push_back(-2);
    /// list.retain(|&mut value| value >= 0);
    ///
    /// let mut iter = list.iter();
    /// assert_eq!(iter.next(), Some(&0));
    /// assert_eq!(iter.next(), Some(&1));
    /// assert_eq!(iter.next(), None);
    /// ```
    pub fn retain<Predicate>(&mut self, mut predicate: Predicate)
    where
        Predicate: FnMut(&mut EntryData) -> bool,
    {
        let mut next_index = self.head();

        while let Some(index) = next_index {
            let entry = &mut self.entries[index].occupied_mut();
            next_index = entry.next;

            if !predicate(&mut entry.value) {
                self.remove_entry(index);
            }
        }
    }

    /// Convenience function for returning setting the offset head index.
    fn set_head(&mut self, index: usize) {
        self.head = Some(NonZeroUsize::new(index + 1).expect("head should not be 0"));
    }

    /// Convenience function for returning setting the offset tail index.
    fn set_tail(&mut self, index: usize) {
        self.tail = Some(NonZeroUsize::new(index + 1).expect("tail should not be 0"));
    }

    /// Convenience function for returning setting the offset vacant head index.
    fn set_vacant_head(&mut self, index: Option<usize>) {
        self.vacant_head =
            index.map(|index| NonZeroUsize::new(index + 1).expect("vacant head should not be 0"));
    }

    /// Convenience function for returning the actual tail index.
    fn tail(&self) -> Option<usize> {
        self.tail.map(|tail| tail.get() - 1)
    }

    /// Convenience function for returning the actual vacant head index.
    fn vacant_head(&self) -> Option<usize> {
        self.vacant_head.map(|vacant_head| vacant_head.get() - 1)
    }

    /// Creates a new list with the given capacity.
    ///
    /// # Examples
    ///
    /// ```
    /// use dlv_list::VecList;
    ///
    /// let mut list: VecList<u32> = VecList::new();
    /// assert_eq!(list.capacity(), 0);
    ///
    /// let mut list: VecList<u32> = VecList::with_capacity(10);
    /// assert_eq!(list.capacity(), 10);
    /// ```
    pub fn with_capacity(capacity: usize) -> Self {
        VecList {
            entries: Vec::with_capacity(capacity),
            generation: create_initial_generation(),
            head: None,
            length: 0,
            tail: None,
            vacant_head: None,
        }
    }
}

impl<EntryData> Debug for VecList<EntryData>
where
    EntryData: Debug,
{
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        formatter.debug_list().entries(self.iter()).finish()
    }
}

impl<EntryData> Default for VecList<EntryData> {
    fn default() -> Self {
        VecList {
            entries: Vec::default(),
            generation: create_initial_generation(),
            head: None,
            length: 0,
            tail: None,
            vacant_head: None,
        }
    }
}

impl<EntryData> Eq for VecList<EntryData> where EntryData: Eq {}

impl<EntryData> Extend<EntryData> for VecList<EntryData> {
    fn extend<Iter>(&mut self, iter: Iter)
    where
        Iter: IntoIterator<Item = EntryData>,
    {
        let iter = iter.into_iter();
        self.reserve(iter.size_hint().0);

        for value in iter {
            self.push_back(value);
        }
    }
}

impl<'entries, EntryData> Extend<&'entries EntryData> for VecList<EntryData>
where
    EntryData: 'entries + Copy,
{
    fn extend<Iter>(&mut self, iter: Iter)
    where
        Iter: IntoIterator<Item = &'entries EntryData>,
    {
        self.extend(iter.into_iter().cloned())
    }
}

impl<EntryData> FromIterator<EntryData> for VecList<EntryData> {
    fn from_iter<Iter>(iter: Iter) -> Self
    where
        Iter: IntoIterator<Item = EntryData>,
    {
        let mut list = VecList::new();
        list.extend(iter);
        list
    }
}

impl<EntryData> Hash for VecList<EntryData>
where
    EntryData: Hash,
{
    fn hash<StateHasher>(&self, state: &mut StateHasher)
    where
        StateHasher: Hasher,
    {
        self.len().hash(state);

        for value in self {
            value.hash(state);
        }
    }
}

impl<EntryData> ops::Index<Index<EntryData>> for VecList<EntryData> {
    type Output = EntryData;

    fn index(&self, index: Index<EntryData>) -> &Self::Output {
        self.get(index).expect("expected entry at index")
    }
}

impl<EntryData> ops::IndexMut<Index<EntryData>> for VecList<EntryData> {
    fn index_mut(&mut self, index: Index<EntryData>) -> &mut Self::Output {
        self.get_mut(index).expect("expected entry at index")
    }
}

impl<EntryData> IntoIterator for VecList<EntryData> {
    type IntoIter = IntoIter<EntryData>;
    type Item = EntryData;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            head: self.head(),
            remaining: self.length,
            tail: self.tail(),
            list: self,
        }
    }
}

impl<'entries, EntryData> IntoIterator for &'entries VecList<EntryData> {
    type IntoIter = Iter<'entries, EntryData>;
    type Item = &'entries EntryData;

    fn into_iter(self) -> Self::IntoIter {
        Iter {
            entries: &self.entries,
            head: self.head(),
            remaining: self.length,
            tail: self.tail(),
        }
    }
}

impl<'entries, EntryData> IntoIterator for &'entries mut VecList<EntryData> {
    type IntoIter = IterMut<'entries, EntryData>;
    type Item = &'entries mut EntryData;

    fn into_iter(self) -> Self::IntoIter {
        IterMut {
            entries: &mut self.entries as *mut _,
            head: self.head(),
            phantom: PhantomData,
            remaining: self.length,
            tail: self.tail(),
        }
    }
}

impl<EntryData> Ord for VecList<EntryData>
where
    EntryData: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.iter().cmp(other)
    }
}

impl<EntryData> PartialEq for VecList<EntryData>
where
    EntryData: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().eq(other)
    }
}

impl<EntryData> PartialEq<LinkedList<EntryData>> for VecList<EntryData>
where
    EntryData: PartialEq,
{
    fn eq(&self, other: &LinkedList<EntryData>) -> bool {
        self.len() == other.len() && self.iter().eq(other)
    }
}

impl<EntryData> PartialEq<Vec<EntryData>> for VecList<EntryData>
where
    EntryData: PartialEq,
{
    fn eq(&self, other: &Vec<EntryData>) -> bool {
        self.len() == other.len() && self.iter().eq(other)
    }
}

impl<'slice, EntryData> PartialEq<&'slice [EntryData]> for VecList<EntryData>
where
    EntryData: PartialEq,
{
    fn eq(&self, other: &&'slice [EntryData]) -> bool {
        self.len() == other.len() && self.iter().eq(other.iter())
    }
}

impl<EntryData> PartialOrd for VecList<EntryData>
where
    EntryData: PartialOrd<EntryData>,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.iter().partial_cmp(other)
    }
}

/// A wrapper type that indicates an index into the list.
///
/// This index may be invalidated by operations on the list itself.
pub struct Index<EntryData> {
    /// The generation of the entry currently at this index. This is used to avoid the ABA problem.
    generation: u64,

    /// The actual index into the entry list.
    index: usize,

    /// This type is parameterized on the entry data type to avoid indices being used across
    /// differently typed lists.
    phantom: PhantomData<EntryData>,
}

impl<EntryData> Clone for Index<EntryData> {
    fn clone(&self) -> Self {
        Index {
            generation: self.generation,
            index: self.index,
            phantom: PhantomData,
        }
    }
}

impl<EntryData> Copy for Index<EntryData> {}

impl<EntryData> Debug for Index<EntryData> {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        formatter
            .debug_tuple("Index")
            .field(&self.index)
            .field(&self.generation)
            .finish()
    }
}

impl<EntryData> Eq for Index<EntryData> {}

impl<EntryData> Hash for Index<EntryData> {
    fn hash<StateHasher>(&self, hasher: &mut StateHasher)
    where
        StateHasher: Hasher,
    {
        self.index.hash(hasher);
        self.generation.hash(hasher);
    }
}

impl<EntryData> PartialEq for Index<EntryData> {
    fn eq(&self, other: &Self) -> bool {
        self.generation == other.generation && self.index == other.index
    }
}

impl<EntryData> Index<EntryData> {
    /// Convenience function for creating new index.
    pub(self) fn new(index: usize, generation: u64) -> Index<EntryData> {
        Index {
            generation,
            index,
            phantom: PhantomData,
        }
    }
}

/// An entry in the list. This can be either occupied or vacant.
#[derive(Clone)]
enum Entry<EntryData> {
    /// An occupied entry contains actual entry data inserted by the user.
    Occupied(OccupiedEntry<EntryData>),

    /// A vacant entry is one that can be reused.
    Vacant(VacantEntry),
}

impl<EntryData> Entry<EntryData> {
    /// Returns the occupied entry by moving it out of the entry.
    ///
    /// # Panics
    ///
    /// Panics if the variant is actually [`Entry::Vacant`].
    pub fn occupied(self) -> OccupiedEntry<EntryData> {
        use self::Entry::*;

        match self {
            Occupied(entry) => entry,
            Vacant(_) => panic!("expected occupied entry"),
        }
    }

    /// Returns an immutable reference to the occupied entry.
    ///
    /// # Panics
    ///
    /// Panics if the variant is actually [`Entry::Vacant`].
    pub fn occupied_ref(&self) -> &OccupiedEntry<EntryData> {
        use self::Entry::*;

        match self {
            Occupied(entry) => entry,
            Vacant(_) => panic!("expected occupied entry"),
        }
    }

    /// Returns a mutable reference to the occupied entry.
    ///
    /// # Panics
    ///
    /// Panics if the variant is actually [`Entry::Vacant`].
    pub fn occupied_mut(&mut self) -> &mut OccupiedEntry<EntryData> {
        use self::Entry::*;

        match self {
            Occupied(entry) => entry,
            Vacant(_) => panic!("expected occupied entry"),
        }
    }

    /// Returns an immutable reference to the vacant entry.
    ///
    /// # Panics
    ///
    /// Panics if the variant is actually [`Entry::Occupied`].
    pub fn vacant_ref(&self) -> &VacantEntry {
        use self::Entry::*;

        match self {
            Vacant(entry) => entry,
            Occupied(_) => panic!("expected vacant entry"),
        }
    }
}

/// An occupied entry in the list.
#[derive(Clone)]
struct OccupiedEntry<EntryData> {
    /// The generation of when this entry was inserted. This is used to avoid the ABA problem.
    generation: u64,

    /// The index of the next occupied entry in the list.
    next: Option<usize>,

    /// The index of the previous occupied entry in the list.
    previous: Option<usize>,

    /// The actual value being stored in this entry.
    value: EntryData,
}

impl<EntryData> OccupiedEntry<EntryData> {
    /// Convenience function for creating a new occupied entry.
    pub fn new(
        generation: u64,
        previous: Option<usize>,
        next: Option<usize>,
        value: EntryData,
    ) -> OccupiedEntry<EntryData> {
        OccupiedEntry {
            generation,
            next,
            previous,
            value,
        }
    }
}

/// A vacant entry in the list.
#[derive(Clone, Debug)]
struct VacantEntry {
    /// The index of the next vacant entry in the list.
    next: Option<usize>,
}

impl VacantEntry {
    /// Convenience function for creating a new vacant entry.
    pub fn new(next: Option<usize>) -> VacantEntry {
        VacantEntry { next }
    }
}

/// An iterator that yields and removes all entries from the list.
pub struct Drain<'entries, EntryData> {
    /// The index of the head of the unvisited portion of the list.
    head: Option<usize>,

    /// A reference to the entry list.
    list: &'entries mut VecList<EntryData>,

    /// The number of entries that have not been visited.
    remaining: usize,

    /// The index of the tail of the unvisited portion of the list.
    tail: Option<usize>,
}

impl<EntryData> Drain<'_, EntryData> {
    /// Creates an iterator that yields immutable references to entries in the list.
    pub fn iter(&self) -> Iter<EntryData> {
        Iter {
            entries: &self.list.entries,
            head: self.head,
            remaining: self.remaining,
            tail: self.tail,
        }
    }
}

impl<EntryData> Debug for Drain<'_, EntryData>
where
    EntryData: Debug,
{
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        formatter.write_str("Drain(")?;
        formatter.debug_list().entries(self.iter()).finish()?;
        formatter.write_str(")")
    }
}

impl<EntryData> DoubleEndedIterator for Drain<'_, EntryData> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            None
        } else {
            self.tail.map(|index| {
                let entry = self
                    .list
                    .remove_entry(index)
                    .expect("expected occupied entry");
                self.tail = entry.previous;
                self.remaining -= 1;
                entry.value
            })
        }
    }
}

impl<EntryData> Drop for Drain<'_, EntryData> {
    fn drop(&mut self) {
        self.list.clear();
    }
}

impl<EntryData> ExactSizeIterator for Drain<'_, EntryData> {}

impl<EntryData> FusedIterator for Drain<'_, EntryData> {}

impl<EntryData> Iterator for Drain<'_, EntryData> {
    type Item = EntryData;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            None
        } else {
            self.head.map(|index| {
                let entry = self
                    .list
                    .remove_entry(index)
                    .expect("expected occupied entry");
                self.head = entry.next;
                self.remaining -= 1;
                entry.value
            })
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

/// An iterator that yields all indices in the list.
pub struct Indices<'entries, EntryData> {
    /// A reference to the actual storage for the entry list.
    entries: &'entries Vec<Entry<EntryData>>,

    /// The index of the head of the unvisited portion of the list.
    head: Option<usize>,

    /// The number of entries that have not been visited.
    remaining: usize,

    /// The index of the tail of the unvisited portion of the list.
    tail: Option<usize>,
}

impl<EntryData> Clone for Indices<'_, EntryData> {
    fn clone(&self) -> Self {
        Indices {
            entries: self.entries,
            head: self.head,
            remaining: self.remaining,
            tail: self.tail,
        }
    }
}

impl<EntryData> Debug for Indices<'_, EntryData>
where
    EntryData: Debug,
{
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        formatter.write_str("Indices(")?;
        formatter.debug_list().entries(self.clone()).finish()?;
        formatter.write_str(")")
    }
}

impl<EntryData> DoubleEndedIterator for Indices<'_, EntryData> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            None
        } else {
            self.tail.map(|index| {
                let entry = self.entries[index].occupied_ref();
                let index = Index::new(index, entry.generation);
                self.tail = entry.previous;
                self.remaining -= 1;
                index
            })
        }
    }
}

impl<EntryData> ExactSizeIterator for Indices<'_, EntryData> {}

impl<EntryData> FusedIterator for Indices<'_, EntryData> {}

impl<EntryData> Iterator for Indices<'_, EntryData> {
    type Item = Index<EntryData>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            None
        } else {
            self.head.map(|index| {
                let entry = self.entries[index].occupied_ref();
                let index = Index::new(index, entry.generation);
                self.head = entry.next;
                self.remaining -= 1;
                index
            })
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

/// An iterator that moves all entries out of the entry list.
#[derive(Clone)]
pub struct IntoIter<EntryData> {
    /// The index of the head of the unvisited portion of the list.
    head: Option<usize>,

    /// The entry list from which entries are yielded.
    list: VecList<EntryData>,

    /// The number of entries that have not been visited.
    remaining: usize,

    /// The index of the tail of the unvisited portion of the list.
    tail: Option<usize>,
}

impl<EntryData> IntoIter<EntryData> {
    /// Creates an iterator that yields immutable references to entries in the list.
    pub fn iter(&self) -> Iter<EntryData> {
        Iter {
            entries: &self.list.entries,
            head: self.head,
            remaining: self.remaining,
            tail: self.tail,
        }
    }
}

impl<EntryData> Debug for IntoIter<EntryData>
where
    EntryData: Debug,
{
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        formatter.write_str("IntoIter(")?;
        formatter.debug_list().entries(self.iter()).finish()?;
        formatter.write_str(")")
    }
}

impl<EntryData> DoubleEndedIterator for IntoIter<EntryData> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            None
        } else {
            self.tail.map(|index| {
                let entry = self
                    .list
                    .remove_entry(index)
                    .expect("expected occupied entry");
                self.tail = entry.previous;
                self.remaining -= 1;
                entry.value
            })
        }
    }
}

impl<EntryData> ExactSizeIterator for IntoIter<EntryData> {}

impl<EntryData> FusedIterator for IntoIter<EntryData> {}

impl<EntryData> Iterator for IntoIter<EntryData> {
    type Item = EntryData;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            None
        } else {
            self.head.map(|index| {
                let entry = self
                    .list
                    .remove_entry(index)
                    .expect("expected occupied entry");
                self.head = entry.next;
                self.remaining -= 1;
                entry.value
            })
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

/// An iterator that yields immutable references to entries in the list.
pub struct Iter<'entries, EntryData> {
    /// A reference to the actual storage for the entry list.
    entries: &'entries Vec<Entry<EntryData>>,

    /// The index of the head of the unvisited portion of the list.
    head: Option<usize>,

    /// The number of entries that have not been visited.
    remaining: usize,

    /// The index of the tail of the unvisited portion of the list.
    tail: Option<usize>,
}

impl<'entries, EntryData> Clone for Iter<'entries, EntryData> {
    fn clone(&self) -> Iter<'entries, EntryData> {
        Iter {
            entries: self.entries,
            head: self.head,
            remaining: self.remaining,
            tail: self.tail,
        }
    }
}

impl<EntryData> Debug for Iter<'_, EntryData>
where
    EntryData: Debug,
{
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        formatter.write_str("Iter(")?;
        formatter.debug_list().entries(self.clone()).finish()?;
        formatter.write_str(")")
    }
}

impl<EntryData> DoubleEndedIterator for Iter<'_, EntryData> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            None
        } else {
            self.tail.map(|index| {
                let entry = self.entries[index].occupied_ref();
                self.tail = entry.previous;
                self.remaining -= 1;
                &entry.value
            })
        }
    }
}

impl<EntryData> ExactSizeIterator for Iter<'_, EntryData> {}

impl<EntryData> FusedIterator for Iter<'_, EntryData> {}

impl<'entries, EntryData> Iterator for Iter<'entries, EntryData> {
    type Item = &'entries EntryData;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            None
        } else {
            self.head.map(|index| {
                let entry = self.entries[index].occupied_ref();
                self.head = entry.next;
                self.remaining -= 1;
                &entry.value
            })
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

/// An iterator that yields mutable references to entries in the list.
pub struct IterMut<'entries, EntryData> {
    entries: *mut Vec<Entry<EntryData>>,

    /// The index of the head of the unvisited portion of the list.
    head: Option<usize>,

    /// Because [`IterMut::entries`] is a pointer, we need to have a phantom data here for the
    /// lifetime parameter.
    phantom: PhantomData<&'entries mut Vec<Entry<EntryData>>>,

    /// The number of entries that have not been visited.
    remaining: usize,

    /// The index of the tail of the unvisited portion of the list.
    tail: Option<usize>,
}

impl<EntryData> IterMut<'_, EntryData> {
    /// Creates an iterator that yields immutable references to entries in the list.
    pub fn iter(&self) -> Iter<EntryData> {
        Iter {
            entries: unsafe { &*self.entries },
            head: self.head,
            remaining: self.remaining,
            tail: self.tail,
        }
    }
}

impl<EntryData> Debug for IterMut<'_, EntryData>
where
    EntryData: Debug,
{
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        formatter.write_str("IterMut(")?;
        formatter.debug_list().entries(self.iter()).finish()?;
        formatter.write_str(")")
    }
}

impl<EntryData> DoubleEndedIterator for IterMut<'_, EntryData> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            None
        } else {
            self.tail.map(|index| {
                let entry = unsafe { &mut (*self.entries)[index] }.occupied_mut();
                self.tail = entry.previous;
                self.remaining -= 1;
                &mut entry.value
            })
        }
    }
}

impl<EntryData> ExactSizeIterator for IterMut<'_, EntryData> {}

impl<EntryData> FusedIterator for IterMut<'_, EntryData> {}

impl<'entries, EntryData> Iterator for IterMut<'entries, EntryData> {
    type Item = &'entries mut EntryData;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            None
        } else {
            self.head.map(|index| {
                let entry = unsafe { &mut (*self.entries)[index] }.occupied_mut();
                self.head = entry.next;
                self.remaining -= 1;
                &mut entry.value
            })
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

unsafe impl<EntryData> Send for IterMut<'_, EntryData> where EntryData: Send {}

unsafe impl<EntryData> Sync for IterMut<'_, EntryData> where EntryData: Sync {}

/// Creates the initial generation seeded by the current time.
fn create_initial_generation() -> u64 {
    let mut hasher = RandomState::new().build_hasher();
    hasher.write_u32(0);
    hasher.finish()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_bounds() {
        fn check_bounds<Type: Send + Sync>() {}

        check_bounds::<VecList<()>>();
        check_bounds::<Index<()>>();
        check_bounds::<Drain<()>>();
        check_bounds::<Indices<()>>();
        check_bounds::<IntoIter<()>>();
        check_bounds::<Iter<()>>();
        check_bounds::<IterMut<()>>();
    }

    #[test]
    fn test_drain_debug() {
        let mut list = VecList::new();
        list.push_back(0);
        list.push_back(1);
        list.push_back(-1);
        list.push_back(2);
        list.push_back(-2);

        let drain = list.drain();
        assert_eq!(format!("{:?}", drain), "Drain([0, 1, -1, 2, -2])");
    }

    #[test]
    fn test_drain_double_ended() {
        let mut list = VecList::new();
        list.push_back(0);
        list.push_back(1);
        list.push_back(-1);
        list.push_back(2);
        list.push_back(-2);

        let mut drain = list.drain();
        assert_eq!(drain.next(), Some(0));
        assert_eq!(drain.next_back(), Some(-2));
        assert_eq!(drain.next(), Some(1));
        assert_eq!(drain.next_back(), Some(2));
        assert_eq!(drain.next(), Some(-1));
        assert_eq!(drain.next_back(), None);
    }

    #[test]
    fn test_drain_empty() {
        let mut list: VecList<i32> = VecList::new();
        let mut drain = list.drain();
        assert_eq!(drain.next(), None);
    }

    #[test]
    fn test_drain_fused() {
        let mut list: VecList<i32> = VecList::new();
        list.push_back(0);
        let mut drain = list.drain();
        assert_eq!(drain.next(), Some(0));
        assert_eq!(drain.next(), None);
        assert_eq!(drain.next(), None);
        assert_eq!(drain.next(), None);
    }

    #[test]
    fn test_drain_size_hint() {
        let mut list = VecList::new();
        list.push_back(0);
        list.push_back(1);
        list.push_back(-1);
        list.push_back(2);
        list.push_back(-2);

        let mut drain = list.drain();

        assert_eq!(drain.size_hint(), (5, Some(5)));
        drain.next();
        assert_eq!(drain.size_hint(), (4, Some(4)));
        drain.next();
        assert_eq!(drain.size_hint(), (3, Some(3)));
        drain.next();
        assert_eq!(drain.size_hint(), (2, Some(2)));
        drain.next();
        assert_eq!(drain.size_hint(), (1, Some(1)));
        drain.next();
        assert_eq!(drain.size_hint(), (0, Some(0)));
    }

    #[test]
    fn test_index_debug() {
        let mut list = VecList::new();
        let index = list.push_back(5);

        assert_eq!(
            format!("{:?}", index),
            format!("Index(0, {})", index.generation)
        );
    }

    #[test]
    fn test_index_equality() {
        let mut list = VecList::new();
        let index_1 = list.push_back(0);
        let index_2 = list.indices().next().unwrap();

        assert_eq!(index_1, index_2);
    }

    #[test]
    fn test_indices_debug() {
        let mut list = VecList::new();
        list.push_back(0);
        list.push_back(1);
        list.push_back(-1);
        list.push_back(2);
        list.push_back(-2);

        let indices = list.indices();
        assert_eq!(
            format!("{:?}", indices),
            format!(
                "Indices([Index(0, {}), Index(1, {}), Index(2, {}), Index(3, {}), Index(4, {})])",
                list.generation, list.generation, list.generation, list.generation, list.generation
            )
        );
    }

    #[test]
    fn test_indices_double_ended() {
        let mut list = VecList::new();
        list.push_back(0);
        list.push_back(1);
        list.push_back(-1);
        list.push_back(2);
        list.push_back(-2);

        let mut indices = list.indices();
        assert_eq!(indices.next().unwrap().index, 0);
        assert_eq!(indices.next_back().unwrap().index, 4);
        assert_eq!(indices.next().unwrap().index, 1);
        assert_eq!(indices.next_back().unwrap().index, 3);
        assert_eq!(indices.next().unwrap().index, 2);
        assert_eq!(indices.next_back(), None);
    }

    #[test]
    fn test_indices_empty() {
        let list: VecList<i32> = VecList::new();
        let mut indices = list.indices();
        assert_eq!(indices.next(), None);
    }

    #[test]
    fn test_indices_fused() {
        let mut list: VecList<i32> = VecList::new();
        list.push_back(0);
        let mut indices = list.indices();
        assert_eq!(indices.next().unwrap().index, 0);
        assert_eq!(indices.next(), None);
        assert_eq!(indices.next(), None);
        assert_eq!(indices.next(), None);
    }

    #[test]
    fn test_indices_size_hint() {
        let mut list = VecList::new();
        list.push_back(0);
        list.push_back(1);
        list.push_back(-1);
        list.push_back(2);
        list.push_back(-2);

        let mut indices = list.indices();

        assert_eq!(indices.size_hint(), (5, Some(5)));
        indices.next();
        assert_eq!(indices.size_hint(), (4, Some(4)));
        indices.next();
        assert_eq!(indices.size_hint(), (3, Some(3)));
        indices.next();
        assert_eq!(indices.size_hint(), (2, Some(2)));
        indices.next();
        assert_eq!(indices.size_hint(), (1, Some(1)));
        indices.next();
        assert_eq!(indices.size_hint(), (0, Some(0)));
    }

    #[test]
    fn test_into_iter_debug() {
        let mut list = VecList::new();
        list.push_back(0);
        list.push_back(1);
        list.push_back(-1);
        list.push_back(2);
        list.push_back(-2);

        let iter = list.into_iter();
        assert_eq!(format!("{:?}", iter), "IntoIter([0, 1, -1, 2, -2])");
    }

    #[test]
    fn test_into_iter_double_ended() {
        let mut list = VecList::new();
        list.push_back(0);
        list.push_back(1);
        list.push_back(-1);
        list.push_back(2);
        list.push_back(-2);

        let mut iter = list.into_iter();
        assert_eq!(iter.next(), Some(0));
        assert_eq!(iter.next_back(), Some(-2));
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.next_back(), Some(2));
        assert_eq!(iter.next(), Some(-1));
        assert_eq!(iter.next_back(), None);
    }

    #[test]
    fn test_into_iter_empty() {
        let list: VecList<i32> = VecList::new();
        let mut iter = list.into_iter();
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_into_iter_fused() {
        let mut list: VecList<i32> = VecList::new();
        list.push_back(0);
        let mut iter = list.into_iter();
        assert_eq!(iter.next(), Some(0));
        assert_eq!(iter.next(), None);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_into_iter_size_hint() {
        let mut list = VecList::new();
        list.push_back(0);
        list.push_back(1);
        list.push_back(-1);
        list.push_back(2);
        list.push_back(-2);

        let mut iter = list.into_iter();

        assert_eq!(iter.size_hint(), (5, Some(5)));
        iter.next();
        assert_eq!(iter.size_hint(), (4, Some(4)));
        iter.next();
        assert_eq!(iter.size_hint(), (3, Some(3)));
        iter.next();
        assert_eq!(iter.size_hint(), (2, Some(2)));
        iter.next();
        assert_eq!(iter.size_hint(), (1, Some(1)));
        iter.next();
        assert_eq!(iter.size_hint(), (0, Some(0)));
    }

    #[test]
    fn test_iter_debug() {
        let mut list = VecList::new();
        list.push_back(0);
        list.push_back(1);
        list.push_back(-1);
        list.push_back(2);
        list.push_back(-2);

        let iter = list.iter();
        assert_eq!(format!("{:?}", iter), "Iter([0, 1, -1, 2, -2])");
    }

    #[test]
    fn test_iter_double_ended() {
        let mut list = VecList::new();
        list.push_back(0);
        list.push_back(1);
        list.push_back(-1);
        list.push_back(2);
        list.push_back(-2);

        let mut iter = list.iter();
        assert_eq!(iter.next(), Some(&0));
        assert_eq!(iter.next_back(), Some(&-2));
        assert_eq!(iter.next(), Some(&1));
        assert_eq!(iter.next_back(), Some(&2));
        assert_eq!(iter.next(), Some(&-1));
        assert_eq!(iter.next_back(), None);
    }

    #[test]
    fn test_iter_empty() {
        let list: VecList<i32> = VecList::new();
        let mut iter = list.iter();
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_iter_fused() {
        let mut list: VecList<i32> = VecList::new();
        list.push_back(0);
        let mut iter = list.iter();
        assert_eq!(iter.next(), Some(&0));
        assert_eq!(iter.next(), None);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_iter_size_hint() {
        let mut list = VecList::new();
        list.push_back(0);
        list.push_back(1);
        list.push_back(-1);
        list.push_back(2);
        list.push_back(-2);

        let mut iter = list.iter();

        assert_eq!(iter.size_hint(), (5, Some(5)));
        iter.next();
        assert_eq!(iter.size_hint(), (4, Some(4)));
        iter.next();
        assert_eq!(iter.size_hint(), (3, Some(3)));
        iter.next();
        assert_eq!(iter.size_hint(), (2, Some(2)));
        iter.next();
        assert_eq!(iter.size_hint(), (1, Some(1)));
        iter.next();
        assert_eq!(iter.size_hint(), (0, Some(0)));
    }

    #[test]
    fn test_iter_mut_debug() {
        let mut list = VecList::new();
        list.push_back(0);
        list.push_back(1);
        list.push_back(-1);
        list.push_back(2);
        list.push_back(-2);

        let iter = list.iter_mut();
        assert_eq!(format!("{:?}", iter), "IterMut([0, 1, -1, 2, -2])");
    }

    #[test]
    fn test_iter_mut_double_ended() {
        let mut list = VecList::new();
        list.push_back(0);
        list.push_back(1);
        list.push_back(-1);
        list.push_back(2);
        list.push_back(-2);

        let mut iter = list.iter_mut();
        assert_eq!(iter.next(), Some(&mut 0));
        assert_eq!(iter.next_back(), Some(&mut -2));
        assert_eq!(iter.next(), Some(&mut 1));
        assert_eq!(iter.next_back(), Some(&mut 2));
        assert_eq!(iter.next(), Some(&mut -1));
        assert_eq!(iter.next_back(), None);
    }

    #[test]
    fn test_iter_mut_empty() {
        let mut list: VecList<i32> = VecList::new();
        let mut iter = list.iter_mut();
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_iter_mut_fused() {
        let mut list: VecList<i32> = VecList::new();
        list.push_back(0);
        let mut iter = list.iter_mut();
        assert_eq!(iter.next(), Some(&mut 0));
        assert_eq!(iter.next(), None);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_iter_mut_size_hint() {
        let mut list = VecList::new();
        list.push_back(0);
        list.push_back(1);
        list.push_back(-1);
        list.push_back(2);
        list.push_back(-2);

        let mut iter = list.iter_mut();

        assert_eq!(iter.size_hint(), (5, Some(5)));
        iter.next();
        assert_eq!(iter.size_hint(), (4, Some(4)));
        iter.next();
        assert_eq!(iter.size_hint(), (3, Some(3)));
        iter.next();
        assert_eq!(iter.size_hint(), (2, Some(2)));
        iter.next();
        assert_eq!(iter.size_hint(), (1, Some(1)));
        iter.next();
        assert_eq!(iter.size_hint(), (0, Some(0)));
    }

    #[test]
    fn test_vec_list_back() {
        let mut list = VecList::new();
        assert_eq!(list.back(), None);

        let index_1 = list.push_back(0);
        assert_eq!(list.back(), Some(&0));

        let index_2 = list.push_back(1);
        assert_eq!(list.back(), Some(&1));

        list.remove(index_2);
        assert_eq!(list.back(), Some(&0));

        list.remove(index_1);
        assert_eq!(list.back(), None);
    }

    #[test]
    fn test_vec_list_back_mut() {
        let mut list = VecList::new();
        assert_eq!(list.back_mut(), None);

        let index_1 = list.push_back(0);
        assert_eq!(list.back_mut(), Some(&mut 0));

        let index_2 = list.push_back(1);
        assert_eq!(list.back_mut(), Some(&mut 1));

        list.remove(index_2);
        assert_eq!(list.back_mut(), Some(&mut 0));

        list.remove(index_1);
        assert_eq!(list.back_mut(), None);
    }

    #[test]
    fn test_vec_list_capacity() {
        let list: VecList<i32> = VecList::new();
        assert_eq!(list.capacity(), 0);
    }

    #[test]
    fn test_vec_list_clear() {
        let mut list = VecList::new();
        let index = list.push_back(0);
        list.clear();
        assert!(list.is_empty());
        assert_eq!(list.get(index), None);
    }

    #[test]
    fn test_vec_list_contains() {
        let mut list = VecList::new();
        assert!(!list.contains(&0));

        let index = list.push_back(0);
        assert!(list.contains(&0));

        list.remove(index);
        assert!(!list.contains(&0));
    }

    #[test]
    fn test_vec_list_drain() {
        let mut list = VecList::new();
        list.drain();
        assert!(list.is_empty());

        list.push_back(0);
        list.push_back(1);
        list.push_back(-1);
        list.drain();
        assert!(list.is_empty());
    }

    #[test]
    fn test_vec_list_debug() {
        let mut list = VecList::new();
        list.push_back(0);
        list.push_back(1);
        list.push_back(-1);
        list.push_back(2);
        list.push_back(-2);

        assert_eq!(format!("{:?}", list), "[0, 1, -1, 2, -2]");
    }

    #[test]
    fn test_vec_list_equality() {
        let mut list_1 = VecList::new();
        list_1.push_back(0);
        list_1.push_back(1);
        list_1.push_back(-1);
        list_1.push_back(2);
        list_1.push_back(-2);

        let mut list_2 = list_1.clone();
        list_2.pop_back();
        assert_ne!(list_1, list_2);

        list_2.push_back(-2);
        assert_eq!(list_1, list_2);
    }

    #[test]
    fn test_vec_list_extend() {
        let mut list = VecList::new();
        list.push_back(0);
        list.push_back(1);
        list.extend([-1, 2, -2].iter());

        assert_eq!(list, &[0, 1, -1, 2, -2][..]);
    }

    #[test]
    fn test_vec_list_from_iterator() {
        let list = VecList::from_iter([0, 1, -1, 2, -2].iter().cloned());
        assert_eq!(list, &[0, 1, -1, 2, -2][..]);
    }

    #[test]
    fn test_vec_list_front() {
        let mut list = VecList::new();
        assert_eq!(list.front(), None);

        let index_1 = list.push_front(0);
        assert_eq!(list.front(), Some(&0));

        let index_2 = list.push_front(1);
        assert_eq!(list.front(), Some(&1));

        list.remove(index_2);
        assert_eq!(list.front(), Some(&0));

        list.remove(index_1);
        assert_eq!(list.front(), None);
    }

    #[test]
    fn test_vec_list_front_mut() {
        let mut list = VecList::new();
        assert_eq!(list.front_mut(), None);

        let index_1 = list.push_front(0);
        assert_eq!(list.front_mut(), Some(&mut 0));

        let index_2 = list.push_front(1);
        assert_eq!(list.front_mut(), Some(&mut 1));

        list.remove(index_2);
        assert_eq!(list.front_mut(), Some(&mut 0));

        list.remove(index_1);
        assert_eq!(list.front_mut(), None);
    }

    #[test]
    fn test_vec_list_get() {
        let mut list = VecList::new();
        let index = list.push_back(0);
        assert_eq!(list.get(index), Some(&0));
        list.remove(index);
        assert_eq!(list.get(index), None);

        let mut list = VecList::new();
        let index_1 = list.push_back(0);
        let index_2 = list.push_back(1);
        let index_3 = list.push_back(2);

        list.remove(index_1);
        list.pack_to_fit();
        assert_eq!(list.get(index_1), None);
        assert_eq!(list.get(index_2), None);
        assert_eq!(list.get(index_3), None);
    }

    #[test]
    fn test_vec_list_get_mut() {
        let mut list = VecList::new();
        let index = list.push_back(0);
        assert_eq!(list.get_mut(index), Some(&mut 0));
        list.remove(index);
        assert_eq!(list.get_mut(index), None);

        let mut list = VecList::new();
        let index_1 = list.push_back(0);
        let index_2 = list.push_back(1);
        let index_3 = list.push_back(2);

        list.remove(index_1);
        list.pack_to_fit();
        assert_eq!(list.get_mut(index_1), None);
        assert_eq!(list.get_mut(index_2), None);
        assert_eq!(list.get_mut(index_3), None);
    }

    #[test]
    fn test_vec_list_get_next_index() {
        let mut list = VecList::new();

        let index = list.push_back(0);
        assert_eq!(list.get_next_index(index), None);

        list.push_back(1);
        assert_eq!(list.get_next_index(index).unwrap().index, 1);
    }

    #[test]
    fn test_vec_list_get_previous_index() {
        let mut list = VecList::new();

        let index = list.push_front(0);
        assert_eq!(list.get_previous_index(index), None);

        list.push_front(1);
        assert_eq!(list.get_previous_index(index).unwrap().index, 1);
    }

    #[test]
    fn test_vec_list_index() {
        let mut list = VecList::new();

        let index = list.push_back(5);
        assert_eq!(list[index], 5);

        list[index] = 10;
        assert_eq!(list[index], 10);
    }

    #[should_panic]
    #[test]
    fn test_vec_list_index_panic() {
        let mut list = VecList::new();
        let index = list.push_back(0);
        list.pop_back();
        list[index];
    }

    #[test]
    fn test_vec_list_indices() {
        let mut list = VecList::new();
        let mut iter = list.indices();
        assert_eq!(iter.next(), None);

        list.push_back(0);
        let index = list.push_back(1);
        list.push_back(-1);
        list.remove(index);

        let mut iter = list.indices();
        assert_eq!(iter.next().unwrap().index, 0);
        assert_eq!(iter.next().unwrap().index, 2);
        assert_eq!(iter.next(), None);

        list.pack_to_fit();

        let mut iter = list.indices();
        assert_eq!(iter.next().unwrap().index, 0);
        assert_eq!(iter.next().unwrap().index, 1);
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_vec_list_insert_after() {
        let mut list = VecList::new();
        let index_1 = list.push_front(0);
        let index_2 = list.insert_after(index_1, 1);

        assert_eq!(list.back(), Some(&1));
        assert_eq!(list.get_previous_index(index_2), Some(index_1));
        assert_eq!(list.get_next_index(index_1), Some(index_2));

        let index_3 = list.insert_after(index_1, 2);

        assert_eq!(list.get_previous_index(index_3), Some(index_1));
        assert_eq!(list.get_next_index(index_1), Some(index_3));
        assert_eq!(list.get_next_index(index_3), Some(index_2));
    }

    #[should_panic]
    #[test]
    fn test_vec_list_insert_after_panic_index_invalidated() {
        let mut list = VecList::new();
        let index = list.push_front(0);
        list.remove(index);
        list.insert_after(index, 1);
    }

    #[should_panic]
    #[test]
    fn test_vec_list_insert_after_panic_index_out_of_bounds() {
        let mut list = VecList::new();
        let index_1 = list.push_back(0);
        list.push_back(1);
        let index_2 = list.push_back(2);

        list.remove(index_1);
        list.pack_to_fit();
        list.insert_after(index_2, 3);
    }

    #[test]
    fn test_vec_list_insert_before() {
        let mut list = VecList::new();
        let index_1 = list.push_back(0);
        let index_2 = list.insert_before(index_1, 1);

        assert_eq!(list.front(), Some(&1));
        assert_eq!(list.get_previous_index(index_1), Some(index_2));
        assert_eq!(list.get_next_index(index_2), Some(index_1));

        let index_3 = list.insert_before(index_1, 2);

        assert_eq!(list.get_previous_index(index_1), Some(index_3));
        assert_eq!(list.get_next_index(index_3), Some(index_1));
        assert_eq!(list.get_next_index(index_2), Some(index_3));
    }

    #[should_panic]
    #[test]
    fn test_vec_list_insert_before_panic_index_invalidated() {
        let mut list = VecList::new();
        let index = list.push_front(0);
        list.remove(index);
        list.insert_before(index, 1);
    }

    #[should_panic]
    #[test]
    fn test_vec_list_insert_before_panic_index_out_of_bounds() {
        let mut list = VecList::new();
        let index_1 = list.push_back(0);
        list.push_back(1);
        let index_2 = list.push_back(2);

        list.remove(index_1);
        list.pack_to_fit();
        list.insert_before(index_2, 3);
    }

    #[test]
    fn test_vec_list_into_iterator() {
        let mut list = VecList::new();
        list.push_back(0);
        list.push_back(1);
        list.push_back(-1);
        list.push_back(2);
        list.push_back(-2);

        assert_eq!(list.into_iter().collect::<Vec<_>>(), [0, 1, -1, 2, -2]);
    }

    #[test]
    fn test_vec_list_is_empty() {
        let mut list = VecList::new();
        assert!(list.is_empty());
        list.push_back(0);
        assert!(!list.is_empty());
    }

    #[test]
    fn test_vec_list_iter() {
        let mut list = VecList::new();
        list.push_back(0);
        list.push_back(1);
        list.push_back(2);

        let mut iter = list.iter();
        assert_eq!(iter.next(), Some(&0));
        assert_eq!(iter.next(), Some(&1));
        assert_eq!(iter.next(), Some(&2));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_vec_list_iter_mut() {
        let mut list = VecList::new();
        list.push_back(0);
        list.push_back(1);
        list.push_back(2);

        let mut iter = list.iter_mut();
        let value = iter.next().unwrap();
        *value = 100;

        assert_eq!(iter.next(), Some(&mut 1));
        assert_eq!(iter.next(), Some(&mut 2));
        assert_eq!(iter.next(), None);
        assert_eq!(list.front(), Some(&100));
    }

    #[test]
    fn test_vec_list_len() {
        let mut list = VecList::new();
        assert_eq!(list.len(), 0);
        let index = list.push_back(0);
        assert_eq!(list.len(), 1);
        list.remove(index);
        assert_eq!(list.len(), 0);
    }

    #[test]
    fn test_vec_list_new() {
        let list: VecList<i32> = VecList::new();
        assert_eq!(list.capacity(), 0);
        assert_eq!(list.len(), 0);
    }

    #[test]
    fn test_vec_list_ordering() {
        let mut list_1 = VecList::new();
        list_1.push_back(0);
        list_1.push_back(1);
        list_1.push_back(-1);
        list_1.push_back(2);
        list_1.push_back(-2);

        let mut list_2 = list_1.clone();

        list_2.push_back(5);
        assert!(list_1 < list_2);

        list_2.pop_back();
        list_2.pop_back();
        assert!(list_1 > list_2);

        list_2.push_back(3);
        assert!(list_1 < list_2);

        list_2.pop_back();
        list_2.push_back(-3);
        assert!(list_1 > list_2);
    }

    #[test]
    fn test_vec_list_pop_back() {
        let mut list = VecList::new();
        assert_eq!(list.pop_back(), None);

        list.push_back(0);
        assert_eq!(list.pop_back(), Some(0));
    }

    #[test]
    fn test_vec_list_pop_front() {
        let mut list = VecList::new();
        assert_eq!(list.pop_front(), None);

        list.push_front(0);
        assert_eq!(list.pop_front(), Some(0));
    }

    #[test]
    fn test_vec_list_push_back() {
        let mut list = VecList::new();
        list.push_back(0);
        assert_eq!(list.back(), Some(&0));
        list.push_back(1);
        assert_eq!(list.back(), Some(&1));
        list.push_back(2);
        assert_eq!(list.back(), Some(&2));
    }

    #[test]
    fn test_vec_list_push_front() {
        let mut list = VecList::new();
        list.push_front(0);
        assert_eq!(list.front(), Some(&0));
        list.push_front(1);
        assert_eq!(list.front(), Some(&1));
        list.push_front(2);
        assert_eq!(list.front(), Some(&2));
    }

    #[test]
    fn test_vec_list_remove() {
        let mut list = VecList::new();
        let index = list.push_back(0);
        assert_eq!(list.remove(index), Some(0));
        assert_eq!(list.remove(index), None);
    }

    #[test]
    fn test_vec_list_reserve() {
        let mut list: VecList<i32> = VecList::new();
        assert_eq!(list.capacity(), 0);

        list.reserve(10);
        let capacity = list.capacity();

        assert!(capacity >= 10);
        list.reserve(5);

        assert_eq!(list.capacity(), capacity);
    }

    #[test]
    fn test_vec_list_retain() {
        let mut list = VecList::new();
        list.push_back(0);
        list.push_back(1);
        list.push_back(-1);
        list.push_back(2);
        list.push_back(-2);

        list.retain(|&mut value| value >= 0);
        assert_eq!(list.into_iter().collect::<Vec<_>>(), [0, 1, 2]);
    }

    #[test]
    fn test_vec_list_pack_to() {
        let mut list = VecList::new();
        let index_1 = list.push_back(0);
        let index_2 = list.push_back(1);
        let index_3 = list.push_back(2);
        assert!(list.capacity() >= 3);

        list.remove(index_1);
        assert!(list.capacity() >= 3);

        let indices = list.indices();
        assert_eq!(indices.map(|index| index.index).collect::<Vec<_>>(), [1, 2]);

        let map = list.pack_to(5);
        assert_eq!(list.capacity(), 5);

        let indices = list.indices();
        assert_eq!(indices.map(|index| index.index).collect::<Vec<_>>(), [0, 1]);

        assert_eq!(map.len(), 2);
        assert_eq!(map.get(&index_2).unwrap().index, 0);
        assert_eq!(map.get(&index_3).unwrap().index, 1);
    }

    #[test]
    fn test_vec_list_pack_to_empty() {
        let mut list: VecList<i32> = VecList::with_capacity(5);
        list.pack_to(0);
        assert_eq!(list.capacity(), 0);
    }

    #[should_panic]
    #[test]
    fn test_vec_list_pack_to_panic() {
        let mut list = VecList::new();
        list.push_back(0);
        list.push_back(1);
        list.push_back(2);
        list.pack_to(2);
    }

    #[test]
    fn test_vec_list_pack_to_fit() {
        let mut list = VecList::new();
        let index_1 = list.push_back(0);
        let index_2 = list.push_back(1);
        let index_3 = list.push_back(2);
        assert!(list.capacity() >= 3);

        list.remove(index_1);
        assert!(list.capacity() >= 3);

        let indices = list.indices();
        assert_eq!(indices.map(|index| index.index).collect::<Vec<_>>(), [1, 2]);

        let map = list.pack_to_fit();
        assert_eq!(list.capacity(), 2);

        let indices = list.indices();
        assert_eq!(indices.map(|index| index.index).collect::<Vec<_>>(), [0, 1]);

        assert_eq!(map.len(), 2);
        assert_eq!(map.get(&index_2).unwrap().index, 0);
        assert_eq!(map.get(&index_3).unwrap().index, 1);
    }

    #[test]
    fn test_vec_list_with_capacity() {
        let list: VecList<i32> = VecList::with_capacity(10);
        assert_eq!(list.capacity(), 10);
    }
}
