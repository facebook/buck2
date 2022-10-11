/*!
Parses the rules into a state machine using a pair table. Each value in the table specifies the
next state and whether it's an forced/allowed break. To handles rules such as

    B SP* ÷ A

the extra state BSP is employed in the pair table friendly equivalent rules

    (B | BSP) ÷ A, Treat (B | BSP) SP as if it were BSP, Treat BSP as if it were SP
*/
#![recursion_limit = "512"]

use hashbrown::hash_map::{Entry, RawEntryMut};
use hashbrown::HashMap;
use regex::Regex;
use std::borrow::Borrow;
use std::cmp::{max, min};
use std::fs::File;
use std::hash::{BuildHasher, Hash, Hasher};
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::marker::PhantomData;
use std::ops::Range;
use std::path::Path;
use std::str::FromStr;
use std::{env, error, iter};

include!("src/shared.rs");

impl FromStr for BreakClass {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "BK" => BK,
            "CR" => CR,
            "LF" => LF,
            "CM" => CM,
            "NL" => NL,
            "SG" => SG,
            "WJ" => WJ,
            "ZW" => ZW,
            "GL" => GL,
            "SP" => SP,
            "ZWJ" => ZWJ,
            "B2" => B2,
            "BA" => BA,
            "BB" => BB,
            "HY" => HY,
            "CB" => CB,
            "CL" => CL,
            "CP" => CP,
            "EX" => EX,
            "IN" => IN,
            "NS" => NS,
            "OP" => OP,
            "QU" => QU,
            "IS" => IS,
            "NU" => NU,
            "PO" => PO,
            "PR" => PR,
            "SY" => SY,
            "AI" => AI,
            "AL" => AL,
            "CJ" => CJ,
            "EB" => EB,
            "EM" => EM,
            "H2" => H2,
            "H3" => H3,
            "HL" => HL,
            "ID" => ID,
            "JL" => JL,
            "JV" => JV,
            "JT" => JT,
            "RI" => RI,
            "SA" => SA,
            "XX" => XX,
            _ => return Err("Invalid break class"),
        })
    }
}

const NUM_CLASSES: usize = 43;
static BREAK_CLASS_TABLE: [&str; NUM_CLASSES] = [
    "BK", "CR", "LF", "CM", "NL", "SG", "WJ", "ZW", "GL", "SP", "ZWJ", "B2", "BA", "BB", "HY",
    "CB", "CL", "CP", "EX", "IN", "NS", "OP", "QU", "IS", "NU", "PO", "PR", "SY", "AI", "AL", "CJ",
    "EB", "EM", "H2", "H3", "HL", "ID", "JL", "JV", "JT", "RI", "SA", "XX",
];

#[derive(Copy, Clone)]
#[repr(u8)]
enum ExtraState {
    ZWSP = sot + 1,
    OPSP,
    QUSP,
    CLSP,
    CPSP,
    B2SP,
    HLHYBA,
    RIRI,
}

use ExtraState::*;

/// The number of classes plus the eot state.
const NUM_CLASSES_EOT: usize = NUM_CLASSES + 1;
const NUM_STATES: usize = NUM_CLASSES + 10;

/// Separate implementation to prevent infinite recursion.
#[doc(hidden)]
macro_rules! rules2table_impl {
    // Operators
    (($len:ident $($args:tt)*) '÷' $($tt:tt)+) => {rules2table_impl! {(NUM_CLASSES_EOT $($args)* '÷') $($tt)+}};
    (($len:ident $($args:tt)*) '×' $($tt:tt)+) => {rules2table_impl! {(NUM_CLASSES_EOT $($args)* '×') $($tt)+}};
    (($len:ident $($args:tt)*) '!' $($tt:tt)+) => {rules2table_impl! {(NUM_CLASSES_EOT $($args)* '!') $($tt)+}};
    // Perform operator
    (($len:ident $pair_table:ident $($first:ident)? $operator:literal $($second:ident)?) $(, $($tt:tt)*)?) => {
        $(rules2table_impl! {(NUM_STATES $pair_table) $($tt)*})?
        #[allow(unused)] let first = 0..NUM_STATES; // Default to ALL
        $(let first = $first;)?
        #[allow(unused)] let second = 0..NUM_CLASSES_EOT; // Default to ALL
        $(let second = $second;)?
        for i in first {
            for j in second.clone() {
                let cell = &mut $pair_table[i][j];
                match $operator {
                    '!' => *cell |= ALLOWED_BREAK_BIT | MANDATORY_BREAK_BIT,
                    '÷' => *cell |= ALLOWED_BREAK_BIT,
                    '×' => *cell &= !(ALLOWED_BREAK_BIT | MANDATORY_BREAK_BIT),
                    _ => unreachable!("Bad operator"),
                }
            }
        }
    };

    (($len:ident $($args:tt)*) Treat X $($tt:tt)*) => {
        rules2table_impl! {(NUM_CLASSES_EOT $($args)* treat_x) $($tt)*}
    };
    (($len:ident $($args:tt)*) Treat $($tt:tt)*) => {
        rules2table_impl! {(NUM_STATES $($args)* treat) $($tt)*}
    };
    (($len:ident $($args:tt)*) * as if it were X where X = $($tt:tt)*) => {
        rules2table_impl! {(NUM_STATES $($args)* as_if_it_were_x_where_x_is) $($tt)*}
    };

    (($len:ident $pair_table:ident treat_x $second:ident as_if_it_were_x_where_x_is $X:ident) $(, $($tt:tt)*)?) => {
        $(rules2table_impl! {(NUM_STATES $pair_table) $($tt)*})?
        for i in $X {
            for j in $second.clone() {
                $pair_table[i][j] = i as u8;
            }
        }
    };
    (($len:ident $pair_table:ident treat $first:ident $second:ident) as if it were $cls:ident $(, $($tt:tt)*)?) => {
        $(rules2table_impl! {(NUM_STATES $pair_table) $($tt)*})?
        let cls = $cls as u8;
        for i in $first {
            for j in $second.clone() {
                $pair_table[i][j] = cls;
            }
        }
    };
    (($len:ident $pair_table:ident treat $first:ident) as if it were $cls:ident $(, $($tt:tt)*)?) => {
        $(rules2table_impl! {(NUM_STATES $pair_table) $($tt)*})?
        for j in $first.clone().filter(|&j| j < NUM_CLASSES_EOT) {
            for row in $pair_table.iter_mut() {
                row[j] = row[$cls as usize];
            }
        }
        for i in $first {
            $pair_table.copy_within($cls as usize..$cls as usize + 1, i);
        }
    };

    // All classes pattern
    (($len:ident $($args:tt)*) ALL $($tt:tt)*) => {
        let indices = 0..$len;
        rules2table_impl! {(NUM_CLASSES_EOT $($args)* indices) $($tt)*}
    };
    // Single class pattern
    (($len:ident $($args:tt)*) $cls:ident $($tt:tt)*) => {
        let indices = iter::once($cls as usize);
        rules2table_impl! {(NUM_CLASSES_EOT $($args)* indices) $($tt)*}
    };
    // Parse (X | ...) patterns
    (($len:ident $($args:tt)*) ($($cls:ident)|+) $($tt:tt)*) => {
        let indices = [$($cls as usize),+].into_iter();
        rules2table_impl! {(NUM_CLASSES_EOT $($args)* indices) $($tt)*}
    };
    // Parse [^ ...] patterns
    (($len:ident $($args:tt)*) [^$($cls:ident)+] $($tt:tt)*) => {
        let excluded = [$($cls as usize),+];
        let indices = (0..$len).filter(|i| !excluded.contains(i));
        rules2table_impl! {(NUM_CLASSES_EOT $($args)* indices) $($tt)*}
    };

    (($len:ident $pair_table:ident)) => {}; // Exit condition
}

/// Returns a pair table conforming to the specified rules.
///
/// The rule syntax is a modified subset of the one in Unicode Standard Annex #14.
macro_rules! rules2table {
    ($($tt:tt)+) => {{
        let mut pair_table = [{
            let mut row = [0; NUM_CLASSES_EOT];
            for (i, x) in row.iter_mut().enumerate() {
                *x = i as u8;
            }
            row
        }; NUM_STATES];
        rules2table_impl! {(NUM_STATES pair_table) $($tt)+}
        pair_table
    }};
}

trait IteratorExt: Iterator {
    /// Tests if all elements of the iterator are equal.
    fn all_equal(&mut self) -> bool
    where
        Self::Item: PartialEq,
        Self: Sized,
    {
        self.next().map_or(true, |first| self.all(|x| x == first))
    }
}

impl<I: Iterator> IteratorExt for I {}

fn overlap<T: PartialEq, I: IntoIterator>(a: &[T], b: I) -> usize
where
    I::Item: Borrow<T>,
    I::IntoIter: ExactSizeIterator + Clone,
{
    let b = b.into_iter();
    (1..min(a.len(), b.len()))
        .rev()
        .find(|&n| {
            a[a.len() - n..]
                .iter()
                .zip(b.clone())
                .all(|(x, y)| x == y.borrow())
        })
        .unwrap_or(0)
}

const UNICODE_LIMIT: u32 = 0x110000;
const ASCII_LIMIT: u32 = 0x80;
const SMALL_DATA_BLOCKS_PER_BMP_BLOCK: u32 = 1 << (BMP_SHIFT - SHIFT_3);
/// Number of code points per index-2 table entry.
const CP_PER_INDEX_2_ENTRY: u32 = 1 << SHIFT_2;

#[derive(Clone, Copy, PartialEq, Debug)]
enum Index<T> {
    AllSame { value: T },
    Mixed { data_block: u32 },
}

/// UCPTrie builder.
///
/// See: [ICU Code Point Tries]
///
/// [ICU Code Point Tries]: https://icu.unicode.org/design/struct/utrie
#[derive(Default)]
struct CpTrieBuilder<T> {
    /// Index-3 table.
    index: Vec<Index<T>>,
    data: Vec<T>,
    initial_value: T,
}

impl<T: Copy + PartialEq + Eq + Hash> CpTrieBuilder<T> {
    fn new(initial_value: T) -> Self {
        Self {
            index: Vec::with_capacity(UNICODE_LIMIT as usize >> SHIFT_3),
            data: Vec::new(),
            initial_value,
        }
    }

    fn set_range(&mut self, Range { mut start, end }: Range<u32>, value: T) {
        if start >= end {
            return; // Empty range
        }
        if end as usize > self.index.len() {
            // Round up to CP_PER_INDEX_2_ENTRY boundary to simplify compaction
            let c = (end + CP_PER_INDEX_2_ENTRY - 1) & !(CP_PER_INDEX_2_ENTRY - 1);
            self.index.resize(
                c as usize >> SHIFT_3,
                Index::AllSame {
                    value: self.initial_value,
                },
            );
        }

        // Set partial block at [start, next block boundary)
        let block_start = start & !(SMALL_DATA_BLOCK_LENGTH - 1);
        if start > block_start {
            let block = self.data_block(start);
            let block = &mut self.data[block as usize..][..SMALL_DATA_BLOCK_LENGTH as usize]
                [(start & (SMALL_DATA_BLOCK_LENGTH - 1)) as usize..];
            if end < block_start + SMALL_DATA_BLOCK_LENGTH {
                block[..((end - start) & (SMALL_DATA_BLOCK_LENGTH - 1)) as usize].fill(value);
                return;
            }
            block.fill(value);
            start = block_start + SMALL_DATA_BLOCK_LENGTH;
        }

        // Fill all full blocks
        while start < end & !(SMALL_DATA_BLOCK_LENGTH - 1) {
            match &mut self.index[start as usize >> SHIFT_3] {
                Index::AllSame { value: prev_value } => *prev_value = value,
                Index::Mixed { data_block } => {
                    self.data[*data_block as usize..][..SMALL_DATA_BLOCK_LENGTH as usize]
                        .fill(value);
                }
            }
            start += SMALL_DATA_BLOCK_LENGTH;
        }

        // Set partial block at [last block boundary..end)
        let rest = end & (SMALL_DATA_BLOCK_LENGTH - 1);
        if rest > 0 {
            let block = self.data_block(start) as u32;
            self.data[block as usize..][..rest as usize].fill(value);
        }
    }

    fn data_block(&mut self, c: u32) -> u32 {
        let i = c as usize >> SHIFT_3;
        if let Index::Mixed { data_block } = self.index[i] {
            return data_block; // Already allocated
        }

        let (block_len, small_blocks) = if i < (BMP_LIMIT << SHIFT_3) as usize {
            let i_start = i & !(SMALL_DATA_BLOCKS_PER_BMP_BLOCK as usize - 1);
            (
                BMP_DATA_BLOCK_LENGTH,
                i_start..i_start + SMALL_DATA_BLOCKS_PER_BMP_BLOCK as usize,
            )
        } else {
            (SMALL_DATA_BLOCK_LENGTH, i..i + 1)
        };
        // Allocate a new data block
        let new_block = self.data.len() as u32;
        self.data
            .extend(iter::repeat(self.initial_value).take(block_len as usize));

        for (k, i) in small_blocks.clone().enumerate() {
            let prev_value = if let Index::AllSame { value } = self.index[i] {
                value
            } else {
                unreachable!()
            };
            let block = new_block + k as u32 * SMALL_DATA_BLOCK_LENGTH;
            self.data[block as usize..][..SMALL_DATA_BLOCK_LENGTH as usize].fill(prev_value);
            self.index[i] = Index::Mixed { data_block: block };
        }
        new_block + SMALL_DATA_BLOCK_LENGTH * (i - small_blocks.start) as u32
    }

    fn get(&self, c: u32) -> T {
        match self.index[c as usize >> SHIFT_3] {
            Index::AllSame { value } => value,
            Index::Mixed { data_block } => {
                self.data[(data_block + (c & (SMALL_DATA_BLOCK_LENGTH - 1))) as usize]
            }
        }
    }

    // Compact arrays by
    //
    // * removing blocks identical to earlier ones
    // * overlapping each block as much as possible with the previously written one

    fn compact_data(&mut self) {
        let mut new_data = Vec::with_capacity(self.data.len());
        // Always store ASCII data linearly at start
        new_data.extend((0..ASCII_LIMIT).map(|i| self.get(i)));
        self.index
            .iter_mut()
            .take(ASCII_LIMIT as usize >> SHIFT_3)
            .step_by(SMALL_DATA_BLOCKS_PER_BMP_BLOCK as usize)
            .enumerate()
            .for_each(|(i, x)| {
                *x = Index::Mixed {
                    data_block: BMP_DATA_BLOCK_LENGTH * i as u32,
                }
            });

        let mut block_len = BMP_DATA_BLOCK_LENGTH;
        let mut uniform_blocks = HashMap::new();
        let mut block_index = BlockIndex::new(self.data.len(), block_len as usize);
        let mut inc = SMALL_DATA_BLOCKS_PER_BMP_BLOCK as usize;
        let mut i = ASCII_LIMIT as usize >> SHIFT_3;
        while i < self.index.len() {
            if i == BMP_LIMIT as usize >> SHIFT_3 {
                block_len = SMALL_DATA_BLOCK_LENGTH;
                inc = 1;
                block_index.clear(block_len as usize);
                block_index.extend(&new_data);
            }

            let old_index = match self.index[i] {
                // Check if all of fast-range data block's blocks have all same or turn into mixed
                Index::AllSame { value }
                    if !self.index[i..][1..inc]
                        .iter()
                        .all(|x| matches!(x, Index::AllSame { value: v } if *v == value)) =>
                {
                    Index::Mixed {
                        data_block: self.data_block((i as u32) << SHIFT_3), // Turn into mixed block
                    }
                }
                // Check if really mixed
                x @ Index::Mixed { data_block } => {
                    let block = &self.data[data_block as usize..][..block_len as usize];
                    let all_same = block.iter().skip(1).all(|&x| x == block[0]);
                    if all_same {
                        Index::AllSame { value: block[0] }
                    } else {
                        x
                    }
                }
                x => x,
            };
            let new_index = match old_index {
                Index::AllSame { value } => {
                    // Is there another uniform block with the same value?
                    if let Some(j) = match uniform_blocks.entry(value) {
                        Entry::Occupied(entry) => Some(*entry.get()),
                        Entry::Vacant(entry) => {
                            entry.insert(i as u32);
                            None
                        }
                    } {
                        if let Index::Mixed { data_block } = self.index[j as usize] {
                            data_block
                        } else {
                            unreachable!()
                        }
                    } else if let Some(n) = block_index
                        .find_block(&new_data, iter::repeat(value).take(block_len as usize))
                    {
                        n
                    } else {
                        let overlap = new_data
                            .iter()
                            .rev()
                            .take(block_len as usize - 1)
                            .take_while(|&&x| x == value)
                            .count();
                        let new_index = (new_data.len() - overlap) as u32;
                        new_data.extend(iter::repeat(value).take(block_len as usize - overlap));
                        block_index.extend(&new_data);
                        new_index
                    }
                }
                Index::Mixed { data_block } => {
                    let block = &self.data[data_block as usize..][..block_len as usize];
                    if let Some(n) = block_index.find_block(&new_data, block) {
                        n
                    } else {
                        let overlap = overlap(&new_data, block);
                        let new_index = (new_data.len() - overlap) as u32;
                        new_data.extend_from_slice(&block[overlap as usize..]);
                        block_index.extend(&new_data);
                        new_index
                    }
                }
            };
            self.index[i] = Index::Mixed {
                data_block: new_index,
            };
            i += inc;
        }

        self.data = new_data;
    }

    fn compact_index(&mut self) -> Vec<u16> {
        let fast_index_len = BMP_LIMIT as usize >> BMP_SHIFT;
        let index2_capacity =
            (self.index.len() - (BMP_LIMIT as usize >> SHIFT_3)) >> (SHIFT_2 - SHIFT_3);
        let index1_len =
            (index2_capacity + INDEX_2_BLOCK_LENGTH as usize - 1) >> (SHIFT_1 - SHIFT_2);
        let index1_end = fast_index_len + index1_len;
        let mut index16 = Vec::with_capacity(index1_end + index2_capacity);
        let mut block_index = BlockIndex::new(index16.capacity(), INDEX_3_BLOCK_LENGTH as usize);

        let (fast_index, small_index) = self.index.split_at(BMP_LIMIT as usize >> SHIFT_3);
        // Condense fast index table
        index16.extend(
            fast_index
                .iter()
                .step_by(SMALL_DATA_BLOCKS_PER_BMP_BLOCK as usize)
                .map(|x| {
                    if let Index::Mixed { data_block: i3 } = x {
                        *i3 as u16
                    } else {
                        unreachable!()
                    }
                }),
        );
        debug_assert_eq!(index16.len(), fast_index_len);
        block_index.extend(&index16);

        index16.extend(iter::repeat(0).take(index1_len)); // Reserve space for index-1 table
        block_index.skip(index1_len);

        // Compact the index-3 table and write uncompacted index-2 table
        let index2: Vec<_> = small_index
            .chunks_exact(INDEX_3_BLOCK_LENGTH as usize)
            .map(|block| {
                let block = block.iter().map(|x| {
                    if let Index::Mixed { data_block } = x {
                        *data_block
                    } else {
                        unreachable!()
                    }
                });
                let ored = block.clone().fold(0, |acc, i3| acc | i3);

                if ored <= 0xffff {
                    let block = block.map(|x| x as u16);
                    if let Some(n) = block_index.find_block(&index16, block.clone()) {
                        n as u16
                    } else {
                        let overlap = overlap(&index16[index1_end..], block.clone());
                        let i3 = (index16.len() - overlap) as u16;
                        index16.extend(block.skip(overlap));
                        block_index.extend(&index16);
                        i3
                    }
                } else {
                    todo!() // Encode index-3 block with one or more data indices exceeding 16 bits
                }
            })
            .collect();

        // Compact the index-2 table and write the index-1 table
        debug_assert_eq!(
            INDEX_2_BLOCK_LENGTH, INDEX_3_BLOCK_LENGTH,
            "cannot reuse block index"
        );
        for (i, block) in index2.chunks(INDEX_2_BLOCK_LENGTH as usize).enumerate() {
            let i2 = if let Some(n) = block_index.find_block(&index16, block) {
                n as u16
            } else {
                let overlap = overlap(&index16[index1_end..], block);
                let i2 = (index16.len() - overlap) as u16;
                index16.extend(&block[overlap..]);
                block_index.extend(&index16);
                i2
            };

            let i1 = fast_index_len + i;
            index16[i1] = i2;
        }

        index16
    }

    fn build(mut self) -> CpTrie<T> {
        if self.index.len() < BMP_LIMIT as usize >> SHIFT_3 {
            self.index.resize(
                BMP_LIMIT as usize >> SHIFT_3,
                Index::AllSame {
                    value: self.initial_value,
                },
            );
        }
        self.compact_data();
        let high_start = {
            let i = self
                .index
                .last()
                .filter(|&x| {
                    if let Index::Mixed { data_block } = x {
                        self.data[*data_block as usize..][..SMALL_DATA_BLOCK_LENGTH as usize]
                            .iter()
                            .all(|&x| x == self.initial_value)
                    } else {
                        false
                    }
                })
                .map(|i| self.index.iter().rposition(|x| x != i).unwrap())
                .map_or(self.index.len(), |i| i + 1) as u32;
            let c = ((i << SHIFT_3) + CP_PER_INDEX_2_ENTRY - 1) & !(CP_PER_INDEX_2_ENTRY - 1);
            max(c, BMP_LIMIT)
        };
        self.index.truncate(high_start as usize >> SHIFT_3);
        let index = self.compact_index();

        CpTrie {
            high_start,
            index,
            data: self.data,
        }
    }
}

struct BlockIndex<T> {
    set: hashbrown::HashMap<u32, ()>,
    block_len: usize,
    prev_end: usize,
    phantom: PhantomData<T>,
}

impl<T: PartialEq + Hash> BlockIndex<T> {
    fn new(capacity: usize, block_len: usize) -> Self {
        Self {
            set: hashbrown::HashMap::with_capacity(capacity),
            block_len,
            prev_end: 0,
            phantom: PhantomData,
        }
    }

    fn clear(&mut self, new_block_len: usize) {
        self.set.clear();
        self.block_len = new_block_len;
        self.prev_end = 0;
    }

    fn skip(&mut self, n: usize) {
        self.prev_end += n + self.block_len - 1;
    }

    fn entry<I: IntoIterator>(
        &mut self,
        data: &[T],
        block: I,
    ) -> RawEntryMut<u32, (), hashbrown::hash_map::DefaultHashBuilder>
    where
        I::Item: Borrow<T>,
        I::IntoIter: Clone,
    {
        let block = block.into_iter();
        let hash = {
            let mut state = self.set.hasher().build_hasher();
            block.clone().for_each(|x| x.borrow().hash(&mut state));
            state.finish()
        };
        let is_match = |&j: &u32| {
            data[j as usize..][..self.block_len]
                .iter()
                .zip(block.clone())
                .all(|(x, y)| x == y.borrow())
        };
        self.set.raw_entry_mut().from_hash(hash, is_match)
    }

    fn extend(&mut self, data: &[T]) {
        let start = (self.prev_end + 1).saturating_sub(self.block_len);
        if data.len() <= start {
            return;
        }
        for (i, block) in data[start..].windows(self.block_len).enumerate() {
            let i = (start + i) as u32;
            self.entry(data, block).insert(i, ());
        }
        self.prev_end = data.len();
    }

    fn find_block<I: IntoIterator>(&mut self, data: &[T], block: I) -> Option<u32>
    where
        I::Item: Borrow<T>,
        I::IntoIter: Clone,
    {
        if let RawEntryMut::Occupied(x) = self.entry(data, block) {
            Some(*x.key())
        } else {
            None
        }
    }
}

struct CpTrie<T> {
    high_start: u32,
    index: Vec<u16>,
    data: Vec<T>,
}

fn main() -> Result<(), Box<dyn error::Error>> {
    println!("cargo:rerun-if-changed=LineBreak.txt");
    debug_assert!(NUM_STATES <= 0x3F, "too many states");

    let pair_table = rules2table! {
        // Non-tailorable Line Breaking Rules
        // LB1 Assign a line breaking class to each code point of the input. Resolve AI, CB, CJ,
        // SA, SG, and XX into other line breaking classes depending on criteria outside the scope
        // of this algorithm.
        Treat (AI | SG | XX | SA) as if it were AL, Treat CJ as if it were NS,
        // Start and end of text:
        sot '×', // LB2 Never break at the start of text.
        '!' eot, // LB3 Always break at the end of text.
        // Mandatory breaks:
        BK '!', // LB4 Always break after hard line breaks.
        // LB5 Treat CR followed by LF, as well as CR, LF, and NL as hard line breaks.
        CR '×' LF, CR '!', LF '!', NL '!',
        '×' (BK | CR | LF | NL), // LB6 Do not break before hard line breaks.
        // Explicit breaks and non-breaks:
        '×' SP, '×' ZW, // LB7 Do not break before spaces or zero width space.
        // LB8 Break before any character following a zero-width space, even if one or more spaces
        // intervene.
        (ZW | ZWSP) '÷', Treat (ZW | ZWSP) SP as if it were ZWSP, Treat ZWSP as if it were SP,
        // ZWJ '×', // XXX Handled explicitly // LB8a Do not break after a zero width joiner.
        // Combining marks:
        // LB9 Do not break a combining character sequence; treat it as if it has the line breaking
        // class of the base character in all of the following rules. Treat ZWJ as if it were CM.
        Treat X (CM | ZWJ)* as if it were X where X = [^BK CR LF NL SP ZW sot eot ZWSP OPSP QUSP CLSP CPSP B2SP],
        Treat (CM | ZWJ) as if it were AL, // LB10 Treat any remaining combining mark or ZWJ as AL.
        // Word joiner:
        '×' WJ, WJ '×', // LB11 Do not break before or after Word joiner and related characters.
        // Non-breaking characters:
        GL '×', // LB12 Do not break after NBSP and related characters.

        // Tailorable Line Breaking Rules
        // LB12a Do not break before NBSP and related characters, except after spaces and hyphens.
        [^SP BA HY sot eot ZWSP OPSP QUSP CLSP CPSP B2SP] '×' GL,
        // LB13 Do not break before ‘]’ or ‘!’ or ‘;’ or ‘/’, even after spaces.
        '×' CL, '×' CP, '×' EX, '×' IS, '×' SY,
        // LB14 Do not break after ‘[’, even after spaces.
        (OP | OPSP) '×', Treat (OP | OPSP) SP as if it were OPSP, Treat ZWSP as if it were SP,
        // LB15 Do not break within ‘”[’, even with intervening spaces.
        (QU | QUSP) '×' OP, Treat (QU | QUSP) SP as if it were QUSP, Treat QUSP as if it were SP,
        // LB16 Do not break between closing punctuation and a nonstarter (lb=NS), even with
        // intervening spaces.
        (CL | CLSP | CP | CPSP) '×' NS,
        Treat (CL | CLSP) SP as if it were CLSP, Treat CLSP as if it were SP,
        Treat (CP | CPSP) SP as if it were CPSP, Treat CPSP as if it were SP,
        // LB17 Do not break within ‘——’, even with intervening spaces.
        (B2 | B2SP) '×' B2, Treat (B2 | B2SP) SP as if it were B2SP, Treat B2SP as if it were SP,
        // Spaces:
        SP '÷', // LB18 Break after spaces.
        // Special case rules:
        '×' QU, QU '×', // LB19 Do not break before or after quotation marks, such as ‘”’.
        '÷' CB, CB '÷', // LB20 Break before and after unresolved CB.
        // LB21 Do not break before hyphen-minus, other hyphens, fixed-width spaces, small kana,
        // and other non-starters, or after acute accents.
        '×' BA, '×' HY, '×' NS, BB '×',
        // LB21a Don't break after Hebrew + Hyphen. // XXX Use a single state, HLHYBA, for HLHY and HLBA
        HLHYBA '×', Treat HL (HY | BA) as if it were HLHYBA, Treat HLHYBA as if it were HY,
        SY '×' HL, // LB21b Don’t break between Solidus and Hebrew letters.
        '×' IN, // LB22 Do not break before ellipses.
        // Numbers:
        (AL | HL) '×' NU, NU '×' (AL | HL), // LB23 Do not break between digits and letters.
        // LB23a Do not break between numeric prefixes and ideographs, or between ideographs and
        // numeric postfixes.
        PR '×' (ID | EB | EM), (ID | EB | EM) '×' PO,
        // LB24 Do not break between numeric prefix/postfix and letters, or between letters and
        // prefix/postfix.
        (PR | PO) '×' (AL | HL), (AL | HL) '×' (PR | PO),
        // LB25 Do not break between the following pairs of classes relevant to numbers:
        CL '×' PO, CP '×' PO, CL '×' PR, CP '×' PR, NU '×' PO, NU '×' PR, PO '×' OP, PO '×' NU, PR '×' OP, PR '×' NU, HY '×' NU, IS '×' NU, NU '×' NU, SY '×' NU,
        // Korean syllable blocks
        // LB26 Do not break a Korean syllable.
        JL '×' (JL | JV | H2 | H3), (JV | H2) '×' (JV | JT), (JT | H3) '×' JT,
        // LB27 Treat a Korean Syllable Block the same as ID.
        (JL | JV | JT | H2 | H3) '×' PO, PR '×' (JL | JV | JT | H2 | H3),
        // Finally, join alphabetic letters into words and break everything else.
        (AL | HL) '×' (AL | HL), // LB28 Do not break between alphabetics (“at”).
        IS '×' (AL | HL), // LB29 Do not break between numeric punctuation and alphabetics (“e.g.”).
        // LB30 Do not break between letters, numbers, or ordinary symbols and opening or closing
        // parentheses.
        (AL | HL | NU) '×' OP, CP '×' (AL | HL | NU),
        // LB30a Break between two regional indicator symbols if and only if there are an even
        // number of regional indicators preceding the position of the break.
        RI '×' RI, Treat RI RI as if it were RIRI, Treat RIRI as if it were RI,
        EB '×' EM, // LB30b Do not break between an emoji base and an emoji modifier.
        '÷' ALL, ALL '÷', // LB31 Break everywhere else.
    };

    // Synthesize all non-"safe" pairs from pair table
    let unsafe_pairs = (0..NUM_CLASSES).into_iter().flat_map(|j| {
        (0..NUM_CLASSES).into_iter().filter_map(move |i| {
            // All states that could have resulted from break class "i"
            let possible_states = pair_table
                .iter()
                .map(|row| (row[i] & !(ALLOWED_BREAK_BIT | MANDATORY_BREAK_BIT)) as usize);
            // Check if all state transitions due to "j" are the same
            if possible_states.map(|s| pair_table[s][j]).all_equal() {
                None
            } else {
                Some((i, j))
            }
        })
    });

    let re = Regex::new(
        r"(?x)^
(?P<start>[[:xdigit:]]{4,}) # Unicode code point
(?:\.{2}(?P<end>[[:xdigit:]]{4,}))? # End of range
;
(?P<lb>\w{2,3}) # Line_Break property",
    )?;
    let prop_ranges = BufReader::new(File::open("LineBreak.txt")?)
        .lines()
        .map(Result::unwrap)
        .filter(|l| !(l.starts_with('#') || l.is_empty()))
        .map(|l| {
            let caps = re.captures(&l).unwrap();
            let start = u32::from_str_radix(&caps["start"], 16).unwrap();
            let end = caps
                .name("end")
                .map_or(start, |m| u32::from_str_radix(m.as_str(), 16).unwrap());
            let lb: BreakClass = caps["lb"].parse().unwrap();
            (start..end + 1, lb)
        });
    let trie = {
        // All code points, assigned and unassigned, that are not listed explicitly are given the value "XX"
        let mut builder = CpTrieBuilder::new(XX);
        // The unassigned code points in the following blocks default to "ID"
        builder.set_range(0x3400..0x4DBF + 1, ID);
        builder.set_range(0x4E00..0x9FFF + 1, ID);
        builder.set_range(0xF900..0xFAFF + 1, ID);
        // All undesignated code points in Planes 2 and 3, whether inside or outside of allocated blocks, default to "ID"
        builder.set_range(0x20000..0x2FFFD + 1, ID);
        builder.set_range(0x30000..0x3FFFD + 1, ID);
        // All unassigned code points in the following Plane 1 range, whether inside or outside of allocated blocks, also default to "ID"
        builder.set_range(0x1F000..0x1FAFF + 1, ID);
        builder.set_range(0x1FC00..0x1FFFD + 1, ID);
        // The unassigned code points in the following block default to "PR"
        builder.set_range(0x20A0..0x20CF + 1, PR);

        prop_ranges.for_each(|(range, lb)| builder.set_range(range, lb));
        builder.build()
    };

    let out_dir = env::var("OUT_DIR")?;
    let dest_path = Path::new(&out_dir).join("tables.rs");
    let mut stream = BufWriter::new(File::create(&dest_path)?);
    writeln!(
        stream,
        "const BREAK_PROP_TRIE_HIGH_START: u32 = {};
static BREAK_PROP_TRIE_INDEX: [u16; {}] = {:?};
static BREAK_PROP_TRIE_DATA: [BreakClass; {}] = [",
        trie.high_start,
        trie.index.len(),
        trie.index,
        trie.data.len(),
    )?;
    trie.data
        .into_iter()
        .flat_map(|x| [BREAK_CLASS_TABLE[x as usize], ","])
        .try_for_each(|s| write!(stream, "{}", s))?;
    write!(
        stream,
        "];

static PAIR_TABLE: [[u8; {}]; {}] = [",
        NUM_CLASSES_EOT, NUM_STATES
    )?;
    for row in &pair_table {
        write!(stream, "[")?;
        for x in row {
            write!(stream, "{},", x)?;
        }
        write!(stream, "],")?;
    }
    writeln!(
        stream,
        r"];

        fn is_safe_pair(a: BreakClass, b: BreakClass) -> bool {{
            !matches!((a, b), {})
        }}",
        unsafe_pairs
            .map(|(i, j)| format!("({}, {})", BREAK_CLASS_TABLE[i], BREAK_CLASS_TABLE[j]))
            .collect::<Vec<_>>()
            .join("|")
    )?;

    Ok(())
}
