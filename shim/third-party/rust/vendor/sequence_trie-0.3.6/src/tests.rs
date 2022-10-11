use std::collections::HashSet;

use super::SequenceTrie;

fn make_trie() -> SequenceTrie<char, u32> {
    let mut trie = SequenceTrie::new();
    trie.insert(&[], 0u32);
    trie.insert(&['a'], 1u32);
    trie.insert(&['a', 'b', 'c', 'd'], 4u32);
    trie.insert(&['a', 'b', 'x', 'y'], 25u32);
    trie
}

#[test]
fn get() {
    let trie = make_trie();
    let data = [(vec![], Some(0u32)),
                (vec!['a'], Some(1u32)),
                (vec!['a', 'b'], None),
                (vec!['a', 'b', 'c'], None),
                (vec!['a', 'b', 'x'], None),
                (vec!['a', 'b', 'c', 'd'], Some(4u32)),
                (vec!['a', 'b', 'x', 'y'], Some(25u32)),
                (vec!['b', 'x', 'y'], None)];
    for &(ref key, value) in data.iter() {
        assert_eq!(trie.get(key), value.as_ref());
    }
}

#[test]
fn get_mut() {
    let mut trie = make_trie();
    let key = ['a', 'b', 'c', 'd'];
    *trie.get_mut(&key).unwrap() = 77u32;
    assert_eq!(*trie.get(&key).unwrap(), 77u32);
}

#[test]
fn get_ancestor() {
    let trie = make_trie();
    let data = [(vec![], 0u32),
                (vec!['a'], 1u32),
                (vec!['a', 'b'], 1u32),
                (vec!['a', 'b', 'c'], 1u32),
                (vec!['a', 'b', 'c', 'd'], 4u32),
                (vec!['a', 'b', 'x'], 1u32),
                (vec!['a', 'b', 'x', 'y'], 25u32),
                (vec!['p', 'q'], 0u32),
                (vec!['a', 'p', 'q'], 1u32)];
    for &(ref key, value) in data.iter() {
        assert_eq!(*trie.get_ancestor(key).unwrap(), value);
    }
}

#[test]
fn get_prefix_nodes() {
    let trie = make_trie();
    let prefix_nodes = trie.get_prefix_nodes(&['a', 'b', 'z']);
    // There should be 3 nodes: root, a, b.
    assert_eq!(prefix_nodes.len(), 3);
    let values = [Some(0u32), Some(1u32), None];
    for (node, value) in prefix_nodes.iter().zip(values.iter()) {
        assert_eq!(node.value, *value);
    }
}

#[test]
fn remove() {
    let mut trie = make_trie();
    // If the node has children, its value should be set to `None`.
    println!("Remove ['a']");
    println!("Before: {:?}", trie);
    trie.remove(&['a']);
    println!("After: {:?}", trie);
    assert_eq!(trie.get_node(&['a']).unwrap().value, None);

    // Same as above, but for the root.
    println!("Remove []");
    println!("Before: {:?}", trie);
    trie.remove(&[]);
    println!("After: {:?}", trie);
    assert_eq!(trie.get_node(&[]).unwrap().value, None);

    // Check that lower levels are still accessible.
    assert_eq!(trie.get(&['a', 'b', 'c', 'd']), Some(&4u32));

    // Check that removing a leaf with an empty parent also
    // deletes the parent.
    println!("Remove ['a', 'b', 'c', 'd']");
    println!("Before: {:?}", trie);
    trie.remove(&['a', 'b', 'c', 'd']);
    println!("After: {:?}", trie);
    assert!(trie.get_node(&['a', 'b', 'c', 'd']).is_none());
    assert!(trie.get_node(&['a', 'b', 'c']).is_none());
    assert!(trie.get_node(&['a', 'b']).is_some());

    // Bump off the rest of the Trie!
    println!("Remove ['a', 'b', 'x', 'y']");
    println!("Before: {:?}", trie);
    trie.remove(&['a', 'b', 'x', 'y']);
    println!("After: {:?}", trie);
    assert!(trie.get_node(&['a', 'b', 'x', 'y']).is_none());
    assert!(trie.get_node(&['a', 'b', 'x']).is_none());
    assert!(trie.get_node(&['a', 'b']).is_none());
    assert!(trie.get_node(&['a']).is_none());
    assert!(trie.value.is_none());
    assert!(trie.children.is_empty());
}

#[test]
fn key_iter() {
    let trie = make_trie();
    let obs_keys: HashSet<Vec<char>> = trie.keys()
        .map(|v| -> Vec<char> { v.iter().map(|&&x| x).collect() })
        .collect();
    let mut exp_keys: HashSet<Vec<char>> = HashSet::new();
    exp_keys.insert(vec![]);
    exp_keys.insert(vec!['a']);
    exp_keys.insert(vec!['a', 'b', 'c', 'd']);
    exp_keys.insert(vec!['a', 'b', 'x', 'y']);
    assert_eq!(exp_keys, obs_keys);
}

#[derive(PartialEq, Eq, Hash, Clone, PartialOrd, Ord)]
struct Key {
    field: usize,
}

#[test]
fn struct_key() {
    SequenceTrie::<Key, usize>::new();
}

#[test]
fn eq() {
    let first_trie = make_trie();
    let second_trie = make_trie();
    assert_eq!(first_trie, second_trie);
}

#[test]
fn eq_empty() {
    let first_trie: SequenceTrie<u8, i32> = SequenceTrie::new();
    let second_trie = SequenceTrie::new();
    assert_eq!(first_trie, second_trie);
}

#[test]
fn ne_value() {
    let mut first_trie = SequenceTrie::new();
    first_trie.insert(b"1234", 1234);
    first_trie.insert(b"1235", 1235);
    let mut second_trie = SequenceTrie::new();
    second_trie.insert(b"1234", 1234);
    second_trie.insert(b"1235", 1236);
    assert!(first_trie != second_trie);
}

#[test]
fn ne_key() {
    let mut first_trie = SequenceTrie::new();
    first_trie.insert(b"1234", 1234);
    first_trie.insert(b"1235", 1235);
    let mut second_trie = SequenceTrie::new();
    second_trie.insert(b"1234", 1234);
    second_trie.insert(b"1236", 1235);
    assert!(first_trie != second_trie);
}

#[test]
fn ne_missing_key() {
    let mut first_trie = SequenceTrie::new();
    first_trie.insert(b"1234", 1234);
    first_trie.insert(b"1235", 1235);
    let mut second_trie = SequenceTrie::new();
    second_trie.insert(b"1234", 1234);
    assert!(first_trie != second_trie);
}

#[test]
fn clone() {
    let first_trie = make_trie();
    let second_trie = first_trie.clone();
    assert_eq!(first_trie, second_trie);
}

#[test]
fn default() {
    let empty_trie: SequenceTrie<u8, i32> = ::std::default::Default::default();
    assert_eq!(empty_trie, SequenceTrie::new());
}

#[test]
fn string_trie() {
    let mut trie: SequenceTrie<String, ()> = SequenceTrie::new();
    trie.insert_owned(vec!["hello".to_string(), "world".to_string()], ());
    trie.insert(&["hello".to_string(), "world".to_string()], ());
    trie.insert(vec!["hello", "world"], ());
    trie.insert(["hello", "world"].iter().map(|&x| x), ());
}

#[test]
fn map() {
    let mut trie = SequenceTrie::new();
    trie.insert(&[0u32], 0u32);
    trie.insert(&[5u32], 10u32);
    trie.map(|node| node.value().map(|x| x + 1));
    for (k, v) in trie.iter() {
        assert_eq!(*v, 2*k[0] + 1);
    }
}

#[test]
fn non_static_lifetime() {
    let a = format!("a");
    let b = format!("b");
    let mut trie: SequenceTrie<&str, u32> = SequenceTrie::new();

    trie.insert(&[&a[..], &b[..]], 0u32);
}
