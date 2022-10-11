extern crate sequence_trie;

use sequence_trie::SequenceTrie;

fn main() {
    let mut trie = SequenceTrie::<&str, bool>::new();
    trie.insert(&["wow", "cow"], true);
    trie.insert(&["wow", "this"], false);
    println!("{:?}", trie);
}
