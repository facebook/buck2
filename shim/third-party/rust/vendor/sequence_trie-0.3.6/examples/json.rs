extern crate sequence_trie;
extern crate serde_json;

use sequence_trie::SequenceTrie;

fn main() {
    let mut trie = SequenceTrie::new();
    trie.insert(&["hello", "world"], 56);
    trie.insert(&["hello", "moon"], 57);

    println!("{}", serde_json::to_string_pretty(&trie).unwrap());
}
