extern crate serde_json;

use super::SequenceTrie;

type TestTrie = SequenceTrie<String, u32>;

fn test_trie() -> TestTrie {
    let mut trie = SequenceTrie::new();
    trie.insert(vec!["a"], 1u32);
    trie.insert(vec!["a", "b", "c", "d"], 4u32);
    trie.insert(vec!["a", "b", "x", "y"], 25u32);
    trie
}

#[test]
fn roundtrip() {
    let trie = test_trie();
    let rtrip = serde_json::to_string(&trie)
        .and_then(|s| serde_json::from_str::<TestTrie>(&s))
        .unwrap();
    assert_eq!(rtrip, trie);
}

#[test]
fn json_deserialise() {
    let test_str = r#"
        {
          "value": null,
          "children": {
            "hello": {
              "value": null,
              "children": {
                "world": {
                  "value": 56,
                  "children": {}
                },
                "moon": {
                  "value": 57,
                  "children": {}
                }
              }
            }
          }
        }
    "#;

    let trie = serde_json::from_str::<TestTrie>(test_str).unwrap();

    assert_eq!(trie.get(vec!["hello"]), None);
    assert_eq!(trie.get(vec!["hello", "world"]), Some(&56));
    assert_eq!(trie.get(vec!["hello", "moon"]), Some(&57));
}
