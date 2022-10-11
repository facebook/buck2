use std::cmp::max;

// strsplit is like `s.split(split)`, except that if `split` is "", it
// trims the leading and trailing empty elements, since the `lcs`
// logic won't handle those properly.
fn strsplit<'a>(s: &'a str, split: &str) -> Vec<&'a str> {
    let mut si = s.split(split);
    if split == "" {
        si.next();
    }
    let mut v: Vec<&str> = si.collect();
    if split == "" {
        v.pop();
    }
    v
}

// finds the longest common subsequences
// outputs the edit distance and a string containing
// all chars both inputs have in common
//
// This algorithm is based on
// https://en.wikipedia.org/wiki/Longest_common_subsequence_problem#Code_for_the_dynamic_programming_solution
#[allow(non_snake_case)]
#[cfg_attr(feature = "cargo-clippy", allow(many_single_char_names))]
pub fn lcs(orig: &str, edit: &str, split: &str) -> (i32, String) {
    // make list by custom splits
    let a = strsplit(orig, split);
    let b = strsplit(edit, split);

    let N = a.len();
    let M = b.len();

    let mut idx: Vec<usize> = Vec::with_capacity(N * M);
    idx.resize(N * M, 0);

    for i in 0..N {
        for j in 0..M {
            if b[j] == a[i] {
                if i == 0 || j == 0 {
                    idx[i * M + j] = 1;
                } else {
                    idx[i * M + j] = idx[(i - 1) * M + j - 1] + 1;
                }
            } else if i == 0 {
                if j == 0 {
                    idx[i * M + j] = 0;
                } else {
                    idx[i * M + j] = idx[i * M + j - 1];
                }
            } else if j == 0 {
                idx[i * M + j] = idx[(i - 1) * M + j];
            } else {
                idx[i * M + j] = max(idx[i * M + j - 1], idx[(i - 1) * M + j]);
            }
        }
    }

    let mut i = (N as isize) - 1;
    let mut j = (M as isize) - 1;
    let mut lcs = Vec::new();
    while i >= 0 && j >= 0 {
        let ui = i as usize;
        let uj = j as usize;
        if a[ui] == b[uj] {
            lcs.push(a[ui]);
            i -= 1;
            j -= 1;
        } else if j == 0 && i == 0 {
            break;
        } else if i == 0 || idx[ui * M + uj - 1] > idx[(ui - 1) * M + uj] {
            j -= 1;
        } else {
            i -= 1;
        }
    }

    lcs.reverse();
    ((N + M - 2 * lcs.len()) as i32, lcs.join(split))
}

#[test]
fn test_lcs() {
    assert_eq!(lcs("test", "tost", ""), (2, "tst".to_string()));
    assert_eq!(lcs("test", "test", ""), (0, "test".to_string()));

    assert_eq!(lcs("test", "test", " "), (0, "test".to_string()));

    assert_eq!(
        lcs(
            "The quick brown fox jumps over the lazy dog",
            "The quick brown dog leaps over the lazy cat",
            "",
        ),
        (16, "The quick brown o ps over the lazy ".to_string())
    );
    assert_eq!(
        lcs(
            "The quick brown fox jumps over the lazy dog",
            "The quick brown dog leaps over the lazy cat",
            " ",
        ),
        (6, "The quick brown over the lazy".to_string())
    );

    assert_eq!(
        lcs(
            "The quick brown fox jumps over the lazy dog",
            "The quick brown dog leaps over the lazy cat",
            "\n",
        ),
        (2, "".to_string())
    );
    assert_eq!(
        lcs(
            "The quick brown fox jumps over the lazy dog",
            "The quick brown fox jumps over the lazy dog",
            "\n",
        ),
        (0, "The quick brown fox jumps over the lazy dog".to_string())
    );

    assert_eq!(
        lcs("a b : c", "b a : b : c", " "),
        (2, "a b : c".to_string())
    );

    assert_eq!(lcs("", "a b c", ""), (5, "".to_string()));

    assert_eq!(lcs("", " a", " "), (1, "".to_string()));
}
