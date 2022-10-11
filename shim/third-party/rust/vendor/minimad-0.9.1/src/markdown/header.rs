use {
    crate::*,
    std::cmp,
};

/// count the number of '#' at start. Return 0 if they're
/// not followed by a ' ' or if they're too many
pub fn header_level(src: &str) -> usize {
    let src = src.as_bytes();
    let mut l: usize = src.len();
    if l > 2 {
        l = cmp::min(src.len() - 1, MAX_HEADER_DEPTH + 1);
        for i in 0..l {
            match src[i] {
                b'#' => {}
                b' ' => {
                    return i;
                }
                _ => {
                    return 0;
                }
            }
        }
    }
    0
}

#[test]
fn header_level_count() {
    assert_eq!(header_level(""), 0);
    assert_eq!(header_level("#"), 0);
    assert_eq!(header_level("# "), 0); // we don't allow empty headers
    assert_eq!(header_level("# A"), 1);
    assert_eq!(header_level(" "), 0);
    assert_eq!(header_level("test"), 0);
    assert_eq!(header_level("###b"), 0);
    assert_eq!(header_level("###"), 0);
    assert_eq!(header_level("### b"), 3);
    assert_eq!(header_level(" a b"), 0);
    assert_eq!(header_level("# titre"), 1);
    assert_eq!(header_level("#### *titre*"), 4);
    assert_eq!(header_level("######## a b"), 8);
    assert_eq!(header_level("######### a b"), 0); // too deep
}

