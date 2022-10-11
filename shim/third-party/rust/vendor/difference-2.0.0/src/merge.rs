use Difference;

// merges the changes from two strings, given a common substring
pub fn merge(orig: &str, edit: &str, common: &str, split: &str) -> Vec<Difference> {
    let mut ret = Vec::new();

    let mut l = orig.split(split).peekable();
    let mut r = edit.split(split).peekable();
    let mut c = common.split(split).peekable();

    // Turn empty strings into [], not [""]
    if orig == "" {
        l.next();
    }
    if edit == "" {
        r.next();
    }
    if common == "" {
        c.next();
    }

    while l.peek().is_some() || r.peek().is_some() {
        let mut same = Vec::new();
        while l.peek().is_some() && l.peek() == c.peek() && r.peek() == c.peek() {
            same.push(l.next().unwrap());
            r.next();
            c.next();
        }
        if !same.is_empty() {
            let joined = same.join(split);
            if split != "" || joined != "" {
                ret.push(Difference::Same(joined));
            }
        }

        let mut rem = Vec::new();
        while l.peek().is_some() && l.peek() != c.peek() {
            rem.push(l.next().unwrap());
        }
        if !rem.is_empty() {
            ret.push(Difference::Rem(rem.join(split)));
        }

        let mut add = Vec::new();
        while r.peek().is_some() && r.peek() != c.peek() {
            add.push(r.next().unwrap());
        }
        if !add.is_empty() {
            ret.push(Difference::Add(add.join(split)));
        }
    }

    ret
}


#[test]
fn test_merge() {
    assert_eq!(
        merge("testa", "tost", "tst", ""),
        vec![
            Difference::Same("t".to_string()),
            Difference::Rem("e".to_string()),
            Difference::Add("o".to_string()),
            Difference::Same("st".to_string()),
            Difference::Rem("a".to_string()),
        ]
    );

    assert_eq!(
        merge("", "a", "", ""),
        vec![Difference::Add("a".to_string())]
    );

    assert_eq!(
        merge("a\nb", "a\n\nb", "a\nb", "\n"),
        vec![
            Difference::Same("a".to_string()),
            Difference::Add("".to_string()),
            Difference::Same("b".to_string()),
        ]
    );

    assert_eq!(
        merge("a\n", "c\n", "\n", "\n"),
        vec![
            Difference::Rem("a".to_string()),
            Difference::Add("c".to_string()),
            Difference::Same("".to_string()),
        ]
    );
}
