/// Parse a Python fromfile
///
/// fromfile's are
/// - One line equates to one argument
///   - No escaping is needed for spaces
///   - `--foo bar` would need to be `--foo\nbar` or `--foo=bar`
/// - Recursive
///
/// See [Python fromfile](https://docs.python.org/3/library/argparse.html#fromfile-prefix-chars).
pub fn parse_fromfile(content: &str, prefix: char) -> Vec<crate::Argument> {
    content
        .lines()
        .map(move |l| crate::Argument::parse_ref(l, prefix))
        .collect()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn empty() {
        let input = "";
        let expected: Vec<crate::Argument> = vec![];
        let actual = parse_fromfile(input, crate::PREFIX);
        assert_eq!(expected, actual);
    }

    #[test]
    fn sample() {
        let input = "--hello
world
@moon.txt
--goodbye
sun";
        let expected: Vec<crate::Argument> = vec![
            crate::Argument::PassThrough("--hello".into()),
            crate::Argument::PassThrough("world".into()),
            crate::Argument::Path("moon.txt".into()),
            crate::Argument::PassThrough("--goodbye".into()),
            crate::Argument::PassThrough("sun".into()),
        ];
        let actual = parse_fromfile(input, crate::PREFIX);
        assert_eq!(expected, actual);
    }
}
