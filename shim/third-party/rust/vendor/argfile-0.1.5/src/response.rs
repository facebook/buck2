/// Parse a Microsoft response file
///
/// See [Microsoft response files](https://docs.microsoft.com/en-us/cpp/build/reference/at-specify-a-compiler-response-file?view=msvc-170).
#[cfg(feature = "response")]
pub fn parse_response(content: &str, _prefix: char) -> Vec<crate::Argument> {
    shlex::split(content)
        .unwrap_or_default()
        .into_iter()
        .map(|s| crate::Argument::PassThrough(s.into()))
        .collect()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn empty() {
        let input = "";
        let expected: Vec<crate::Argument> = vec![];
        let actual = parse_response(input, crate::PREFIX);
        assert_eq!(expected, actual);
    }

    #[test]
    fn sample() {
        let input = "--hello world
@moon.txt
--goodbye 'walker texas'
sun";
        let expected: Vec<crate::Argument> = vec![
            crate::Argument::PassThrough("--hello".into()),
            crate::Argument::PassThrough("world".into()),
            crate::Argument::PassThrough("@moon.txt".into()),
            crate::Argument::PassThrough("--goodbye".into()),
            crate::Argument::PassThrough("walker texas".into()),
            crate::Argument::PassThrough("sun".into()),
        ];
        let actual = parse_response(input, crate::PREFIX);
        assert_eq!(expected, actual);
    }
}
