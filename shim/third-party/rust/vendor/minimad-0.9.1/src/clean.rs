pub fn is_blank(s: &str) -> bool {
    s.chars().all(char::is_whitespace)
}

/// remove the superfluous lines and indentations you get when you insert
///  in your code a multi-line raw literal.
pub fn lines(src: &str) -> Vec<&str> {
    let mut result_lines: Vec<&str> = Vec::new();
    let mut src_lines = src.lines();
    if let Some(mut first_line) = src_lines.next() {
        if first_line.is_empty() {
            if let Some(s) = src_lines.next() {
                first_line = s;
            }
        }
        result_lines.push(first_line);
        for line in src_lines {
            result_lines.push(line);
        }
        if is_blank(result_lines[result_lines.len() - 1]) {
            result_lines.truncate(result_lines.len() - 1);
        }
        if result_lines.len() > 1 {
            let mut white_prefix = String::new();
            for char in first_line.chars() {
                if char.is_whitespace() {
                    white_prefix.push(char);
                } else {
                    break;
                }
            }
            if !white_prefix.is_empty()
                && result_lines
                    .iter()
                    .all(|line| line.starts_with(&white_prefix) || is_blank(line))
            {
                result_lines = result_lines
                    .iter()
                    .map(|line| {
                        if is_blank(line) {
                            line
                        } else {
                            &line[white_prefix.len()..]
                        }
                    })
                    .collect();
            }
        }
    }
    result_lines
}

#[test]
fn test_lines_cleaning() {
    let lines = lines(
        r#"
        test
            hop
        hip
    "#,
    );
    assert_eq!(lines.len(), 3);
    assert_eq!(lines[0], "test");
    assert_eq!(lines[1], "    hop");
    assert_eq!(lines[2], "hip");
}
