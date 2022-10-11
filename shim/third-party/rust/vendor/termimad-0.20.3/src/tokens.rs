use {
    crate::*,
    minimad::*,
    unicode_width::UnicodeWidthChar,
};

#[derive(Debug)]
pub(crate) struct Token<'s> {
    pub compound: Compound<'s>,
    pub blank: bool,
    pub width: usize,
    pub start_in_compound: usize,
    pub end_in_compound: usize,
}

impl<'s> Token<'s> {
    pub fn to_compound(&self) -> Compound<'s> {
        let mut compound = self.compound.clone();
        compound.set_str(&self.compound.src[self.start_in_compound..self.end_in_compound]);
        compound
    }
}

/// Cut a composite into token, each one being either only spaces or without space, and
/// each one from one compound
pub(crate) fn tokenize<'s, 'c>(
    composite: &'c Composite<'s>,
    max_token_width: usize,
) -> Vec<Token<'s>> {
    let mut tokens: Vec<Token<'s>> = Vec::new();
    for compound in &composite.compounds {
        let mut token: Option<Token> = None;
        for (idx, char) in compound.src.char_indices() {
            let blank = char.is_whitespace() && !compound.code;
            let char_width = char.width().unwrap_or(0);
            if let Some(token) = token.as_mut() {
                if token.blank == blank && token.width + char_width <= max_token_width {
                    token.width += char_width;
                    token.end_in_compound += char.len_utf8();
                    continue;
                }
            }
            let new_token = Token {
                compound: compound.clone(),
                blank,
                width: char_width,
                start_in_compound: idx,
                end_in_compound: idx + char.len_utf8(),
            };
            if let Some(token) = token.replace(new_token) {
                tokens.push(token);
            }
        }
        if let Some(token) = token {
            tokens.push(token);
        }
    }
    tokens
}


