/// Left, Center, Right or Unspecified
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Alignment {
    Unspecified,
    Left,
    Center,
    Right,
}

impl Default for Alignment {
    fn default() -> Self {
        Alignment::Unspecified
    }
}

impl Alignment {
    pub fn col_spec(self) -> &'static str {
        match self {
            Self::Left => "|:-",
            Self::Right => "|-:",
            Self::Center => "|:-:",
            Self::Unspecified => "|-",
        }
    }
}

