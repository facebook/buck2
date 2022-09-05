use crate::stdin::Stdin;

pub struct ConsoleInteractionStream<'a> {
    #[allow(unused)] // temporary
    stdin: &'a mut Stdin,
}

impl<'a> ConsoleInteractionStream<'a> {
    pub fn new(stdin: &'a mut Stdin) -> Self {
        Self { stdin }
    }
}
