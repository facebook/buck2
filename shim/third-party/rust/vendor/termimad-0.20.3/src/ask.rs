use {
    crate::*,
    std::io,
};

/// a question that can be asked to the user, requiring
/// him to type the key of the desired answer
///
/// A question can be built using [Question::new] or with
/// the [ask!] macro
pub struct Question {
    pub md: Option<String>,
    pub answers: Vec<Answer>,
    pub default_answer: Option<String>,
}

/// one of the proposed answers to a question
pub struct Answer {
    pub key: String,
    pub md: String,
}

impl Question {
    /// Create a new question with some text.
    pub fn new<S: Into<String>>(md: S) -> Self {
        Self {
            md: Some(md.into()),
            answers: Vec::new(),
            default_answer: None,
        }
    }

    /// add a proposed answer, with a key
    ///
    /// The user will have to type the result of calling `to_string()` on
    /// the key (numbers, chars, or strings are naturally good options for keys)
    pub fn add_answer<K: std::fmt::Display, S: Into<String>>(&mut self, key: K, md: S) {
        self.answers.push(Answer {
            key: key.to_string(),
            md: md.into(),
        });
    }

    /// set the value which will be returned if the user only hits enter.
    ///
    /// It does *not* have to be one of the answers' key, except when you
    /// use the [ask!] macro.
    pub fn set_default<K: std::fmt::Display>(&mut self, default_answer: K) {
        self.default_answer = Some(default_answer.to_string());
    }

    /// has a default been defined which isn't among the list of answers?
    pub fn has_exotic_default(&self) -> bool {
        if let Some(da) = self.default_answer.as_ref() {
            for answer in &self.answers {
                if &answer.key == da {
                    return false;
                }
            }
            true
        } else {
            false
        }
    }

    /// Does the asking and returns the inputted string, unless
    /// the user just typed *enter* and there was a default value.
    ///
    /// If the user types something not recognized, he's asking to
    /// try again.
    pub fn ask(&self, skin: &MadSkin) -> io::Result<String> {
        if let Some(md) = &self.md {
            skin.print_text(md);
        }
        for a in &self.answers {
            if self.default_answer.as_ref() == Some(&a.key) {
                mad_print_inline!(skin, "[**$0**] ", a.key);
            } else {
                mad_print_inline!(skin, "[$0] ", a.key);
            }
            skin.print_text(&a.md);
        }
        loop {
            let mut input = String::new();
            io::stdin().read_line(&mut input)?;
            input.truncate(input.trim_end().len());
            if input.is_empty() {
                if let Some(da) = &self.default_answer {
                    return Ok(da.clone());
                }
            }
            for a in &self.answers {
                if a.key == input {
                    return Ok(input);
                }
            }
            println!("answer {:?} not understood", input);
        }
    }
}

/// ask the user to choose among proposed answers.
///
/// This macro makes it possible to propose several choices, with
/// an optional default ones, to execute blocks, to optionaly return
/// a value.
///
/// Example:
///
/// ```no_run
/// use termimad::*;
///
/// let skin = get_default_skin();
///
/// let beverage = ask!(skin, "What do I serve you ?", {
///     ('b', "**B**eer") => {
///         ask!(skin, "Really ? We have wine and orange juice too", (2) {
///             ("oj", "**o**range **j**uice") => { "orange juice" }
///             ('w' , "ok for some wine") => { "wine" }
///             ('b' , "I said **beer**") => { "beer" }
///             ( 2  , "Make it **2** beer glasses!") => { "beer x 2" }
///         })
///     }
///     ('w', "**W**ine") => {
///         println!("An excellent choice!");
///         "wine"
///     }
/// });
/// ```
///
/// Limits compared to the [Question] API:
/// - the default answer, if any, must be among the declared ones
///
/// Note that examples/ask contains several examples of this macro.
#[macro_export]
macro_rules! ask {
    (
        $skin: expr,
        $question: expr,
        { $(($key: expr, $answer: expr) => $r: block)+ }
    ) => {{
        let mut question = Question {
            md: Some($question.to_string()),
            answers: vec![$(Answer { key: $key.to_string(), md: $answer.to_string() }),*],
            default_answer: None,
        };
        let key = question.ask($skin).unwrap();
        let mut answers = question.answers.drain(..);
        match key {
            $(
                _ if answers.next().unwrap().key == key => { $r }
            )*
            _ => { unreachable!(); }
        }
    }};
    (
        $skin: expr,
        $question: expr,
        ($default_answer: expr)
        { $(($key: expr, $answer: expr) => $r: block)+ }
    ) => {{
        let mut question = Question {
            md: Some($question.to_string()),
            answers: vec![$(Answer { key: $key.to_string(), md: $answer.to_string() }),*],
            default_answer: Some($default_answer.to_string()),
        };
        if question.has_exotic_default() {
            // I should rewrite this macro as a proc macro...
            panic!("default answer when using the ask! macro must be among declared answers");
        }
        let key = question.ask($skin).unwrap();
        let mut answers = question.answers.drain(..);
        match key {
            $(
                _ if answers.next().unwrap().key == key => { $r }
            )*
            _ => { unreachable!() }
        }
    }}
}
