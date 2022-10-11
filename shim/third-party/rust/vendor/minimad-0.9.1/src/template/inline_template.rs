use crate::Composite;

// There's an ergonomics limit here: https://stackoverflow.com/q/59306592/263525
// It could probably be solved by defining 10 functions, each one with a different number of
// arguments, and a global macro doing the routing. I won't try this until there's enough
// users of termimad to justify it...
//

#[derive(Debug, Default, PartialEq, Eq)]
struct Arg {
    compounds_idx: Vec<usize>, // indexes of the compounds the arg should fill
}
#[allow(dead_code)] // this isn't really dead code, it depends on the macro used
impl Arg {
    fn add(&mut self, idx: usize) {
        self.compounds_idx.push(idx);
    }
}

/// a template built from a markdown string, with optional placeholder
///
/// It can be used to build a composite and insert parts not interpreted
/// as markdown.
///
///
/// The [`mad_inline!`](macro.mad_inline.html) macro wraps the call to the `InlineTemplate` and
/// is more convenient for most uses.
#[derive(Debug, PartialEq, Eq)]
pub struct InlineTemplate<'a> {
    composite: Composite<'a>,
    args: [Arg; 10],
}

impl<'a> InlineTemplate<'a> {
    /// build a template from a markdown string which may contain `$0` to `$9`
    pub fn from(md: &'a str) -> InlineTemplate<'a> {
        let mut composite = Composite::from_inline(md);
        let mut compounds = Vec::new();
        let mut args: [Arg; 10] = Default::default();
        for compound in composite.compounds {
            // we iterate over the compounds of the template strings
            // looking for the $i locus
            let mut after_dollar = false;
            let mut start = 0;
            for (idx, char) in compound.as_str().char_indices() {
                if after_dollar {
                    if char.is_ascii_digit() {
                        let num: u8 = (char as u8) - b'0';
                        if start + 1 < idx {
                            compounds.push(compound.sub(start, idx - 1));
                        }
                        start = idx + 1;
                        args[usize::from(num)].compounds_idx.push(compounds.len());
                        compounds.push(compound.sub(idx - 1, start)); // placeholder
                    }
                    after_dollar = false;
                } else if char == '$' {
                    after_dollar = true;
                }
            }
            let tail = compound.tail(start);
            if !tail.is_empty() {
                compounds.push(tail);
            }
        }
        composite.compounds = compounds;
        InlineTemplate { composite, args }
    }

    pub fn raw_composite(&self) -> Composite<'a> {
        self.composite.clone()
    }

    pub fn apply(&self, composite: &mut Composite<'a>, arg_idx: usize, value: &'a str) {
        if arg_idx > 9 {
            return;
        }
        for compound_idx in &self.args[arg_idx].compounds_idx {
            composite.compounds[*compound_idx].set_str(value.as_ref());
        }
    }
}

/// build an inline from a string literal intepreted as markdown and
/// optional arguments which may fill places designed as `$0`..`$9`
///
/// Differences with parsing a string built with `format!`:
/// * the arguments aren't interpreted as markdown, which is convenient to insert user supplied
/// strings in a markdown template.
/// * markdown parsing and template building are done only once (the template is stored in a lazy
/// static)
/// * arguments can be omited, repeated, or given in arbitrary order
/// * no support for fmt parameters or arguments other than `&str`
///
/// Example:
/// ```
/// use minimad::*;
///
/// let composite = mad_inline!(
///     "**$0 formula:** *$1*", // the markdown template, interpreted only once
///     "Disk",  // fills $0
///     "2*π*r", // fills $1. Note that the stars don't mess the markdown
/// );
/// ```
///
#[macro_export]
macro_rules! mad_inline {
    ( $md: literal $(, $value: expr )* $(,)? ) => {{
        use minimad::once_cell::sync::Lazy;
        static TEMPLATE: Lazy<minimad::InlineTemplate<'static>> = Lazy::new(|| {
            minimad::InlineTemplate::from($md)
        });
        #[allow(unused_mut)]
        #[allow(unused_variables)]
        let mut arg_idx = 0;
        #[allow(unused_mut)]
        let mut composite = TEMPLATE.raw_composite();
        $(
            TEMPLATE.apply(&mut composite, arg_idx, $value);
            #[allow(unused_assignments)] // rustc bug
            { arg_idx += 1; }
        )*
        composite
    }};
}

#[cfg(test)]
mod tests {
    use crate::{
        *,
        self as minimad, // because the macro knows "minimad"
        template::inline_template::*,
    };

    #[test]
    fn simple_template_parsing() {
        let mut args = <[Arg; 10]>::default();
        args[0].add(3);
        args[1].add(1);
        assert_eq!(
            InlineTemplate::from("test $1 and $0"),
            InlineTemplate {
                composite: Composite::from(vec![
                    Compound::raw_str("test "),
                    Compound::raw_str("$1"),
                    Compound::raw_str(" and "),
                    Compound::raw_str("$0"),
                ]),
                args,
            },
        );

        let mut args = <[Arg; 10]>::default();
        args[0].add(1);
        args[2].add(0);
        args[2].add(2);
        assert_eq!(
            InlineTemplate::from("$2$0$2 "), // repetition and hole
            InlineTemplate {
                composite: Composite::from(vec![
                    Compound::raw_str("$2"),
                    Compound::raw_str("$0"),
                    Compound::raw_str("$2"),
                    Compound::raw_str(" "),
                ]),
                args,
            },
        );
    }

    #[test]
    fn simple_composition() {
        let template = InlineTemplate::from("using $1 and **$0**");
        let mut composite = template.raw_composite();
        template.apply(&mut composite, 0, "First");
        assert_eq!(
            composite,
            Composite::from(vec![
                Compound::raw_str("using "),
                Compound::raw_str("$1"),
                Compound::raw_str(" and "),
                Compound::raw_str("First").bold(),
            ]),
        );
    }

    #[test]
    fn macro_empty_composition() {
        let composite = mad_inline!("some `code`");
        assert_eq!(
            composite,
            Composite::from(vec![
                Compound::raw_str("some "),
                Compound::raw_str("code").code(),
            ]),
        );
    }

    #[test]
    fn macro_complex_composition() {
        let composite = mad_inline!("**$1:** `$0`", "π*r²", "area");
        assert_eq!(
            composite,
            Composite::from(vec![
                Compound::raw_str("area").bold(),
                Compound::raw_str(":").bold(),
                Compound::raw_str(" "),
                Compound::raw_str("π*r²").code(),
            ]),
        );
    }
}
