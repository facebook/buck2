

/// print a markdown template, with other arguments taking `$0` to `$9` places in the template.
///
/// Example:
///
/// ```
/// use termimad::*;
///
/// let skin = MadSkin::default();
/// mad_print_inline!(
///     &skin,
///     "**$0 formula:** *$1*", // the markdown template, interpreted once
///     "Disk",  // fills $0
///     "2*π*r", // fills $1. Note that the stars don't mess the markdown
/// );
/// ```
#[macro_export]
macro_rules! mad_print_inline {
    ($skin: expr, $md: literal $(, $value: expr )* $(,)? ) => {{
        let vals: Vec<String> = vec![$($value.to_string(),)*];
        #[allow(unused_variables)]
        #[allow(unused_mut)]
        let mut i: usize = 0;
        use $crate::minimad::{once_cell::sync::Lazy, InlineTemplate};
        static TEMPLATE: Lazy<InlineTemplate<'static>> = Lazy::new(|| {
            InlineTemplate::from($md)
        });
        let mut composite = TEMPLATE.raw_composite();
        for (arg_idx, val) in vals.iter().enumerate() {
            TEMPLATE.apply(&mut composite, arg_idx, val);
        }
        $skin.print_composite(composite)
    }};
}

/// write a markdown template, with other arguments taking `$0` to `$9` places in the template.
///
/// Example:
///
/// ```
/// use termimad::*;
///
/// let skin = MadSkin::default();
/// mad_write_inline!(
///     &mut std::io::stdout(),
///     &skin,
///     "**$0 formula:** *$1*", // the markdown template, interpreted once
///     "Disk",  // fills $0
///     "2*π*r", // fills $1. Note that the stars don't mess the markdown
/// ).unwrap();
/// ```
#[macro_export]
macro_rules! mad_write_inline {
    ($w: expr, $skin: expr, $md: literal $(, $value: expr )* $(,)? ) => {{
        use std::io::Write;
        let vals: Vec<String> = vec![$($value.to_string(),)*];
        let mut i: usize = 0;
        use $crate::minimad::{once_cell::sync::Lazy, InlineTemplate};
        static TEMPLATE: Lazy<InlineTemplate<'static>> = Lazy::new(|| {
            InlineTemplate::from($md)
        });
        let mut composite = TEMPLATE.raw_composite();
        for (arg_idx, val) in vals.iter().enumerate() {
            TEMPLATE.apply(&mut composite, arg_idx, val);
        }
        $skin.write_composite($w, composite)
    }};
}



