
use {
    crate::{
        Alignment,
        MadSkin,
        Spacing,
        Fitter,
    },
    minimad::{Composite, Compound},
    unicode_width::UnicodeWidthStr,
};

/// Wrap a Minimad Composite, which is a list of Compounds
/// (which are strings with an homogeneous style)
#[derive(Debug, Clone)]
pub struct FmtComposite<'s> {
    pub composite: Composite<'s>,
    pub visible_length: usize, // to avoid recomputing it again and again
    pub spacing: Option<Spacing>,
}

impl<'s> FmtComposite<'s> {
    pub fn new() -> Self {
        FmtComposite {
            composite: Composite::new(),
            visible_length: 0,
            spacing: None,
        }
    }
    pub fn from(composite: Composite<'s>, skin: &MadSkin) -> Self {
        FmtComposite {
            visible_length: skin.visible_composite_length(&composite),
            composite,
            spacing: None,
        }
    }
    pub fn from_compound(compound: Compound<'s>) -> Self {
        let mut fc = Self::new();
        fc.add_compound(compound);
        fc
    }
    /// Return the number of characters (usually spaces) to insert both
    /// sides of the composite
    #[inline(always)]
    pub const fn completions(&self) -> (usize, usize) {
        match &self.spacing {
            Some(spacing) => spacing.completions_for(self.visible_length),
            None => (0, 0),
        }
    }
    /// Add a compound and modifies `visible_length` accordingly
    #[inline(always)]
    pub fn add_compound(&mut self, compound: Compound<'s>) {
        self.visible_length += compound.src.width();
        self.composite.compounds.push(compound);
    }
    /// Ensure the cached visible_length is correct.
    ///
    /// It's normally not necessary to call it, but
    /// this must be called if compounds are added,
    /// removed or modified without using the FmtComposite API
    pub fn recompute_width(&mut self, skin: &MadSkin) {
        self.visible_length = skin.visible_composite_length(&self.composite);
    }
    /// try to ensure the composite's width doesn't exceed the given
    /// width.
    ///
    /// The alignment can be used, if necessary, to know which side it's better
    /// to remove content (for example if the alignment is left then we remove at
    /// right).
    /// The fitter may remove a part in the core of the composite if it looks
    /// good enough. In this specific case an ellipsis will replace the removed part.
    pub fn fit_width(&mut self, width: usize, align: Alignment, skin: &MadSkin) {
        Fitter::for_align(align).fit(self, width, skin);
    }
    /// if the composite is smaller than the given width, pad it
    /// according to the alignment.
    pub fn extend_width(&mut self, width: usize, align: Alignment) {
        if let Some(ref mut spacing) = self.spacing {
            if spacing.width < width {
                spacing.width = width;
            }
            spacing.align = align;
        } else if self.visible_length < width {
            self.spacing = Some(Spacing { width, align });
        }
    }
    /// try to make it so that the composite has exactly the given width,
    /// either by shortening it or by adding space.
    ///
    /// This calls the `fit_width` and `extend_width` methods.
    pub fn fill_width(&mut self, width: usize, align: Alignment, skin: &MadSkin) {
        self.fit_width(width, align, skin);
        self.extend_width(width, align);
    }
}

impl Default for FmtComposite<'_> {
    fn default() -> Self {
        Self::new()
    }
}
