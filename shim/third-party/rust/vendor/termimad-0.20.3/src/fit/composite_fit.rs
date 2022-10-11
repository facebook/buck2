use {
    crate::*,
    minimad::*,
    unicode_width::{
        UnicodeWidthChar,
        UnicodeWidthStr,
    },
};

pub static ELLIPSIS: &str = "…";

/// A fitter can shorten a composite to make it fit a target width
/// without wrapping (by removing parts and replacing them with
/// ellipsis)
#[derive(Debug, Clone, Copy)]
pub struct Fitter {
    /// whether to try remove the central part of big "token"
    /// (parts without space nor style change)
    mid_token_ellision: bool,

    /// whether to try remove the central part of big compounds
    mid_compound_ellision: bool,

    align: Alignment,
}

impl Default for Fitter {
    fn default() -> Self {
        Self {
            mid_token_ellision: true,
            mid_compound_ellision: true,
            align: Alignment::Unspecified,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct CharInfo {
    byte_idx: usize,
    width: usize, // virer
}
fn str_char_infos(s: &str) -> Vec<CharInfo> {
    s.char_indices()
        .map(|(byte_idx, char)| CharInfo {
            byte_idx,
            width: char.width().unwrap_or(0),
        })
        .collect()
}

#[derive(Debug, Clone)]
struct Zone {
    compound_idx: usize,
    byte_start_idx: usize,
    char_infos: Vec<CharInfo>,
    removable_width: usize, // cell width of string minus one character each end
}
impl Zone {
    fn token(composite: &Composite, min_removable_width: usize) -> Vec<Zone> {
        let mut zones = Vec::new();
        for (compound_idx, compound) in composite.compounds.iter().enumerate() {
            let s = compound.src;
            if s.len() < min_removable_width + 2 {
                continue;
            }
            let mut byte_start: Option<usize> = None;
            for (byte_idx, char) in s.char_indices() {
                if char.is_whitespace() {
                    if let Some(byte_start_idx) = byte_start {
                        if byte_idx - byte_start_idx >= min_removable_width + 2 {
                            let zs = &s[byte_start_idx..byte_idx];
                            let removable_width = zs.width();
                            if removable_width >= min_removable_width {
                                let char_infos = str_char_infos(zs);
                                zones.push(Zone {
                                    compound_idx,
                                    byte_start_idx,
                                    char_infos,
                                    removable_width,
                                });
                            }
                        }
                        byte_start = None;
                    }
                } else if byte_start.is_none() {
                    byte_start = Some(byte_idx);
                }
            }
            if let Some(byte_start_idx) = byte_start {
                let byte_end_idx = s.len();
                if byte_end_idx - byte_start_idx >= min_removable_width + 2 {
                    let zs = &s[byte_start_idx..];
                    let removable_width = zs.width();
                    if removable_width >= min_removable_width {
                        let char_infos = str_char_infos(zs);
                        zones.push(Zone {
                            compound_idx,
                            byte_start_idx,
                            char_infos,
                            removable_width,
                        });
                    }
                }
            }
        }
        zones
    }
    fn biggest_token(composite: &Composite, min_removable_width: usize) -> Option<Zone> {
        Zone::token(composite, min_removable_width)
            .drain(..)
            .max_by_key(|z| z.removable_width)
    }
    /// make a zone from each compound large enough
    fn compounds(composite: &Composite, min_removable_width: usize) -> Vec<Zone> {
        composite.compounds.iter()
            .enumerate()
            .filter_map(|(compound_idx, compound)| {
                let char_infos = str_char_infos(compound.src);
                if char_infos.len() < 2 + min_removable_width {
                    return None;
                }
                let removable = &compound.src[char_infos[1].byte_idx..char_infos[char_infos.len()-1].byte_idx];
                let removable_width = removable.width();
                if removable_width < min_removable_width {
                    None
                } else {
                    Some(Zone {
                        compound_idx,
                        byte_start_idx: 0,
                        char_infos,
                        removable_width,
                    })
                }
            })
            .collect()
    }
    fn biggest_compound(composite: &Composite, min_removable_width: usize) -> Option<Zone> {
        Zone::compounds(composite, min_removable_width)
            .drain(..)
            .max_by_key(|z| z.removable_width)
    }
    /// return the gain (that is the removed minus 1 for the ellipsis length)
    fn cut(&self, composite: &mut Composite, to_remove: usize) -> usize {
        if self.removable_width < 2 {
            return 0;
        }
        let compound = &composite.compounds[self.compound_idx];
        let len = self.char_infos.len();
        let mut start_char_idx = len / 2;
        let mut end_char_idx = start_char_idx;
        let mut removed_width = 0;
        loop {
            // we alternatively grow left and right
            if (end_char_idx-start_char_idx)%2 == 0 {
                if end_char_idx + 1 >= len {
                    break;
                }
                end_char_idx += 1;
            } else {
                if start_char_idx <= 1 {
                    break;
                }
                start_char_idx -= 1;
            }
            let start_byte_idx = self.byte_start_idx + self.char_infos[start_char_idx].byte_idx;
            let end_byte_idx = self.byte_start_idx + self.char_infos[end_char_idx].byte_idx;
            removed_width = (&compound.src[start_byte_idx..end_byte_idx]).width();
            if removed_width >= to_remove {
                break;
            }
        }
        let start_byte_idx = self.byte_start_idx + self.char_infos[start_char_idx].byte_idx;
        let end_byte_idx = self.byte_start_idx + self.char_infos[end_char_idx].byte_idx;
        let head = compound.sub(0, start_byte_idx);
        let tail = compound.tail(end_byte_idx);
        composite.compounds[self.compound_idx] = head;
        composite.compounds.insert(self.compound_idx+1, Compound::raw_str(ELLIPSIS));
        composite.compounds.insert(self.compound_idx+2, tail);

        removed_width - 1
    }
}

impl Fitter {

    /// create a fitter for when you want a specific alignment.
    ///
    /// You may still change the mid_token_ellision and mid_compound_ellision
    /// later
    pub fn for_align(align: Alignment) -> Self {
        let internal_ellision = align == Alignment::Unspecified;
        Self {
            mid_token_ellision: internal_ellision,
            mid_compound_ellision: internal_ellision,
            align,
        }
    }

    /// ensure the composite fits the max_width, by replacing some parts
    /// with ellisions
    pub fn fit<'s>(
        self,
        fc: &mut FmtComposite<'s>,
        max_width: usize,
        skin: &MadSkin
    ) {
        // some special cases because they're hard to check after
        if fc.visible_length <= max_width {
            return;
        } else if max_width == 0 {
            fc.composite.compounds.clear();
            fc.visible_length = 0;
            return;
        } else if max_width == 1 {
            fc.composite.compounds.clear();
            fc.composite.compounds.push(Compound::raw_str(ELLIPSIS));
            fc.visible_length = 1;
            return;
        }

        let mut excess = fc.visible_length - max_width;

        // note: computing all zones once would be faster but would involve either
        // recomputing compound_idx ou finding another index scheme

        if self.mid_token_ellision {
            // cutting in the middle of big no space parts
            while excess > 0 {
                let mut gain = 0;
                if let Some(zone) = Zone::biggest_token(&fc.composite, 3) {
                    gain = zone.cut(&mut fc.composite, excess + 1);
                }
                if gain == 0 {
                    break;
                }
                excess -= gain.min(excess);
            }
        }

        if self.mid_compound_ellision {
            // cutting in the middle of big compounds
            while excess > 0 {
                let mut gain = 0;
                // we'll look for zones of removable width at least 2
                // (because we put the ellipsis in place)
                if let Some(zone) = Zone::biggest_compound(&fc.composite, 2) {
                    gain = zone.cut(&mut fc.composite, excess + 1);
                }
                if gain == 0 {
                    break;
                }
                excess -= gain.min(excess);
            }
        }

        if excess == 0 {
            fc.recompute_width(skin);
            return;
        }

        let compounds = &mut fc.composite.compounds;
        // we'll have to compensate with 1 or 2 ellipsis, so the "excess" is
        // increased accordingly we increase
        let (mut excess_left, mut excess_right) = match self.align {
            Alignment::Right => (excess + 1, 0),
            Alignment::Left | Alignment:: Unspecified => (0, excess + 1),
            Alignment::Center => {
                let left = excess / 2;
                let right = excess - left;
                if left > 0 {
                    (left + 1, right + 1)
                } else {
                    (0, right + 1)
                }
            },
        };

        if excess_left > 0 {
            // left truncating
            while excess_left > 0 && !compounds.is_empty() {
                let compound = &mut compounds[0];
                let char_infos = str_char_infos(compound.src);
                let mut last_removed_char_idx = 0;
                let mut removed_width = 0;
                loop {
                    removed_width += char_infos[last_removed_char_idx].width;
                    if removed_width >= excess_left || last_removed_char_idx + 1 == char_infos.len() {
                        break;
                    }
                    last_removed_char_idx += 1;
                }
                if last_removed_char_idx  + 1 == char_infos.len() {
                    // we remove the whole compound
                    compounds.remove(0);
                    excess_left -= removed_width.min(excess_left);
                } else {
                    // we cut the left part
                    compound.src = &compound.src[char_infos[last_removed_char_idx+1].byte_idx..];
                    excess_left = 0;
                }
            }
            compounds.insert(0, Compound::raw_str(ELLIPSIS));
        }

        if excess_right > 0 {
            // right truncating
            while excess_right > 0 && !compounds.is_empty() {
                let last_idx = compounds.len()-1;
                let compound = &mut compounds[last_idx];
                let char_infos = str_char_infos(compound.src);
                let mut removed_width = 0;
                let mut end_byte_idx = compound.src.len();
                for ci in char_infos.iter().rev() {
                    end_byte_idx = ci.byte_idx;
                    removed_width += ci.width;
                    if removed_width >= excess_right {
                        break;
                    }
                }
                if end_byte_idx == 0 {
                    // we remove the whole compound
                    compounds.pop();
                    excess_right -= removed_width.min(excess_right);
                } else {
                    // we cut the right part
                    compound.src = &compound.src[..end_byte_idx];
                    excess_right = 0;
                }
            }
            compounds.push(Compound::raw_str(ELLIPSIS));
        }

        fc.recompute_width(skin);
    }

}

/// Tests of fitting, that is cutting the composite at best to make it
///  fit a given width (if possible)
///
/// The print which happens in case of failure isn't really well
/// formatted. A solution if a test fails is to do
///      cargo test fit_tests -- --nocapture
#[cfg(test)]
mod fit_tests {

    use minimad::{
        Alignment,
        Composite,
    };
    use crate::{
        Fitter,
        FmtComposite,
    };

    fn check_fit_align(src: &str, target_width: usize, align: Alignment) {
        dbg!((target_width, align));
        let skin = crate::get_default_skin();
        let mut fc = FmtComposite::from(Composite::from_inline(src), &skin);
        let fitter = Fitter::for_align(align);
        fitter.fit(&mut fc, target_width, &skin);
        dbg!(&fc);
        assert!(fc.visible_length <= target_width); // can be smaller
    }

    fn check_fit(src: &str, target_width: usize) {
        check_fit_align(src, target_width, Alignment::Right);
        check_fit_align(src, target_width, Alignment::Left);
        check_fit_align(src, target_width, Alignment::Center);
        check_fit_align(src, target_width, Alignment::Unspecified);
    }

    #[test]
    fn test_fit() {

        let sentence = "This sentence has **short** and **much longer** parts, and some Korean: *一曰道，二曰天*.";
        check_fit(sentence, 60);
        check_fit(sentence, 40);

        let five_issues = "一曰道，二曰天，三曰地，四曰將，五曰法。";
        check_fit(five_issues, 15);
        check_fit(five_issues, 8);

        let status = "ab *cd* `12345 123456789`";
        check_fit(status, 17);
        check_fit(status, 2);
    }

}
