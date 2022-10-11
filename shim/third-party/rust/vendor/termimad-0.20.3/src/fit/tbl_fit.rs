use {
    crate::InsufficientWidthError,
    std::{
        cmp,
    },
};

/// A fitter, accumulating data about the table which must fit into
/// a given width, then computing the best column widths.
pub struct TblFit {
    cols: Vec<ColData>,
    available_sum_width: usize,
}

/// Information observed with calls to see_cell
#[derive(Debug, Clone, Copy)]
struct ColData {
    sum_widths: usize,
    width: usize,
    count: usize,
}

impl Default for ColData {
    fn default() -> Self {
        Self {
            sum_widths: 0,
            width: 3, // minimal col width
            count: 0, // number of observations on which we summed
        }
    }
}

impl ColData {
    const fn avg_width(self) -> usize {
        div_ceil(self.sum_widths, self.count)
    }
    pub fn see_cell(&mut self, cell_width: usize) {
        self.sum_widths += cell_width;
        self.width = self.width.max(cell_width);
        self.count += 1;
    }
}

/// Result of the fitting operation (always a success)
pub struct TblFitResult {
    /// whether some cells will have to be wrapped
    pub reduced: bool,

    /// the widths of all columns, so that they're guaranteed to fit
    /// into the available_width (taking the borders into account)
    pub col_widths: Vec<usize>,
}

impl TblFit {
    /// Build a new fitter, or return an error if the width isn't enough
    /// for the given number of columns.
    ///
    /// available_width: total available width, including external borders
    pub fn new(cols_count: usize, available_width: usize) -> Result<Self, InsufficientWidthError> {
        if available_width < cols_count*4 + 1 {
            return Err(InsufficientWidthError { available_width });
        }
        let cols = vec![ColData::default(); cols_count];
        let available_sum_width = available_width - 1 - cols_count;
        Ok(Self {
            cols,
            available_sum_width,
        })
    }
    pub fn see_cell(&mut self, col_idx: usize, cell_width: usize) {
        if let Some(col) = self.cols.get_mut(col_idx) {
            col.see_cell(cell_width);
        }
    }
    /// compute the fitting
    pub fn fit(&self) -> TblFitResult {
        let sum_widths: usize = self.cols.iter().map(|c| c.width).sum();
        if sum_widths <= self.available_sum_width {
            return TblFitResult {
                reduced: false,
                col_widths: self.cols.iter().map(|c| c.width).collect(),
            };
        }
        if self.cols.is_empty() {
            return TblFitResult {
                reduced: false,
                col_widths: Vec::new(),
            };
        }
        if self.cols.len() == 1 {
            return TblFitResult {
                reduced: false,
                col_widths: vec![self.available_sum_width],
            };
        }
        #[derive(Debug)]
        struct ColFit {
            idx: usize,       // index of the col
            std_width: usize, // col internal max width
            avg_width: usize, // col internal average width
            width: usize, // final width
        }
        let mut fits: Vec<ColFit> = self.cols.iter()
            .enumerate()
            .map(|(idx, c)| ColFit {
                idx,
                std_width: c.width,
                avg_width: c.avg_width(),
                width: c.width,
            })
            .collect();

        if self.available_sum_width >= sum_widths {
            return TblFitResult {
                reduced: false,
                col_widths: self.cols.iter().map(|c| c.width).collect(),
            };
        }

        let mut excess = sum_widths - self.available_sum_width;
        fits.sort_by_key(|c| cmp::Reverse(c.width));

        // Step 1
        // We do a first reduction, if possible, on columns wider
        // than 5, and trying to keep above the average width
        let potential_uncut_gain_1 = fits.iter()
            .filter(|c| c.width > 4 && c.width > c.avg_width + 1)
            .map(|c| (c.width - c.avg_width).min(4))
            .sum::<usize>();
        let potential_cut_gain_1 = potential_uncut_gain_1
            .min(excess);
        if potential_cut_gain_1 > 0 {
            for c in fits.iter_mut() {
                if c.std_width > 4 && c.std_width > c.avg_width {
                    let gain_1 = div_ceil((c.width - c.avg_width) * potential_cut_gain_1, potential_uncut_gain_1);
                    let gain_1 = gain_1.min(excess).min(c.width - 4);
                    c.width -= gain_1;
                    excess -= gain_1;
                    if excess == 0 {
                        break;
                    }
                }
            }
        }

        // Step 2
        // We remove excess proportionnally
        if excess > 0 {
            let potential_total_gain_2 = fits.iter()
                .map(|c| c.width - 3)
                .sum::<usize>()
                .min(excess);
            let excess_before_2 = excess;
            for c in fits.iter_mut() {
                let gain_2 = div_ceil((c.width - 3) * excess_before_2, potential_total_gain_2);
                let gain_2 = gain_2.min(excess).min(c.width - 3);
                c.width -= gain_2;
                excess -= gain_2;
                if excess == 0 {
                    break;
                }
            }
        }

        // it should be OK now

        fits.sort_by_key(|c| c.idx);
        TblFitResult {
            reduced: true,
            col_widths: fits.iter().map(|c| c.width).collect(),
        }
    }
}

/// divide, rounding to the top
const fn div_ceil(q: usize, r: usize) -> usize {
    let mut res =  q / r;
    if q%r != 0 {
        res += 1;
    }
    res
}
