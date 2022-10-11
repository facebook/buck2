use super::*;

/// Trait to determine color difference between various types.
/// As long as the type can be converted to Lab, we can calculate DeltaE.
pub trait Delta: Into<LabValue> {
    /// Calculate DeltaE between 2 types
    /// ```
    /// use deltae::*;
    ///
    /// let lch = LchValue::new(60.3, 89.2, 270.0).unwrap();
    /// let xyz = XyzValue::new(0.347, 0.912, 0.446).unwrap();
    /// let de  = lch.delta(xyz, DE1976);
    /// assert_eq!(de, 180.18364);
    /// ```
    #[inline]
    fn delta<L: Into<LabValue>>(self, other: L, method: DEMethod) -> DeltaE {
        let reference: LabValue = self.into();
        let sample: LabValue = other.into();
        let value = match method {
            DEMethod::DE1976 => delta_e_1976(&reference, &sample),
            DEMethod::DE1994T => delta_e_1994(&reference, &sample, true),
            DEMethod::DE1994G => delta_e_1994(&reference, &sample, false),
            DEMethod::DE2000 => delta_e_2000(&reference, &sample),
            DEMethod::DECMC(t_l, t_c) => delta_e_cmc(&reference, &sample, t_l, t_c),
        };

        DeltaE { value, method, reference, sample }
    }
}

impl<T: Into<LabValue>> Delta for T {}

/// DeltaE 1976. Basic euclidian distance formula.
#[inline]
fn delta_e_1976(lab_0: &LabValue, lab_1: &LabValue) -> f32 {
    ( (lab_0.l - lab_1.l).powi(2) + (lab_0.a - lab_1.a).powi(2) + (lab_0.b - lab_1.b).powi(2) ).sqrt()
}

/// DeltaE 1994. Weighted for textiles (`true`) or graphics (`false`)
#[inline]
fn delta_e_1994(lab_0: &LabValue, lab_1: &LabValue, textiles: bool) -> f32 {
    let delta_l = lab_0.l - lab_1.l;
    let chroma_0 = (lab_0.a.powi(2) + lab_0.b.powi(2)).sqrt();
    let chroma_1 = (lab_1.a.powi(2) + lab_1.b.powi(2)).sqrt();
    let delta_chroma = chroma_0 - chroma_1;
    let delta_a = lab_0.a - lab_1.a;
    let delta_b = lab_0.b - lab_1.b;
    let delta_hue = (delta_a.powi(2) + delta_b.powi(2) - delta_chroma.powi(2)).sqrt();

    let (kl, k1, k2) = match textiles {
        true => (2.0, 0.048, 0.014),
        false => (1.0, 0.045, 0.015),
    };

    let s_l = 1.0;
    let s_c = 1.0 + k1 * chroma_0;
    let s_h = 1.0 + k2 * chroma_0;

    ((delta_l / kl * s_l).powi(2) + (delta_chroma / s_c).powi(2) + (delta_hue / s_h).powi(2)).sqrt()
}

/// DeltaE 2000. This is a ridiculously complicated formula.
#[inline]
fn delta_e_2000(lab_0: &LabValue, lab_1: &LabValue) -> f32 {
    let chroma_0 = (lab_0.a.powi(2) + lab_0.b.powi(2)).sqrt();
    let chroma_1 = (lab_1.a.powi(2) + lab_1.b.powi(2)).sqrt();

    let c_bar = (chroma_0 + chroma_1) / 2.0;

    let g = 0.5 * (1.0 - ( c_bar.powi(7) / (c_bar.powi(7) + 25_f32.powi(7)) ).sqrt());

    let a_prime_0 = lab_0.a * (1.0 + g);
    let a_prime_1 = lab_1.a * (1.0 + g);

    let c_prime_0 = (a_prime_0.powi(2) + lab_0.b.powi(2)).sqrt();
    let c_prime_1 = (a_prime_1.powi(2) + lab_1.b.powi(2)).sqrt();

    let l_bar_prime = (lab_0.l + lab_1.l)/2.0;
    let c_bar_prime = (c_prime_0 + c_prime_1) / 2.0;

    let h_prime_0 = convert::get_h_prime(a_prime_0, lab_0.b);
    let h_prime_1 = convert::get_h_prime(a_prime_1, lab_1.b);

    let h_bar_prime = if (h_prime_0 - h_prime_1).abs() > 180.0 {
        if (h_prime_0 - h_prime_1) < 360.0 {
            (h_prime_0 + h_prime_1 + 360.0) / 2.0
        } else {
            (h_prime_0 + h_prime_1 - 360.0) / 2.0
        }
    } else {
        (h_prime_0 + h_prime_1) / 2.0
    };

    let t = 1.0 - 0.17 * ((      h_bar_prime - 30.0).to_radians()).cos()
                + 0.24 * ((2.0 * h_bar_prime       ).to_radians()).cos()
                + 0.32 * ((3.0 * h_bar_prime +  6.0).to_radians()).cos()
                - 0.20 * ((4.0 * h_bar_prime - 63.0).to_radians()).cos();

    let mut delta_h = h_prime_1 - h_prime_0;
    if delta_h > 180.0 && h_prime_1 <= h_prime_0 {
        delta_h += 360.0;
    } else if delta_h > 180.0 {
        delta_h -= 360.0;
    };

    let delta_l_prime = lab_1.l - lab_0.l;
    let delta_c_prime = c_prime_1 - c_prime_0;
    let delta_h_prime = 2.0 * (c_prime_0 * c_prime_1).sqrt() * (delta_h.to_radians() / 2.0).sin();

    let s_l = 1.0 + (
              (0.015 * (l_bar_prime - 50.0).powi(2))
            / (20.00 + (l_bar_prime - 50.0).powi(2)).sqrt()
        );
    let s_c = 1.0 + 0.045 * c_bar_prime;
    let s_h = 1.0 + 0.015 * c_bar_prime * t;

    let delta_theta = 30.0 * (-((h_bar_prime - 275.0)/25.0).powi(2)).exp();
    let r_c =  2.0 * (c_bar_prime.powi(7)/(c_bar_prime.powi(7) + 25_f32.powi(7))).sqrt();
    let r_t = -(r_c * (2.0 * delta_theta.to_radians()).sin());

    let k_l = 1.0;
    let k_c = 1.0;
    let k_h = 1.0;

    (
        (delta_l_prime/(k_l*s_l)).powi(2)
      + (delta_c_prime/(k_c*s_c)).powi(2)
      + (delta_h_prime/(k_h*s_h)).powi(2)
      + (r_t * (delta_c_prime/(k_c*s_c)) * (delta_h_prime/(k_h*s_h)))
    ).sqrt()
}

/// Custom weighted DeltaE formula
#[inline]
fn delta_e_cmc(lab0: &LabValue, lab1: &LabValue, tolerance_l: f32, tolerance_c: f32) -> f32 {
    let chroma_0 = (lab0.a.powi(2) + lab0.b.powi(2)).sqrt();
    let chroma_1 = (lab1.a.powi(2) + lab1.b.powi(2)).sqrt();
    let delta_c = chroma_0 - chroma_1;

    let delta_l = lab0.l - lab1.l;
    let delta_a = lab0.a - lab1.a;
    let delta_b = lab0.b - lab1.b;

    let delta_h = (delta_a.powi(2) + delta_b.powi(2) - delta_c.powi(2)).sqrt();

    let s_l = if lab0.l < 16.0 {
        0.511
    } else {
        (0.040975 * lab0.l) / (1.0 + (0.01765 * lab0.l))
    };

    let s_c = ((0.0638 * chroma_0) / (1.0 + (0.0131 * chroma_0))) + 0.638;

    let h = lab0.b.atan2(lab0.a).to_degrees();

    let h_1 = if h >= 0.0 { h } else { h + 360.0 };

    let f = (chroma_0.powi(4) / (chroma_0.powi(4) + 1900.0)).sqrt();

    let t = if (164.0..345.0).contains(&h_1) {
        0.56 + (0.2 * (h_1 + 168.0).to_radians().cos()).abs()
    } else {
        0.36 + (0.4 * (h_1 + 35.0).to_radians().cos()).abs()
    };

    let s_h = s_c * (f * t + 1.0 - f);

    (
        (delta_l / (tolerance_l * s_l)).powi(2)
      + (delta_c / (tolerance_c * s_c)).powi(2)
      + (delta_h / s_h).powi(2)
    )
    .sqrt()
}
