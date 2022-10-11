// GENERATED CODE DO NOT MANUALLY EDIT

use crate::grapheme_clusters::tests::grapheme_test;

#[test]
fn standard_grapheme_test() {
	grapheme_test("\u{0020}\u{0020}",
		&["\u{0020}", "\u{0020}"],
		"  ÷ [0.2] SPACE (Other) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{0308}\u{0020}",
		&["\u{0020}\u{0308}", "\u{0020}"],
		"  ÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{000D}",
		&["\u{0020}", "\u{000D}"],
		"  ÷ [0.2] SPACE (Other) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{0308}\u{000D}",
		&["\u{0020}\u{0308}", "\u{000D}"],
		"  ÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{000A}",
		&["\u{0020}", "\u{000A}"],
		"  ÷ [0.2] SPACE (Other) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{0308}\u{000A}",
		&["\u{0020}\u{0308}", "\u{000A}"],
		"  ÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{0001}",
		&["\u{0020}", "\u{0001}"],
		"  ÷ [0.2] SPACE (Other) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{0308}\u{0001}",
		&["\u{0020}\u{0308}", "\u{0001}"],
		"  ÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{034F}",
		&["\u{0020}\u{034F}"],
		"  ÷ [0.2] SPACE (Other) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{0308}\u{034F}",
		&["\u{0020}\u{0308}\u{034F}"],
		"  ÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{1F1E6}",
		&["\u{0020}", "\u{1F1E6}"],
		"  ÷ [0.2] SPACE (Other) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{0308}\u{1F1E6}",
		&["\u{0020}\u{0308}", "\u{1F1E6}"],
		"  ÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{0600}",
		&["\u{0020}", "\u{0600}"],
		"  ÷ [0.2] SPACE (Other) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{0308}\u{0600}",
		&["\u{0020}\u{0308}", "\u{0600}"],
		"  ÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{0903}",
		&["\u{0020}\u{0903}"],
		"  ÷ [0.2] SPACE (Other) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{0308}\u{0903}",
		&["\u{0020}\u{0308}\u{0903}"],
		"  ÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{1100}",
		&["\u{0020}", "\u{1100}"],
		"  ÷ [0.2] SPACE (Other) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{0308}\u{1100}",
		&["\u{0020}\u{0308}", "\u{1100}"],
		"  ÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{1160}",
		&["\u{0020}", "\u{1160}"],
		"  ÷ [0.2] SPACE (Other) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{0308}\u{1160}",
		&["\u{0020}\u{0308}", "\u{1160}"],
		"  ÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{11A8}",
		&["\u{0020}", "\u{11A8}"],
		"  ÷ [0.2] SPACE (Other) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{0308}\u{11A8}",
		&["\u{0020}\u{0308}", "\u{11A8}"],
		"  ÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{AC00}",
		&["\u{0020}", "\u{AC00}"],
		"  ÷ [0.2] SPACE (Other) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{0308}\u{AC00}",
		&["\u{0020}\u{0308}", "\u{AC00}"],
		"  ÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{AC01}",
		&["\u{0020}", "\u{AC01}"],
		"  ÷ [0.2] SPACE (Other) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{0308}\u{AC01}",
		&["\u{0020}\u{0308}", "\u{AC01}"],
		"  ÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{231A}",
		&["\u{0020}", "\u{231A}"],
		"  ÷ [0.2] SPACE (Other) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{0308}\u{231A}",
		&["\u{0020}\u{0308}", "\u{231A}"],
		"  ÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{0300}",
		&["\u{0020}\u{0300}"],
		"  ÷ [0.2] SPACE (Other) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{0308}\u{0300}",
		&["\u{0020}\u{0308}\u{0300}"],
		"  ÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{200D}",
		&["\u{0020}\u{200D}"],
		"  ÷ [0.2] SPACE (Other) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{0308}\u{200D}",
		&["\u{0020}\u{0308}\u{200D}"],
		"  ÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{0378}",
		&["\u{0020}", "\u{0378}"],
		"  ÷ [0.2] SPACE (Other) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{0308}\u{0378}",
		&["\u{0020}\u{0308}", "\u{0378}"],
		"  ÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0020}",
		&["\u{000D}", "\u{0020}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0308}\u{0020}",
		&["\u{000D}", "\u{0308}", "\u{0020}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{000D}",
		&["\u{000D}", "\u{000D}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0308}\u{000D}",
		&["\u{000D}", "\u{0308}", "\u{000D}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{000A}",
		&["\u{000D}\u{000A}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) × [3.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0308}\u{000A}",
		&["\u{000D}", "\u{0308}", "\u{000A}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0001}",
		&["\u{000D}", "\u{0001}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0308}\u{0001}",
		&["\u{000D}", "\u{0308}", "\u{0001}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{034F}",
		&["\u{000D}", "\u{034F}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0308}\u{034F}",
		&["\u{000D}", "\u{0308}\u{034F}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{1F1E6}",
		&["\u{000D}", "\u{1F1E6}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0308}\u{1F1E6}",
		&["\u{000D}", "\u{0308}", "\u{1F1E6}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0600}",
		&["\u{000D}", "\u{0600}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0308}\u{0600}",
		&["\u{000D}", "\u{0308}", "\u{0600}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0903}",
		&["\u{000D}", "\u{0903}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0308}\u{0903}",
		&["\u{000D}", "\u{0308}\u{0903}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{1100}",
		&["\u{000D}", "\u{1100}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0308}\u{1100}",
		&["\u{000D}", "\u{0308}", "\u{1100}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{1160}",
		&["\u{000D}", "\u{1160}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0308}\u{1160}",
		&["\u{000D}", "\u{0308}", "\u{1160}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{11A8}",
		&["\u{000D}", "\u{11A8}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0308}\u{11A8}",
		&["\u{000D}", "\u{0308}", "\u{11A8}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{AC00}",
		&["\u{000D}", "\u{AC00}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0308}\u{AC00}",
		&["\u{000D}", "\u{0308}", "\u{AC00}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{AC01}",
		&["\u{000D}", "\u{AC01}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0308}\u{AC01}",
		&["\u{000D}", "\u{0308}", "\u{AC01}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{231A}",
		&["\u{000D}", "\u{231A}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0308}\u{231A}",
		&["\u{000D}", "\u{0308}", "\u{231A}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0300}",
		&["\u{000D}", "\u{0300}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0308}\u{0300}",
		&["\u{000D}", "\u{0308}\u{0300}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{200D}",
		&["\u{000D}", "\u{200D}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0308}\u{200D}",
		&["\u{000D}", "\u{0308}\u{200D}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0378}",
		&["\u{000D}", "\u{0378}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{0308}\u{0378}",
		&["\u{000D}", "\u{0308}", "\u{0378}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0020}",
		&["\u{000A}", "\u{0020}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0308}\u{0020}",
		&["\u{000A}", "\u{0308}", "\u{0020}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{000D}",
		&["\u{000A}", "\u{000D}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0308}\u{000D}",
		&["\u{000A}", "\u{0308}", "\u{000D}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{000A}",
		&["\u{000A}", "\u{000A}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0308}\u{000A}",
		&["\u{000A}", "\u{0308}", "\u{000A}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0001}",
		&["\u{000A}", "\u{0001}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0308}\u{0001}",
		&["\u{000A}", "\u{0308}", "\u{0001}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{034F}",
		&["\u{000A}", "\u{034F}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0308}\u{034F}",
		&["\u{000A}", "\u{0308}\u{034F}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{1F1E6}",
		&["\u{000A}", "\u{1F1E6}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0308}\u{1F1E6}",
		&["\u{000A}", "\u{0308}", "\u{1F1E6}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0600}",
		&["\u{000A}", "\u{0600}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0308}\u{0600}",
		&["\u{000A}", "\u{0308}", "\u{0600}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0903}",
		&["\u{000A}", "\u{0903}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0308}\u{0903}",
		&["\u{000A}", "\u{0308}\u{0903}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{1100}",
		&["\u{000A}", "\u{1100}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0308}\u{1100}",
		&["\u{000A}", "\u{0308}", "\u{1100}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{1160}",
		&["\u{000A}", "\u{1160}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0308}\u{1160}",
		&["\u{000A}", "\u{0308}", "\u{1160}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{11A8}",
		&["\u{000A}", "\u{11A8}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0308}\u{11A8}",
		&["\u{000A}", "\u{0308}", "\u{11A8}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{AC00}",
		&["\u{000A}", "\u{AC00}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0308}\u{AC00}",
		&["\u{000A}", "\u{0308}", "\u{AC00}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{AC01}",
		&["\u{000A}", "\u{AC01}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0308}\u{AC01}",
		&["\u{000A}", "\u{0308}", "\u{AC01}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{231A}",
		&["\u{000A}", "\u{231A}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0308}\u{231A}",
		&["\u{000A}", "\u{0308}", "\u{231A}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0300}",
		&["\u{000A}", "\u{0300}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0308}\u{0300}",
		&["\u{000A}", "\u{0308}\u{0300}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{200D}",
		&["\u{000A}", "\u{200D}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0308}\u{200D}",
		&["\u{000A}", "\u{0308}\u{200D}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0378}",
		&["\u{000A}", "\u{0378}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{000A}\u{0308}\u{0378}",
		&["\u{000A}", "\u{0308}", "\u{0378}"],
		"  ÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0020}",
		&["\u{0001}", "\u{0020}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0308}\u{0020}",
		&["\u{0001}", "\u{0308}", "\u{0020}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{000D}",
		&["\u{0001}", "\u{000D}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0308}\u{000D}",
		&["\u{0001}", "\u{0308}", "\u{000D}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{000A}",
		&["\u{0001}", "\u{000A}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0308}\u{000A}",
		&["\u{0001}", "\u{0308}", "\u{000A}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0001}",
		&["\u{0001}", "\u{0001}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0308}\u{0001}",
		&["\u{0001}", "\u{0308}", "\u{0001}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{034F}",
		&["\u{0001}", "\u{034F}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0308}\u{034F}",
		&["\u{0001}", "\u{0308}\u{034F}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{1F1E6}",
		&["\u{0001}", "\u{1F1E6}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0308}\u{1F1E6}",
		&["\u{0001}", "\u{0308}", "\u{1F1E6}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0600}",
		&["\u{0001}", "\u{0600}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0308}\u{0600}",
		&["\u{0001}", "\u{0308}", "\u{0600}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0903}",
		&["\u{0001}", "\u{0903}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0308}\u{0903}",
		&["\u{0001}", "\u{0308}\u{0903}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{1100}",
		&["\u{0001}", "\u{1100}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0308}\u{1100}",
		&["\u{0001}", "\u{0308}", "\u{1100}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{1160}",
		&["\u{0001}", "\u{1160}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0308}\u{1160}",
		&["\u{0001}", "\u{0308}", "\u{1160}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{11A8}",
		&["\u{0001}", "\u{11A8}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0308}\u{11A8}",
		&["\u{0001}", "\u{0308}", "\u{11A8}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{AC00}",
		&["\u{0001}", "\u{AC00}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0308}\u{AC00}",
		&["\u{0001}", "\u{0308}", "\u{AC00}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{AC01}",
		&["\u{0001}", "\u{AC01}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0308}\u{AC01}",
		&["\u{0001}", "\u{0308}", "\u{AC01}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{231A}",
		&["\u{0001}", "\u{231A}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0308}\u{231A}",
		&["\u{0001}", "\u{0308}", "\u{231A}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0300}",
		&["\u{0001}", "\u{0300}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0308}\u{0300}",
		&["\u{0001}", "\u{0308}\u{0300}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{200D}",
		&["\u{0001}", "\u{200D}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0308}\u{200D}",
		&["\u{0001}", "\u{0308}\u{200D}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0378}",
		&["\u{0001}", "\u{0378}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0001}\u{0308}\u{0378}",
		&["\u{0001}", "\u{0308}", "\u{0378}"],
		"  ÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0020}",
		&["\u{034F}", "\u{0020}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0308}\u{0020}",
		&["\u{034F}\u{0308}", "\u{0020}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{000D}",
		&["\u{034F}", "\u{000D}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0308}\u{000D}",
		&["\u{034F}\u{0308}", "\u{000D}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{000A}",
		&["\u{034F}", "\u{000A}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0308}\u{000A}",
		&["\u{034F}\u{0308}", "\u{000A}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0001}",
		&["\u{034F}", "\u{0001}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0308}\u{0001}",
		&["\u{034F}\u{0308}", "\u{0001}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{034F}",
		&["\u{034F}\u{034F}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0308}\u{034F}",
		&["\u{034F}\u{0308}\u{034F}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{1F1E6}",
		&["\u{034F}", "\u{1F1E6}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0308}\u{1F1E6}",
		&["\u{034F}\u{0308}", "\u{1F1E6}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0600}",
		&["\u{034F}", "\u{0600}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0308}\u{0600}",
		&["\u{034F}\u{0308}", "\u{0600}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0903}",
		&["\u{034F}\u{0903}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0308}\u{0903}",
		&["\u{034F}\u{0308}\u{0903}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{1100}",
		&["\u{034F}", "\u{1100}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0308}\u{1100}",
		&["\u{034F}\u{0308}", "\u{1100}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{1160}",
		&["\u{034F}", "\u{1160}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0308}\u{1160}",
		&["\u{034F}\u{0308}", "\u{1160}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{11A8}",
		&["\u{034F}", "\u{11A8}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0308}\u{11A8}",
		&["\u{034F}\u{0308}", "\u{11A8}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{AC00}",
		&["\u{034F}", "\u{AC00}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0308}\u{AC00}",
		&["\u{034F}\u{0308}", "\u{AC00}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{AC01}",
		&["\u{034F}", "\u{AC01}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0308}\u{AC01}",
		&["\u{034F}\u{0308}", "\u{AC01}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{231A}",
		&["\u{034F}", "\u{231A}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0308}\u{231A}",
		&["\u{034F}\u{0308}", "\u{231A}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0300}",
		&["\u{034F}\u{0300}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0308}\u{0300}",
		&["\u{034F}\u{0308}\u{0300}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{200D}",
		&["\u{034F}\u{200D}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0308}\u{200D}",
		&["\u{034F}\u{0308}\u{200D}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0378}",
		&["\u{034F}", "\u{0378}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{034F}\u{0308}\u{0378}",
		&["\u{034F}\u{0308}", "\u{0378}"],
		"  ÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0020}",
		&["\u{1F1E6}", "\u{0020}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0308}\u{0020}",
		&["\u{1F1E6}\u{0308}", "\u{0020}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{000D}",
		&["\u{1F1E6}", "\u{000D}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0308}\u{000D}",
		&["\u{1F1E6}\u{0308}", "\u{000D}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{000A}",
		&["\u{1F1E6}", "\u{000A}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0308}\u{000A}",
		&["\u{1F1E6}\u{0308}", "\u{000A}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0001}",
		&["\u{1F1E6}", "\u{0001}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0308}\u{0001}",
		&["\u{1F1E6}\u{0308}", "\u{0001}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{034F}",
		&["\u{1F1E6}\u{034F}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0308}\u{034F}",
		&["\u{1F1E6}\u{0308}\u{034F}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{1F1E6}",
		&["\u{1F1E6}\u{1F1E6}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [12.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0308}\u{1F1E6}",
		&["\u{1F1E6}\u{0308}", "\u{1F1E6}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0600}",
		&["\u{1F1E6}", "\u{0600}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0308}\u{0600}",
		&["\u{1F1E6}\u{0308}", "\u{0600}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0903}",
		&["\u{1F1E6}\u{0903}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0308}\u{0903}",
		&["\u{1F1E6}\u{0308}\u{0903}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{1100}",
		&["\u{1F1E6}", "\u{1100}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0308}\u{1100}",
		&["\u{1F1E6}\u{0308}", "\u{1100}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{1160}",
		&["\u{1F1E6}", "\u{1160}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0308}\u{1160}",
		&["\u{1F1E6}\u{0308}", "\u{1160}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{11A8}",
		&["\u{1F1E6}", "\u{11A8}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0308}\u{11A8}",
		&["\u{1F1E6}\u{0308}", "\u{11A8}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{AC00}",
		&["\u{1F1E6}", "\u{AC00}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0308}\u{AC00}",
		&["\u{1F1E6}\u{0308}", "\u{AC00}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{AC01}",
		&["\u{1F1E6}", "\u{AC01}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0308}\u{AC01}",
		&["\u{1F1E6}\u{0308}", "\u{AC01}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{231A}",
		&["\u{1F1E6}", "\u{231A}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0308}\u{231A}",
		&["\u{1F1E6}\u{0308}", "\u{231A}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0300}",
		&["\u{1F1E6}\u{0300}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0308}\u{0300}",
		&["\u{1F1E6}\u{0308}\u{0300}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{200D}",
		&["\u{1F1E6}\u{200D}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0308}\u{200D}",
		&["\u{1F1E6}\u{0308}\u{200D}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0378}",
		&["\u{1F1E6}", "\u{0378}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{0308}\u{0378}",
		&["\u{1F1E6}\u{0308}", "\u{0378}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0020}",
		&["\u{0600}\u{0020}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.2] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0308}\u{0020}",
		&["\u{0600}\u{0308}", "\u{0020}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{000D}",
		&["\u{0600}", "\u{000D}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0308}\u{000D}",
		&["\u{0600}\u{0308}", "\u{000D}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{000A}",
		&["\u{0600}", "\u{000A}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0308}\u{000A}",
		&["\u{0600}\u{0308}", "\u{000A}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0001}",
		&["\u{0600}", "\u{0001}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0308}\u{0001}",
		&["\u{0600}\u{0308}", "\u{0001}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{034F}",
		&["\u{0600}\u{034F}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0308}\u{034F}",
		&["\u{0600}\u{0308}\u{034F}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{1F1E6}",
		&["\u{0600}\u{1F1E6}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0308}\u{1F1E6}",
		&["\u{0600}\u{0308}", "\u{1F1E6}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0600}",
		&["\u{0600}\u{0600}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.2] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0308}\u{0600}",
		&["\u{0600}\u{0308}", "\u{0600}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0903}",
		&["\u{0600}\u{0903}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0308}\u{0903}",
		&["\u{0600}\u{0308}\u{0903}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{1100}",
		&["\u{0600}\u{1100}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.2] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0308}\u{1100}",
		&["\u{0600}\u{0308}", "\u{1100}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{1160}",
		&["\u{0600}\u{1160}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.2] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0308}\u{1160}",
		&["\u{0600}\u{0308}", "\u{1160}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{11A8}",
		&["\u{0600}\u{11A8}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.2] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0308}\u{11A8}",
		&["\u{0600}\u{0308}", "\u{11A8}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{AC00}",
		&["\u{0600}\u{AC00}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.2] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0308}\u{AC00}",
		&["\u{0600}\u{0308}", "\u{AC00}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{AC01}",
		&["\u{0600}\u{AC01}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.2] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0308}\u{AC01}",
		&["\u{0600}\u{0308}", "\u{AC01}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{231A}",
		&["\u{0600}\u{231A}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.2] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0308}\u{231A}",
		&["\u{0600}\u{0308}", "\u{231A}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0300}",
		&["\u{0600}\u{0300}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0308}\u{0300}",
		&["\u{0600}\u{0308}\u{0300}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{200D}",
		&["\u{0600}\u{200D}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0308}\u{200D}",
		&["\u{0600}\u{0308}\u{200D}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0378}",
		&["\u{0600}\u{0378}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.2] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0600}\u{0308}\u{0378}",
		&["\u{0600}\u{0308}", "\u{0378}"],
		"  ÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0020}",
		&["\u{0903}", "\u{0020}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0308}\u{0020}",
		&["\u{0903}\u{0308}", "\u{0020}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{000D}",
		&["\u{0903}", "\u{000D}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0308}\u{000D}",
		&["\u{0903}\u{0308}", "\u{000D}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{000A}",
		&["\u{0903}", "\u{000A}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0308}\u{000A}",
		&["\u{0903}\u{0308}", "\u{000A}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0001}",
		&["\u{0903}", "\u{0001}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0308}\u{0001}",
		&["\u{0903}\u{0308}", "\u{0001}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{034F}",
		&["\u{0903}\u{034F}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0308}\u{034F}",
		&["\u{0903}\u{0308}\u{034F}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{1F1E6}",
		&["\u{0903}", "\u{1F1E6}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0308}\u{1F1E6}",
		&["\u{0903}\u{0308}", "\u{1F1E6}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0600}",
		&["\u{0903}", "\u{0600}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0308}\u{0600}",
		&["\u{0903}\u{0308}", "\u{0600}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0903}",
		&["\u{0903}\u{0903}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0308}\u{0903}",
		&["\u{0903}\u{0308}\u{0903}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{1100}",
		&["\u{0903}", "\u{1100}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0308}\u{1100}",
		&["\u{0903}\u{0308}", "\u{1100}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{1160}",
		&["\u{0903}", "\u{1160}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0308}\u{1160}",
		&["\u{0903}\u{0308}", "\u{1160}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{11A8}",
		&["\u{0903}", "\u{11A8}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0308}\u{11A8}",
		&["\u{0903}\u{0308}", "\u{11A8}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{AC00}",
		&["\u{0903}", "\u{AC00}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0308}\u{AC00}",
		&["\u{0903}\u{0308}", "\u{AC00}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{AC01}",
		&["\u{0903}", "\u{AC01}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0308}\u{AC01}",
		&["\u{0903}\u{0308}", "\u{AC01}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{231A}",
		&["\u{0903}", "\u{231A}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0308}\u{231A}",
		&["\u{0903}\u{0308}", "\u{231A}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0300}",
		&["\u{0903}\u{0300}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0308}\u{0300}",
		&["\u{0903}\u{0308}\u{0300}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{200D}",
		&["\u{0903}\u{200D}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0308}\u{200D}",
		&["\u{0903}\u{0308}\u{200D}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0378}",
		&["\u{0903}", "\u{0378}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0903}\u{0308}\u{0378}",
		&["\u{0903}\u{0308}", "\u{0378}"],
		"  ÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0020}",
		&["\u{1100}", "\u{0020}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0308}\u{0020}",
		&["\u{1100}\u{0308}", "\u{0020}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{000D}",
		&["\u{1100}", "\u{000D}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0308}\u{000D}",
		&["\u{1100}\u{0308}", "\u{000D}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{000A}",
		&["\u{1100}", "\u{000A}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0308}\u{000A}",
		&["\u{1100}\u{0308}", "\u{000A}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0001}",
		&["\u{1100}", "\u{0001}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0308}\u{0001}",
		&["\u{1100}\u{0308}", "\u{0001}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{034F}",
		&["\u{1100}\u{034F}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0308}\u{034F}",
		&["\u{1100}\u{0308}\u{034F}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{1F1E6}",
		&["\u{1100}", "\u{1F1E6}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0308}\u{1F1E6}",
		&["\u{1100}\u{0308}", "\u{1F1E6}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0600}",
		&["\u{1100}", "\u{0600}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0308}\u{0600}",
		&["\u{1100}\u{0308}", "\u{0600}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0903}",
		&["\u{1100}\u{0903}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0308}\u{0903}",
		&["\u{1100}\u{0308}\u{0903}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{1100}",
		&["\u{1100}\u{1100}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [6.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0308}\u{1100}",
		&["\u{1100}\u{0308}", "\u{1100}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{1160}",
		&["\u{1100}\u{1160}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [6.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0308}\u{1160}",
		&["\u{1100}\u{0308}", "\u{1160}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{11A8}",
		&["\u{1100}", "\u{11A8}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0308}\u{11A8}",
		&["\u{1100}\u{0308}", "\u{11A8}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{AC00}",
		&["\u{1100}\u{AC00}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [6.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0308}\u{AC00}",
		&["\u{1100}\u{0308}", "\u{AC00}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{AC01}",
		&["\u{1100}\u{AC01}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [6.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0308}\u{AC01}",
		&["\u{1100}\u{0308}", "\u{AC01}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{231A}",
		&["\u{1100}", "\u{231A}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0308}\u{231A}",
		&["\u{1100}\u{0308}", "\u{231A}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0300}",
		&["\u{1100}\u{0300}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0308}\u{0300}",
		&["\u{1100}\u{0308}\u{0300}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{200D}",
		&["\u{1100}\u{200D}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0308}\u{200D}",
		&["\u{1100}\u{0308}\u{200D}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0378}",
		&["\u{1100}", "\u{0378}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{0308}\u{0378}",
		&["\u{1100}\u{0308}", "\u{0378}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0020}",
		&["\u{1160}", "\u{0020}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0308}\u{0020}",
		&["\u{1160}\u{0308}", "\u{0020}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{000D}",
		&["\u{1160}", "\u{000D}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0308}\u{000D}",
		&["\u{1160}\u{0308}", "\u{000D}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{000A}",
		&["\u{1160}", "\u{000A}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0308}\u{000A}",
		&["\u{1160}\u{0308}", "\u{000A}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0001}",
		&["\u{1160}", "\u{0001}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0308}\u{0001}",
		&["\u{1160}\u{0308}", "\u{0001}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{034F}",
		&["\u{1160}\u{034F}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0308}\u{034F}",
		&["\u{1160}\u{0308}\u{034F}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{1F1E6}",
		&["\u{1160}", "\u{1F1E6}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0308}\u{1F1E6}",
		&["\u{1160}\u{0308}", "\u{1F1E6}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0600}",
		&["\u{1160}", "\u{0600}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0308}\u{0600}",
		&["\u{1160}\u{0308}", "\u{0600}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0903}",
		&["\u{1160}\u{0903}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0308}\u{0903}",
		&["\u{1160}\u{0308}\u{0903}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{1100}",
		&["\u{1160}", "\u{1100}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0308}\u{1100}",
		&["\u{1160}\u{0308}", "\u{1100}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{1160}",
		&["\u{1160}\u{1160}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [7.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0308}\u{1160}",
		&["\u{1160}\u{0308}", "\u{1160}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{11A8}",
		&["\u{1160}\u{11A8}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [7.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0308}\u{11A8}",
		&["\u{1160}\u{0308}", "\u{11A8}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{AC00}",
		&["\u{1160}", "\u{AC00}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0308}\u{AC00}",
		&["\u{1160}\u{0308}", "\u{AC00}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{AC01}",
		&["\u{1160}", "\u{AC01}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0308}\u{AC01}",
		&["\u{1160}\u{0308}", "\u{AC01}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{231A}",
		&["\u{1160}", "\u{231A}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0308}\u{231A}",
		&["\u{1160}\u{0308}", "\u{231A}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0300}",
		&["\u{1160}\u{0300}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0308}\u{0300}",
		&["\u{1160}\u{0308}\u{0300}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{200D}",
		&["\u{1160}\u{200D}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0308}\u{200D}",
		&["\u{1160}\u{0308}\u{200D}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0378}",
		&["\u{1160}", "\u{0378}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{1160}\u{0308}\u{0378}",
		&["\u{1160}\u{0308}", "\u{0378}"],
		"  ÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0020}",
		&["\u{11A8}", "\u{0020}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0308}\u{0020}",
		&["\u{11A8}\u{0308}", "\u{0020}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{000D}",
		&["\u{11A8}", "\u{000D}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0308}\u{000D}",
		&["\u{11A8}\u{0308}", "\u{000D}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{000A}",
		&["\u{11A8}", "\u{000A}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0308}\u{000A}",
		&["\u{11A8}\u{0308}", "\u{000A}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0001}",
		&["\u{11A8}", "\u{0001}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0308}\u{0001}",
		&["\u{11A8}\u{0308}", "\u{0001}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{034F}",
		&["\u{11A8}\u{034F}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0308}\u{034F}",
		&["\u{11A8}\u{0308}\u{034F}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{1F1E6}",
		&["\u{11A8}", "\u{1F1E6}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0308}\u{1F1E6}",
		&["\u{11A8}\u{0308}", "\u{1F1E6}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0600}",
		&["\u{11A8}", "\u{0600}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0308}\u{0600}",
		&["\u{11A8}\u{0308}", "\u{0600}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0903}",
		&["\u{11A8}\u{0903}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0308}\u{0903}",
		&["\u{11A8}\u{0308}\u{0903}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{1100}",
		&["\u{11A8}", "\u{1100}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0308}\u{1100}",
		&["\u{11A8}\u{0308}", "\u{1100}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{1160}",
		&["\u{11A8}", "\u{1160}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0308}\u{1160}",
		&["\u{11A8}\u{0308}", "\u{1160}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{11A8}",
		&["\u{11A8}\u{11A8}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [8.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0308}\u{11A8}",
		&["\u{11A8}\u{0308}", "\u{11A8}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{AC00}",
		&["\u{11A8}", "\u{AC00}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0308}\u{AC00}",
		&["\u{11A8}\u{0308}", "\u{AC00}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{AC01}",
		&["\u{11A8}", "\u{AC01}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0308}\u{AC01}",
		&["\u{11A8}\u{0308}", "\u{AC01}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{231A}",
		&["\u{11A8}", "\u{231A}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0308}\u{231A}",
		&["\u{11A8}\u{0308}", "\u{231A}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0300}",
		&["\u{11A8}\u{0300}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0308}\u{0300}",
		&["\u{11A8}\u{0308}\u{0300}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{200D}",
		&["\u{11A8}\u{200D}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0308}\u{200D}",
		&["\u{11A8}\u{0308}\u{200D}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0378}",
		&["\u{11A8}", "\u{0378}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{11A8}\u{0308}\u{0378}",
		&["\u{11A8}\u{0308}", "\u{0378}"],
		"  ÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0020}",
		&["\u{AC00}", "\u{0020}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0308}\u{0020}",
		&["\u{AC00}\u{0308}", "\u{0020}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{000D}",
		&["\u{AC00}", "\u{000D}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0308}\u{000D}",
		&["\u{AC00}\u{0308}", "\u{000D}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{000A}",
		&["\u{AC00}", "\u{000A}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0308}\u{000A}",
		&["\u{AC00}\u{0308}", "\u{000A}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0001}",
		&["\u{AC00}", "\u{0001}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0308}\u{0001}",
		&["\u{AC00}\u{0308}", "\u{0001}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{034F}",
		&["\u{AC00}\u{034F}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0308}\u{034F}",
		&["\u{AC00}\u{0308}\u{034F}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{1F1E6}",
		&["\u{AC00}", "\u{1F1E6}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0308}\u{1F1E6}",
		&["\u{AC00}\u{0308}", "\u{1F1E6}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0600}",
		&["\u{AC00}", "\u{0600}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0308}\u{0600}",
		&["\u{AC00}\u{0308}", "\u{0600}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0903}",
		&["\u{AC00}\u{0903}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0308}\u{0903}",
		&["\u{AC00}\u{0308}\u{0903}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{1100}",
		&["\u{AC00}", "\u{1100}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0308}\u{1100}",
		&["\u{AC00}\u{0308}", "\u{1100}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{1160}",
		&["\u{AC00}\u{1160}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [7.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0308}\u{1160}",
		&["\u{AC00}\u{0308}", "\u{1160}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{11A8}",
		&["\u{AC00}\u{11A8}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [7.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0308}\u{11A8}",
		&["\u{AC00}\u{0308}", "\u{11A8}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{AC00}",
		&["\u{AC00}", "\u{AC00}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0308}\u{AC00}",
		&["\u{AC00}\u{0308}", "\u{AC00}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{AC01}",
		&["\u{AC00}", "\u{AC01}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0308}\u{AC01}",
		&["\u{AC00}\u{0308}", "\u{AC01}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{231A}",
		&["\u{AC00}", "\u{231A}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0308}\u{231A}",
		&["\u{AC00}\u{0308}", "\u{231A}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0300}",
		&["\u{AC00}\u{0300}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0308}\u{0300}",
		&["\u{AC00}\u{0308}\u{0300}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{200D}",
		&["\u{AC00}\u{200D}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0308}\u{200D}",
		&["\u{AC00}\u{0308}\u{200D}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0378}",
		&["\u{AC00}", "\u{0378}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{0308}\u{0378}",
		&["\u{AC00}\u{0308}", "\u{0378}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0020}",
		&["\u{AC01}", "\u{0020}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0308}\u{0020}",
		&["\u{AC01}\u{0308}", "\u{0020}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{000D}",
		&["\u{AC01}", "\u{000D}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0308}\u{000D}",
		&["\u{AC01}\u{0308}", "\u{000D}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{000A}",
		&["\u{AC01}", "\u{000A}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0308}\u{000A}",
		&["\u{AC01}\u{0308}", "\u{000A}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0001}",
		&["\u{AC01}", "\u{0001}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0308}\u{0001}",
		&["\u{AC01}\u{0308}", "\u{0001}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{034F}",
		&["\u{AC01}\u{034F}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0308}\u{034F}",
		&["\u{AC01}\u{0308}\u{034F}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{1F1E6}",
		&["\u{AC01}", "\u{1F1E6}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0308}\u{1F1E6}",
		&["\u{AC01}\u{0308}", "\u{1F1E6}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0600}",
		&["\u{AC01}", "\u{0600}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0308}\u{0600}",
		&["\u{AC01}\u{0308}", "\u{0600}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0903}",
		&["\u{AC01}\u{0903}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0308}\u{0903}",
		&["\u{AC01}\u{0308}\u{0903}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{1100}",
		&["\u{AC01}", "\u{1100}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0308}\u{1100}",
		&["\u{AC01}\u{0308}", "\u{1100}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{1160}",
		&["\u{AC01}", "\u{1160}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0308}\u{1160}",
		&["\u{AC01}\u{0308}", "\u{1160}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{11A8}",
		&["\u{AC01}\u{11A8}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [8.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0308}\u{11A8}",
		&["\u{AC01}\u{0308}", "\u{11A8}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{AC00}",
		&["\u{AC01}", "\u{AC00}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0308}\u{AC00}",
		&["\u{AC01}\u{0308}", "\u{AC00}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{AC01}",
		&["\u{AC01}", "\u{AC01}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0308}\u{AC01}",
		&["\u{AC01}\u{0308}", "\u{AC01}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{231A}",
		&["\u{AC01}", "\u{231A}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0308}\u{231A}",
		&["\u{AC01}\u{0308}", "\u{231A}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0300}",
		&["\u{AC01}\u{0300}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0308}\u{0300}",
		&["\u{AC01}\u{0308}\u{0300}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{200D}",
		&["\u{AC01}\u{200D}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0308}\u{200D}",
		&["\u{AC01}\u{0308}\u{200D}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0378}",
		&["\u{AC01}", "\u{0378}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{0308}\u{0378}",
		&["\u{AC01}\u{0308}", "\u{0378}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0020}",
		&["\u{231A}", "\u{0020}"],
		"  ÷ [0.2] WATCH (ExtPict) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0308}\u{0020}",
		&["\u{231A}\u{0308}", "\u{0020}"],
		"  ÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{000D}",
		&["\u{231A}", "\u{000D}"],
		"  ÷ [0.2] WATCH (ExtPict) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0308}\u{000D}",
		&["\u{231A}\u{0308}", "\u{000D}"],
		"  ÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{000A}",
		&["\u{231A}", "\u{000A}"],
		"  ÷ [0.2] WATCH (ExtPict) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0308}\u{000A}",
		&["\u{231A}\u{0308}", "\u{000A}"],
		"  ÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0001}",
		&["\u{231A}", "\u{0001}"],
		"  ÷ [0.2] WATCH (ExtPict) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0308}\u{0001}",
		&["\u{231A}\u{0308}", "\u{0001}"],
		"  ÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{034F}",
		&["\u{231A}\u{034F}"],
		"  ÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0308}\u{034F}",
		&["\u{231A}\u{0308}\u{034F}"],
		"  ÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{1F1E6}",
		&["\u{231A}", "\u{1F1E6}"],
		"  ÷ [0.2] WATCH (ExtPict) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0308}\u{1F1E6}",
		&["\u{231A}\u{0308}", "\u{1F1E6}"],
		"  ÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0600}",
		&["\u{231A}", "\u{0600}"],
		"  ÷ [0.2] WATCH (ExtPict) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0308}\u{0600}",
		&["\u{231A}\u{0308}", "\u{0600}"],
		"  ÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0903}",
		&["\u{231A}\u{0903}"],
		"  ÷ [0.2] WATCH (ExtPict) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0308}\u{0903}",
		&["\u{231A}\u{0308}\u{0903}"],
		"  ÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{1100}",
		&["\u{231A}", "\u{1100}"],
		"  ÷ [0.2] WATCH (ExtPict) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0308}\u{1100}",
		&["\u{231A}\u{0308}", "\u{1100}"],
		"  ÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{1160}",
		&["\u{231A}", "\u{1160}"],
		"  ÷ [0.2] WATCH (ExtPict) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0308}\u{1160}",
		&["\u{231A}\u{0308}", "\u{1160}"],
		"  ÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{11A8}",
		&["\u{231A}", "\u{11A8}"],
		"  ÷ [0.2] WATCH (ExtPict) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0308}\u{11A8}",
		&["\u{231A}\u{0308}", "\u{11A8}"],
		"  ÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{AC00}",
		&["\u{231A}", "\u{AC00}"],
		"  ÷ [0.2] WATCH (ExtPict) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0308}\u{AC00}",
		&["\u{231A}\u{0308}", "\u{AC00}"],
		"  ÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{AC01}",
		&["\u{231A}", "\u{AC01}"],
		"  ÷ [0.2] WATCH (ExtPict) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0308}\u{AC01}",
		&["\u{231A}\u{0308}", "\u{AC01}"],
		"  ÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{231A}",
		&["\u{231A}", "\u{231A}"],
		"  ÷ [0.2] WATCH (ExtPict) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0308}\u{231A}",
		&["\u{231A}\u{0308}", "\u{231A}"],
		"  ÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0300}",
		&["\u{231A}\u{0300}"],
		"  ÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0308}\u{0300}",
		&["\u{231A}\u{0308}\u{0300}"],
		"  ÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{200D}",
		&["\u{231A}\u{200D}"],
		"  ÷ [0.2] WATCH (ExtPict) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0308}\u{200D}",
		&["\u{231A}\u{0308}\u{200D}"],
		"  ÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0378}",
		&["\u{231A}", "\u{0378}"],
		"  ÷ [0.2] WATCH (ExtPict) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{231A}\u{0308}\u{0378}",
		&["\u{231A}\u{0308}", "\u{0378}"],
		"  ÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0020}",
		&["\u{0300}", "\u{0020}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0308}\u{0020}",
		&["\u{0300}\u{0308}", "\u{0020}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{000D}",
		&["\u{0300}", "\u{000D}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0308}\u{000D}",
		&["\u{0300}\u{0308}", "\u{000D}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{000A}",
		&["\u{0300}", "\u{000A}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0308}\u{000A}",
		&["\u{0300}\u{0308}", "\u{000A}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0001}",
		&["\u{0300}", "\u{0001}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0308}\u{0001}",
		&["\u{0300}\u{0308}", "\u{0001}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{034F}",
		&["\u{0300}\u{034F}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0308}\u{034F}",
		&["\u{0300}\u{0308}\u{034F}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{1F1E6}",
		&["\u{0300}", "\u{1F1E6}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0308}\u{1F1E6}",
		&["\u{0300}\u{0308}", "\u{1F1E6}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0600}",
		&["\u{0300}", "\u{0600}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0308}\u{0600}",
		&["\u{0300}\u{0308}", "\u{0600}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0903}",
		&["\u{0300}\u{0903}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0308}\u{0903}",
		&["\u{0300}\u{0308}\u{0903}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{1100}",
		&["\u{0300}", "\u{1100}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0308}\u{1100}",
		&["\u{0300}\u{0308}", "\u{1100}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{1160}",
		&["\u{0300}", "\u{1160}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0308}\u{1160}",
		&["\u{0300}\u{0308}", "\u{1160}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{11A8}",
		&["\u{0300}", "\u{11A8}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0308}\u{11A8}",
		&["\u{0300}\u{0308}", "\u{11A8}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{AC00}",
		&["\u{0300}", "\u{AC00}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0308}\u{AC00}",
		&["\u{0300}\u{0308}", "\u{AC00}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{AC01}",
		&["\u{0300}", "\u{AC01}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0308}\u{AC01}",
		&["\u{0300}\u{0308}", "\u{AC01}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{231A}",
		&["\u{0300}", "\u{231A}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0308}\u{231A}",
		&["\u{0300}\u{0308}", "\u{231A}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0300}",
		&["\u{0300}\u{0300}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0308}\u{0300}",
		&["\u{0300}\u{0308}\u{0300}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{200D}",
		&["\u{0300}\u{200D}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0308}\u{200D}",
		&["\u{0300}\u{0308}\u{200D}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0378}",
		&["\u{0300}", "\u{0378}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0300}\u{0308}\u{0378}",
		&["\u{0300}\u{0308}", "\u{0378}"],
		"  ÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0020}",
		&["\u{200D}", "\u{0020}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0308}\u{0020}",
		&["\u{200D}\u{0308}", "\u{0020}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{000D}",
		&["\u{200D}", "\u{000D}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0308}\u{000D}",
		&["\u{200D}\u{0308}", "\u{000D}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{000A}",
		&["\u{200D}", "\u{000A}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0308}\u{000A}",
		&["\u{200D}\u{0308}", "\u{000A}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0001}",
		&["\u{200D}", "\u{0001}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0308}\u{0001}",
		&["\u{200D}\u{0308}", "\u{0001}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{034F}",
		&["\u{200D}\u{034F}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0308}\u{034F}",
		&["\u{200D}\u{0308}\u{034F}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{1F1E6}",
		&["\u{200D}", "\u{1F1E6}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0308}\u{1F1E6}",
		&["\u{200D}\u{0308}", "\u{1F1E6}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0600}",
		&["\u{200D}", "\u{0600}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0308}\u{0600}",
		&["\u{200D}\u{0308}", "\u{0600}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0903}",
		&["\u{200D}\u{0903}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0308}\u{0903}",
		&["\u{200D}\u{0308}\u{0903}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{1100}",
		&["\u{200D}", "\u{1100}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0308}\u{1100}",
		&["\u{200D}\u{0308}", "\u{1100}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{1160}",
		&["\u{200D}", "\u{1160}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0308}\u{1160}",
		&["\u{200D}\u{0308}", "\u{1160}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{11A8}",
		&["\u{200D}", "\u{11A8}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0308}\u{11A8}",
		&["\u{200D}\u{0308}", "\u{11A8}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{AC00}",
		&["\u{200D}", "\u{AC00}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0308}\u{AC00}",
		&["\u{200D}\u{0308}", "\u{AC00}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{AC01}",
		&["\u{200D}", "\u{AC01}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0308}\u{AC01}",
		&["\u{200D}\u{0308}", "\u{AC01}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{231A}",
		&["\u{200D}", "\u{231A}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0308}\u{231A}",
		&["\u{200D}\u{0308}", "\u{231A}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0300}",
		&["\u{200D}\u{0300}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0308}\u{0300}",
		&["\u{200D}\u{0308}\u{0300}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{200D}",
		&["\u{200D}\u{200D}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0308}\u{200D}",
		&["\u{200D}\u{0308}\u{200D}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0378}",
		&["\u{200D}", "\u{0378}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{200D}\u{0308}\u{0378}",
		&["\u{200D}\u{0308}", "\u{0378}"],
		"  ÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0020}",
		&["\u{0378}", "\u{0020}"],
		"  ÷ [0.2] <reserved-0378> (Other) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0308}\u{0020}",
		&["\u{0378}\u{0308}", "\u{0020}"],
		"  ÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{000D}",
		&["\u{0378}", "\u{000D}"],
		"  ÷ [0.2] <reserved-0378> (Other) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0308}\u{000D}",
		&["\u{0378}\u{0308}", "\u{000D}"],
		"  ÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{000A}",
		&["\u{0378}", "\u{000A}"],
		"  ÷ [0.2] <reserved-0378> (Other) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0308}\u{000A}",
		&["\u{0378}\u{0308}", "\u{000A}"],
		"  ÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0001}",
		&["\u{0378}", "\u{0001}"],
		"  ÷ [0.2] <reserved-0378> (Other) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0308}\u{0001}",
		&["\u{0378}\u{0308}", "\u{0001}"],
		"  ÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{034F}",
		&["\u{0378}\u{034F}"],
		"  ÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0308}\u{034F}",
		&["\u{0378}\u{0308}\u{034F}"],
		"  ÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{1F1E6}",
		&["\u{0378}", "\u{1F1E6}"],
		"  ÷ [0.2] <reserved-0378> (Other) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0308}\u{1F1E6}",
		&["\u{0378}\u{0308}", "\u{1F1E6}"],
		"  ÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0600}",
		&["\u{0378}", "\u{0600}"],
		"  ÷ [0.2] <reserved-0378> (Other) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0308}\u{0600}",
		&["\u{0378}\u{0308}", "\u{0600}"],
		"  ÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0903}",
		&["\u{0378}\u{0903}"],
		"  ÷ [0.2] <reserved-0378> (Other) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0308}\u{0903}",
		&["\u{0378}\u{0308}\u{0903}"],
		"  ÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{1100}",
		&["\u{0378}", "\u{1100}"],
		"  ÷ [0.2] <reserved-0378> (Other) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0308}\u{1100}",
		&["\u{0378}\u{0308}", "\u{1100}"],
		"  ÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{1160}",
		&["\u{0378}", "\u{1160}"],
		"  ÷ [0.2] <reserved-0378> (Other) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0308}\u{1160}",
		&["\u{0378}\u{0308}", "\u{1160}"],
		"  ÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{11A8}",
		&["\u{0378}", "\u{11A8}"],
		"  ÷ [0.2] <reserved-0378> (Other) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0308}\u{11A8}",
		&["\u{0378}\u{0308}", "\u{11A8}"],
		"  ÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{AC00}",
		&["\u{0378}", "\u{AC00}"],
		"  ÷ [0.2] <reserved-0378> (Other) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0308}\u{AC00}",
		&["\u{0378}\u{0308}", "\u{AC00}"],
		"  ÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{AC01}",
		&["\u{0378}", "\u{AC01}"],
		"  ÷ [0.2] <reserved-0378> (Other) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0308}\u{AC01}",
		&["\u{0378}\u{0308}", "\u{AC01}"],
		"  ÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{231A}",
		&["\u{0378}", "\u{231A}"],
		"  ÷ [0.2] <reserved-0378> (Other) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0308}\u{231A}",
		&["\u{0378}\u{0308}", "\u{231A}"],
		"  ÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0300}",
		&["\u{0378}\u{0300}"],
		"  ÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0308}\u{0300}",
		&["\u{0378}\u{0308}\u{0300}"],
		"  ÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{200D}",
		&["\u{0378}\u{200D}"],
		"  ÷ [0.2] <reserved-0378> (Other) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0308}\u{200D}",
		&["\u{0378}\u{0308}\u{200D}"],
		"  ÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0378}",
		&["\u{0378}", "\u{0378}"],
		"  ÷ [0.2] <reserved-0378> (Other) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0378}\u{0308}\u{0378}",
		&["\u{0378}\u{0308}", "\u{0378}"],
		"  ÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
	);
	grapheme_test("\u{000D}\u{000A}\u{0061}\u{000A}\u{0308}",
		&["\u{000D}\u{000A}", "\u{0061}", "\u{000A}", "\u{0308}"],
		"  ÷ [0.2] <CARRIAGE RETURN (CR)> (CR) × [3.0] <LINE FEED (LF)> (LF) ÷ [4.0] LATIN SMALL LETTER A (Other) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0061}\u{0308}",
		&["\u{0061}\u{0308}"],
		"  ÷ [0.2] LATIN SMALL LETTER A (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0020}\u{200D}\u{0646}",
		&["\u{0020}\u{200D}", "\u{0646}"],
		"  ÷ [0.2] SPACE (Other) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] ARABIC LETTER NOON (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0646}\u{200D}\u{0020}",
		&["\u{0646}\u{200D}", "\u{0020}"],
		"  ÷ [0.2] ARABIC LETTER NOON (Other) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
	);
	grapheme_test("\u{1100}\u{1100}",
		&["\u{1100}\u{1100}"],
		"  ÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [6.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{AC00}\u{11A8}\u{1100}",
		&["\u{AC00}\u{11A8}", "\u{1100}"],
		"  ÷ [0.2] HANGUL SYLLABLE GA (LV) × [7.0] HANGUL JONGSEONG KIYEOK (T) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{AC01}\u{11A8}\u{1100}",
		&["\u{AC01}\u{11A8}", "\u{1100}"],
		"  ÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [8.0] HANGUL JONGSEONG KIYEOK (T) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
	);
	grapheme_test("\u{1F1E6}\u{1F1E7}\u{1F1E8}\u{0062}",
		&["\u{1F1E6}\u{1F1E7}", "\u{1F1E8}", "\u{0062}"],
		"  ÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [12.0] REGIONAL INDICATOR SYMBOL LETTER B (RI) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER C (RI) ÷ [999.0] LATIN SMALL LETTER B (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0061}\u{1F1E6}\u{1F1E7}\u{1F1E8}\u{0062}",
		&["\u{0061}", "\u{1F1E6}\u{1F1E7}", "\u{1F1E8}", "\u{0062}"],
		"  ÷ [0.2] LATIN SMALL LETTER A (Other) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [13.0] REGIONAL INDICATOR SYMBOL LETTER B (RI) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER C (RI) ÷ [999.0] LATIN SMALL LETTER B (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0061}\u{1F1E6}\u{1F1E7}\u{200D}\u{1F1E8}\u{0062}",
		&["\u{0061}", "\u{1F1E6}\u{1F1E7}\u{200D}", "\u{1F1E8}", "\u{0062}"],
		"  ÷ [0.2] LATIN SMALL LETTER A (Other) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [13.0] REGIONAL INDICATOR SYMBOL LETTER B (RI) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER C (RI) ÷ [999.0] LATIN SMALL LETTER B (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0061}\u{1F1E6}\u{200D}\u{1F1E7}\u{1F1E8}\u{0062}",
		&["\u{0061}", "\u{1F1E6}\u{200D}", "\u{1F1E7}\u{1F1E8}", "\u{0062}"],
		"  ÷ [0.2] LATIN SMALL LETTER A (Other) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER B (RI) × [13.0] REGIONAL INDICATOR SYMBOL LETTER C (RI) ÷ [999.0] LATIN SMALL LETTER B (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0061}\u{1F1E6}\u{1F1E7}\u{1F1E8}\u{1F1E9}\u{0062}",
		&["\u{0061}", "\u{1F1E6}\u{1F1E7}", "\u{1F1E8}\u{1F1E9}", "\u{0062}"],
		"  ÷ [0.2] LATIN SMALL LETTER A (Other) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [13.0] REGIONAL INDICATOR SYMBOL LETTER B (RI) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER C (RI) × [13.0] REGIONAL INDICATOR SYMBOL LETTER D (RI) ÷ [999.0] LATIN SMALL LETTER B (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0061}\u{200D}",
		&["\u{0061}\u{200D}"],
		"  ÷ [0.2] LATIN SMALL LETTER A (Other) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
	);
	grapheme_test("\u{0061}\u{0308}\u{0062}",
		&["\u{0061}\u{0308}", "\u{0062}"],
		"  ÷ [0.2] LATIN SMALL LETTER A (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] LATIN SMALL LETTER B (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0061}\u{0903}\u{0062}",
		&["\u{0061}\u{0903}", "\u{0062}"],
		"  ÷ [0.2] LATIN SMALL LETTER A (Other) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [999.0] LATIN SMALL LETTER B (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0061}\u{0600}\u{0062}",
		&["\u{0061}", "\u{0600}\u{0062}"],
		"  ÷ [0.2] LATIN SMALL LETTER A (Other) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) × [9.2] LATIN SMALL LETTER B (Other) ÷ [0.3]"
	);
	grapheme_test("\u{1F476}\u{1F3FF}\u{1F476}",
		&["\u{1F476}\u{1F3FF}", "\u{1F476}"],
		"  ÷ [0.2] BABY (ExtPict) × [9.0] EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend) ÷ [999.0] BABY (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{0061}\u{1F3FF}\u{1F476}",
		&["\u{0061}\u{1F3FF}", "\u{1F476}"],
		"  ÷ [0.2] LATIN SMALL LETTER A (Other) × [9.0] EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend) ÷ [999.0] BABY (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{0061}\u{1F3FF}\u{1F476}\u{200D}\u{1F6D1}",
		&["\u{0061}\u{1F3FF}", "\u{1F476}\u{200D}\u{1F6D1}"],
		"  ÷ [0.2] LATIN SMALL LETTER A (Other) × [9.0] EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend) ÷ [999.0] BABY (ExtPict) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [11.0] OCTAGONAL SIGN (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{1F476}\u{1F3FF}\u{0308}\u{200D}\u{1F476}\u{1F3FF}",
		&["\u{1F476}\u{1F3FF}\u{0308}\u{200D}\u{1F476}\u{1F3FF}"],
		"  ÷ [0.2] BABY (ExtPict) × [9.0] EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [11.0] BABY (ExtPict) × [9.0] EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend) ÷ [0.3]"
	);
	grapheme_test("\u{1F6D1}\u{200D}\u{1F6D1}",
		&["\u{1F6D1}\u{200D}\u{1F6D1}"],
		"  ÷ [0.2] OCTAGONAL SIGN (ExtPict) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [11.0] OCTAGONAL SIGN (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{0061}\u{200D}\u{1F6D1}",
		&["\u{0061}\u{200D}", "\u{1F6D1}"],
		"  ÷ [0.2] LATIN SMALL LETTER A (Other) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] OCTAGONAL SIGN (ExtPict) ÷ [0.3]"
	);
	grapheme_test("\u{2701}\u{200D}\u{2701}",
		&["\u{2701}\u{200D}\u{2701}"],
		"  ÷ [0.2] UPPER BLADE SCISSORS (Other) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [11.0] UPPER BLADE SCISSORS (Other) ÷ [0.3]"
	);
	grapheme_test("\u{0061}\u{200D}\u{2701}",
		&["\u{0061}\u{200D}", "\u{2701}"],
		"  ÷ [0.2] LATIN SMALL LETTER A (Other) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] UPPER BLADE SCISSORS (Other) ÷ [0.3]"
	);
}
