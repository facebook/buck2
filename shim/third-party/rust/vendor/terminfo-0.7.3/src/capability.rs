//            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
//                    Version 2, December 2004
//
// Copyleft (ↄ) meh. <meh@schizofreni.co> | http://meh.schizofreni.co
//
// Everyone is permitted to copy and distribute verbatim or modified
// copies of this license document, and changing it is allowed as long
// as the name is changed.
//
//            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
//   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
//
//  0. You just DO WHAT THE FUCK YOU WANT TO.

///! Standard capabilities.

use std::io::Write;
use std::borrow::Cow;

use crate::expand::{Expand, Parameter, Context};
use crate::error;

/// A trait for any object that will represent a terminal capability.
pub trait Capability<'a>: Sized {
	/// Returns the name of the capability in its long form.
	fn name() -> &'static str;

	/// Parse the capability from its raw value.
	fn from(value: Option<&'a Value>) -> Option<Self>;

	/// Convert the capability into its raw value.
	fn into(self) -> Option<Value>;
}

/// Possible value types for capabilities.
#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Value {
	/// A boolean.
	True,

	/// A number.
	Number(i32),

	/// An ASCII string requiring expansion.
	String(Vec<u8>),
}

/// Expansion helper struct.
#[derive(Debug)]
pub struct Expansion<'a, T: 'a + AsRef<[u8]>> {
	string:  &'a T,
	params:  [Parameter; 9],
	context: Option<&'a mut Context>,
}

impl<'a, T: AsRef<[u8]>> Expansion<'a, T> {
	/// Expand using the given context.
	pub fn with<'c: 'a>(mut self, context: &'c mut Context) -> Self {
		self.context = Some(context);
		self
	}

	/// Expand to the given output.
	pub fn to<W: Write>(self, output: W) -> error::Result<()> {
		self.string.as_ref().expand(output, &self.params,
			self.context.unwrap_or(&mut Default::default()))
	}

	/// Expand into a vector.
	pub fn to_vec(self) -> error::Result<Vec<u8>> {
		let mut result = Vec::with_capacity(self.string.as_ref().len());
		self.to(&mut result)?;
		Ok(result)
	}
}

macro_rules! from {
	(number $ty:ty) => (
		impl From<$ty> for Value {
			fn from(value: $ty) -> Self {
				Value::Number(value as i32)
			}
		}
	);

	(string ref $ty:ty) => (
		impl<'a> From<&'a $ty> for Value {
			fn from(value: &'a $ty) -> Self {
				Value::String(value.into())
			}
		}
	);

	(string $ty:ty) => (
		impl From<$ty> for Value {
			fn from(value: $ty) -> Self {
				Value::String(value.into())
			}
		}
	);
}

impl From<()> for Value {
	fn from(_: ()) -> Self {
		Value::True
	}
}

from!(number u8);
from!(number i8);
from!(number u16);
from!(number i16);
from!(number u32);
from!(number i32);

from!(string String);
from!(string ref str);
from!(string Vec<u8>);
from!(string ref [u8]);

macro_rules! define {
	(boolean $ident:ident => $capability:expr) => (
		#[derive(Eq, PartialEq, Copy, Clone, Debug)]
		pub struct $ident(pub bool);

		impl<'a> Capability<'a> for $ident {
			#[inline]
			fn name() -> &'static str {
				$capability
			}

			#[inline]
			fn from(value: Option<&Value>) -> Option<Self> {
				if let Some(&Value::True) = value {
					Some($ident(true))
				}
				else {
					Some($ident(false))
				}
			}

			#[inline]
			fn into(self) -> Option<Value> {
				if self.0 {
					Some(Value::True)
				}
				else {
					None
				}
			}
		}

		impl Into<bool> for $ident {
			fn into(self) -> bool {
				self.0
			}
		}
	);

	(number $ident:ident => $capability:expr) => (
		#[derive(Eq, PartialEq, Copy, Clone, Debug)]
		pub struct $ident(pub i32);

		impl<'a> Capability<'a> for $ident {
			#[inline]
			fn name() -> &'static str {
				$capability
			}

			#[inline]
			fn from(value: Option<&Value>) -> Option<Self> {
				if let Some(&Value::Number(value)) = value {
					Some($ident(value))
				}
				else {
					None
				}
			}

			#[inline]
			fn into(self) -> Option<Value> {
				Some(Value::Number(self.0))
			}
		}

		impl Into<i32> for $ident {
			fn into(self) -> i32 {
				self.0
			}
		}
	);

	(string define $ident:ident => $capability:expr) => (
		#[derive(Eq, PartialEq, Clone, Debug)]
		pub struct $ident<'a>(Cow<'a, [u8]>);

		impl<'a> Capability<'a> for $ident<'a> {
			#[inline]
			fn name() -> &'static str {
				$capability
			}

			#[inline]
			fn from(value: Option<&'a Value>) -> Option<$ident<'a>> {
				if let Some(&Value::String(ref value)) = value {
					Some($ident(Cow::Borrowed(value)))
				}
				else {
					None
				}
			}

			#[inline]
			fn into(self) -> Option<Value> {
				Some(Value::String(match self.0 {
					Cow::Borrowed(value) =>
						value.into(),

					Cow::Owned(value) =>
						value
				}))
			}
		}

		impl<'a, T: AsRef<&'a [u8]>> From<T> for $ident<'a> {
			#[inline]
			fn from(value: T) -> Self {
				$ident(Cow::Borrowed(value.as_ref()))
			}
		}

		impl<'a> AsRef<[u8]> for $ident<'a> {
			#[inline]
			fn as_ref(&self) -> &[u8] {
				&self.0
			}
		}

		impl<'a> $ident<'a> {
			/// Begin expanding the capability.
			#[inline]
			pub fn expand(&self) -> Expansion<$ident> {
				Expansion {
					string:  self,
					params:  Default::default(),
					context: None,
				}
			}
		}
	);

	(string $ident:ident => $capability:expr) => (
		define!(string define $ident => $capability);

		impl<'a> Expansion<'a, $ident<'a>> {
			/// Pass all expansion parameters at once.
			#[inline]
			pub fn parameters(self) -> Self {
				self
			}
		}
	);

	(string $ident:ident => $capability:expr; $($rest:tt)+) => (
		define!(string define $ident => $capability);
		define!(string parameters $ident; $($rest)+);
		define!(string builder $ident; 0, $($rest)+, );
	);

	(string parameters $ident:ident; $($name:ident : $ty:ty),+) => (
		impl<'a> Expansion<'a, $ident<'a>> {
			/// Pass all expansion parameters at once.
			#[allow(unused_assignments)]
			#[inline]
			pub fn parameters(mut self, $($name: $ty),*) -> Self {
				let mut index = 0;

				$({
					self.params[index]  = $name.into();
					index              += 1;
				})*;

				self
			}
		}
	);

	(string builder $ident:ident; $index:expr, ) => ();

	(string builder $ident:ident; $index:expr, $name:ident : u8, $($rest:tt)*) => (
		define!(string builder direct $ident; $index, $name : u8);
		define!(string builder $ident; $index + 1, $($rest)*);
	);

	(string builder $ident:ident; $index:expr, $name:ident : i8, $($rest:tt)*) => (
		define!(string builder direct $ident; $index, $name : i8);
		define!(string builder $ident; $index + 1, $($rest)*);
	);

	(string builder $ident:ident; $index:expr, $name:ident : u16, $($rest:tt)*) => (
		define!(string builder direct $ident; $index, $name : u16);
		define!(string builder $ident; $index + 1, $($rest)*);
	);

	(string builder $ident:ident; $index:expr, $name:ident : i16 $($rest:tt)*) => (
		define!(string builder direct $ident; $index, $name : i16);
		define!(string builder $ident; $index + 1, $($rest)*);
	);

	(string builder $ident:ident; $index:expr, $name:ident : u32, $($rest:tt)*) => (
		define!(string builder direct $ident; $index, $name : u32);
		define!(string builder $ident; $index + 1, $($rest)*);
	);

	(string builder $ident:ident; $index:expr, $name:ident : i32, $($rest:tt)*) => (
		define!(string builder direct $ident; $index, $name : i32);
		define!(string builder $ident; $index + 1, $($rest)*);
	);

	(string builder $ident:ident; $index:expr, $name:ident : $ty:ty, $($rest:tt)*) => (
		define!(string builder into $ident; $index, $name : $ty);
		define!(string builder $ident; $index + 1, $($rest)*);
	);

	(string builder direct $ident:ident; $index:expr, $name:ident : $ty:ty) => (
		impl<'a> Expansion<'a, $ident<'a>> {
			/// Set the given parameter.
			#[inline]
			pub fn $name(mut self, value: $ty) -> Self {
				self.params[$index] = value.into();
				self
			}
		}
	);

	(string builder into $ident:ident; $index:expr, $name:ident : $ty:ty) => (
		impl<'a> Expansion<'a, $ident<'a>> {
			/// Set the given parameter.
			#[inline]
			pub fn $name<T: Into<$ty>>(mut self, value: T) -> Self {
				self.params[$index] = value.into().into();
				self
			}
		}
	);
}

define!(boolean AutoLeftMargin => "auto_left_margin");
define!(boolean AutoRightMargin => "auto_right_margin");
define!(boolean NoEscCtlc => "no_esc_ctlc");
define!(boolean CeolStandoutGlitch => "ceol_standout_glitch");
define!(boolean EatNewlineGlitch => "eat_newline_glitch");
define!(boolean EraseOverstrike => "erase_overstrike");
define!(boolean GenericType => "generic_type");
define!(boolean HardCopy => "hard_copy");
define!(boolean HasMetaKey => "has_meta_key");
define!(boolean HasStatusLine => "has_status_line");
define!(boolean InsertNullGlitch => "insert_null_glitch");
define!(boolean MemoryAbove => "memory_above");
define!(boolean MemoryBelow => "memory_below");
define!(boolean MoveInsertMode => "move_insert_mode");
define!(boolean MoveStandoutMode => "move_standout_mode");
define!(boolean OverStrike => "over_strike");
define!(boolean StatusLineEscOk => "status_line_esc_ok");
define!(boolean DestTabsMagicSmso => "dest_tabs_magic_smso");
define!(boolean TildeGlitch => "tilde_glitch");
define!(boolean TransparentUnderline => "transparent_underline");
define!(boolean XonXoff => "xon_xoff");
define!(boolean NeedsXonXoff => "needs_xon_xoff");
define!(boolean PrtrSilent => "prtr_silent");
define!(boolean HardCursor => "hard_cursor");
define!(boolean NonRevRmcup => "non_rev_rmcup");
define!(boolean NoPadChar => "no_pad_char");
define!(boolean NonDestScrollRegion => "non_dest_scroll_region");
define!(boolean CanChange => "can_change");
define!(boolean BackColorErase => "back_color_erase");
define!(boolean HueLightnessSaturation => "hue_lightness_saturation");
define!(boolean ColAddrGlitch => "col_addr_glitch");
define!(boolean CrCancelsMicroMode => "cr_cancels_micro_mode");
define!(boolean HasPrintWheel => "has_print_wheel");
define!(boolean RowAddrGlitch => "row_addr_glitch");
define!(boolean SemiAutoRightMargin => "semi_auto_right_margin");
define!(boolean CpiChangesRes => "cpi_changes_res");
define!(boolean LpiChangesRes => "lpi_changes_res");
define!(boolean BackspacesWithBs => "backspaces_with_bs");
define!(boolean CrtNoScrolling => "crt_no_scrolling");
define!(boolean NoCorrectlyWorkingCr => "no_correctly_working_cr");
define!(boolean GnuHasMetaKey => "gnu_has_meta_key");
define!(boolean LinefeedIsNewline => "linefeed_is_newline");
define!(boolean HasHardwareTabs => "has_hardware_tabs");
define!(boolean ReturnDoesClrEol => "return_does_clr_eol");

define!(number Columns => "columns");
define!(number InitTabs => "init_tabs");
define!(number Lines => "lines");
define!(number LinesOfMemory => "lines_of_memory");
define!(number MagicCookieGlitch => "magic_cookie_glitch");
define!(number PaddingBaudRate => "padding_baud_rate");
define!(number VirtualTerminal => "virtual_terminal");
define!(number WidthStatusLine => "width_status_line");
define!(number NumLabels => "num_labels");
define!(number LabelHeight => "label_height");
define!(number LabelWidth => "label_width");
define!(number MaxAttributes => "max_attributes");
define!(number MaximumWindows => "maximum_windows");
define!(number MaxColors => "max_colors");
define!(number MaxPairs => "max_pairs");
define!(number NoColorVideo => "no_color_video");
define!(number BufferCapacity => "buffer_capacity");
define!(number DotVertSpacing => "dot_vert_spacing");
define!(number DotHorzSpacing => "dot_horz_spacing");
define!(number MaxMicroAddress => "max_micro_address");
define!(number MaxMicroJump => "max_micro_jump");
define!(number MicroColSize => "micro_col_size");
define!(number MicroLineSize => "micro_line_size");
define!(number NumberOfPins => "number_of_pins");
define!(number OutputResChar => "output_res_char");
define!(number OutputResLine => "output_res_line");
define!(number OutputResHorzInch => "output_res_horz_inch");
define!(number OutputResVertInch => "output_res_vert_inch");
define!(number PrintRate => "print_rate");
define!(number WideCharSize => "wide_char_size");
define!(number Buttons => "buttons");
define!(number BitImageEntwining => "bit_image_entwining");
define!(number BitImageType => "bit_image_type");
define!(number MagicCookieGlitchUl => "magic_cookie_glitch_ul");
define!(number CarriageReturnDelay => "carriage_return_delay");
define!(number NewLineDelay => "new_line_delay");
define!(number BackspaceDelay => "backspace_delay");
define!(number HorizontalTabDelay => "horizontal_tab_delay");
define!(number NumberOfFunctionKeys => "number_of_function_keys");

define!(string BackTab => "back_tab");
define!(string Bell => "bell");
define!(string CarriageReturn => "carriage_return");
define!(string ClearAllTabs => "clear_all_tabs");
define!(string ClearScreen => "clear_screen");
define!(string ClrEol => "clr_eol");
define!(string ClrEos => "clr_eos");
define!(string CommandCharacter => "command_character");
define!(string CursorDown => "cursor_down");
define!(string CursorHome => "cursor_home");
define!(string CursorInvisible => "cursor_invisible");
define!(string CursorLeft => "cursor_left");
define!(string CursorMemAddress => "cursor_mem_address");
define!(string CursorNormal => "cursor_normal");
define!(string CursorRight => "cursor_right");
define!(string CursorToLl => "cursor_to_ll");
define!(string CursorUp => "cursor_up");
define!(string CursorVisible => "cursor_visible");
define!(string DeleteCharacter => "delete_character");
define!(string DeleteLine => "delete_line");
define!(string DisStatusLine => "dis_status_line");
define!(string DownHalfLine => "down_half_line");
define!(string EnterAltCharsetMode => "enter_alt_charset_mode");
define!(string EnterBlinkMode => "enter_blink_mode");
define!(string EnterBoldMode => "enter_bold_mode");
define!(string EnterCaMode => "enter_ca_mode");
define!(string EnterDeleteMode => "enter_delete_mode");
define!(string EnterDimMode => "enter_dim_mode");
define!(string EnterInsertMode => "enter_insert_mode");
define!(string EnterSecureMode => "enter_secure_mode");
define!(string EnterProtectedMode => "enter_protected_mode");
define!(string EnterReverseMode => "enter_reverse_mode");
define!(string EnterStandoutMode => "enter_standout_mode");
define!(string EnterUnderlineMode => "enter_underline_mode");
define!(string ExitAltCharsetMode => "exit_alt_charset_mode");
define!(string ExitAttributeMode => "exit_attribute_mode");
define!(string ExitCaMode => "exit_ca_mode");
define!(string ExitDeleteMode => "exit_delete_mode");
define!(string ExitInsertMode => "exit_insert_mode");
define!(string ExitStandoutMode => "exit_standout_mode");
define!(string ExitUnderlineMode => "exit_underline_mode");
define!(string FlashScreen => "flash_screen");
define!(string FormFeed => "form_feed");
define!(string FromStatusLine => "from_status_line");
define!(string Init1String => "init_1string");
define!(string Init2String => "init_2string");
define!(string Init3String => "init_3string");
define!(string InitFile => "init_file");
define!(string InsertCharacter => "insert_character");
define!(string InsertLine => "insert_line");
define!(string InsertPadding => "insert_padding");
define!(string KeyBackspace => "key_backspace");
define!(string KeyCATab => "key_catab");
define!(string KeyClear => "key_clear");
define!(string KeyCTab => "key_ctab");
define!(string KeyDc => "key_dc");
define!(string KeyDl => "key_dl");
define!(string KeyDown => "key_down");
define!(string KeyEic => "key_eic");
define!(string KeyEol => "key_eol");
define!(string KeyEos => "key_eos");
define!(string KeyF0 => "key_f0");
define!(string KeyF1 => "key_f1");
define!(string KeyF10 => "key_f10");
define!(string KeyF2 => "key_f2");
define!(string KeyF3 => "key_f3");
define!(string KeyF4 => "key_f4");
define!(string KeyF5 => "key_f5");
define!(string KeyF6 => "key_f6");
define!(string KeyF7 => "key_f7");
define!(string KeyF8 => "key_f8");
define!(string KeyF9 => "key_f9");
define!(string KeyHome => "key_home");
define!(string KeyIc => "key_ic");
define!(string KeyIl => "key_il");
define!(string KeyLeft => "key_left");
define!(string KeyLl => "key_ll");
define!(string KeyNPage => "key_npage");
define!(string KeyPPage => "key_ppage");
define!(string KeyRight => "key_right");
define!(string KeySf => "key_sf");
define!(string KeySr => "key_sr");
define!(string KeySTab => "key_stab");
define!(string KeyUp => "key_up");
define!(string KeypadLocal => "keypad_local");
define!(string KeypadXmit => "keypad_xmit");
define!(string LabF0 => "lab_f0");
define!(string LabF1 => "lab_f1");
define!(string LabF10 => "lab_f10");
define!(string LabF2 => "lab_f2");
define!(string LabF3 => "lab_f3");
define!(string LabF4 => "lab_f4");
define!(string LabF5 => "lab_f5");
define!(string LabF6 => "lab_f6");
define!(string LabF7 => "lab_f7");
define!(string LabF8 => "lab_f8");
define!(string LabF9 => "lab_f9");
define!(string MetaOff => "meta_off");
define!(string MetaOn => "meta_on");
define!(string Newline => "newline");
define!(string PadChar => "pad_char");
define!(string PKeyKey => "pkey_key");
define!(string PKeyLocal => "pkey_local");
define!(string PKeyXmit => "pkey_xmit");
define!(string PrintScreen => "print_screen");
define!(string PrtrOff => "prtr_off");
define!(string PrtrOn => "prtr_on");
define!(string RepeatChar => "repeat_char");
define!(string Reset1String => "reset_1string");
define!(string Reset2String => "reset_2string");
define!(string Reset3String => "reset_3string");
define!(string ResetFile => "reset_file");
define!(string RestoreCursor => "restore_cursor");
define!(string SaveCursor => "save_cursor");
define!(string ScrollForward => "scroll_forward");
define!(string ScrollReverse => "scroll_reverse");
define!(string SetTab => "set_tab");
define!(string SetWindow => "set_window");
define!(string Tab => "tab");
define!(string ToStatusLine => "to_status_line");
define!(string UnderlineChar => "underline_char");
define!(string UpHalfLine => "up_half_line");
define!(string InitProg => "init_prog");
define!(string KeyA1 => "key_a1");
define!(string KeyA3 => "key_a3");
define!(string KeyB2 => "key_b2");
define!(string KeyC1 => "key_c1");
define!(string KeyC3 => "key_c3");
define!(string PrtrNon => "prtr_non");
define!(string CharPadding => "char_padding");
define!(string AcsChars => "acs_chars");
define!(string PlabNorm => "plab_norm");
define!(string KeyBTab => "key_btab");
define!(string EnterXonMode => "enter_xon_mode");
define!(string ExitXonMode => "exit_xon_mode");
define!(string EnterAmMode => "enter_am_mode");
define!(string ExitAmMode => "exit_am_mode");
define!(string XonCharacter => "xon_character");
define!(string XoffCharacter => "xoff_character");
define!(string EnaAcs => "ena_acs");
define!(string LabelOn => "label_on");
define!(string LabelOff => "label_off");
define!(string KeyBeg => "key_beg");
define!(string KeyCancel => "key_cancel");
define!(string KeyClose => "key_close");
define!(string KeyCommand => "key_command");
define!(string KeyCopy => "key_copy");
define!(string KeyCreate => "key_create");
define!(string KeyEnd => "key_end");
define!(string KeyEnter => "key_enter");
define!(string KeyExit => "key_exit");
define!(string KeyFind => "key_find");
define!(string KeyHelp => "key_help");
define!(string KeyMark => "key_mark");
define!(string KeyMessage => "key_message");
define!(string KeyMove => "key_move");
define!(string KeyNext => "key_next");
define!(string KeyOpen => "key_open");
define!(string KeyOptions => "key_options");
define!(string KeyPrevious => "key_previous");
define!(string KeyPrint => "key_print");
define!(string KeyRedo => "key_redo");
define!(string KeyReference => "key_reference");
define!(string KeyRefresh => "key_refresh");
define!(string KeyReplace => "key_replace");
define!(string KeyRestart => "key_restart");
define!(string KeyResume => "key_resume");
define!(string KeySave => "key_save");
define!(string KeySuspend => "key_suspend");
define!(string KeyUndo => "key_undo");
define!(string KeySBeg => "key_sbeg");
define!(string KeySCancel => "key_scancel");
define!(string KeySCommand => "key_scommand");
define!(string KeySCopy => "key_scopy");
define!(string KeySCreate => "key_screate");
define!(string KeySDc => "key_sdc");
define!(string KeySDl => "key_sdl");
define!(string KeySelect => "key_select");
define!(string KeySEnd => "key_send");
define!(string KeySEol => "key_seol");
define!(string KeySExit => "key_sexit");
define!(string KeySFind => "key_sfind");
define!(string KeySHelp => "key_shelp");
define!(string KeySHome => "key_shome");
define!(string KeySIc => "key_sic");
define!(string KeySLeft => "key_sleft");
define!(string KeySMessage => "key_smessage");
define!(string KeySMove => "key_smove");
define!(string KeySNext => "key_snext");
define!(string KeySOptions => "key_soptions");
define!(string KeySPrevious => "key_sprevious");
define!(string KeySPrint => "key_sprint");
define!(string KeySRedo => "key_sredo");
define!(string KeySReplace => "key_sreplace");
define!(string KeySRight => "key_sright");
define!(string KeySRsume => "key_srsume");
define!(string KeySSave => "key_ssave");
define!(string KeySSuspend => "key_ssuspend");
define!(string KeySUndo => "key_sundo");
define!(string ReqForInput => "req_for_input");
define!(string KeyF11 => "key_f11");
define!(string KeyF12 => "key_f12");
define!(string KeyF13 => "key_f13");
define!(string KeyF14 => "key_f14");
define!(string KeyF15 => "key_f15");
define!(string KeyF16 => "key_f16");
define!(string KeyF17 => "key_f17");
define!(string KeyF18 => "key_f18");
define!(string KeyF19 => "key_f19");
define!(string KeyF20 => "key_f20");
define!(string KeyF21 => "key_f21");
define!(string KeyF22 => "key_f22");
define!(string KeyF23 => "key_f23");
define!(string KeyF24 => "key_f24");
define!(string KeyF25 => "key_f25");
define!(string KeyF26 => "key_f26");
define!(string KeyF27 => "key_f27");
define!(string KeyF28 => "key_f28");
define!(string KeyF29 => "key_f29");
define!(string KeyF30 => "key_f30");
define!(string KeyF31 => "key_f31");
define!(string KeyF32 => "key_f32");
define!(string KeyF33 => "key_f33");
define!(string KeyF34 => "key_f34");
define!(string KeyF35 => "key_f35");
define!(string KeyF36 => "key_f36");
define!(string KeyF37 => "key_f37");
define!(string KeyF38 => "key_f38");
define!(string KeyF39 => "key_f39");
define!(string KeyF40 => "key_f40");
define!(string KeyF41 => "key_f41");
define!(string KeyF42 => "key_f42");
define!(string KeyF43 => "key_f43");
define!(string KeyF44 => "key_f44");
define!(string KeyF45 => "key_f45");
define!(string KeyF46 => "key_f46");
define!(string KeyF47 => "key_f47");
define!(string KeyF48 => "key_f48");
define!(string KeyF49 => "key_f49");
define!(string KeyF50 => "key_f50");
define!(string KeyF51 => "key_f51");
define!(string KeyF52 => "key_f52");
define!(string KeyF53 => "key_f53");
define!(string KeyF54 => "key_f54");
define!(string KeyF55 => "key_f55");
define!(string KeyF56 => "key_f56");
define!(string KeyF57 => "key_f57");
define!(string KeyF58 => "key_f58");
define!(string KeyF59 => "key_f59");
define!(string KeyF60 => "key_f60");
define!(string KeyF61 => "key_f61");
define!(string KeyF62 => "key_f62");
define!(string KeyF63 => "key_f63");
define!(string ClrBol => "clr_bol");
define!(string ClearMargins => "clear_margins");
define!(string SetLeftMargin => "set_left_margin");
define!(string SetRightMargin => "set_right_margin");
define!(string LabelFormat => "label_format");
define!(string SetClock => "set_clock");
define!(string DisplayClock => "display_clock");
define!(string RemoveClock => "remove_clock");
define!(string CreateWindow => "create_window");
define!(string GotoWindow => "goto_window");
define!(string Hangup => "hangup");
define!(string DialPhone => "dial_phone");
define!(string QuickDial => "quick_dial");
define!(string Tone => "tone");
define!(string Pulse => "pulse");
define!(string FlashHook => "flash_hook");
define!(string FixedPause => "fixed_pause");
define!(string WaitTone => "wait_tone");
define!(string User0 => "user0");
define!(string User1 => "user1");
define!(string User2 => "user2");
define!(string User3 => "user3");
define!(string User4 => "user4");
define!(string User5 => "user5");
define!(string User6 => "user6");
define!(string User7 => "user7");
define!(string User8 => "user8");
define!(string User9 => "user9");
define!(string OrigPair => "orig_pair");
define!(string OrigColors => "orig_colors");
define!(string InitializeColor => "initialize_color");
define!(string InitializePair => "initialize_pair");
define!(string SetColorPair => "set_color_pair");
define!(string ChangeCharPitch => "change_char_pitch");
define!(string ChangeLinePitch => "change_line_pitch");
define!(string ChangeResHorz => "change_res_horz");
define!(string ChangeResVert => "change_res_vert");
define!(string DefineChar => "define_char");
define!(string EnterDoublewideMode => "enter_doublewide_mode");
define!(string EnterDraftQuality => "enter_draft_quality");
define!(string EnterItalicsMode => "enter_italics_mode");
define!(string EnterLeftwardMode => "enter_leftward_mode");
define!(string EnterMicroMode => "enter_micro_mode");
define!(string EnterNearLetterQuality => "enter_near_letter_quality");
define!(string EnterNormalQuality => "enter_normal_quality");
define!(string EnterShadowMode => "enter_shadow_mode");
define!(string EnterSubscriptMode => "enter_subscript_mode");
define!(string EnterSuperscriptMode => "enter_superscript_mode");
define!(string EnterUpwardMode => "enter_upward_mode");
define!(string ExitDoublewideMode => "exit_doublewide_mode");
define!(string ExitItalicsMode => "exit_italics_mode");
define!(string ExitLeftwardMode => "exit_leftward_mode");
define!(string ExitMicroMode => "exit_micro_mode");
define!(string ExitShadowMode => "exit_shadow_mode");
define!(string ExitSubscriptMode => "exit_subscript_mode");
define!(string ExitSuperscriptMode => "exit_superscript_mode");
define!(string ExitUpwardMode => "exit_upward_mode");
define!(string MicroColumnAddress => "micro_column_address");
define!(string MicroDown => "micro_down");
define!(string MicroLeft => "micro_left");
define!(string MicroRight => "micro_right");
define!(string MicroRowAddress => "micro_row_address");
define!(string MicroUp => "micro_up");
define!(string OrderOfPins => "order_of_pins");
define!(string SelectCharSet => "select_char_set");
define!(string SetBottomMargin => "set_bottom_margin");
define!(string SetBottomMarginParm => "set_bottom_margin_parm");
define!(string SetLeftMarginParm => "set_left_margin_parm");
define!(string SetRightMarginParm => "set_right_margin_parm");
define!(string SetTopMargin => "set_top_margin");
define!(string SetTopMarginParm => "set_top_margin_parm");
define!(string StartBitImage => "start_bit_image");
define!(string StartCharSetDef => "start_char_set_def");
define!(string StopBitImage => "stop_bit_image");
define!(string StopCharSetDef => "stop_char_set_def");
define!(string SubscriptCharacters => "subscript_characters");
define!(string SuperscriptCharacters => "superscript_characters");
define!(string TheseCauseCr => "these_cause_cr");
define!(string ZeroMotion => "zero_motion");
define!(string CharSetNames => "char_set_names");
define!(string KeyMouse => "key_mouse");
define!(string MouseInfo => "mouse_info");
define!(string ReqMousePos => "req_mouse_pos");
define!(string GetMouse => "get_mouse");
define!(string PkeyPlab => "pkey_plab");
define!(string DeviceType => "device_type");
define!(string CodeSetInit => "code_set_init");
define!(string Set0DesSeq => "set0_des_seq");
define!(string Set1DesSeq => "set1_des_seq");
define!(string Set2DesSeq => "set2_des_seq");
define!(string Set3DesSeq => "set3_des_seq");
define!(string SetLrMargin => "set_lr_margin");
define!(string SetTbMargin => "set_tb_margin");
define!(string BitImageRepeat => "bit_image_repeat");
define!(string BitImageNewline => "bit_image_newline");
define!(string BitImageCarriageReturn => "bit_image_carriage_return");
define!(string ColorNames => "color_names");
define!(string DefineBitImageRegion => "define_bit_image_region");
define!(string EndBitImageRegion => "end_bit_image_region");
define!(string SetColorBand => "set_color_band");
define!(string SetPageLength => "set_page_length");
define!(string DisplayPcChar => "display_pc_char");
define!(string EnterPcCharsetMode => "enter_pc_charset_mode");
define!(string ExitPcCharsetMode => "exit_pc_charset_mode");
define!(string EnterScancodeMode => "enter_scancode_mode");
define!(string ExitScancodeMode => "exit_scancode_mode");
define!(string PcTermOptions => "pc_term_options");
define!(string ScancodeEscape => "scancode_escape");
define!(string AltScancodeEsc => "alt_scancode_esc");
define!(string EnterHorizontalHlMode => "enter_horizontal_hl_mode");
define!(string EnterLeftHlMode => "enter_left_hl_mode");
define!(string EnterLowHlMode => "enter_low_hl_mode");
define!(string EnterRightHlMode => "enter_right_hl_mode");
define!(string EnterTopHlMode => "enter_top_hl_mode");
define!(string EnterVerticalHlMode => "enter_vertical_hl_mode");
define!(string SetAAttributes => "set_a_attributes");
define!(string SetPglenInch => "set_pglen_inch");
define!(string TermcapInit2 => "termcap_init2");
define!(string TermcapReset => "termcap_reset");
define!(string LinefeedIfNotLf => "linefeed_if_not_lf");
define!(string BackspaceIfNotBs => "backspace_if_not_bs");
define!(string OtherNonFunctionKeys => "other_non_function_keys");
define!(string ArrowKeyMap => "arrow_key_map");
define!(string AcsULcorner => "acs_ulcorner");
define!(string AcsLLcorner => "acs_llcorner");
define!(string AcsURcorner => "acs_urcorner");
define!(string AcsLRcorner => "acs_lrcorner");
define!(string AcsLTee => "acs_ltee");
define!(string AcsRTee => "acs_rtee");
define!(string AcsBTee => "acs_btee");
define!(string AcsTTee => "acs_ttee");
define!(string AcsHLine => "acs_hline");
define!(string AcsVLine => "acs_vline");
define!(string AcsPlus => "acs_plus");
define!(string MemoryLock => "memory_lock");
define!(string MemoryUnlock => "memory_unlock");
define!(string BoxChars1 => "box_chars_1");

define!(string ChangeScrollRegion => "change_scroll_region";
	top:    u32,
	bottom: u32);

define!(string ColumnAddress => "column_address";
	x: u32);

define!(string CursorAddress => "cursor_address";
	y: u32,
	x: u32);

define!(string EraseChars => "erase_chars";
	count: u32);

define!(string ParmDch => "parm_dch";
	count: u32);

define!(string ParmDeleteLine => "parm_delete_line";
	count: u32);

define!(string ParmDownCursor => "parm_down_cursor";
	count: u32);

define!(string ParmIch => "parm_ich";
	count: u32);

define!(string ParmIndex => "parm_index";
	count: u32);

define!(string ParmInsertLine => "parm_insert_line";
	count: u32);

define!(string ParmLeftCursor => "parm_left_cursor";
	count: u32);

define!(string ParmRightCursor => "parm_right_cursor";
	count: u32);

define!(string ParmRindex => "parm_rindex";
	count: u32);

define!(string ParmUpCursor => "parm_up_cursor";
	count: u32);

define!(string ParmDownMicro => "parm_down_micro";
	count: u32);

define!(string ParmLeftMicro => "parm_left_micro";
	count: u32);

define!(string ParmRightMicro => "parm_right_micro";
	count: u32);

define!(string ParmUpMicro => "parm_up_micro";
	count: u32);

define!(string RowAddress => "row_address";
	y: u32);

define!(string SetAttributes => "set_attributes";
    standout:    bool,
    underline:   bool,
    reverse:     bool,
    blink:       bool,
    dim:         bool,
    bold:        bool,
    invisible:   bool,
    protected:   bool,
    alt_charset: bool);

define!(string SetAForeground => "set_a_foreground";
	color: u8);

define!(string SetABackground => "set_a_background";
	color: u8);

define!(string SetForeground => "set_foreground";
	color: u8);

define!(string SetBackground => "set_background";
	color: u8);

// Extended capabilities from screen.
define!(boolean XTermTitle => "XT");
define!(boolean BrightAttribute => "AX");
define!(boolean XTermMouse => "XM");

// Extended capabilities from tmux.
define!(boolean TrueColor => "Tc");

define!(string SetClipboard => "Ms";
	selection: String,
	content:   Vec<u8>);

define!(string SetCursorStyle => "Ss";
	kind: u8);

define!(string ResetCursorStyle => "Se");

// True color extended capabilities from vim.
define!(string SetTrueColorForeground => "8f";
	r: u8,
	g: u8,
	b: u8);

define!(string SetTrueColorBackground => "8b";
	r: u8,
	g: u8,
	b: u8);

define!(string ResetCursorColor => "Cr");

define!(string SetCursorColor => "Cs";
	color: String);

#[cfg(test)]
mod test {
	use crate::Database;
	use super::*;

	#[test]
	fn cursor_address() {
		assert_eq!(b"\x1B[3;5H".to_vec(),
			Database::from_path("tests/cancer-256color").unwrap()
				.get::<CursorAddress>().unwrap()
				.expand().parameters(2, 4).to_vec().unwrap());

		assert_eq!(b"\x1B[3;5H".to_vec(),
			Database::from_path("tests/cancer-256color").unwrap()
				.get::<CursorAddress>().unwrap()
				.expand().x(4).y(2).to_vec().unwrap());

		assert_eq!(b"\x1B[38;2;50;100;150m".to_vec(),
			Database::from_path("tests/cancer-256color").unwrap()
				.get::<SetTrueColorForeground>().unwrap()
				.expand().r(50).g(100).b(150).to_vec().unwrap());

		assert_eq!(b"\x1B]clipboard:set:PRIMARY:hue\x07".to_vec(),
			Database::from_path("tests/cancer-256color").unwrap()
				.get::<SetClipboard>().unwrap()
				.expand().selection("PRIMARY").content("hue").to_vec().unwrap());
	}
}
