use std::io;

use crate::{
    event::{Event, KeyCode, KeyEvent, KeyModifiers, MouseButton, MouseEvent, MouseEventKind},
    ErrorKind, Result,
};

use super::super::super::InternalEvent;

// Event parsing
//
// This code (& previous one) are kind of ugly. We have to think about this,
// because it's really not maintainable, no tests, etc.
//
// Every fn returns Result<Option<InputEvent>>
//
// Ok(None) -> wait for more bytes
// Err(_) -> failed to parse event, clear the buffer
// Ok(Some(event)) -> we have event, clear the buffer
//

fn could_not_parse_event_error() -> ErrorKind {
    io::Error::new(io::ErrorKind::Other, "Could not parse an event.")
}

pub(crate) fn parse_event(buffer: &[u8], input_available: bool) -> Result<Option<InternalEvent>> {
    if buffer.is_empty() {
        return Ok(None);
    }

    match buffer[0] {
        b'\x1B' => {
            if buffer.len() == 1 {
                if input_available {
                    // Possible Esc sequence
                    Ok(None)
                } else {
                    Ok(Some(InternalEvent::Event(Event::Key(KeyCode::Esc.into()))))
                }
            } else {
                match buffer[1] {
                    b'O' => {
                        if buffer.len() == 2 {
                            Ok(None)
                        } else {
                            match buffer[2] {
                                // F1-F4
                                val @ b'P'..=b'S' => Ok(Some(InternalEvent::Event(Event::Key(
                                    KeyCode::F(1 + val - b'P').into(),
                                )))),
                                _ => Err(could_not_parse_event_error()),
                            }
                        }
                    }
                    b'[' => parse_csi(buffer),
                    b'\x1B' => Ok(Some(InternalEvent::Event(Event::Key(KeyCode::Esc.into())))),
                    _ => parse_event(&buffer[1..], input_available).map(|event_option| {
                        event_option.map(|event| {
                            if let InternalEvent::Event(Event::Key(key_event)) = event {
                                let mut alt_key_event = key_event;
                                alt_key_event.modifiers |= KeyModifiers::ALT;
                                InternalEvent::Event(Event::Key(alt_key_event))
                            } else {
                                event
                            }
                        })
                    }),
                }
            }
        }
        b'\r' => Ok(Some(InternalEvent::Event(Event::Key(
            KeyCode::Enter.into(),
        )))),
        // Issue #371: \n = 0xA, which is also the keycode for Ctrl+J. The only reason we get
        // newlines as input is because the terminal converts \r into \n for us. When we
        // enter raw mode, we disable that, so \n no longer has any meaning - it's better to
        // use Ctrl+J. Waiting to handle it here means it gets picked up later
        b'\n' if !crate::terminal::sys::is_raw_mode_enabled() => Ok(Some(InternalEvent::Event(
            Event::Key(KeyCode::Enter.into()),
        ))),
        b'\t' => Ok(Some(InternalEvent::Event(Event::Key(KeyCode::Tab.into())))),
        b'\x7F' => Ok(Some(InternalEvent::Event(Event::Key(
            KeyCode::Backspace.into(),
        )))),
        c @ b'\x01'..=b'\x1A' => Ok(Some(InternalEvent::Event(Event::Key(KeyEvent::new(
            KeyCode::Char((c as u8 - 0x1 + b'a') as char),
            KeyModifiers::CONTROL,
        ))))),
        c @ b'\x1C'..=b'\x1F' => Ok(Some(InternalEvent::Event(Event::Key(KeyEvent::new(
            KeyCode::Char((c as u8 - 0x1C + b'4') as char),
            KeyModifiers::CONTROL,
        ))))),
        b'\0' => Ok(Some(InternalEvent::Event(Event::Key(KeyEvent::new(
            KeyCode::Char(' '),
            KeyModifiers::CONTROL,
        ))))),
        _ => parse_utf8_char(buffer).map(|maybe_char| {
            maybe_char
                .map(KeyCode::Char)
                .map(char_code_to_event)
                .map(Event::Key)
                .map(InternalEvent::Event)
        }),
    }
}

// converts KeyCode to KeyEvent (adds shift modifier in case of uppercase characters)
fn char_code_to_event(code: KeyCode) -> KeyEvent {
    let modifiers = match code {
        KeyCode::Char(c) if c.is_uppercase() => KeyModifiers::SHIFT,
        _ => KeyModifiers::empty(),
    };
    KeyEvent::new(code, modifiers)
}

pub(crate) fn parse_csi(buffer: &[u8]) -> Result<Option<InternalEvent>> {
    assert!(buffer.starts_with(&[b'\x1B', b'['])); // ESC [

    if buffer.len() == 2 {
        return Ok(None);
    }

    let input_event = match buffer[2] {
        b'[' => {
            if buffer.len() == 3 {
                None
            } else {
                match buffer[3] {
                    // NOTE (@imdaveho): cannot find when this occurs;
                    // having another '[' after ESC[ not a likely scenario
                    val @ b'A'..=b'E' => Some(Event::Key(KeyCode::F(1 + val - b'A').into())),
                    _ => return Err(could_not_parse_event_error()),
                }
            }
        }
        b'D' => Some(Event::Key(KeyCode::Left.into())),
        b'C' => Some(Event::Key(KeyCode::Right.into())),
        b'A' => Some(Event::Key(KeyCode::Up.into())),
        b'B' => Some(Event::Key(KeyCode::Down.into())),
        b'H' => Some(Event::Key(KeyCode::Home.into())),
        b'F' => Some(Event::Key(KeyCode::End.into())),
        b'Z' => Some(Event::Key(KeyEvent {
            code: KeyCode::BackTab,
            modifiers: KeyModifiers::SHIFT,
        })),
        b'M' => return parse_csi_normal_mouse(buffer),
        b'<' => return parse_csi_sgr_mouse(buffer),
        b'0'..=b'9' => {
            // Numbered escape code.
            if buffer.len() == 3 {
                None
            } else {
                // The final byte of a CSI sequence can be in the range 64-126, so
                // let's keep reading anything else.
                let last_byte = *buffer.last().unwrap();
                if !(64..=126).contains(&last_byte) {
                    None
                } else {
                    match buffer[buffer.len() - 1] {
                        b'M' => return parse_csi_rxvt_mouse(buffer),
                        b'~' => return parse_csi_special_key_code(buffer),
                        b'u' => return parse_csi_u_encoded_key_code(buffer),
                        b'R' => return parse_csi_cursor_position(buffer),
                        _ => return parse_csi_modifier_key_code(buffer),
                    }
                }
            }
        }
        _ => return Err(could_not_parse_event_error()),
    };

    Ok(input_event.map(InternalEvent::Event))
}

pub(crate) fn next_parsed<T>(iter: &mut dyn Iterator<Item = &str>) -> Result<T>
where
    T: std::str::FromStr,
{
    iter.next()
        .ok_or_else(could_not_parse_event_error)?
        .parse::<T>()
        .map_err(|_| could_not_parse_event_error())
}

pub(crate) fn parse_csi_cursor_position(buffer: &[u8]) -> Result<Option<InternalEvent>> {
    // ESC [ Cy ; Cx R
    //   Cy - cursor row number (starting from 1)
    //   Cx - cursor column number (starting from 1)
    assert!(buffer.starts_with(&[b'\x1B', b'['])); // ESC [
    assert!(buffer.ends_with(&[b'R']));

    let s = std::str::from_utf8(&buffer[2..buffer.len() - 1])
        .map_err(|_| could_not_parse_event_error())?;

    let mut split = s.split(';');

    let y = next_parsed::<u16>(&mut split)? - 1;
    let x = next_parsed::<u16>(&mut split)? - 1;

    Ok(Some(InternalEvent::CursorPosition(x, y)))
}

fn parse_modifiers(mask: u8) -> KeyModifiers {
    let modifier_mask = mask.saturating_sub(1);
    let mut modifiers = KeyModifiers::empty();
    if modifier_mask & 1 != 0 {
        modifiers |= KeyModifiers::SHIFT;
    }
    if modifier_mask & 2 != 0 {
        modifiers |= KeyModifiers::ALT;
    }
    if modifier_mask & 4 != 0 {
        modifiers |= KeyModifiers::CONTROL;
    }
    modifiers
}

pub(crate) fn parse_csi_modifier_key_code(buffer: &[u8]) -> Result<Option<InternalEvent>> {
    assert!(buffer.starts_with(&[b'\x1B', b'['])); // ESC [

    let modifier_mask = buffer[buffer.len() - 2];
    let key = buffer[buffer.len() - 1];

    let modifiers = parse_modifiers(modifier_mask);

    let keycode = match key {
        b'A' => KeyCode::Up,
        b'B' => KeyCode::Down,
        b'C' => KeyCode::Right,
        b'D' => KeyCode::Left,
        b'F' => KeyCode::End,
        b'H' => KeyCode::Home,
        b'P' => KeyCode::F(1),
        b'Q' => KeyCode::F(2),
        b'R' => KeyCode::F(3),
        b'S' => KeyCode::F(4),
        _ => return Err(could_not_parse_event_error()),
    };

    let input_event = Event::Key(KeyEvent::new(keycode, modifiers));

    Ok(Some(InternalEvent::Event(input_event)))
}

pub(crate) fn parse_csi_u_encoded_key_code(buffer: &[u8]) -> Result<Option<InternalEvent>> {
    assert!(buffer.starts_with(&[b'\x1B', b'['])); // ESC [
    assert!(buffer.ends_with(&[b'u']));

    let s = std::str::from_utf8(&buffer[2..buffer.len() - 1])
        .map_err(|_| could_not_parse_event_error())?;
    let mut split = s.split(';');

    // This CSI sequence a tuple of semicolon-separated numbers.
    // CSI [codepoint];[modifiers] u
    // codepoint: ASCII Dec value
    let codepoint = next_parsed::<u32>(&mut split)?;

    let modifiers = if let Ok(modifier_mask) = next_parsed::<u8>(&mut split) {
        parse_modifiers(modifier_mask)
    } else {
        KeyModifiers::NONE
    };

    let keycode = {
        if let Some(c) = char::from_u32(codepoint) {
            match c {
                '\x1B' => KeyCode::Esc,
                '\r' => KeyCode::Enter,
                // Issue #371: \n = 0xA, which is also the keycode for Ctrl+J. The only reason we get
                // newlines as input is because the terminal converts \r into \n for us. When we
                // enter raw mode, we disable that, so \n no longer has any meaning - it's better to
                // use Ctrl+J. Waiting to handle it here means it gets picked up later
                '\n' if !crate::terminal::sys::is_raw_mode_enabled() => KeyCode::Enter,
                '\t' => {
                    if modifiers.contains(KeyModifiers::SHIFT) {
                        KeyCode::BackTab
                    } else {
                        KeyCode::Tab
                    }
                }
                '\x7F' => KeyCode::Backspace,
                _ => KeyCode::Char(c),
            }
        } else {
            return Err(could_not_parse_event_error());
        }
    };

    let input_event = Event::Key(KeyEvent::new(keycode, modifiers));

    Ok(Some(InternalEvent::Event(input_event)))
}

pub(crate) fn parse_csi_special_key_code(buffer: &[u8]) -> Result<Option<InternalEvent>> {
    assert!(buffer.starts_with(&[b'\x1B', b'['])); // ESC [
    assert!(buffer.ends_with(&[b'~']));

    let s = std::str::from_utf8(&buffer[2..buffer.len() - 1])
        .map_err(|_| could_not_parse_event_error())?;
    let mut split = s.split(';');

    // This CSI sequence can be a list of semicolon-separated numbers.
    let first = next_parsed::<u8>(&mut split)?;

    let modifiers = if let Ok(modifier_mask) = next_parsed::<u8>(&mut split) {
        parse_modifiers(modifier_mask)
    } else {
        KeyModifiers::NONE
    };

    let keycode = match first {
        1 | 7 => KeyCode::Home,
        2 => KeyCode::Insert,
        3 => KeyCode::Delete,
        4 | 8 => KeyCode::End,
        5 => KeyCode::PageUp,
        6 => KeyCode::PageDown,
        v @ 11..=15 => KeyCode::F(v - 10),
        v @ 17..=21 => KeyCode::F(v - 11),
        v @ 23..=26 => KeyCode::F(v - 12),
        v @ 28..=29 => KeyCode::F(v - 15),
        v @ 31..=34 => KeyCode::F(v - 17),
        _ => return Err(could_not_parse_event_error()),
    };

    let input_event = Event::Key(KeyEvent::new(keycode, modifiers));

    Ok(Some(InternalEvent::Event(input_event)))
}

pub(crate) fn parse_csi_rxvt_mouse(buffer: &[u8]) -> Result<Option<InternalEvent>> {
    // rxvt mouse encoding:
    // ESC [ Cb ; Cx ; Cy ; M

    assert!(buffer.starts_with(&[b'\x1B', b'['])); // ESC [
    assert!(buffer.ends_with(&[b'M']));

    let s = std::str::from_utf8(&buffer[2..buffer.len() - 1])
        .map_err(|_| could_not_parse_event_error())?;
    let mut split = s.split(';');

    let cb = next_parsed::<u8>(&mut split)?
        .checked_sub(32)
        .ok_or_else(could_not_parse_event_error)?;
    let (kind, modifiers) = parse_cb(cb)?;

    let cx = next_parsed::<u16>(&mut split)? - 1;
    let cy = next_parsed::<u16>(&mut split)? - 1;

    Ok(Some(InternalEvent::Event(Event::Mouse(MouseEvent {
        kind,
        column: cx,
        row: cy,
        modifiers,
    }))))
}

pub(crate) fn parse_csi_normal_mouse(buffer: &[u8]) -> Result<Option<InternalEvent>> {
    // Normal mouse encoding: ESC [ M CB Cx Cy (6 characters only).

    assert!(buffer.starts_with(&[b'\x1B', b'[', b'M'])); // ESC [ M

    if buffer.len() < 6 {
        return Ok(None);
    }

    let cb = buffer[3]
        .checked_sub(32)
        .ok_or_else(could_not_parse_event_error)?;
    let (kind, modifiers) = parse_cb(cb)?;

    // See http://www.xfree86.org/current/ctlseqs.html#Mouse%20Tracking
    // The upper left character position on the terminal is denoted as 1,1.
    // Subtract 1 to keep it synced with cursor
    let cx = u16::from(buffer[4].saturating_sub(32)) - 1;
    let cy = u16::from(buffer[5].saturating_sub(32)) - 1;

    Ok(Some(InternalEvent::Event(Event::Mouse(MouseEvent {
        kind,
        column: cx,
        row: cy,
        modifiers,
    }))))
}

pub(crate) fn parse_csi_sgr_mouse(buffer: &[u8]) -> Result<Option<InternalEvent>> {
    // ESC [ < Cb ; Cx ; Cy (;) (M or m)

    assert!(buffer.starts_with(&[b'\x1B', b'[', b'<'])); // ESC [ <

    if !buffer.ends_with(&[b'm']) && !buffer.ends_with(&[b'M']) {
        return Ok(None);
    }

    let s = std::str::from_utf8(&buffer[3..buffer.len() - 1])
        .map_err(|_| could_not_parse_event_error())?;
    let mut split = s.split(';');

    let cb = next_parsed::<u8>(&mut split)?;
    let (kind, modifiers) = parse_cb(cb)?;

    // See http://www.xfree86.org/current/ctlseqs.html#Mouse%20Tracking
    // The upper left character position on the terminal is denoted as 1,1.
    // Subtract 1 to keep it synced with cursor
    let cx = next_parsed::<u16>(&mut split)? - 1;
    let cy = next_parsed::<u16>(&mut split)? - 1;

    // When button 3 in Cb is used to represent mouse release, you can't tell which button was
    // released. SGR mode solves this by having the sequence end with a lowercase m if it's a
    // button release and an uppercase M if it's a buton press.
    //
    // We've already checked that the last character is a lowercase or uppercase M at the start of
    // this function, so we just need one if.
    let kind = if buffer.last() == Some(&b'm') {
        match kind {
            MouseEventKind::Down(button) => MouseEventKind::Up(button),
            other => other,
        }
    } else {
        kind
    };

    Ok(Some(InternalEvent::Event(Event::Mouse(MouseEvent {
        kind,
        column: cx,
        row: cy,
        modifiers,
    }))))
}

/// Cb is the byte of a mouse input that contains the button being used, the key modifiers being
/// held and whether the mouse is dragging or not.
///
/// Bit layout of cb, from low to high:
///
/// - button number
/// - button number
/// - shift
/// - meta (alt)
/// - control
/// - mouse is dragging
/// - button number
/// - button number
fn parse_cb(cb: u8) -> Result<(MouseEventKind, KeyModifiers)> {
    let button_number = (cb & 0b0000_0011) | ((cb & 0b1100_0000) >> 4);
    let dragging = cb & 0b0010_0000 == 0b0010_0000;

    let kind = match (button_number, dragging) {
        (0, false) => MouseEventKind::Down(MouseButton::Left),
        (1, false) => MouseEventKind::Down(MouseButton::Middle),
        (2, false) => MouseEventKind::Down(MouseButton::Right),
        (0, true) => MouseEventKind::Drag(MouseButton::Left),
        (1, true) => MouseEventKind::Drag(MouseButton::Middle),
        (2, true) => MouseEventKind::Drag(MouseButton::Right),
        (3, false) => MouseEventKind::Up(MouseButton::Left),
        (3, true) | (4, true) | (5, true) => MouseEventKind::Moved,
        (4, false) => MouseEventKind::ScrollUp,
        (5, false) => MouseEventKind::ScrollDown,
        // We do not support other buttons.
        _ => return Err(could_not_parse_event_error()),
    };

    let mut modifiers = KeyModifiers::empty();

    if cb & 0b0000_0100 == 0b0000_0100 {
        modifiers |= KeyModifiers::SHIFT;
    }
    if cb & 0b0000_1000 == 0b0000_1000 {
        modifiers |= KeyModifiers::ALT;
    }
    if cb & 0b0001_0000 == 0b0001_0000 {
        modifiers |= KeyModifiers::CONTROL;
    }

    Ok((kind, modifiers))
}

pub(crate) fn parse_utf8_char(buffer: &[u8]) -> Result<Option<char>> {
    match std::str::from_utf8(buffer) {
        Ok(s) => {
            let ch = s.chars().next().ok_or_else(could_not_parse_event_error)?;

            Ok(Some(ch))
        }
        Err(_) => {
            // from_utf8 failed, but we have to check if we need more bytes for code point
            // and if all the bytes we have no are valid

            let required_bytes = match buffer[0] {
                // https://en.wikipedia.org/wiki/UTF-8#Description
                (0x00..=0x7F) => 1, // 0xxxxxxx
                (0xC0..=0xDF) => 2, // 110xxxxx 10xxxxxx
                (0xE0..=0xEF) => 3, // 1110xxxx 10xxxxxx 10xxxxxx
                (0xF0..=0xF7) => 4, // 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
                (0x80..=0xBF) | (0xF8..=0xFF) => return Err(could_not_parse_event_error()),
            };

            // More than 1 byte, check them for 10xxxxxx pattern
            if required_bytes > 1 && buffer.len() > 1 {
                for byte in &buffer[1..] {
                    if byte & !0b0011_1111 != 0b1000_0000 {
                        return Err(could_not_parse_event_error());
                    }
                }
            }

            if buffer.len() < required_bytes {
                // All bytes looks good so far, but we need more of them
                Ok(None)
            } else {
                Err(could_not_parse_event_error())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::event::{KeyModifiers, MouseButton, MouseEvent};

    use super::*;

    #[test]
    fn test_esc_key() {
        assert_eq!(
            parse_event(b"\x1B", false).unwrap(),
            Some(InternalEvent::Event(Event::Key(KeyCode::Esc.into()))),
        );
    }

    #[test]
    fn test_possible_esc_sequence() {
        assert_eq!(parse_event(b"\x1B", true).unwrap(), None,);
    }

    #[test]
    fn test_alt_key() {
        assert_eq!(
            parse_event(b"\x1Bc", false).unwrap(),
            Some(InternalEvent::Event(Event::Key(KeyEvent::new(
                KeyCode::Char('c'),
                KeyModifiers::ALT
            )))),
        );
    }

    #[test]
    fn test_alt_shift() {
        assert_eq!(
            parse_event(b"\x1BH", false).unwrap(),
            Some(InternalEvent::Event(Event::Key(KeyEvent::new(
                KeyCode::Char('H'),
                KeyModifiers::ALT | KeyModifiers::SHIFT
            )))),
        );
    }

    #[test]
    fn test_alt_ctrl() {
        assert_eq!(
            parse_event(b"\x1B\x14", false).unwrap(),
            Some(InternalEvent::Event(Event::Key(KeyEvent::new(
                KeyCode::Char('t'),
                KeyModifiers::ALT | KeyModifiers::CONTROL
            )))),
        );
    }

    #[test]
    fn test_parse_event_subsequent_calls() {
        // The main purpose of this test is to check if we're passing
        // correct slice to other parse_ functions.

        // parse_csi_cursor_position
        assert_eq!(
            parse_event(b"\x1B[20;10R", false).unwrap(),
            Some(InternalEvent::CursorPosition(9, 19))
        );

        // parse_csi
        assert_eq!(
            parse_event(b"\x1B[D", false).unwrap(),
            Some(InternalEvent::Event(Event::Key(KeyCode::Left.into()))),
        );

        // parse_csi_modifier_key_code
        assert_eq!(
            parse_event(b"\x1B[2D", false).unwrap(),
            Some(InternalEvent::Event(Event::Key(KeyEvent::new(
                KeyCode::Left,
                KeyModifiers::SHIFT
            ))))
        );

        // parse_csi_special_key_code
        assert_eq!(
            parse_event(b"\x1B[3~", false).unwrap(),
            Some(InternalEvent::Event(Event::Key(KeyCode::Delete.into()))),
        );

        // parse_csi_rxvt_mouse
        assert_eq!(
            parse_event(b"\x1B[32;30;40;M", false).unwrap(),
            Some(InternalEvent::Event(Event::Mouse(MouseEvent {
                kind: MouseEventKind::Down(MouseButton::Left),
                column: 29,
                row: 39,
                modifiers: KeyModifiers::empty(),
            })))
        );

        // parse_csi_normal_mouse
        assert_eq!(
            parse_event(b"\x1B[M0\x60\x70", false).unwrap(),
            Some(InternalEvent::Event(Event::Mouse(MouseEvent {
                kind: MouseEventKind::Down(MouseButton::Left),
                column: 63,
                row: 79,
                modifiers: KeyModifiers::CONTROL,
            })))
        );

        // parse_csi_sgr_mouse
        assert_eq!(
            parse_event(b"\x1B[<0;20;10;M", false).unwrap(),
            Some(InternalEvent::Event(Event::Mouse(MouseEvent {
                kind: MouseEventKind::Down(MouseButton::Left),
                column: 19,
                row: 9,
                modifiers: KeyModifiers::empty(),
            })))
        );

        // parse_utf8_char
        assert_eq!(
            parse_event("Ž".as_bytes(), false).unwrap(),
            Some(InternalEvent::Event(Event::Key(KeyEvent::new(
                KeyCode::Char('Ž'),
                KeyModifiers::SHIFT
            )))),
        );
    }

    #[test]
    fn test_parse_event() {
        assert_eq!(
            parse_event(b"\t", false).unwrap(),
            Some(InternalEvent::Event(Event::Key(KeyCode::Tab.into()))),
        );
    }

    #[test]
    fn test_parse_csi_cursor_position() {
        assert_eq!(
            parse_csi_cursor_position(b"\x1B[20;10R").unwrap(),
            Some(InternalEvent::CursorPosition(9, 19))
        );
    }

    #[test]
    fn test_parse_csi() {
        assert_eq!(
            parse_csi(b"\x1B[D").unwrap(),
            Some(InternalEvent::Event(Event::Key(KeyCode::Left.into()))),
        );
    }

    #[test]
    fn test_parse_csi_modifier_key_code() {
        assert_eq!(
            parse_csi_modifier_key_code(b"\x1B[2D").unwrap(),
            Some(InternalEvent::Event(Event::Key(KeyEvent::new(
                KeyCode::Left,
                KeyModifiers::SHIFT
            )))),
        );
    }

    #[test]
    fn test_parse_csi_special_key_code() {
        assert_eq!(
            parse_csi_special_key_code(b"\x1B[3~").unwrap(),
            Some(InternalEvent::Event(Event::Key(KeyCode::Delete.into()))),
        );
    }

    #[test]
    fn test_parse_csi_special_key_code_multiple_values_not_supported() {
        assert_eq!(
            parse_csi_special_key_code(b"\x1B[3;2~").unwrap(),
            Some(InternalEvent::Event(Event::Key(KeyEvent::new(
                KeyCode::Delete,
                KeyModifiers::SHIFT
            )))),
        );
    }

    #[test]
    fn test_parse_csi_rxvt_mouse() {
        assert_eq!(
            parse_csi_rxvt_mouse(b"\x1B[32;30;40;M").unwrap(),
            Some(InternalEvent::Event(Event::Mouse(MouseEvent {
                kind: MouseEventKind::Down(MouseButton::Left),
                column: 29,
                row: 39,
                modifiers: KeyModifiers::empty(),
            })))
        );
    }

    #[test]
    fn test_parse_csi_normal_mouse() {
        assert_eq!(
            parse_csi_normal_mouse(b"\x1B[M0\x60\x70").unwrap(),
            Some(InternalEvent::Event(Event::Mouse(MouseEvent {
                kind: MouseEventKind::Down(MouseButton::Left),
                column: 63,
                row: 79,
                modifiers: KeyModifiers::CONTROL,
            })))
        );
    }

    #[test]
    fn test_parse_csi_sgr_mouse() {
        assert_eq!(
            parse_csi_sgr_mouse(b"\x1B[<0;20;10;M").unwrap(),
            Some(InternalEvent::Event(Event::Mouse(MouseEvent {
                kind: MouseEventKind::Down(MouseButton::Left),
                column: 19,
                row: 9,
                modifiers: KeyModifiers::empty(),
            })))
        );
        assert_eq!(
            parse_csi_sgr_mouse(b"\x1B[<0;20;10M").unwrap(),
            Some(InternalEvent::Event(Event::Mouse(MouseEvent {
                kind: MouseEventKind::Down(MouseButton::Left),
                column: 19,
                row: 9,
                modifiers: KeyModifiers::empty(),
            })))
        );
        assert_eq!(
            parse_csi_sgr_mouse(b"\x1B[<0;20;10;m").unwrap(),
            Some(InternalEvent::Event(Event::Mouse(MouseEvent {
                kind: MouseEventKind::Up(MouseButton::Left),
                column: 19,
                row: 9,
                modifiers: KeyModifiers::empty(),
            })))
        );
        assert_eq!(
            parse_csi_sgr_mouse(b"\x1B[<0;20;10m").unwrap(),
            Some(InternalEvent::Event(Event::Mouse(MouseEvent {
                kind: MouseEventKind::Up(MouseButton::Left),
                column: 19,
                row: 9,
                modifiers: KeyModifiers::empty(),
            })))
        );
    }

    #[test]
    fn test_utf8() {
        // https://www.php.net/manual/en/reference.pcre.pattern.modifiers.php#54805

        // 'Valid ASCII' => "a",
        assert_eq!(parse_utf8_char(b"a").unwrap(), Some('a'),);

        // 'Valid 2 Octet Sequence' => "\xc3\xb1",
        assert_eq!(parse_utf8_char(&[0xC3, 0xB1]).unwrap(), Some('ñ'),);

        // 'Invalid 2 Octet Sequence' => "\xc3\x28",
        assert!(parse_utf8_char(&[0xC3, 0x28]).is_err());

        // 'Invalid Sequence Identifier' => "\xa0\xa1",
        assert!(parse_utf8_char(&[0xA0, 0xA1]).is_err());

        // 'Valid 3 Octet Sequence' => "\xe2\x82\xa1",
        assert_eq!(
            parse_utf8_char(&[0xE2, 0x81, 0xA1]).unwrap(),
            Some('\u{2061}'),
        );

        // 'Invalid 3 Octet Sequence (in 2nd Octet)' => "\xe2\x28\xa1",
        assert!(parse_utf8_char(&[0xE2, 0x28, 0xA1]).is_err());

        // 'Invalid 3 Octet Sequence (in 3rd Octet)' => "\xe2\x82\x28",
        assert!(parse_utf8_char(&[0xE2, 0x82, 0x28]).is_err());

        // 'Valid 4 Octet Sequence' => "\xf0\x90\x8c\xbc",
        assert_eq!(
            parse_utf8_char(&[0xF0, 0x90, 0x8C, 0xBC]).unwrap(),
            Some('𐌼'),
        );

        // 'Invalid 4 Octet Sequence (in 2nd Octet)' => "\xf0\x28\x8c\xbc",
        assert!(parse_utf8_char(&[0xF0, 0x28, 0x8C, 0xBC]).is_err());

        // 'Invalid 4 Octet Sequence (in 3rd Octet)' => "\xf0\x90\x28\xbc",
        assert!(parse_utf8_char(&[0xF0, 0x90, 0x28, 0xBC]).is_err());

        // 'Invalid 4 Octet Sequence (in 4th Octet)' => "\xf0\x28\x8c\x28",
        assert!(parse_utf8_char(&[0xF0, 0x28, 0x8C, 0x28]).is_err());
    }

    #[test]
    fn test_parse_char_event_lowercase() {
        assert_eq!(
            parse_event(b"c", false).unwrap(),
            Some(InternalEvent::Event(Event::Key(KeyEvent::new(
                KeyCode::Char('c'),
                KeyModifiers::empty()
            )))),
        );
    }

    #[test]
    fn test_parse_char_event_uppercase() {
        assert_eq!(
            parse_event(b"C", false).unwrap(),
            Some(InternalEvent::Event(Event::Key(KeyEvent::new(
                KeyCode::Char('C'),
                KeyModifiers::SHIFT
            )))),
        );
    }
}
