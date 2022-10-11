use std::time::Duration;

use crossterm_winapi::{Console, Handle, InputRecord};

use crate::event::{sys::windows::poll::WinApiPoll, Event};

#[cfg(feature = "event-stream")]
use super::super::sys::Waker;
use super::super::{
    source::EventSource,
    sys::windows::parse::{handle_key_event, handle_mouse_event},
    timeout::PollTimeout,
    InternalEvent, Result,
};

pub(crate) struct WindowsEventSource {
    console: Console,
    poll: WinApiPoll,
}

impl WindowsEventSource {
    pub(crate) fn new() -> Result<WindowsEventSource> {
        let console = Console::from(Handle::current_in_handle()?);
        Ok(WindowsEventSource {
            console,

            #[cfg(not(feature = "event-stream"))]
            poll: WinApiPoll::new(),
            #[cfg(feature = "event-stream")]
            poll: WinApiPoll::new()?,
        })
    }
}

impl EventSource for WindowsEventSource {
    fn try_read(&mut self, timeout: Option<Duration>) -> Result<Option<InternalEvent>> {
        let poll_timeout = PollTimeout::new(timeout);

        loop {
            if let Some(event_ready) = self.poll.poll(poll_timeout.leftover())? {
                let number = self.console.number_of_console_input_events()?;
                if event_ready && number != 0 {
                    let event = match self.console.read_single_input_event()? {
                        InputRecord::KeyEvent(record) => handle_key_event(record),
                        InputRecord::MouseEvent(record) => handle_mouse_event(record),
                        InputRecord::WindowBufferSizeEvent(record) => {
                            Some(Event::Resize(record.size.x as u16, record.size.y as u16))
                        }
                        _ => None,
                    };

                    if let Some(event) = event {
                        return Ok(Some(InternalEvent::Event(event)));
                    }
                }
            }

            if poll_timeout.elapsed() {
                return Ok(None);
            }
        }
    }

    #[cfg(feature = "event-stream")]
    fn waker(&self) -> Waker {
        self.poll.waker()
    }
}
