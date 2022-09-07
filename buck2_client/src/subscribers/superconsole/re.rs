use superconsole::Component;
use superconsole::Line;

use crate::subscribers::re::ReState;

/// Draw the test summary line above the `timed_list`
#[derive(Debug)]
pub struct ReHeader;

impl ReHeader {
    pub fn boxed() -> Box<dyn Component> {
        box Self
    }
}

impl Component for ReHeader {
    fn draw_unchecked(
        &self,
        state: &superconsole::State,
        _dimensions: superconsole::Dimensions,
        _mode: superconsole::DrawMode,
    ) -> anyhow::Result<superconsole::Lines> {
        let re = state.get::<ReState>()?;
        Ok(match re.render() {
            Some(text) => {
                let span = superconsole::Span::new_unstyled(text)?;
                vec![Line::from_iter([span])]
            }
            None => {
                vec![]
            }
        })
    }
}
