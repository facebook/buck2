use superconsole::Component;

use crate::subscribers::re_panel::RePanel;

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
        mode: superconsole::DrawMode,
    ) -> anyhow::Result<superconsole::Lines> {
        let re = state.get::<RePanel>()?;
        re.render(mode)
    }
}
