use buck2_client::subscribers::re::ReState;
use superconsole::Component;

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
                vec![superconsole::line!(span)]
            }
            None => {
                vec![]
            }
        })
    }
}
