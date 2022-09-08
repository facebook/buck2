use std::fmt;

use superconsole::Line;

pub(crate) struct ReState {
    session_id: Option<String>,
    last: Option<Snapshot>,
    /// Detailed RE stats.
    pub(crate) detailed: bool,
}

struct Snapshot {
    /// Full snapshot, including data not needed.
    snapshot: buck2_data::Snapshot,
}

impl ReState {
    pub(crate) fn new() -> Self {
        Self {
            session_id: None,
            last: None,
            detailed: false,
        }
    }

    pub(crate) fn add_re_session(&mut self, session: &buck2_data::RemoteExecutionSessionCreated) {
        self.session_id = Some(session.session_id.clone());
    }

    pub(crate) fn update(&mut self, snapshot: &buck2_data::Snapshot) {
        self.last = Some(Snapshot {
            snapshot: snapshot.clone(),
        });
    }

    pub(crate) fn render_header(&self) -> Option<String> {
        let mut parts = Vec::new();

        if let Some(session_id) = self.session_id.as_ref() {
            parts.push(session_id.to_owned());
        }

        if let Some(last) = self.last.as_ref() {
            if last.snapshot.re_upload_bytes > 0 || last.snapshot.re_download_bytes > 0 {
                parts.push(format!(
                    "{}▲,  {}▼",
                    HumanizedBytes(last.snapshot.re_upload_bytes),
                    HumanizedBytes(last.snapshot.re_download_bytes)
                ));
            }
        }

        if parts.is_empty() {
            return None;
        }

        Some(format!("RE: {}", parts.join(" ")))
    }

    fn render_detailed_items(
        &self,
        name: &str,
        started: u32,
        finished_successfully: u32,
        finished_with_error: u32,
    ) -> anyhow::Result<Option<Line>> {
        let in_progress = started
            .saturating_sub(finished_successfully)
            .saturating_sub(finished_with_error);
        if in_progress == 0 && finished_successfully == 0 && finished_with_error == 0 {
            return Ok(None);
        }
        let line = format!(
            "{name:<20}: \
            {in_progress:>5} in progress, \
            {finished_successfully:>5} success, \
            {finished_with_error:>5} error"
        );
        Ok(Some(Line::from_iter([superconsole::Span::new_unstyled(
            line,
        )?])))
    }

    fn render_detailed(&self) -> anyhow::Result<Vec<Line>> {
        let mut r = Vec::new();
        if let Some(last) = &self.last {
            let last = &last.snapshot;
            r.extend(self.render_detailed_items(
                "uploads",
                last.re_uploads_started,
                last.re_uploads_finished_successfully,
                last.re_uploads_finished_with_error,
            )?);
            r.extend(self.render_detailed_items(
                "downloads",
                last.re_downloads_started,
                last.re_downloads_finished_successfully,
                last.re_downloads_finished_with_error,
            )?);
            r.extend(self.render_detailed_items(
                "action_cache",
                last.re_action_cache_started,
                last.re_action_cache_finished_successfully,
                last.re_action_cache_finished_with_error,
            )?);
            r.extend(self.render_detailed_items(
                "executes",
                last.re_executes_started,
                last.re_executes_finished_successfully,
                last.re_executes_finished_with_error,
            )?);
            r.extend(self.render_detailed_items(
                "materializes",
                last.re_materializes_started,
                last.re_materializes_finished_successfully,
                last.re_materializes_finished_with_error,
            )?);
            r.extend(self.render_detailed_items(
                "write_action_results",
                last.re_write_action_results_started,
                last.re_write_action_results_finished_successfully,
                last.re_write_action_results_finished_with_error,
            )?);
        }
        Ok(r)
    }

    pub(crate) fn render(&self) -> anyhow::Result<Vec<Line>> {
        let header = match self.render_header() {
            Some(header) => header,
            None => return Ok(Vec::new()),
        };
        let header = superconsole::Span::new_unstyled(header)?;
        let mut lines = vec![Line::from_iter([header])];
        if self.detailed {
            lines.extend(self.render_detailed()?);
        }
        Ok(lines)
    }
}

/// Write out a u64 as something more readable
struct HumanizedBytes(u64);

impl fmt::Display for HumanizedBytes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut val = self.0 as f64;
        let mut label = "B";

        let factor = 1024.0;

        for next_label in &["KiB", "MiB", "GiB"] {
            if val < factor {
                break;
            }

            val /= factor;
            label = next_label;
        }

        write!(f, "{:.1} {}", val, label)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_humanized() {
        assert_eq!(HumanizedBytes(10).to_string(), "10.0 B");
        assert_eq!(HumanizedBytes(1536).to_string(), "1.5 KiB");
        assert_eq!(HumanizedBytes(1048575).to_string(), "1024.0 KiB");
        assert_eq!(HumanizedBytes(1048576).to_string(), "1.0 MiB");
        assert_eq!(HumanizedBytes(2168958484).to_string(), "2.0 GiB");
    }
}
