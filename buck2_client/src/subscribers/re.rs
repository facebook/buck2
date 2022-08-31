use std::fmt;

pub struct ReState {
    session_id: Option<String>,
    last: Option<Snapshot>,
}

struct Snapshot {
    upload_bytes: u64,
    download_bytes: u64,
}

impl ReState {
    pub fn new() -> Self {
        Self {
            session_id: None,
            last: None,
        }
    }

    pub fn add_re_session(&mut self, session: &buck2_data::RemoteExecutionSessionCreated) {
        self.session_id = Some(session.session_id.clone());
    }

    pub fn update(&mut self, snapshot: &buck2_data::Snapshot) {
        self.last = Some(Snapshot {
            upload_bytes: snapshot.re_upload_bytes,
            download_bytes: snapshot.re_download_bytes,
        });
    }

    pub fn render(&self) -> Option<String> {
        let mut parts = Vec::new();

        if let Some(session_id) = self.session_id.as_ref() {
            parts.push(session_id.to_owned());
        }

        if let Some(last) = self.last.as_ref() {
            if last.upload_bytes > 0 || last.download_bytes > 0 {
                parts.push(format!(
                    "{}▲,  {}▼",
                    HumanizedBytes(last.upload_bytes),
                    HumanizedBytes(last.download_bytes)
                ));
            }
        }

        if parts.is_empty() {
            return None;
        }

        Some(format!("RE: {}", parts.join(" ")))
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
