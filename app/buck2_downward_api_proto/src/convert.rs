use std::collections::HashMap;

use anyhow::Context as _;
use tracing::Level;

use crate::proto;

impl TryInto<HashMap<String, String>> for proto::Event {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<HashMap<String, String>, Self::Error> {
        use std::collections::hash_map::Entry;

        use proto::event::Item;

        let proto::Event { items } = self;

        let mut ret = HashMap::new();
        for Item { key, value } in items {
            match ret.entry(key) {
                Entry::Vacant(e) => {
                    e.insert(value);
                }
                Entry::Occupied(e) => {
                    anyhow::bail!("Duplicate key: {}", e.key());
                }
            }
        }

        Ok(ret)
    }
}

impl<T> From<T> for proto::Event
where
    T: IntoIterator<Item = (String, String)>,
{
    fn from(iter: T) -> proto::Event {
        use proto::event::Item;

        let items = iter
            .into_iter()
            .map(|(key, value)| Item { key, value })
            .collect();

        proto::Event { items }
    }
}

impl TryInto<Level> for proto::LogLevel {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<Level, Self::Error> {
        use proto::log_level::Value;

        let proto::LogLevel { value } = self;
        let value = Value::from_i32(value).context("Invalid `value`")?;

        Ok(match value {
            Value::NotSet => anyhow::bail!("Missing `value`"),
            Value::Trace => Level::TRACE,
            Value::Debug => Level::DEBUG,
            Value::Info => Level::INFO,
            Value::Warn => Level::WARN,
            Value::Error => Level::ERROR,
        })
    }
}

impl TryFrom<Level> for proto::LogLevel {
    type Error = anyhow::Error;

    fn try_from(level: Level) -> Result<proto::LogLevel, Self::Error> {
        use proto::log_level::Value;

        let value = match level {
            v if v == Level::TRACE => Value::Trace,
            v if v == Level::DEBUG => Value::Debug,
            v if v == Level::INFO => Value::Info,
            v if v == Level::WARN => Value::Warn,
            v if v == Level::ERROR => Value::Error,
            v => anyhow::bail!("Unsupported Level: {:?}", v),
        };

        Ok(proto::LogLevel {
            value: value as i32,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_log_level_roundtrip() {
        for v in &[
            Level::TRACE,
            Level::DEBUG,
            Level::INFO,
            Level::WARN,
            Level::ERROR,
        ] {
            let p = proto::LogLevel::try_from(*v).unwrap();
            let v2: Level = p.try_into().unwrap();
            assert_eq!(*v, v2);
        }
    }
}
