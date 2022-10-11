use serde::de::{self, Deserialize, Deserializer, Visitor};
use serde_derive::Deserialize;
use std::fmt;

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub struct InheritEdition {
    pub workspace: True,
}

pub struct True;

impl<'de> Deserialize<'de> for True {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_bool(True)
    }
}

impl<'de> Visitor<'de> for True {
    type Value = True;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("bool")
    }

    fn visit_bool<E>(self, b: bool) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        if b {
            Ok(True)
        } else {
            Err(de::Error::custom(
                "workspace=false is unsupported for package.edition",
            ))
        }
    }
}
