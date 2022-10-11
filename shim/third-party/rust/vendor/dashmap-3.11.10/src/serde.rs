use crate::DashMap;
use core::fmt;
use core::hash::Hash;
use serde::de::{Deserialize, MapAccess, Visitor};
use serde::export::PhantomData;
use serde::ser::{Serialize, SerializeMap, Serializer};
use serde::Deserializer;

pub struct DashMapVisitor<K, V> {
    marker: PhantomData<fn() -> DashMap<K, V>>,
}

impl<K, V> DashMapVisitor<K, V>
where
    K: Eq + Hash,
{
    fn new() -> Self {
        DashMapVisitor {
            marker: PhantomData,
        }
    }
}

impl<'de, K, V> Visitor<'de> for DashMapVisitor<K, V>
where
    K: Deserialize<'de> + Eq + Hash,
    V: Deserialize<'de>,
{
    type Value = DashMap<K, V>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a DashMap")
    }

    fn visit_map<M>(self, mut access: M) -> Result<Self::Value, M::Error>
    where
        M: MapAccess<'de>,
    {
        let map = DashMap::with_capacity(access.size_hint().unwrap_or(0));

        while let Some((key, value)) = access.next_entry()? {
            map.insert(key, value);
        }

        Ok(map)
    }
}

impl<'de, K, V> Deserialize<'de> for DashMap<K, V>
where
    K: Deserialize<'de> + Eq + Hash,
    V: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_map(DashMapVisitor::<K, V>::new())
    }
}

impl<K, V> Serialize for DashMap<K, V>
where
    K: Serialize + Eq + Hash,
    V: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(self.len()))?;

        for ref_multi in self.iter() {
            map.serialize_entry(ref_multi.key(), ref_multi.value())?;
        }

        map.end()
    }
}
