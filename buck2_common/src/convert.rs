use std::time::Duration;

pub trait ProstDurationExt {
    fn try_into_duration(&self) -> anyhow::Result<Duration>;
}

impl ProstDurationExt for prost_types::Duration {
    fn try_into_duration(&self) -> anyhow::Result<Duration> {
        Ok(Duration::from_secs(self.seconds.try_into()?)
            + Duration::from_nanos(self.nanos.try_into()?))
    }
}
