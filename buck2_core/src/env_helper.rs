use std::env;
use std::env::VarError;
use std::str::FromStr;

use anyhow::Context;
use once_cell::sync::OnceCell;

pub struct EnvHelper<T> {
    convert: fn(&str) -> anyhow::Result<T>,
    var: &'static str,
    cell: OnceCell<Option<T>>,
}

impl<T> EnvHelper<T> {
    pub const fn with_converter(var: &'static str, convert: fn(&str) -> anyhow::Result<T>) -> Self {
        Self {
            convert,
            var,
            cell: OnceCell::new(),
        }
    }

    pub const fn new(var: &'static str) -> Self
    where
        T: FromStr,
        anyhow::Error: From<<T as FromStr>::Err>,
    {
        fn convert_from_str<T>(v: &str) -> anyhow::Result<T>
        where
            T: FromStr,
            anyhow::Error: From<<T as FromStr>::Err>,
        {
            Ok(T::from_str(v)?)
        }

        Self::with_converter(var, convert_from_str::<T>)
    }

    // This code does not really require `'static` lifetime.
    // `EnvHelper` caches computed value. When it is used like
    // `EnvHelper::new(...).get(...)`, it performs unnecessary work.
    // To avoid it, we require `'static` lifetime, to force placing `EnvHelper` in static variable.
    pub fn get(&'static self) -> anyhow::Result<Option<&T>> {
        let var = self.var;
        let convert = self.convert;

        self.cell
            .get_or_try_init(move || match env::var(var) {
                Ok(v) => {
                    tracing::info!("Env override found: ${} = {}", var, v);
                    Ok(Some((convert)(&v).map_err(anyhow::Error::from)?))
                }
                Err(VarError::NotPresent) => Ok(None),
                Err(VarError::NotUnicode(..)) => Err(anyhow::anyhow!("Variable is not unicode")),
            })
            .map(Option::as_ref)
            .with_context(|| format!("Invalid value for ${}", var))
    }

    pub fn get_copied(&'static self) -> anyhow::Result<Option<T>>
    where
        T: Copy,
    {
        Ok(self.get()?.copied())
    }
}
