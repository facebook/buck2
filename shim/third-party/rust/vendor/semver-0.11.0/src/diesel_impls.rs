use diesel::backend::Backend;
use diesel::deserialize::{self, FromSql};
use diesel::serialize::{self, IsNull, Output, ToSql};
use diesel::sql_types::Text;
use std::io::Write;

use {Version, VersionReq};

impl<DB> FromSql<Text, DB> for Version
where
    DB: Backend,
    *const str: FromSql<Text, DB>,
{
    fn from_sql(input: Option<&DB::RawValue>) -> deserialize::Result<Self> {
        let str_ptr = <*const str as FromSql<Text, DB>>::from_sql(input)?;
        let s = unsafe { &*str_ptr };
        s.parse().map_err(Into::into)
    }
}

impl<DB: Backend> ToSql<Text, DB> for Version {
    fn to_sql<W: Write>(&self, out: &mut Output<W, DB>) -> serialize::Result {
        write!(out, "{}", self)?;
        Ok(IsNull::No)
    }
}

impl<DB> FromSql<Text, DB> for VersionReq
where
    DB: Backend,
    *const str: FromSql<Text, DB>,
{
    fn from_sql(input: Option<&DB::RawValue>) -> deserialize::Result<Self> {
        let str_ptr = <*const str as FromSql<Text, DB>>::from_sql(input)?;
        let s = unsafe { &*str_ptr };
        s.parse().map_err(Into::into)
    }
}

impl<DB: Backend> ToSql<Text, DB> for VersionReq {
    fn to_sql<W: Write>(&self, out: &mut Output<W, DB>) -> serialize::Result {
        write!(out, "{}", self)?;
        Ok(IsNull::No)
    }
}
