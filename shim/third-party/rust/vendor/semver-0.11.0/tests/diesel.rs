#![cfg(feature = "diesel")]

#[macro_use]
extern crate diesel;
extern crate semver;

use diesel::connection::SimpleConnection;
use diesel::sql_types::Text;
use diesel::*;
use semver::{Version, VersionReq};

table! {
    versions (name) {
        name -> Text,
        vers -> Text,
    }
}

table! {
    version_reqs (name) {
        name -> Text,
        req -> Text,
    }
}

fn connection() -> SqliteConnection {
    let conn = SqliteConnection::establish(":memory:").unwrap();
    conn.batch_execute(
        "
        CREATE TABLE versions (name TEXT PRIMARY KEY NOT NULL, vers TEXT NOT NULL);
        CREATE TABLE version_reqs (name TEXT PRIMARY KEY NOT NULL, req TEXT NOT NULL);
    ",
    )
    .unwrap();
    conn
}

const VERSIONS_TO_TEST: &[&str] = &[
    "0.0.1",
    "0.1.0",
    "1.0.0",
    "1.0.0-beta1",
    "1.0.0-beta.1",
    "1.0.0+129384712983",
    "1.0.0-beta.1+1234.5678",
];

#[test]
fn version_round_trips() {
    let conn = connection();
    for version in VERSIONS_TO_TEST {
        let version = version.parse::<Version>().unwrap();
        let result = select(version.as_sql::<Text>()).get_result(&conn);
        assert_eq!(Ok(version), result);
    }
}

#[test]
fn version_inserts_and_loads() {
    use self::versions::dsl::*;

    let conn = connection();
    let semver_versions = VERSIONS_TO_TEST
        .iter()
        .enumerate()
        .map(|(i, v)| (format!("Version {}", i), v.parse::<Version>().unwrap()))
        .collect::<Vec<_>>();

    let new_versions = semver_versions
        .iter()
        .map(|&(ref n, ref v)| (name.eq(n), vers.eq(v)))
        .collect::<Vec<_>>();
    let inserted_rows = insert_into(versions).values(&new_versions).execute(&conn);
    assert_eq!(Ok(VERSIONS_TO_TEST.len()), inserted_rows);

    let actual_data = versions.load(&conn);
    assert_eq!(Ok(semver_versions.clone()), actual_data);
}

#[test]
fn version_inserts_and_loads_on_struct() {
    #[derive(Debug, PartialEq, Queryable, Insertable)]
    #[table_name = "versions"]
    struct Versioned {
        name: String,
        vers: Version,
    }

    let conn = connection();
    let semver_versions = VERSIONS_TO_TEST
        .iter()
        .enumerate()
        .map(|(i, v)| Versioned {
            name: format!("Version {}", i),
            vers: v.parse::<Version>().unwrap(),
        })
        .collect::<Vec<_>>();

    let inserted_rows = insert_into(versions::table)
        .values(&semver_versions)
        .execute(&conn);
    assert_eq!(Ok(VERSIONS_TO_TEST.len()), inserted_rows);

    let actual_data = versions::table.load(&conn);
    assert_eq!(Ok(semver_versions), actual_data);
}

const VERSION_REQS_TO_TEST: &[&str] = &[
    "^1.0.0",
    "= 1.0.0",
    "= 0.9.0",
    "= 0.1.0-beta2.a",
    ">= 1.0.0",
    ">= 2.1.0-alpha2",
    "< 1.0.0",
    "<= 2.1.0-alpha2",
    "^ 1.2.3+meta",
    "= 1.2.3+meta",
    "> 1.2.3+meta",
    ">= 1.2.3+meta",
    "< 1.2.3+meta",
    "<= 1.2.3+meta",
    "~ 1.2.3+meta",
    "> 0.0.9, <= 2.5.3",
    "0.3.0, 0.4.0",
    "<= 0.2.0, >= 0.5.0",
    "0.1.0, 0.1.4, 0.1.6",
    ">=0.5.1-alpha3, <0.6",
    "~1",
    "~1.2",
    "~1.2.2",
    "~1.2.3-beta.2",
    "^1",
    "^1.1",
    "^1.1.2",
    "^0.1.2",
    "^0.5.1-alpha3",
    "",
    "*",
    "x",
    "1.*",
];

#[test]
fn version_req_round_trips() {
    let conn = connection();
    for version_req in VERSION_REQS_TO_TEST {
        let version_req = version_req.parse::<VersionReq>().unwrap();
        let result = select(version_req.as_sql::<Text>()).get_result(&conn);
        assert_eq!(Ok(version_req), result);
    }
}

#[test]
fn version_req_inserts_and_loads() {
    use self::version_reqs::dsl::*;

    let conn = connection();
    let semver_version_reqs = VERSION_REQS_TO_TEST
        .iter()
        .enumerate()
        .map(|(i, v)| {
            (
                format!("VersionReq {}", i),
                v.parse::<VersionReq>().unwrap(),
            )
        })
        .collect::<Vec<_>>();

    let new_version_reqs = semver_version_reqs
        .iter()
        .map(|&(ref n, ref v)| (name.eq(n), req.eq(v)))
        .collect::<Vec<_>>();
    let inserted_rows = insert_into(version_reqs)
        .values(&new_version_reqs)
        .execute(&conn);
    assert_eq!(Ok(VERSION_REQS_TO_TEST.len()), inserted_rows);

    let actual_data = version_reqs.load(&conn);
    assert_eq!(Ok(semver_version_reqs.clone()), actual_data);
}

#[test]
fn version_req_inserts_and_loads_on_struct() {
    #[derive(Debug, PartialEq, Queryable, Insertable)]
    #[table_name = "version_reqs"]
    struct VersionReqed {
        name: String,
        req: VersionReq,
    }

    let conn = connection();
    let semver_version_reqs = VERSION_REQS_TO_TEST
        .iter()
        .enumerate()
        .map(|(i, v)| VersionReqed {
            name: format!("VersionReq {}", i),
            req: v.parse::<VersionReq>().unwrap(),
        })
        .collect::<Vec<_>>();

    let inserted_rows = insert_into(version_reqs::table)
        .values(&semver_version_reqs)
        .execute(&conn);
    assert_eq!(Ok(VERSION_REQS_TO_TEST.len()), inserted_rows);

    let actual_data = version_reqs::table.load(&conn);
    assert_eq!(Ok(semver_version_reqs), actual_data);
}
