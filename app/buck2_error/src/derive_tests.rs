/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg(test)]

#[derive(buck2_error_derive::Error, Debug)]
#[error("foo")]
#[buck2(user)]
pub struct Error1;

#[test]
fn test_derive_error1() {
    let e: crate::Error = Error1.into();
    assert_eq!(e.get_category(), Some(crate::Category::User));

    let e: anyhow::Error = Error1.into();
    let e: crate::Error = e.into();
    assert_eq!(e.get_category(), Some(crate::Category::User));
}

#[derive(buck2_error_derive::Error, Debug)]
#[error("foo")]
#[buck2(infra, typ = ActionCommandFailure)]
#[allow(unused)]
struct Error2((), ());

#[test]
fn test_derive_error2() {
    let e: crate::Error = Error2((), ()).into();
    assert_eq!(e.get_category(), Some(crate::Category::Infra));
    assert_eq!(
        e.get_error_type(),
        Some(crate::ErrorType::ActionCommandFailure)
    );
}

#[derive(buck2_error_derive::Error, Debug)]
pub enum Error3 {
    #[error("foo")]
    #[buck2(user)]
    #[buck2(typ = DaemonIsBusy)]
    VariantA,
    #[error("bar")]
    #[buck2(infra)]
    VariantB,
    #[error("baz")]
    VariantC,
}

#[test]
fn test_derive_error3() {
    let e: crate::Error = Error3::VariantA.into();
    assert_eq!(e.get_category(), Some(crate::Category::User));
    assert_eq!(e.get_error_type(), Some(crate::ErrorType::DaemonIsBusy));

    let e: crate::Error = Error3::VariantB.into();
    assert_eq!(e.get_category(), Some(crate::Category::Infra));
    assert_eq!(e.get_error_type(), None);

    let e: crate::Error = Error3::VariantC.into();
    assert_eq!(e.get_category(), None);
    assert_eq!(e.get_error_type(), None);
}

#[derive(buck2_error_derive::Error, Debug)]
#[error("Generic error")]
pub struct GenericError<G>(G);

#[test]
fn test_generic_error() {
    let _e: crate::Error = GenericError(42).into();
}

/// Test that no unused fields warning is emitted.
#[derive(buck2_error_derive::Error, Debug)]
#[error("Unused")]
pub struct WithField {
    x: u8,
}

#[test]
fn test_with_field() {
    let _e: crate::Error = WithField { x: 42 }.into();
}

#[derive(buck2_error_derive::Error, Debug)]
#[error("Unused")]
struct Simple;

#[derive(buck2_error_derive::Error, Debug)]
#[error("Unused")]
struct UsesFrom(#[from] Simple);

#[test]
fn test_uses_from() {
    let e: UsesFrom = Simple.into();
    let _e: crate::Error = e.into();
}

#[derive(buck2_error_derive::Error, Debug)]
#[error("Unused")]
struct NoAttrsStruct;

#[derive(buck2_error_derive::Error, Debug)]
#[error("Unused")]
enum NoAttrsEnum {
    Variant,
}

#[test]
fn test_source_location_no_attrs() {
    let e: crate::Error = NoAttrsStruct.into();
    assert_eq!(
        e.source_location(),
        Some("buck2_error/src/derive_tests.rs::NoAttrsStruct")
    );
    let e: crate::Error = NoAttrsEnum::Variant.into();
    assert_eq!(
        e.source_location(),
        Some("buck2_error/src/derive_tests.rs::NoAttrsEnum::Variant")
    );
}
