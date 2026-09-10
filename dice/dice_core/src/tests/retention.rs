/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! What the state retains: certificates pinned by claims (`incrementality.md` §4.1), the
//! revisions they name, and what `take` keeps.

use crate::EpsilonToken;
use crate::arc::Arc;
use crate::tests::StateExt;
use crate::tests::cert;
use crate::tests::k;
use crate::tests::leaf_and_dependent;
use crate::tests::r;
use crate::tests::v;

#[test]
fn a_revision_is_referenced_while_some_claim_names_it() {
    let (mut s, c1) = leaf_and_dependent();
    let root = s.root();
    assert!(s.is_referenced(k(1), r(1)));
    s.assert_at(root, k(0), r(2));
    let c2 = cert(k(1), r(2), &[(k(0), r(2))], EpsilonToken::INITIAL);
    s.write_checked(&c2);
    assert!(s.is_referenced(k(1), r(2)));
    assert!(!s.is_referenced(k(1), r(1)));
    assert!(s.pinned_certs(k(1)).all(|c| Arc::ptr_eq(c, &c2)));
    assert!(!s.pinned_certs(k(1)).any(|c| Arc::ptr_eq(c, &c1)));
}

#[test]
fn a_child_that_still_resolves_a_replaced_claim_keeps_its_revision_referenced() {
    let (mut s, c1) = leaf_and_dependent();
    let root = s.root();
    let child = s.fork_checked(v(root, 2));
    s.assert_at(root, k(0), r(2));
    let c2 = cert(k(1), r(2), &[(k(0), r(2))], EpsilonToken::INITIAL);
    s.write_checked(&c2);
    assert!(s.is_referenced(k(1), r(1)));
    assert!(s.is_referenced(k(1), r(2)));
    assert_eq!(s.valid_at(k(1), v(child, 1)), Some(c1.revision));
}

#[test]
fn pinned_certificates_are_reported_per_claim() {
    let (mut s, c) = leaf_and_dependent();
    let root = s.root();
    let child = s.fork_checked(v(root, 2));
    // The child's commit gives it a closed copy of the same certificate.
    s.assert_at(child, k(0), r(2));
    assert_eq!(s.introspect_key(k(1)).slots.len(), 2);
    assert_eq!(s.pinned_certs(k(1)).count(), 2);
    assert!(s.pinned_certs(k(1)).all(|p| Arc::ptr_eq(p, &c)));
}

#[test]
fn take_forgets_claims_and_edges_but_keeps_histories() {
    let (mut s, c) = leaf_and_dependent();
    let root = s.root();
    let v3 = s.dirty_at(root, k(1));
    let eps = s.epsilon(k(1), v3);
    let taken = s.take_checked();
    assert_eq!(taken, v(root, 4));
    assert_eq!(s.head(root), taken);
    assert!(s.is_unknown_at(k(1), taken));
    assert!(s.is_unknown_at(k(1), v(root, 2)));
    assert!(s.candidate_at(k(1), taken).is_none());
    assert!(!s.is_referenced(k(1), c.revision));
    assert!(s.rdeps(root, k(0)).is_empty());
    assert_eq!(s.epsilon(k(1), taken), eps);
    assert_eq!(s.epsilon(k(1), v(root, 2)), EpsilonToken::INITIAL);
    assert_eq!(s.valid_at(k(0), taken), Some(r(1)));
    // The state works normally afterwards.
    let again = cert(k(1), r(1), &[(k(0), r(1))], eps);
    assert_eq!(s.write_checked(&again).installed, vec![root]);
    assert_eq!(s.valid_at(k(1), taken), Some(r(1)));
    assert!(s.is_unknown_at(k(1), v(root, 2)));
}
