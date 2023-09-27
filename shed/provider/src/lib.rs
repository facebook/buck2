/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![deny(missing_docs)]

//! `std::any::Provider` replacement (which is removed Rust stdlib).

use std::any::TypeId;
use std::marker::PhantomData;

trait DemandImpl {
    fn tag() -> TypeId;
}

struct DemandValue<T: 'static> {
    value: Option<T>,
}

struct DemandRef<'a, T: ?Sized + 'static> {
    value: Option<&'a T>,
}

impl<T: 'static> DemandImpl for DemandValue<T> {
    fn tag() -> TypeId {
        TypeId::of::<DemandValue<T>>()
    }
}

impl<'a, T: ?Sized + 'static> DemandImpl for DemandRef<'a, T> {
    fn tag() -> TypeId {
        TypeId::of::<DemandRef<'static, T>>()
    }
}

/// Supplied value can be passed using this object.
pub struct Demand<'a> {
    /// What we want?
    demand_impl_tag: TypeId,
    // SAFETY: Must be a `&'a mut` to a `DemandRef<T>` or `DemandValue<T>`, matching the `TypeId` above
    demand_impl: *mut (),
    _phantom: PhantomData<&'a mut ()>,
}

impl<'a> Demand<'a> {
    fn new<I>(demand_impl: &'a mut I) -> Demand<'a>
    where
        I: DemandImpl,
    {
        Demand {
            demand_impl_tag: I::tag(),
            demand_impl: demand_impl as *mut _ as *mut (),
            _phantom: PhantomData,
        }
    }

    /// Provide a value. Discard the value if the type is not matched.
    pub fn provide_value<T: 'static>(&mut self, value: T) {
        self.provide_value_with(|| value);
    }

    /// Provide a value. Discard the value if the type is not matched.
    pub fn provide_value_with<T: 'static>(&mut self, f: impl FnOnce() -> T) {
        if self.demand_impl_tag == DemandValue::<T>::tag() {
            // SAFETY: We've checked that this is a pointer to a `DemandValue<T>`
            let demand_impl = unsafe { &mut *(self.demand_impl as *mut DemandValue<T>) };
            demand_impl.value = Some(f());
        }
    }

    /// Provide a reference. Discard the value if the type is not matched.
    pub fn provide_ref<T: ?Sized + 'static>(&mut self, value: &'a T) {
        self.provide_ref_with(|| value);
    }

    /// Provide a reference. Discard the value if the type is not matched.
    pub fn provide_ref_with<T: ?Sized + 'static>(&mut self, f: impl FnOnce() -> &'a T) {
        if self.demand_impl_tag == DemandRef::<T>::tag() {
            // SAFETY: We've checked that this is a pointer to a `DemandRef<T>`
            let demand_impl = unsafe { &mut *(self.demand_impl as *mut DemandRef<T>) };
            demand_impl.value = Some(f());
        }
    }
}

/// Something that can provide a value of a given type.
pub trait Provider {
    /// Provide a value of a given type which is specified by the `demand` object.
    fn provide<'a>(&'a self, demand: &mut Demand<'a>);
}

/// Request a value from the provider.
pub fn request_value<'a, T: 'static>(provider: &'a (impl Provider + ?Sized)) -> Option<T> {
    let mut demand_impl = DemandValue { value: None };
    provider.provide(&mut Demand::new(&mut demand_impl));
    demand_impl.value
}

/// Request a reference from the provider.
pub fn request_ref<'a, T: ?Sized + 'static>(
    provider: &'a (impl Provider + ?Sized),
) -> Option<&'a T> {
    let mut demand_impl = DemandRef { value: None };
    provider.provide(&mut Demand::new(&mut demand_impl));
    demand_impl.value
}

#[cfg(test)]
mod tests {
    use crate::request_ref;
    use crate::request_value;
    use crate::Demand;
    use crate::Provider;

    struct My {
        s0: String,
        s1: Box<str>,
        x0: Vec<u32>,
        x1: Vec<u64>,
    }

    impl Provider for My {
        fn provide<'a>(&'a self, demand: &mut Demand<'a>) {
            demand.provide_value::<String>(self.s0.clone());
            demand.provide_value_with::<Box<str>>(|| self.s1.clone());
            demand.provide_ref::<[u32]>(&self.x0);
            demand.provide_ref_with::<[u64]>(|| &self.x1);
        }
    }

    #[test]
    fn test() {
        let my = My {
            s0: "s0".to_owned(),
            s1: "s1".into(),
            x0: vec![0, 1, 2],
            x1: vec![3, 4, 5],
        };

        let provider: &dyn Provider = &my;

        assert_eq!(Some("s0".to_owned()), request_value(provider));
        assert_eq!(Some(Box::<str>::from("s1")), request_value(provider));
        assert_eq!(Some(&[0, 1, 2][..]), request_ref::<[u32]>(provider));
        assert_eq!(Some(&[3, 4, 5][..]), request_ref::<[u64]>(provider));
        assert_eq!(None, request_value::<bool>(provider));
        assert_eq!(None, request_ref::<String>(provider));
    }
}
