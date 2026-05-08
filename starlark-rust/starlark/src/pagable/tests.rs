/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//! Round-trip serialize/deserialize tests for Starlark values.

use std::sync::Arc;

use allocative::Allocative;
use derive_more::Display;
use pagable::PagableDeserialize;
use pagable::PagableSerialize;
use starlark_derive::NoSerialize;
use starlark_derive::ProvidesStaticType;
use starlark_derive::StarlarkPagable;
use starlark_derive::starlark_value;
use starlark_syntax::codemap::CodeMap;
use starlark_syntax::codemap::FileSpan;
use starlark_syntax::codemap::NativeCodeMap;

use crate as starlark;
use crate::const_frozen_string;
use crate::starlark_simple_value;
use crate::values::FrozenHeap;
use crate::values::FrozenHeapRef;
use crate::values::FrozenValue;
use crate::values::OwnedFrozenValue;
use crate::values::OwnedFrozenValueTyped;
use crate::values::StarlarkValue;
use crate::values::ValueLike;
use crate::values::layout::heap::heap_type::FrozenHeapName;

/// Private test heap name for pagable tests.
#[derive(Debug, Hash)]
struct TestHeapName(String);

impl TestHeapName {
    fn heap_name(name: &str) -> FrozenHeapName {
        FrozenHeapName::User(Box::new(Self(name.to_owned())))
    }
}

/// A simple test type with primitive fields.
#[derive(
    Debug,
    Display,
    Allocative,
    ProvidesStaticType,
    NoSerialize,
    StarlarkPagable
)]
#[display("SimpleData({}, {})", self.flag, self.count)]
struct SimpleData {
    flag: bool,
    count: usize,
}

starlark_simple_value!(SimpleData);

#[starlark_value(type = "SimpleData", skip_pagable)]
impl<'v> StarlarkValue<'v> for SimpleData {
    type Canonical = Self;
}

fn round_trip_heap_ref(heap_ref: &FrozenHeapRef) -> crate::Result<FrozenHeapRef> {
    let mut ser = pagable::testing::TestingSerializer::new();
    heap_ref
        .pagable_serialize(&mut ser)
        .map_err(crate::Error::new_other)?;
    let bytes = ser.finish();

    let mut de = pagable::testing::TestingDeserializer::new(&bytes);
    let restored = FrozenHeapRef::pagable_deserialize(&mut de).map_err(crate::Error::new_other)?;
    Ok(restored)
}

/// A test type with rust heap-allocated fields (Vec, Box, String).
#[derive(
    Debug,
    Display,
    Allocative,
    ProvidesStaticType,
    NoSerialize,
    StarlarkPagable
)]
#[display("HeapData({:?}, {:?}, {:?})", self.items, self.label, self.boxed)]
struct HeapData {
    items: Vec<u32>,
    label: String,
    boxed: Box<i64>,
}

starlark_simple_value!(HeapData);

#[starlark_value(type = "HeapData", skip_pagable)]
impl<'v> StarlarkValue<'v> for HeapData {
    type Canonical = Self;
}

#[test]
fn test_simple_value_round_trip() -> crate::Result<()> {
    // 1. Create heap with a SimpleData value.
    let heap = FrozenHeap::new();
    heap.alloc_simple(SimpleData {
        flag: true,
        count: 42,
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_simple_value"));

    // 2. Round-trip via pagable serialize/deserialize.
    let restored = round_trip_heap_ref(&heap_ref)?;

    // 3. Verify: downcast to typed value and check fields.
    let headers = restored.collect_undrop_headers_ordered();
    assert_eq!(headers.len(), 1);
    let avalue = headers[0].unpack();
    let data: &SimpleData = avalue.downcast_ref().unwrap();
    assert_eq!(data.flag, true);
    assert_eq!(data.count, 42);

    Ok(())
}

#[test]
fn test_heap_allocated_value_round_trip() -> crate::Result<()> {
    // 1. Create heap with a HeapData value containing Vec, String, Box.
    let heap = FrozenHeap::new();
    heap.alloc_simple(HeapData {
        items: vec![10, 20, 30],
        label: "hello".to_owned(),
        boxed: Box::new(-99),
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_heap_data_round_trip"));

    // 2. Round-trip via pagable serialize/deserialize.
    let restored = round_trip_heap_ref(&heap_ref)?;

    // 3. Verify: downcast to typed value and check fields.
    // HeapData has Drop (Vec, String, Box), so it's in the drop bump.
    let headers = restored.collect_drop_headers_ordered();
    assert_eq!(headers.len(), 1);
    let avalue = headers[0].unpack();
    let data: &HeapData = avalue.downcast_ref().unwrap();
    assert_eq!(data.items, vec![10, 20, 30]);
    assert_eq!(data.label, "hello");
    assert_eq!(*data.boxed, -99);

    Ok(())
}

/// A test type with a FrozenValue field that references another value in the same heap.
#[derive(
    Debug,
    Display,
    Allocative,
    ProvidesStaticType,
    NoSerialize,
    StarlarkPagable
)]
#[display("RefData({})", self.label)]
struct RefData {
    label: usize,
    target: FrozenValue,
}

starlark_simple_value!(RefData);

#[starlark_value(type = "RefData", skip_pagable)]
impl<'v> StarlarkValue<'v> for RefData {
    type Canonical = Self;
}

#[test]
fn test_frozen_value_ref_round_trip() -> crate::Result<()> {
    // 1. Create heap with a SimpleData value, then a RefData pointing to it.
    let heap = FrozenHeap::new();
    let target_fv = heap.alloc_simple(SimpleData {
        flag: true,
        count: 99,
    });
    heap.alloc_simple(RefData {
        label: 7,
        target: target_fv,
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test"));

    // 2. Round-trip via pagable serialize/deserialize.
    let restored = round_trip_heap_ref(&heap_ref)?;

    // 3. Verify: both values are in the undrop bump (neither needs Drop).
    let headers = restored.collect_undrop_headers_ordered();
    assert_eq!(headers.len(), 2);

    // First value should be the SimpleData target.
    let target_data: &SimpleData = headers[0].unpack().downcast_ref().unwrap();
    assert_eq!(target_data.flag, true);
    assert_eq!(target_data.count, 99);

    // Second value should be the RefData with a FrozenValue pointing at the target.
    let ref_data: &RefData = headers[1].unpack().downcast_ref().unwrap();
    assert_eq!(ref_data.label, 7);

    // The FrozenValue in RefData should point to the restored SimpleData.
    let resolved: &SimpleData = ref_data
        .target
        .to_value()
        .downcast_ref::<SimpleData>()
        .expect("FrozenValue should point to SimpleData");
    assert_eq!(resolved.flag, true);
    assert_eq!(resolved.count, 99);

    // Verify pointer identity: the FrozenValue should point to the same header.
    let target_header_addr = headers[0] as *const _ as usize;
    let fv_ptr = ref_data.target.ptr_value().ptr_value_untagged();
    assert_eq!(fv_ptr, target_header_addr);

    Ok(())
}

/// A test type with Drop (due to Vec) that holds a FrozenValue reference.
#[derive(
    Debug,
    Display,
    Allocative,
    ProvidesStaticType,
    NoSerialize,
    StarlarkPagable
)]
#[display("DropRefData({:?})", self.items)]
struct DropRefData {
    items: Vec<u32>,
    target: FrozenValue,
}

starlark_simple_value!(DropRefData);

#[starlark_value(type = "DropRefData", skip_pagable)]
impl<'v> StarlarkValue<'v> for DropRefData {
    type Canonical = Self;
}

#[test]
fn test_frozen_value_drop_to_undrop_round_trip() -> crate::Result<()> {
    // DropRefData (drop bump) references SimpleData (undrop bump).
    let heap = FrozenHeap::new();
    let target_fv = heap.alloc_simple(SimpleData {
        flag: false,
        count: 123,
    });
    heap.alloc_simple(DropRefData {
        items: vec![1, 2, 3],
        target: target_fv,
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test"));

    let restored = round_trip_heap_ref(&heap_ref)?;

    // DropRefData is in the drop bump.
    let drop_headers = restored.collect_drop_headers_ordered();
    assert_eq!(drop_headers.len(), 1);
    let drop_ref_data: &DropRefData = drop_headers[0].unpack().downcast_ref().unwrap();
    assert_eq!(drop_ref_data.items, vec![1, 2, 3]);

    // SimpleData is in the undrop bump.
    let undrop_headers = restored.collect_undrop_headers_ordered();
    assert_eq!(undrop_headers.len(), 1);
    let target_data: &SimpleData = undrop_headers[0].unpack().downcast_ref().unwrap();
    assert_eq!(target_data.flag, false);
    assert_eq!(target_data.count, 123);

    // The FrozenValue in DropRefData should point to the restored SimpleData.
    let resolved: &SimpleData = drop_ref_data
        .target
        .to_value()
        .downcast_ref::<SimpleData>()
        .expect("FrozenValue should point to SimpleData");
    assert_eq!(resolved.flag, false);
    assert_eq!(resolved.count, 123);

    // Pointer identity check.
    let target_header_addr = undrop_headers[0] as *const _ as usize;
    let fv_ptr = drop_ref_data.target.ptr_value().ptr_value_untagged();
    assert_eq!(fv_ptr, target_header_addr);

    Ok(())
}

#[test]
fn test_frozen_value_undrop_to_drop_round_trip() -> crate::Result<()> {
    // RefData (undrop bump) references HeapData (drop bump).
    let heap = FrozenHeap::new();
    let target_fv = heap.alloc_simple(HeapData {
        items: vec![10, 20],
        label: "target".to_owned(),
        boxed: Box::new(42),
    });
    heap.alloc_simple(RefData {
        label: 5,
        target: target_fv,
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test"));

    let restored = round_trip_heap_ref(&heap_ref)?;

    // HeapData is in the drop bump.
    let drop_headers = restored.collect_drop_headers_ordered();
    assert_eq!(drop_headers.len(), 1);
    let target_data: &HeapData = drop_headers[0].unpack().downcast_ref().unwrap();
    assert_eq!(target_data.items, vec![10, 20]);
    assert_eq!(target_data.label, "target");
    assert_eq!(*target_data.boxed, 42);

    // RefData is in the undrop bump.
    let undrop_headers = restored.collect_undrop_headers_ordered();
    assert_eq!(undrop_headers.len(), 1);
    let ref_data: &RefData = undrop_headers[0].unpack().downcast_ref().unwrap();
    assert_eq!(ref_data.label, 5);

    // The FrozenValue in RefData should point to the restored HeapData.
    let resolved: &HeapData = ref_data
        .target
        .to_value()
        .downcast_ref::<HeapData>()
        .expect("FrozenValue should point to HeapData");
    assert_eq!(resolved.items, vec![10, 20]);
    assert_eq!(resolved.label, "target");
    assert_eq!(*resolved.boxed, 42);

    // Pointer identity check.
    let target_header_addr = drop_headers[0] as *const _ as usize;
    let fv_ptr = ref_data.target.ptr_value().ptr_value_untagged();
    assert_eq!(fv_ptr, target_header_addr);

    Ok(())
}

#[test]
fn test_frozen_list_round_trip() -> crate::Result<()> {
    use crate::values::list::value::ListGen;
    use crate::values::types::list::value::FrozenListData;

    // Create a heap with SimpleData values and a frozen list referencing them.
    let heap = FrozenHeap::new();
    let a = heap.alloc_simple(SimpleData {
        flag: true,
        count: 10,
    });
    let b = heap.alloc_simple(SimpleData {
        flag: false,
        count: 20,
    });
    heap.alloc_list(&[a, b]);
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test"));

    let restored = round_trip_heap_ref(&heap_ref)?;

    // All three values (2 SimpleData + 1 list) are in the undrop bump (no Drop).
    let undrop_headers = restored.collect_undrop_headers_ordered();
    assert_eq!(undrop_headers.len(), 3);

    let a_data: &SimpleData = undrop_headers[0].unpack().downcast_ref().unwrap();
    assert_eq!(a_data.flag, true);
    assert_eq!(a_data.count, 10);

    let b_data: &SimpleData = undrop_headers[1].unpack().downcast_ref().unwrap();
    assert_eq!(b_data.flag, false);
    assert_eq!(b_data.count, 20);

    // Third header is the list.
    let list_value: &ListGen<FrozenListData> = undrop_headers[2].unpack().downcast_ref().unwrap();
    let content = list_value.0.content();
    assert_eq!(content.len(), 2);

    // Verify list elements point to the restored SimpleData values.
    let elem_a: &SimpleData = content[0]
        .to_value()
        .downcast_ref::<SimpleData>()
        .expect("element 0 should be SimpleData");
    assert_eq!(elem_a.flag, true);
    assert_eq!(elem_a.count, 10);

    let elem_b: &SimpleData = content[1]
        .to_value()
        .downcast_ref::<SimpleData>()
        .expect("element 1 should be SimpleData");
    assert_eq!(elem_b.flag, false);
    assert_eq!(elem_b.count, 20);

    // Pointer identity: list elements should point to the same headers.
    assert_eq!(
        content[0].ptr_value().ptr_value_untagged(),
        undrop_headers[0] as *const _ as usize
    );
    assert_eq!(
        content[1].ptr_value().ptr_value_untagged(),
        undrop_headers[1] as *const _ as usize
    );

    Ok(())
}

#[test]
fn test_frozen_tuple_round_trip() -> crate::Result<()> {
    use crate::values::types::tuple::value::FrozenTuple;

    // Create a heap with SimpleData values and a frozen tuple referencing them.
    let heap = FrozenHeap::new();
    let a = heap.alloc_simple(SimpleData {
        flag: true,
        count: 100,
    });
    let b = heap.alloc_simple(SimpleData {
        flag: false,
        count: 200,
    });
    heap.alloc_tuple(&[a, b]);
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test"));

    let restored = round_trip_heap_ref(&heap_ref)?;

    // All three values (2 SimpleData + 1 tuple) are in the undrop bump.
    let undrop_headers = restored.collect_undrop_headers_ordered();
    assert_eq!(undrop_headers.len(), 3);

    let a_data: &SimpleData = undrop_headers[0].unpack().downcast_ref().unwrap();
    assert_eq!(a_data.flag, true);
    assert_eq!(a_data.count, 100);

    let b_data: &SimpleData = undrop_headers[1].unpack().downcast_ref().unwrap();
    assert_eq!(b_data.flag, false);
    assert_eq!(b_data.count, 200);

    // Third header is the tuple.
    let tuple_value: &FrozenTuple = undrop_headers[2].unpack().downcast_ref().unwrap();
    let content = tuple_value.content();
    assert_eq!(content.len(), 2);

    // Verify tuple elements point to the restored SimpleData values.
    let elem_a: &SimpleData = content[0]
        .to_value()
        .downcast_ref::<SimpleData>()
        .expect("element 0 should be SimpleData");
    assert_eq!(elem_a.flag, true);
    assert_eq!(elem_a.count, 100);

    let elem_b: &SimpleData = content[1]
        .to_value()
        .downcast_ref::<SimpleData>()
        .expect("element 1 should be SimpleData");
    assert_eq!(elem_b.flag, false);
    assert_eq!(elem_b.count, 200);

    // Pointer identity check.
    assert_eq!(
        content[0].ptr_value().ptr_value_untagged(),
        undrop_headers[0] as *const _ as usize
    );
    assert_eq!(
        content[1].ptr_value().ptr_value_untagged(),
        undrop_headers[1] as *const _ as usize
    );

    Ok(())
}

#[test]
fn test_frozen_str_value_round_trip() -> crate::Result<()> {
    use crate::values::string::str_type::StarlarkStr;

    // RefData (undrop) holds a FrozenValue pointing to a frozen string (also undrop).
    // Strings with len > 1 are heap-allocated with StrFrozen tag.
    let heap = FrozenHeap::new();
    let str_fv = heap.alloc_str("hello world").to_frozen_value();
    heap.alloc_simple(RefData {
        label: 42,
        target: str_fv,
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test"));

    let restored = round_trip_heap_ref(&heap_ref)?;

    // Both the string and RefData are in the undrop bump.
    let undrop_headers = restored.collect_undrop_headers_ordered();
    assert_eq!(undrop_headers.len(), 2);

    // First header is the string.
    let restored_str = undrop_headers[0]
        .unpack()
        .downcast_ref::<StarlarkStr>()
        .unwrap();
    assert_eq!(restored_str.as_str(), "hello world");

    // Second header is the RefData.
    let ref_data: &RefData = undrop_headers[1].unpack().downcast_ref().unwrap();
    assert_eq!(ref_data.label, 42);

    // The FrozenValue should be a string (is_str tag set).
    assert!(ref_data.target.is_str());
    assert_eq!(ref_data.target.unpack_str().unwrap(), "hello world");

    // Pointer identity check.
    assert_eq!(
        ref_data.target.ptr_value().ptr_value_untagged(),
        undrop_headers[0] as *const _ as usize
    );

    Ok(())
}

#[test]
fn test_frozen_value_inline_int_round_trip() -> crate::Result<()> {
    // RefData holds a FrozenValue that is an inline integer (not a heap pointer).
    let heap = FrozenHeap::new();
    let int_fv = FrozenValue::testing_new_int(42);
    heap.alloc_simple(RefData {
        label: 1,
        target: int_fv,
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test"));

    let restored = round_trip_heap_ref(&heap_ref)?;

    let undrop_headers = restored.collect_undrop_headers_ordered();
    assert_eq!(undrop_headers.len(), 1);

    let ref_data: &RefData = undrop_headers[0].unpack().downcast_ref().unwrap();
    assert_eq!(ref_data.label, 1);
    assert_eq!(ref_data.target.unpack_i32(), Some(42));

    Ok(())
}

#[test]
fn test_cross_heap_frozen_value_round_trip() -> crate::Result<()> {
    // Create a "dependency" heap with a SimpleData value.
    let dep_heap = FrozenHeap::new();
    let dep_fv = dep_heap.alloc_simple(SimpleData {
        flag: true,
        count: 77,
    });
    let dep_heap_ref = dep_heap.into_ref_named(TestHeapName::heap_name("dep"));

    // Create a "main" heap that references the dep heap.
    let main_heap = FrozenHeap::new();
    main_heap.add_reference(&dep_heap_ref);

    main_heap.alloc_simple(RefData {
        label: 99,
        target: dep_fv,
    });
    let main_heap_ref = main_heap.into_ref_named(TestHeapName::heap_name("main"));

    // Round-trip the main heap (which has a ref to dep heap).
    let restored = round_trip_heap_ref(&main_heap_ref)?;

    // The main heap should have 1 value (RefData) in undrop.
    let undrop_headers = restored.collect_undrop_headers_ordered();
    assert_eq!(undrop_headers.len(), 1);

    let ref_data: &RefData = undrop_headers[0].unpack().downcast_ref().unwrap();
    assert_eq!(ref_data.label, 99);

    // The FrozenValue should resolve to a SimpleData with the dep heap's data.
    let resolved: &SimpleData = ref_data
        .target
        .to_value()
        .downcast_ref::<SimpleData>()
        .expect("FrozenValue should point to SimpleData in dep heap");
    assert_eq!(resolved.flag, true);
    assert_eq!(resolved.count, 77);

    // The main heap should have 1 ref (the dep heap).
    let refs: Vec<_> = restored.refs().collect();
    assert_eq!(refs.len(), 1);

    // Pointer identity: the FrozenValue should point into the restored dep heap's first value.
    let restored_dep_headers = refs[0].collect_undrop_headers_ordered();
    assert_eq!(restored_dep_headers.len(), 1);
    assert_eq!(
        ref_data.target.ptr_value().ptr_value_untagged(),
        restored_dep_headers[0] as *const _ as usize
    );

    Ok(())
}

#[test]
fn test_heap_ref_dedup_round_trip() -> crate::Result<()> {
    // Diamond dependency: heap_a refs [heap_b, heap_c], both heap_b and heap_c ref heap_d. (A → [B, C] → D)
    // After round-trip, the deserialized heap_b and heap_c should share the same heap_d ref.

    // heap_d: shared dependency.
    let heap_d = FrozenHeap::new();
    let d_fv = heap_d.alloc_simple(SimpleData {
        flag: true,
        count: 1,
    });
    let heap_d_ref = heap_d.into_ref_named(TestHeapName::heap_name("d"));

    // heap_b: depends on heap_d, has a value referencing heap_d.
    let heap_b = FrozenHeap::new();
    heap_b.add_reference(&heap_d_ref);
    heap_b.alloc_simple(RefData {
        label: 2,
        target: d_fv,
    });
    let heap_b_ref = heap_b.into_ref_named(TestHeapName::heap_name("b"));

    // heap_c: also depends on heap_d, has a value referencing heap_d.
    let heap_c = FrozenHeap::new();
    heap_c.add_reference(&heap_d_ref);
    heap_c.alloc_simple(RefData {
        label: 3,
        target: d_fv,
    });
    let heap_c_ref = heap_c.into_ref_named(TestHeapName::heap_name("c"));

    // heap_a: depends on both heap_b and heap_c.
    let heap_a = FrozenHeap::new();
    heap_a.add_reference(&heap_b_ref);
    heap_a.add_reference(&heap_c_ref);
    heap_a.alloc_simple(SimpleData {
        flag: false,
        count: 4,
    });
    let heap_a_ref = heap_a.into_ref_named(TestHeapName::heap_name("a"));

    let restored = round_trip_heap_ref(&heap_a_ref)?;

    // heap_a has 2 refs: heap_b and heap_c.
    let a_refs: Vec<_> = restored.refs().collect();
    assert_eq!(a_refs.len(), 2);

    let restored_b = &a_refs[0];
    let restored_c = &a_refs[1];

    // Both heap_b and heap_c should have 1 ref each (heap_d).
    let b_refs: Vec<_> = restored_b.refs().collect();
    let c_refs: Vec<_> = restored_c.refs().collect();
    assert_eq!(b_refs.len(), 1);
    assert_eq!(c_refs.len(), 1);

    // The key dedup check: heap_b's ref to heap_d and heap_c's ref to heap_d
    // should be the same FrozenHeapRef (pointer-equal via Arc).
    assert_eq!(b_refs[0], c_refs[0]);

    // Verify the shared heap_d's value is correct.
    let d_headers = b_refs[0].collect_undrop_headers_ordered();
    assert_eq!(d_headers.len(), 1);
    let d_data: &SimpleData = d_headers[0].unpack().downcast_ref().unwrap();
    assert_eq!(d_data.flag, true);
    assert_eq!(d_data.count, 1);

    // Verify heap_b's and heap_c's values point into the shared heap_d.
    let b_headers = restored_b.collect_undrop_headers_ordered();
    assert_eq!(b_headers.len(), 1);
    let b_data: &RefData = b_headers[0].unpack().downcast_ref().unwrap();
    assert_eq!(b_data.label, 2);
    assert_eq!(
        b_data.target.ptr_value().ptr_value_untagged(),
        d_headers[0] as *const _ as usize
    );

    let c_headers = restored_c.collect_undrop_headers_ordered();
    assert_eq!(c_headers.len(), 1);
    let c_data: &RefData = c_headers[0].unpack().downcast_ref().unwrap();
    assert_eq!(c_data.label, 3);
    assert_eq!(
        c_data.target.ptr_value().ptr_value_untagged(),
        d_headers[0] as *const _ as usize
    );

    Ok(())
}

#[test]
fn test_frozen_value_typed_round_trip() -> crate::Result<()> {
    use crate::values::FrozenValueTyped;

    // Create a heap with a SimpleData, then wrap it as FrozenValueTyped.
    let heap = FrozenHeap::new();
    let fv = heap.alloc_simple(SimpleData {
        flag: true,
        count: 77,
    });
    let typed = FrozenValueTyped::<SimpleData>::new(fv).unwrap();

    // Create a RefData that holds the typed value as a plain FrozenValue.
    // This exercises FrozenValueTyped's StarlarkSerialize (which delegates to FrozenValue).
    heap.alloc_simple(RefData {
        label: 3,
        target: typed.to_frozen_value(),
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_fvt"));

    let restored = round_trip_heap_ref(&heap_ref)?;

    let undrop_headers = restored.collect_undrop_headers_ordered();
    assert_eq!(undrop_headers.len(), 2);

    // Verify the target FrozenValue can be downcast back to SimpleData via FrozenValueTyped.
    let ref_data: &RefData = undrop_headers[1].unpack().downcast_ref().unwrap();
    let restored_typed = FrozenValueTyped::<SimpleData>::new(ref_data.target).unwrap();
    assert_eq!(restored_typed.flag, true);
    assert_eq!(restored_typed.count, 77);

    Ok(())
}

/// A test type with SmallMap<String, FrozenValue> — has Drop (SmallMap), goes in drop bump.
#[derive(
    Debug,
    Display,
    Allocative,
    ProvidesStaticType,
    NoSerialize,
    StarlarkPagable
)]
#[display("SmallMapData")]
struct SmallMapData {
    entries: starlark_map::small_map::SmallMap<String, FrozenValue>,
}

starlark_simple_value!(SmallMapData);

#[starlark_value(type = "SmallMapData", skip_pagable)]
impl<'v> StarlarkValue<'v> for SmallMapData {
    type Canonical = Self;
}

/// A test type with SmallMap<FrozenValue, FrozenValue>.
#[derive(
    Debug,
    Display,
    Allocative,
    ProvidesStaticType,
    NoSerialize,
    StarlarkPagable
)]
#[display("SmallMapFvData")]
struct SmallMapFvData {
    entries: starlark_map::small_map::SmallMap<FrozenValue, FrozenValue>,
}

starlark_simple_value!(SmallMapFvData);

#[starlark_value(type = "SmallMapFvData", skip_pagable)]
impl<'v> StarlarkValue<'v> for SmallMapFvData {
    type Canonical = Self;
}

#[test]
fn test_small_map_string_key_round_trip() -> crate::Result<()> {
    use starlark_map::small_map::SmallMap;

    // SmallMap<String, FrozenValue> with values pointing to SimpleData (undrop bump).
    // SmallMapData is in drop bump.
    let heap = FrozenHeap::new();
    let v1 = heap.alloc_simple(SimpleData {
        flag: true,
        count: 10,
    });
    let v2 = heap.alloc_simple(SimpleData {
        flag: false,
        count: 20,
    });

    let mut entries = SmallMap::new();
    entries.insert("beta".to_owned(), v2);
    entries.insert("alpha".to_owned(), v1);

    heap.alloc_simple(SmallMapData { entries });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_sm_string"));

    let restored = round_trip_heap_ref(&heap_ref)?;

    let drop_headers = restored.collect_drop_headers_ordered();
    assert_eq!(drop_headers.len(), 1);
    let map_data: &SmallMapData = drop_headers[0].unpack().downcast_ref().unwrap();

    // Verify key order preserved.
    let keys: Vec<&str> = map_data.entries.iter().map(|(k, _)| k.as_str()).collect();
    assert_eq!(keys, vec!["beta", "alpha"]);

    // Verify values resolve correctly.
    let v1_restored: &SimpleData = map_data
        .entries
        .get("alpha")
        .unwrap()
        .downcast_ref::<SimpleData>()
        .unwrap();
    assert_eq!(v1_restored.count, 10);

    let v2_restored: &SimpleData = map_data
        .entries
        .get("beta")
        .unwrap()
        .downcast_ref::<SimpleData>()
        .unwrap();
    assert_eq!(v2_restored.count, 20);

    Ok(())
}

#[test]
fn test_small_map_frozen_value_key_backward_ref() -> crate::Result<()> {
    use starlark_map::small_map::SmallMap;

    use crate::values::ValueLike;

    // Backward reference: SmallMapFvData (drop bump) has FrozenValue keys
    // pointing to frozen strings (undrop bump) and values pointing to HeapData
    // (also drop bump). HeapData is allocated BEFORE SmallMapFvData, so during
    // deserialization it's already initialized when SmallMap is deserialized.
    // `ensure_initialized` sees it's already done and returns immediately.
    let heap = FrozenHeap::new();

    // Keys: frozen strings in undrop bump (hashable).
    let k1 = heap.alloc_str("key_one").to_frozen_value();
    let k2 = heap.alloc_str("key_two").to_frozen_value();

    // Values: HeapData in drop bump.
    let v1 = heap.alloc_simple(HeapData {
        items: vec![100],
        label: "val_one".to_owned(),
        boxed: Box::new(1),
    });
    let v2 = heap.alloc_simple(HeapData {
        items: vec![200],
        label: "val_two".to_owned(),
        boxed: Box::new(2),
    });

    let mut entries = SmallMap::new();
    entries.insert_hashed(k1.get_hashed()?, v1);
    entries.insert_hashed(k2.get_hashed()?, v2);

    heap.alloc_simple(SmallMapFvData { entries });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_sm_fv_key"));

    let restored = round_trip_heap_ref(&heap_ref)?;

    // Drop bump: HeapData v1, HeapData v2, SmallMapFvData.
    let drop_headers = restored.collect_drop_headers_ordered();
    assert_eq!(drop_headers.len(), 3);

    let map_data: &SmallMapFvData = drop_headers[2].unpack().downcast_ref().unwrap();
    assert_eq!(map_data.entries.len(), 2);

    // Verify key order: k1 first, k2 second.
    let key_strs: Vec<&str> = map_data
        .entries
        .iter()
        .map(|(k, _)| k.unpack_str().unwrap())
        .collect();
    assert_eq!(key_strs, vec!["key_one", "key_two"]);

    // Verify values.
    let val_labels: Vec<&str> = map_data
        .entries
        .iter()
        .map(|(_, v)| v.downcast_ref::<HeapData>().unwrap().label.as_str())
        .collect();
    assert_eq!(val_labels, vec!["val_one", "val_two"]);

    // Verify key lookup by hash works.
    // If ensure_initialized was missing, hashes would be computed on uninitialized
    // data and lookups would fail.
    for (k, _) in map_data.entries.iter() {
        let hashed = k.get_hashed()?;
        assert!(
            map_data.entries.get_hashed(hashed.as_ref()).is_some(),
            "lookup by key {:?} should succeed",
            k.unpack_str()
        );
    }

    Ok(())
}

#[test]
fn test_small_map_frozen_value_key_forward_ref() -> crate::Result<()> {
    use starlark_map::small_map::SmallMap;

    use crate::values::ValueLike;

    // Forward reference test: SmallMap is in drop bump (deserialized first),
    // its FrozenValue keys point to frozen strings in undrop bump (deserialized later).
    // During SmallMap deserialization, the string targets are NOT yet initialized.
    // `ensure_initialized` must seek forward to initialize them before `get_hashed()`.
    //
    // Serialization order: drop bump values first, then undrop bump values.
    // So SmallMap's data comes BEFORE the strings in the stream.
    let heap = FrozenHeap::new();

    // Allocate strings in undrop bump.
    let k1 = heap.alloc_str("hello").to_frozen_value();
    let k2 = heap.alloc_str("world").to_frozen_value();

    // Values: inline ints (no heap allocation needed).
    let v1 = FrozenValue::testing_new_int(111);
    let v2 = FrozenValue::testing_new_int(222);

    // SmallMap<FrozenValue, FrozenValue> goes in drop bump.
    let mut entries = SmallMap::new();
    entries.insert_hashed(k1.get_hashed()?, v1);
    entries.insert_hashed(k2.get_hashed()?, v2);
    heap.alloc_simple(SmallMapFvData { entries });

    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_forward_ref"));

    let restored = round_trip_heap_ref(&heap_ref)?;

    // Drop bump: SmallMapFvData.
    let drop_headers = restored.collect_drop_headers_ordered();
    assert_eq!(drop_headers.len(), 1);

    // Undrop bump: 2 frozen strings.
    let undrop_headers = restored.collect_undrop_headers_ordered();
    assert_eq!(undrop_headers.len(), 2);

    let map_data: &SmallMapFvData = drop_headers[0].unpack().downcast_ref().unwrap();
    assert_eq!(map_data.entries.len(), 2);

    // Verify keys are correctly deserialized strings (were forward references).
    let key_strs: Vec<&str> = map_data
        .entries
        .iter()
        .map(|(k, _)| k.unpack_str().unwrap())
        .collect();
    assert_eq!(key_strs, vec!["hello", "world"]);

    // Verify values are inline ints.
    let values: Vec<i32> = map_data
        .entries
        .iter()
        .map(|(_, v)| v.unpack_i32().unwrap())
        .collect();
    assert_eq!(values, vec![111, 222]);

    // Verify key lookup by hash works. This catches corrupted hashes from
    // missing ensure_initialized — the stored hash would have been computed on
    // uninitialized string data, while get_hashed() now computes from initialized
    // data, causing a mismatch.
    for (k, _) in map_data.entries.iter() {
        let hashed = k.get_hashed()?;
        assert!(
            map_data.entries.get_hashed(hashed.as_ref()).is_some(),
            "lookup by key {:?} should succeed",
            k.unpack_str()
        );
    }

    // Verify key pointers point to the restored strings in undrop bump.
    let key_ptrs: Vec<usize> = map_data
        .entries
        .iter()
        .map(|(k, _)| k.ptr_value().ptr_value_untagged())
        .collect();
    assert_eq!(key_ptrs[0], undrop_headers[0] as *const _ as usize);
    assert_eq!(key_ptrs[1], undrop_headers[1] as *const _ as usize);

    Ok(())
}

#[test]
fn test_range_round_trip() -> crate::Result<()> {
    use std::num::NonZeroI32;

    use crate::values::types::range::Range;

    // Create a heap with a Range value.
    let heap = FrozenHeap::new();
    heap.alloc_simple(Range::new(1, 10, NonZeroI32::new(2).unwrap()));
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_range"));

    let restored = round_trip_heap_ref(&heap_ref)?;

    // Range is Copy (no Drop), so it's in the undrop bump.
    let undrop_headers = restored.collect_undrop_headers_ordered();
    assert_eq!(undrop_headers.len(), 1);

    let range: &Range = undrop_headers[0].unpack().downcast_ref().unwrap();
    // Verify by Display output: range(1, 10, 2)
    assert_eq!(format!("{}", range), "range(1, 10, 2)");

    Ok(())
}

#[test]
fn test_owned_frozen_value_round_trip() -> crate::Result<()> {
    // Create a heap with a SimpleData value.
    let heap = FrozenHeap::new();
    let fv = heap.alloc_simple(SimpleData {
        flag: true,
        count: 42,
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_owned"));

    // Create an OwnedFrozenValue.
    let owned = unsafe { OwnedFrozenValue::new(heap_ref, fv) };

    // Round-trip via pagable ser/de.
    let mut ser = pagable::testing::TestingSerializer::new();
    owned
        .pagable_serialize(&mut ser)
        .map_err(crate::Error::new_other)?;
    let bytes = ser.finish();

    let mut de = pagable::testing::TestingDeserializer::new(&bytes);
    let restored =
        OwnedFrozenValue::pagable_deserialize(&mut de).map_err(crate::Error::new_other)?;

    // Verify the restored value.
    let simple: &SimpleData = restored
        .value()
        .downcast_ref()
        .expect("should be SimpleData");
    assert_eq!(simple.flag, true);
    assert_eq!(simple.count, 42);

    Ok(())
}

#[test]
fn test_owned_frozen_value_typed_round_trip() -> crate::Result<()> {
    // Create a heap with a SimpleData value.
    let heap = FrozenHeap::new();
    let fv = heap.alloc_simple(SimpleData {
        flag: false,
        count: 99,
    });
    let typed_fv =
        crate::values::FrozenValueTyped::<SimpleData>::new(fv).expect("should be SimpleData");
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_owned_typed"));

    // Create an OwnedFrozenValueTyped.
    let owned = unsafe { OwnedFrozenValueTyped::new(heap_ref, typed_fv) };

    // Round-trip via pagable ser/de.
    let mut ser = pagable::testing::TestingSerializer::new();
    owned
        .pagable_serialize(&mut ser)
        .map_err(crate::Error::new_other)?;
    let bytes = ser.finish();

    let mut de = pagable::testing::TestingDeserializer::new(&bytes);
    let restored = OwnedFrozenValueTyped::<SimpleData>::pagable_deserialize(&mut de)
        .map_err(crate::Error::new_other)?;

    // Verify the restored value via Deref (OwnedFrozenValueTyped<T> derefs to T).
    assert_eq!(restored.flag, false);
    assert_eq!(restored.count, 99);

    Ok(())
}

/// Test type mirroring StackFrame: String + Option<FileSpan>.
#[derive(
    Debug,
    Display,
    Allocative,
    ProvidesStaticType,
    NoSerialize,
    StarlarkPagable
)]
#[display("TestStackFrame({}, {:?})", self.name, self.location)]
struct TestStackFrame {
    name: String,
    #[starlark_pagable(pagable)]
    location: Option<FileSpan>,
}

starlark_simple_value!(TestStackFrame);

#[starlark_value(type = "TestStackFrame", skip_pagable)]
impl<'v> StarlarkValue<'v> for TestStackFrame {
    type Canonical = Self;
}

#[test]
fn test_stack_frame_data_round_trip() -> crate::Result<()> {
    let heap = FrozenHeap::new();

    // With location = None.
    heap.alloc_simple(TestStackFrame {
        name: "native_func".to_owned(),
        location: None,
    });

    // With location = Some(FileSpan).
    let codemap = CodeMap::new(
        "test.bzl".to_owned(),
        "load('foo')\ndef bar():\n  pass\n".to_owned(),
    );
    let span = codemap.full_span();
    heap.alloc_simple(TestStackFrame {
        name: "bar".to_owned(),
        location: Some(FileSpan {
            file: codemap,
            span,
        }),
    });

    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_stack_frame_data"));
    let restored = round_trip_heap_ref(&heap_ref)?;

    let headers = restored.collect_drop_headers_ordered();
    assert_eq!(headers.len(), 2);

    // First value: location = None.
    let data0: &TestStackFrame = headers[0].unpack().downcast_ref().unwrap();
    assert_eq!(data0.name, "native_func");
    assert!(data0.location.is_none());

    // Second value: location = Some, with filename and span preserved.
    let data1: &TestStackFrame = headers[1].unpack().downcast_ref().unwrap();
    assert_eq!(data1.name, "bar");
    let loc = data1.location.as_ref().expect("should have location");
    assert_eq!(loc.file.filename(), "test.bzl");
    assert_eq!(loc.file.source(), "load('foo')\ndef bar():\n  pass\n");

    Ok(())
}

#[test]
fn test_native_codemap_round_trip() -> crate::Result<()> {
    // Register a static NativeCodeMap via the pagable static value framework.
    static NATIVE: NativeCodeMap = NativeCodeMap::new("test_native.rs", 42, 10);
    pagable::static_value!(
        NATIVE_STATIC: NativeCodeMap = &NATIVE,
        starlark_syntax::codemap::NativeCodeMapStaticEntry
    );

    let codemap = NativeCodeMap::to_codemap(NATIVE_STATIC);
    let file_span = FileSpan {
        file: codemap,
        span: NativeCodeMap::FULL_SPAN,
    };

    let heap = FrozenHeap::new();
    heap.alloc_simple(TestStackFrame {
        name: "native_call".to_owned(),
        location: Some(file_span),
    });

    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_native_codemap"));
    let restored = round_trip_heap_ref(&heap_ref)?;

    let headers = restored.collect_drop_headers_ordered();
    assert_eq!(headers.len(), 1);
    let data: &TestStackFrame = headers[0].unpack().downcast_ref().unwrap();
    assert_eq!(data.name, "native_call");
    let loc = data.location.as_ref().expect("should have location");
    // Verify the NativeCodeMap round-tripped: filename and source preserved.
    assert_eq!(loc.file.filename(), "test_native.rs");
    assert_eq!(loc.file.source(), "<native>");
    // Verify identity: it should be the same static NativeCodeMap,
    // so CodeMap::id() should match.
    assert_eq!(loc.file.id(), NativeCodeMap::to_codemap(NATIVE_STATIC).id());

    Ok(())
}

#[test]
fn test_frozen_dict_round_trip() -> crate::Result<()> {
    use crate::values::dict::AllocDict;
    use crate::values::types::dict::value::FrozenDict;

    let heap = FrozenHeap::new();

    // Dict with entries: {"hello": 1, "world": 2}.
    heap.alloc(AllocDict([("hello", 1), ("world", 2)]));

    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_frozen_dict"));
    let restored = round_trip_heap_ref(&heap_ref)?;

    // Dict has Drop (SmallMap), so it's in the drop bump.
    let headers = restored.collect_drop_headers_ordered();
    assert_eq!(headers.len(), 1);
    let dict: &FrozenDict = headers[0].unpack().downcast_ref().unwrap();
    assert_eq!(dict.0.content.len(), 2);
    // Verify keys and values are present.
    let keys: Vec<&str> = dict
        .0
        .content
        .keys()
        .map(|k| k.to_value().unpack_str().unwrap())
        .collect();
    assert!(keys.contains(&"hello"));
    assert!(keys.contains(&"world"));

    Ok(())
}

#[test]
fn test_frozen_struct_round_trip() -> crate::Result<()> {
    use crate::values::structs::AllocStruct;

    let heap = FrozenHeap::new();
    heap.alloc(AllocStruct([("name", "alice"), ("age", "30")]));

    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_frozen_struct"));
    let restored = round_trip_heap_ref(&heap_ref)?;

    // Struct has Drop (SmallMap), so it's in the drop bump.
    let headers = restored.collect_drop_headers_ordered();
    assert_eq!(headers.len(), 1);
    let attrs = headers[0].unpack().dir_attr();
    assert_eq!(attrs.len(), 2);
    assert!(attrs.contains(&"name".to_owned()));
    assert!(attrs.contains(&"age".to_owned()));

    Ok(())
}

#[test]
fn test_frozen_set_round_trip() -> crate::Result<()> {
    use starlark_map::small_set::SmallSet;

    use crate::values::types::set::value::FrozenSet;
    use crate::values::types::set::value::FrozenSetData;
    use crate::values::types::set::value::SetGen;

    let heap = FrozenHeap::new();

    // Build a set with values {1, 2, 3}.
    let mut content: SmallSet<FrozenValue> = SmallSet::new();
    content.insert_hashed(FrozenValue::testing_new_int(1).get_hashed()?);
    content.insert_hashed(FrozenValue::testing_new_int(2).get_hashed()?);
    content.insert_hashed(FrozenValue::testing_new_int(3).get_hashed()?);
    heap.alloc_simple(SetGen(FrozenSetData::new(content)));

    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_frozen_set"));
    let restored = round_trip_heap_ref(&heap_ref)?;

    // Set has Drop (SmallSet), so it's in the drop bump.
    let headers = restored.collect_drop_headers_ordered();
    assert_eq!(headers.len(), 1);
    let set: &FrozenSet = headers[0].unpack().downcast_ref().unwrap();
    assert_eq!(set.0.len(), 3);
    // Verify values are present.
    let values: Vec<i32> = set.0.iter().map(|v| v.unpack_i32().unwrap()).collect();
    assert!(values.contains(&1));
    assert!(values.contains(&2));
    assert!(values.contains(&3));

    Ok(())
}

#[test]
fn test_frozen_record_type_round_trip() -> crate::Result<()> {
    use std::sync::Arc;

    use starlark_map::small_map::SmallMap;

    use crate::eval::ParametersSpec;
    use crate::eval::ParametersSpecParam;
    use crate::typing::Ty;
    use crate::values::record::field::FieldGen;
    use crate::values::record::record_type::FrozenRecordType;
    use crate::values::record::ty_record_type::TyRecordData;
    use crate::values::types::type_instance_id::TypeInstanceId;
    use crate::values::typing::type_compiled::compiled::TypeCompiled;

    let heap = FrozenHeap::new();

    // Build a single Arc<TyRecordData> shared by two FrozenRecordType
    // allocations — this exercises Arc identity preservation through pagable's
    // dedup mechanism (TyRecordData routes through `#[starlark_pagable(pagable)]`).
    let shared = Arc::new(TyRecordData {
        name: "MyRec".to_owned(),
        ty_record: Ty::any(),
        ty_record_type: Ty::any(),
        parameter_spec: ParametersSpec::<FrozenValue>::new_named_only(
            "MyRec",
            [
                ("x", ParametersSpecParam::Required),
                ("y", ParametersSpecParam::Required),
            ],
        ),
    });

    let make_fields = || {
        let mut fields: SmallMap<String, FieldGen<FrozenValue>> = SmallMap::new();
        fields.insert(
            "x".to_owned(),
            FieldGen {
                typ: TypeCompiled::any(),
                default: None,
            },
        );
        fields.insert(
            "y".to_owned(),
            FieldGen {
                typ: TypeCompiled::any(),
                default: None,
            },
        );
        fields
    };

    let id_a = TypeInstanceId::r#gen();
    let id_b = TypeInstanceId::r#gen();
    heap.alloc_simple(FrozenRecordType {
        id: id_a,
        ty_record_data: Some(shared.clone()),
        fields: make_fields(),
    });
    heap.alloc_simple(FrozenRecordType {
        id: id_b,
        ty_record_data: Some(shared),
        fields: make_fields(),
    });

    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_frozen_record_type"));
    let restored = round_trip_heap_ref(&heap_ref)?;

    // RecordTypeGen has Drop (SmallMap + Arc), so it's in the drop bump.
    let headers = restored.collect_drop_headers_ordered();
    assert_eq!(headers.len(), 2);
    let rt_a: &FrozenRecordType = headers[0].unpack().downcast_ref().unwrap();
    let rt_b: &FrozenRecordType = headers[1].unpack().downcast_ref().unwrap();

    // Per-record state round-trips independently.
    assert_eq!(rt_a.id, id_a);
    assert_eq!(rt_b.id, id_b);
    for rt in [rt_a, rt_b] {
        let field_names: Vec<&str> = rt.fields.keys().map(String::as_str).collect();
        assert_eq!(field_names, vec!["x", "y"]);
        for (_, field) in rt.fields.iter() {
            assert!(field.default.is_none());
        }
    }

    // ty_record_data contents survive the round-trip, including the inline
    // parameter_spec (which is now serialized through TyRecordData's
    // StarlarkPagable derive rather than being rebuilt on deserialize).
    let data_a = rt_a
        .ty_record_data
        .as_ref()
        .expect("ty_record_data restored");
    let data_b = rt_b
        .ty_record_data
        .as_ref()
        .expect("ty_record_data restored");
    assert_eq!(data_a.name, "MyRec");
    assert_eq!(data_a.ty_record, Ty::any());
    assert_eq!(data_a.ty_record_type, Ty::any());
    assert_eq!(data_a.parameter_spec.len(), 2);

    // Arc identity preservation: both restored RecordTypeGen point at the
    // same Arc<TyRecordData> allocation, just like the original.
    assert!(
        Arc::ptr_eq(data_a, data_b),
        "pagable Arc dedup should round-trip the shared Arc<TyRecordData> as a single allocation",
    );

    Ok(())
}

#[test]
fn test_static_frozen_value_round_trip() -> crate::Result<()> {
    // Test that FrozenValues pointing to static values (not on any heap)
    // survive a round-trip through pagable serialization.
    //
    // Static values include: None, True, False, empty tuple, static strings
    // (const_frozen_string!), and other inventory-registered statics.
    let heap = FrozenHeap::new();

    // Create RefData values that hold various static FrozenValues.
    let none_fv = FrozenValue::new_none();
    let true_fv = FrozenValue::new_bool(true);
    let false_fv = FrozenValue::new_bool(false);
    let empty_tuple_fv = FrozenValue::new_empty_tuple();
    let static_str_fv = const_frozen_string!("static_test_str").to_frozen_value();

    heap.alloc_simple(RefData {
        label: 1,
        target: none_fv,
    });
    heap.alloc_simple(RefData {
        label: 2,
        target: true_fv,
    });
    heap.alloc_simple(RefData {
        label: 3,
        target: false_fv,
    });
    heap.alloc_simple(RefData {
        label: 4,
        target: empty_tuple_fv,
    });
    heap.alloc_simple(RefData {
        label: 5,
        target: static_str_fv,
    });

    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_static"));

    // Round-trip.
    let restored = round_trip_heap_ref(&heap_ref)?;

    let undrop_headers = restored.collect_undrop_headers_ordered();
    assert_eq!(undrop_headers.len(), 5);

    // Verify None.
    let ref0: &RefData = undrop_headers[0].unpack().downcast_ref().unwrap();
    assert_eq!(ref0.label, 1);
    assert!(ref0.target.is_none());
    // Static values should preserve pointer identity (same static address).
    assert_eq!(
        ref0.target.ptr_value().ptr_value_untagged(),
        none_fv.ptr_value().ptr_value_untagged(),
        "None should point to the same static address"
    );

    // Verify True.
    let ref1: &RefData = undrop_headers[1].unpack().downcast_ref().unwrap();
    assert_eq!(ref1.label, 2);
    assert_eq!(ref1.target.unpack_bool(), Some(true));
    assert_eq!(
        ref1.target.ptr_value().ptr_value_untagged(),
        true_fv.ptr_value().ptr_value_untagged(),
        "True should point to the same static address"
    );

    // Verify False.
    let ref2: &RefData = undrop_headers[2].unpack().downcast_ref().unwrap();
    assert_eq!(ref2.label, 3);
    assert_eq!(ref2.target.unpack_bool(), Some(false));
    assert_eq!(
        ref2.target.ptr_value().ptr_value_untagged(),
        false_fv.ptr_value().ptr_value_untagged(),
        "False should point to the same static address"
    );

    // Verify empty tuple.
    let ref3: &RefData = undrop_headers[3].unpack().downcast_ref().unwrap();
    assert_eq!(ref3.label, 4);
    assert_eq!(
        ref3.target.ptr_value().ptr_value_untagged(),
        empty_tuple_fv.ptr_value().ptr_value_untagged(),
        "Empty tuple should point to the same static address"
    );

    // Verify static string.
    let ref4: &RefData = undrop_headers[4].unpack().downcast_ref().unwrap();
    assert_eq!(ref4.label, 5);
    assert_eq!(ref4.target.unpack_str(), Some("static_test_str"));
    assert_eq!(
        ref4.target.ptr_value().ptr_value_untagged(),
        static_str_fv.ptr_value().ptr_value_untagged(),
        "Static string should point to the same static address"
    );

    Ok(())
}

// ============================================================================
// `StarlarkSerializerImpl::recover_from_pagable` /
// `StarlarkDeserializerImpl::recover_from_pagable`:
// pagable Arc<T> dedup combined with starlark FrozenValue resolution.
//
// `InnerArcData` is reachable from the starlark layer via a derived
// `StarlarkPagable` (so its `target: FrozenValue` is resolved against the
// currently-deserializing heap), but it is also wrapped in `Arc<InnerArcData>`
// inside `OuterArcValue` and routed through `pagable::PagableSerialize` (with
// `#[starlark_pagable(pagable)]`) so the pagable Arc-identity dedup mechanism
// can fire — two `OuterArcValue`s sharing the same `Arc<InnerArcData>`
// serialize the body once and round-trip to pointer-equal Arcs.
//
// To bridge between the two layers, `InnerArcData::pagable_serialize` /
// `pagable_deserialize` use `StarlarkSerializerImpl::recover_from_pagable`
// and `StarlarkDeserializerImpl::recover_from_pagable` to recover the
// starlark heap context inside what is otherwise a pure-pagable
// (typetag/Arc) dispatch path.
// ============================================================================

/// Inner type carried inside an `Arc`. Holds a `FrozenValue` that must be
/// resolved against the currently-(de)serializing heap. Implements:
///   - `StarlarkPagable` via derive (so the `FrozenValue` field works).
///   - `pagable::Pagable*` manually, bridging into the starlark context via
///     `with_starlark_*_context` so the `Arc<InnerArcData>` field on
///     `OuterArcValue` can be routed through pagable's Arc-dedup mechanism
///     (which serializes the body using `pagable::PagableSerialize`, not
///     `StarlarkSerialize`).
#[derive(Debug, Allocative, ProvidesStaticType, StarlarkPagable)]
struct InnerArcData {
    target: FrozenValue,
    label: u32,
}

impl pagable::PagableSerialize for InnerArcData {
    fn pagable_serialize(
        &self,
        serializer: &mut dyn pagable::PagableSerializer,
    ) -> pagable::Result<()> {
        let mut ctx = crate::pagable::StarlarkSerializerImpl::recover_from_pagable(serializer)
            .map_err(|e: crate::Error| e.into_anyhow())?;
        <Self as crate::pagable::StarlarkSerialize>::starlark_serialize(self, &mut ctx)
            .map_err(|e: crate::Error| e.into_anyhow())
    }
}

impl<'de> pagable::PagableDeserialize<'de> for InnerArcData {
    fn pagable_deserialize<D: pagable::PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> pagable::Result<Self> {
        let mut ctx =
            crate::pagable::StarlarkDeserializerImpl::recover_from_pagable(deserializer.as_dyn())
                .map_err(|e: crate::Error| e.into_anyhow())?;
        <Self as crate::pagable::StarlarkDeserialize>::starlark_deserialize(&mut ctx)
            .map_err(|e: crate::Error| e.into_anyhow())
    }
}

/// Outer `StarlarkValue` holding an `Arc<InnerArcData>` plus a `Vec<u32>`.
/// The `Vec` puts it firmly on the **drop bump**, which is serialized
/// *first* on the wire (before the non-drop bump). The inner
/// `target: FrozenValue` then points into the non-drop bump (`SimpleData`),
/// which is serialized *second*. So when the deserializer is processing
/// an `OuterArcValue` body, the inner `FrozenValue` is a forward
/// reference into a not-yet-deserialized non-drop slot — exercising the
/// `ensure_initialized` work-queue path through the shared
/// `HeapDeserializationState` that `StarlarkDeserializerImpl::recover_from_pagable`
/// hands off via the session context.
///
/// The `Arc<InnerArcData>` field is routed through `pagable::PagableSerialize`
/// (via `#[starlark_pagable(pagable)]`) so multiple `OuterArcValue`s sharing
/// the same Arc dedup on the wire and round-trip to a pointer-equal Arc.
#[derive(
    Debug,
    Display,
    Allocative,
    ProvidesStaticType,
    NoSerialize,
    StarlarkPagable
)]
#[display("OuterArcValue({})", self.outer_label)]
struct OuterArcValue {
    #[starlark_pagable(pagable)]
    inner: Arc<InnerArcData>,
    outer_label: u32,
    items: Vec<u32>,
}

starlark_simple_value!(OuterArcValue);

#[starlark_value(type = "OuterArcValue", skip_pagable)]
impl<'v> StarlarkValue<'v> for OuterArcValue {
    type Canonical = Self;
}

#[test]
fn test_with_starlark_context_arc_dedup_round_trip() -> crate::Result<()> {
    let heap = FrozenHeap::new();
    let target_fv = heap.alloc_simple(SimpleData {
        flag: true,
        count: 314,
    });

    // Build a single Arc shared by the two OuterArcValues.
    let shared = Arc::new(InnerArcData {
        target: target_fv,
        label: 99,
    });

    heap.alloc_simple(OuterArcValue {
        inner: shared.clone(),
        outer_label: 1,
        items: vec![10, 20, 30],
    });
    heap.alloc_simple(OuterArcValue {
        inner: shared,
        outer_label: 2,
        items: vec![40, 50],
    });

    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_with_starlark_context_arc"));
    let restored = round_trip_heap_ref(&heap_ref)?;

    // SimpleData has no Drop, so it lives in the undrop bump.
    let undrop = restored.collect_undrop_headers_ordered();
    assert_eq!(undrop.len(), 1);
    let target_data: &SimpleData = undrop[0].unpack().downcast_ref().unwrap();
    assert_eq!(target_data.flag, true);
    assert_eq!(target_data.count, 314);

    // OuterArcValue has `Vec<u32>` (and `Arc`), so it lives in the drop bump.
    let drop_headers = restored.collect_drop_headers_ordered();
    assert_eq!(drop_headers.len(), 2);
    let outer_a: &OuterArcValue = drop_headers[0].unpack().downcast_ref().unwrap();
    let outer_b: &OuterArcValue = drop_headers[1].unpack().downcast_ref().unwrap();
    assert_eq!(outer_a.outer_label, 1);
    assert_eq!(outer_b.outer_label, 2);
    assert_eq!(outer_a.items, vec![10, 20, 30]);
    assert_eq!(outer_b.items, vec![40, 50]);

    // Inner data round-tripped correctly: both Arcs see the same label and a
    // FrozenValue resolving to the SimpleData target above.
    assert_eq!(outer_a.inner.label, 99);
    assert_eq!(outer_b.inner.label, 99);
    let target_addr = undrop[0] as *const _ as usize;
    assert_eq!(
        outer_a.inner.target.ptr_value().ptr_value_untagged(),
        target_addr,
    );
    assert_eq!(
        outer_b.inner.target.ptr_value().ptr_value_untagged(),
        target_addr,
    );

    // Arc dedup: the two restored Arcs must point at the same allocation.
    assert!(
        Arc::ptr_eq(&outer_a.inner, &outer_b.inner),
        "pagable Arc dedup should round-trip the shared Arc as a single allocation",
    );

    Ok(())
}

// ============================================================================
// Arc<T> blanket: starlark-only T, plain `Arc<T>` field — round-trips through
// the `Arc<T>: StarlarkSerialize` blanket, no manual bridge.
// ============================================================================

/// Inner type carries inside an `Arc` and h a `FrozenValue`
#[derive(Debug, Allocative, ProvidesStaticType, StarlarkPagable)]
struct ArcBlanketInner {
    target: FrozenValue,
    label: u32,
}

/// Outer `StarlarkValue` with a plain `Arc<ArcBlanketInner>` field
#[derive(
    Debug,
    Display,
    Allocative,
    ProvidesStaticType,
    NoSerialize,
    StarlarkPagable
)]
#[display("ArcBlanketOuter({})", self.outer_label)]
struct ArcBlanketOuter {
    inner: Arc<ArcBlanketInner>,
    outer_label: u32,
    items: Vec<u32>,
}

starlark_simple_value!(ArcBlanketOuter);

#[starlark_value(type = "ArcBlanketOuter", skip_pagable)]
impl<'v> StarlarkValue<'v> for ArcBlanketOuter {
    type Canonical = Self;
}

#[test]
fn test_arc_blanket_round_trip() -> crate::Result<()> {
    let heap = FrozenHeap::new();
    let target_fv = heap.alloc_simple(SimpleData {
        flag: true,
        count: 271,
    });

    let shared = Arc::new(ArcBlanketInner {
        target: target_fv,
        label: 7,
    });

    heap.alloc_simple(ArcBlanketOuter {
        inner: shared.clone(),
        outer_label: 1,
        items: vec![1, 2, 3],
    });
    heap.alloc_simple(ArcBlanketOuter {
        inner: shared,
        outer_label: 2,
        items: vec![4, 5],
    });

    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_arc_blanket"));
    let restored = round_trip_heap_ref(&heap_ref)?;

    let undrop = restored.collect_undrop_headers_ordered();
    assert_eq!(undrop.len(), 1);
    let target_data: &SimpleData = undrop[0].unpack().downcast_ref().unwrap();
    assert_eq!(target_data.flag, true);
    assert_eq!(target_data.count, 271);

    let drop_headers = restored.collect_drop_headers_ordered();
    assert_eq!(drop_headers.len(), 2);
    let outer_a: &ArcBlanketOuter = drop_headers[0].unpack().downcast_ref().unwrap();
    let outer_b: &ArcBlanketOuter = drop_headers[1].unpack().downcast_ref().unwrap();
    assert_eq!(outer_a.outer_label, 1);
    assert_eq!(outer_b.outer_label, 2);
    assert_eq!(outer_a.items, vec![1, 2, 3]);
    assert_eq!(outer_b.items, vec![4, 5]);

    // Inner data round-tripped, including the FrozenValue resolved against
    // the same heap.
    assert_eq!(outer_a.inner.label, 7);
    assert_eq!(outer_b.inner.label, 7);
    let target_addr = undrop[0] as *const _ as usize;
    assert_eq!(
        outer_a.inner.target.ptr_value().ptr_value_untagged(),
        target_addr,
    );
    assert_eq!(
        outer_b.inner.target.ptr_value().ptr_value_untagged(),
        target_addr,
    );

    // Arc dedup via the blanket: both restored Arcs must be pointer-equal.
    assert!(
        Arc::ptr_eq(&outer_a.inner, &outer_b.inner),
        "Arc<T> blanket should preserve Arc identity across round-trip",
    );

    Ok(())
}
