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
use strong_hash::StrongHash;

use crate as starlark;
use crate::const_frozen_string;
use crate::environment::GlobalFrozenHeapName;
use crate::environment::GlobalsBuilder;
use crate::starlark_simple_value;
use crate::values::FrozenHeap;
use crate::values::FrozenHeapRef;
use crate::values::FrozenValue;
use crate::values::OwnedFrozenValue;
use crate::values::OwnedFrozenValueTyped;
use crate::values::StarlarkValue;
use crate::values::ValueLike;
use crate::values::dict::globals::register_dict;
use crate::values::layout::heap::heap_type::FrozenHeapName;
use crate::values::list::globals::register_list;

/// Private test heap name for pagable tests.
#[derive(Clone, derive_more::Display, Debug, Hash, StrongHash)]
#[display("TestHeapName({})", _0)]
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

#[starlark_value(type = "SimpleData")]
impl<'v> StarlarkValue<'v> for SimpleData {
    type Canonical = Self;
}

/// Round-trip an OFV through the testing serializer. Returns the restored OFV.
fn round_trip_owned(
    heap_ref: FrozenHeapRef,
    root_fv: FrozenValue,
) -> crate::Result<OwnedFrozenValue> {
    // SAFETY: `heap_ref` owns the arena hosting `root_fv`.
    let owned = unsafe { OwnedFrozenValue::new(heap_ref, root_fv) };
    let mut ser = pagable::testing::TestingSerializer::new();
    owned
        .pagable_serialize(&mut ser)
        .map_err(crate::Error::new_other)?;
    let bytes = ser.finish();

    let mut de = pagable::testing::TestingDeserializer::new(&bytes);
    OwnedFrozenValue::pagable_deserialize(&mut de).map_err(crate::Error::new_other)
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

#[test]
fn test_module_eval_round_trip() -> crate::Result<()> {
    use std::any::TypeId;

    use pagable::PagableDeserialize;
    use pagable::context::PagableDeserializerImpl;
    use pagable::storage::handle::PagableStorageHandle;
    use pagable::storage::in_memory::InMemoryPagableStorage;
    use pagable::storage::support::SerializerForPaging;
    use pagable::storage::traits::PageOutError;

    use crate::environment::FrozenModule;
    use crate::environment::Module;
    use crate::eval::Evaluator;
    use crate::syntax::AstModule;
    use crate::syntax::Dialect;

    let code = r#"
x = 1 + 2
y = "hello" + " world"
a = [1]
a.append(a)
"#;

    let ast = AstModule::parse("test_module.star", code.to_owned(), &Dialect::Extended)?;
    let globals = GlobalsBuilder::new().build_named(GlobalFrozenHeapName { name: "test_eval" });
    let frozen_module = Module::with_temp_heap(|module| {
        {
            let mut eval = Evaluator::new(&module);
            eval.eval_module(ast, &globals).unwrap();
        }
        module.freeze_named(TestHeapName::heap_name("test_module_eval"))
    })?;

    let backing = InMemoryPagableStorage::new();
    let storage = backing.handle();
    let handle = PagableStorageHandle::new(storage.clone());
    let session_ctx = storage.session_context();

    let mut ser = SerializerForPaging::new(session_ctx);
    frozen_module
        .pagable_serialize(&mut ser)
        .map_err(crate::Error::new_other)?;
    let (data, arcs) = ser.finish();

    let top_key = {
        let mut finished = dashmap::DashMap::new();
        storage
            .page_out_item(data, arcs, &mut finished, session_ctx)
            .map_err(|e| match e {
                PageOutError::Failed(e) => crate::Error::new_other(e),
                PageOutError::AlreadyFailed => {
                    crate::Error::new_other(anyhow::anyhow!("page out reported AlreadyFailed"))
                }
            })?
    };

    let top_data = storage
        .fetch_arc_or_data_blocking(&TypeId::of::<()>(), &top_key)
        .map_err(crate::Error::new_other)?
        .right()
        .expect("top-level key should return data, not a cached arc");

    let mut de = PagableDeserializerImpl::new(&top_data.data, &top_data.arcs, &handle);
    let restored = FrozenModule::pagable_deserialize(&mut de).map_err(crate::Error::new_other)?;

    let x = restored.get("x")?.value().unpack_i32().unwrap();
    assert_eq!(x, 3);

    let y = restored.get("y")?;
    assert_eq!(y.value().unpack_str().unwrap(), "hello world");

    let a = restored.get("a")?;
    assert_eq!(a.value().length().unwrap(), 2);

    Ok(())
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

#[starlark_value(type = "HeapData")]
impl<'v> StarlarkValue<'v> for HeapData {
    type Canonical = Self;
}

#[test]
fn test_simple_value_round_trip() -> crate::Result<()> {
    let heap = FrozenHeap::new();
    let root = heap.alloc_simple(SimpleData {
        flag: true,
        count: 42,
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_simple_value"));

    let restored = round_trip_owned(heap_ref, root)?;
    let data: &SimpleData = restored.value().downcast_ref::<SimpleData>().unwrap();
    assert_eq!(data.flag, true);
    assert_eq!(data.count, 42);

    Ok(())
}

#[test]
fn test_heap_allocated_value_round_trip() -> crate::Result<()> {
    let heap = FrozenHeap::new();
    let root = heap.alloc_simple(HeapData {
        items: vec![10, 20, 30],
        label: "hello".to_owned(),
        boxed: Box::new(-99),
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_heap_data_round_trip"));

    let restored = round_trip_owned(heap_ref, root)?;
    let data: &HeapData = restored.value().downcast_ref::<HeapData>().unwrap();
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

#[starlark_value(type = "RefData")]
impl<'v> StarlarkValue<'v> for RefData {
    type Canonical = Self;
}

#[test]
fn test_frozen_value_ref_round_trip() -> crate::Result<()> {
    let heap = FrozenHeap::new();
    let target_fv = heap.alloc_simple(SimpleData {
        flag: true,
        count: 99,
    });
    let root = heap.alloc_simple(RefData {
        label: 7,
        target: target_fv,
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test"));

    let restored = round_trip_owned(heap_ref, root)?;
    let ref_data: &RefData = restored.value().downcast_ref::<RefData>().unwrap();
    assert_eq!(ref_data.label, 7);

    let resolved: &SimpleData = ref_data.target.downcast_ref::<SimpleData>().unwrap();
    assert_eq!(resolved.flag, true);
    assert_eq!(resolved.count, 99);

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

#[starlark_value(type = "DropRefData")]
impl<'v> StarlarkValue<'v> for DropRefData {
    type Canonical = Self;
}

#[test]
fn test_frozen_value_drop_to_undrop_round_trip() -> crate::Result<()> {
    let heap = FrozenHeap::new();
    let target_fv = heap.alloc_simple(SimpleData {
        flag: false,
        count: 123,
    });
    let root = heap.alloc_simple(DropRefData {
        items: vec![1, 2, 3],
        target: target_fv,
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test"));

    let restored = round_trip_owned(heap_ref, root)?;
    let drop_ref_data: &DropRefData = restored.value().downcast_ref::<DropRefData>().unwrap();
    assert_eq!(drop_ref_data.items, vec![1, 2, 3]);

    let resolved: &SimpleData = drop_ref_data.target.downcast_ref::<SimpleData>().unwrap();
    assert_eq!(resolved.flag, false);
    assert_eq!(resolved.count, 123);

    Ok(())
}

#[test]
fn test_frozen_value_undrop_to_drop_round_trip() -> crate::Result<()> {
    let heap = FrozenHeap::new();
    let target_fv = heap.alloc_simple(HeapData {
        items: vec![10, 20],
        label: "target".to_owned(),
        boxed: Box::new(42),
    });
    let root = heap.alloc_simple(RefData {
        label: 5,
        target: target_fv,
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test"));

    let restored = round_trip_owned(heap_ref, root)?;
    let ref_data: &RefData = restored.value().downcast_ref::<RefData>().unwrap();
    assert_eq!(ref_data.label, 5);

    let resolved: &HeapData = ref_data.target.downcast_ref::<HeapData>().unwrap();
    assert_eq!(resolved.items, vec![10, 20]);
    assert_eq!(resolved.label, "target");
    assert_eq!(*resolved.boxed, 42);

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
    let root = heap.alloc_list(&[a, b]);
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test"));

    let restored = round_trip_owned(heap_ref, root)?;
    let list_value: &ListGen<FrozenListData> = restored
        .value()
        .downcast_ref::<ListGen<FrozenListData>>()
        .unwrap();
    let content = list_value.0.content();
    assert_eq!(content.len(), 2);

    let elem_a: &SimpleData = content[0].downcast_ref::<SimpleData>().unwrap();
    assert_eq!(elem_a.flag, true);
    assert_eq!(elem_a.count, 10);

    let elem_b: &SimpleData = content[1].downcast_ref::<SimpleData>().unwrap();
    assert_eq!(elem_b.flag, false);
    assert_eq!(elem_b.count, 20);

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
    let root = heap.alloc_tuple(&[a, b]);
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test"));

    let restored = round_trip_owned(heap_ref, root)?;
    let tuple_value: &FrozenTuple = restored.value().downcast_ref::<FrozenTuple>().unwrap();
    let content = tuple_value.content();
    assert_eq!(content.len(), 2);

    let elem_a: &SimpleData = content[0].downcast_ref::<SimpleData>().unwrap();
    assert_eq!(elem_a.flag, true);
    assert_eq!(elem_a.count, 100);

    let elem_b: &SimpleData = content[1].downcast_ref::<SimpleData>().unwrap();
    assert_eq!(elem_b.flag, false);
    assert_eq!(elem_b.count, 200);

    Ok(())
}

#[test]
fn test_frozen_str_value_round_trip() -> crate::Result<()> {
    let heap = FrozenHeap::new();
    let str_fv = heap.alloc_str("hello world").to_frozen_value();
    let root = heap.alloc_simple(RefData {
        label: 42,
        target: str_fv,
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test"));

    let restored = round_trip_owned(heap_ref, root)?;
    let ref_data: &RefData = restored.value().downcast_ref::<RefData>().unwrap();
    assert_eq!(ref_data.label, 42);
    assert!(ref_data.target.is_str());
    assert_eq!(ref_data.target.unpack_str().unwrap(), "hello world");

    Ok(())
}

#[test]
fn test_frozen_value_inline_int_round_trip() -> crate::Result<()> {
    let heap = FrozenHeap::new();
    let int_fv = FrozenValue::testing_new_int(42);
    let root = heap.alloc_simple(RefData {
        label: 1,
        target: int_fv,
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test"));

    let restored = round_trip_owned(heap_ref, root)?;
    let ref_data: &RefData = restored.value().downcast_ref::<RefData>().unwrap();
    assert_eq!(ref_data.label, 1);
    assert_eq!(ref_data.target.unpack_i32(), Some(42));

    Ok(())
}

#[test]
fn test_cross_heap_frozen_value_round_trip() -> crate::Result<()> {
    let dep_heap = FrozenHeap::new();
    let dep_fv = dep_heap.alloc_simple(SimpleData {
        flag: true,
        count: 77,
    });
    let dep_heap_ref = dep_heap.into_ref_named(TestHeapName::heap_name("dep"));

    let main_heap = FrozenHeap::new();
    main_heap.add_reference(&dep_heap_ref);
    let root = main_heap.alloc_simple(RefData {
        label: 99,
        target: dep_fv,
    });
    let main_heap_ref = main_heap.into_ref_named(TestHeapName::heap_name("main"));

    let restored = round_trip_owned(main_heap_ref, root)?;
    let ref_data: &RefData = restored.value().downcast_ref::<RefData>().unwrap();
    assert_eq!(ref_data.label, 99);

    let resolved: &SimpleData = ref_data.target.downcast_ref::<SimpleData>().unwrap();
    assert_eq!(resolved.flag, true);
    assert_eq!(resolved.count, 77);

    Ok(())
}

#[test]
fn test_heap_ref_dedup_round_trip() -> crate::Result<()> {
    use crate::values::list::value::ListGen;
    use crate::values::types::list::value::FrozenListData;

    // Diamond: A refs [B, C], both B and C ref D. After round-trip the
    // restored B and C should share the same D arc.

    let heap_d = FrozenHeap::new();
    let d_fv = heap_d.alloc_simple(SimpleData {
        flag: true,
        count: 1,
    });
    let heap_d_ref = heap_d.into_ref_named(TestHeapName::heap_name("d"));

    let heap_b = FrozenHeap::new();
    heap_b.add_reference(&heap_d_ref);
    let b_root = heap_b.alloc_simple(RefData {
        label: 2,
        target: d_fv,
    });
    let heap_b_ref = heap_b.into_ref_named(TestHeapName::heap_name("b"));

    let heap_c = FrozenHeap::new();
    heap_c.add_reference(&heap_d_ref);
    let c_root = heap_c.alloc_simple(RefData {
        label: 3,
        target: d_fv,
    });
    let heap_c_ref = heap_c.into_ref_named(TestHeapName::heap_name("c"));

    // Use a Box<[FrozenValue]> via FrozenAnyArray-style wrapper: build a list
    // whose elements are the two RefData roots so a single OFV reaches both
    // heaps' bindings transitively. Simpler: link them through a top wrapper.
    let heap_a = FrozenHeap::new();
    heap_a.add_reference(&heap_b_ref);
    heap_a.add_reference(&heap_c_ref);
    let root = heap_a.alloc_list(&[b_root, c_root]);
    let heap_a_ref = heap_a.into_ref_named(TestHeapName::heap_name("a"));

    let restored = round_trip_owned(heap_a_ref, root)?;

    // Walk via the list root.
    let list_value: &ListGen<FrozenListData> = restored
        .value()
        .downcast_ref::<ListGen<FrozenListData>>()
        .unwrap();
    let content = list_value.0.content();
    assert_eq!(content.len(), 2);

    let b_data: &RefData = content[0].downcast_ref::<RefData>().unwrap();
    let c_data: &RefData = content[1].downcast_ref::<RefData>().unwrap();
    assert_eq!(b_data.label, 2);
    assert_eq!(c_data.label, 3);

    // Both should resolve to the same shared SimpleData in heap_d. Same target
    // ptr proves they share the same materialized header (in the dedup'd D).
    assert_eq!(
        b_data.target.ptr_value().ptr_value_untagged(),
        c_data.target.ptr_value().ptr_value_untagged(),
    );
    let d_data: &SimpleData = b_data.target.downcast_ref::<SimpleData>().unwrap();
    assert_eq!(d_data.flag, true);
    assert_eq!(d_data.count, 1);

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

    let root = heap.alloc_simple(RefData {
        label: 3,
        target: typed.to_frozen_value(),
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_fvt"));

    let restored = round_trip_owned(heap_ref, root)?;
    let ref_data: &RefData = restored.value().downcast_ref::<RefData>().unwrap();
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

#[starlark_value(type = "SmallMapData")]
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

#[starlark_value(type = "SmallMapFvData")]
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

    let root = heap.alloc_simple(SmallMapData { entries });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_sm_string"));

    let restored = round_trip_owned(heap_ref, root)?;
    let map_data: &SmallMapData = restored.value().downcast_ref::<SmallMapData>().unwrap();

    let keys: Vec<&str> = map_data.entries.iter().map(|(k, _)| k.as_str()).collect();
    assert_eq!(keys, vec!["beta", "alpha"]);

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

    let root = heap.alloc_simple(SmallMapFvData { entries });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_sm_fv_key"));

    let restored = round_trip_owned(heap_ref, root)?;
    let map_data: &SmallMapFvData = restored.value().downcast_ref::<SmallMapFvData>().unwrap();
    assert_eq!(map_data.entries.len(), 2);

    let key_strs: Vec<&str> = map_data
        .entries
        .iter()
        .map(|(k, _)| k.unpack_str().unwrap())
        .collect();
    assert_eq!(key_strs, vec!["key_one", "key_two"]);

    let val_labels: Vec<&str> = map_data
        .entries
        .iter()
        .map(|(_, v)| v.downcast_ref::<HeapData>().unwrap().label.as_str())
        .collect();
    assert_eq!(val_labels, vec!["val_one", "val_two"]);

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

    let mut entries = SmallMap::new();
    entries.insert_hashed(k1.get_hashed()?, v1);
    entries.insert_hashed(k2.get_hashed()?, v2);
    let root = heap.alloc_simple(SmallMapFvData { entries });

    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_forward_ref"));

    let restored = round_trip_owned(heap_ref, root)?;
    let map_data: &SmallMapFvData = restored.value().downcast_ref::<SmallMapFvData>().unwrap();
    assert_eq!(map_data.entries.len(), 2);

    let key_strs: Vec<&str> = map_data
        .entries
        .iter()
        .map(|(k, _)| k.unpack_str().unwrap())
        .collect();
    assert_eq!(key_strs, vec!["hello", "world"]);

    let values: Vec<i32> = map_data
        .entries
        .iter()
        .map(|(_, v)| v.unpack_i32().unwrap())
        .collect();
    assert_eq!(values, vec![111, 222]);

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
fn test_range_round_trip() -> crate::Result<()> {
    use std::num::NonZeroI32;

    use crate::values::types::range::Range;

    let heap = FrozenHeap::new();
    let root = heap.alloc_simple(Range::new(1, 10, NonZeroI32::new(2).unwrap()));
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_range"));

    let restored = round_trip_owned(heap_ref, root)?;
    let range: &Range = restored.value().downcast_ref::<Range>().unwrap();
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

#[starlark_value(type = "TestStackFrame")]
impl<'v> StarlarkValue<'v> for TestStackFrame {
    type Canonical = Self;
}

#[test]
fn test_stack_frame_data_round_trip() -> crate::Result<()> {
    use crate::values::types::tuple::value::FrozenTuple;

    let heap = FrozenHeap::new();

    let f0 = heap.alloc_simple(TestStackFrame {
        name: "native_func".to_owned(),
        location: None,
    });

    let codemap = CodeMap::new(
        "test.bzl".to_owned(),
        "load('foo')\ndef bar():\n  pass\n".to_owned(),
    );
    let span = codemap.full_span();
    let f1 = heap.alloc_simple(TestStackFrame {
        name: "bar".to_owned(),
        location: Some(FileSpan {
            file: codemap,
            span,
        }),
    });

    let root = heap.alloc_tuple(&[f0, f1]);
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_stack_frame_data"));

    let restored = round_trip_owned(heap_ref, root)?;
    let tuple: &FrozenTuple = restored.value().downcast_ref::<FrozenTuple>().unwrap();
    let content = tuple.content();
    assert_eq!(content.len(), 2);

    let data0: &TestStackFrame = content[0].downcast_ref::<TestStackFrame>().unwrap();
    assert_eq!(data0.name, "native_func");
    assert!(data0.location.is_none());

    let data1: &TestStackFrame = content[1].downcast_ref::<TestStackFrame>().unwrap();
    assert_eq!(data1.name, "bar");
    let loc = data1.location.as_ref().expect("should have location");
    assert_eq!(loc.file.filename(), "test.bzl");
    assert_eq!(loc.file.source(), "load('foo')\ndef bar():\n  pass\n");

    Ok(())
}

#[test]
fn test_native_codemap_round_trip() -> crate::Result<()> {
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
    let root = heap.alloc_simple(TestStackFrame {
        name: "native_call".to_owned(),
        location: Some(file_span),
    });

    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_native_codemap"));
    let restored = round_trip_owned(heap_ref, root)?;
    let data: &TestStackFrame = restored.value().downcast_ref::<TestStackFrame>().unwrap();
    assert_eq!(data.name, "native_call");
    let loc = data.location.as_ref().expect("should have location");
    assert_eq!(loc.file.filename(), "test_native.rs");
    assert_eq!(loc.file.source(), "<native>");
    assert_eq!(loc.file.id(), NativeCodeMap::to_codemap(NATIVE_STATIC).id());

    Ok(())
}

#[test]
fn test_frozen_dict_round_trip() -> crate::Result<()> {
    use crate::values::dict::AllocDict;
    use crate::values::types::dict::value::FrozenDict;

    let heap = FrozenHeap::new();
    let root = heap.alloc(AllocDict([("hello", 1), ("world", 2)]));
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_frozen_dict"));

    let restored = round_trip_owned(heap_ref, root)?;
    let dict: &FrozenDict = restored.value().downcast_ref::<FrozenDict>().unwrap();
    assert_eq!(dict.0.content.len(), 2);
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
    let root = heap.alloc(AllocStruct([("name", "alice"), ("age", "30")]));
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_frozen_struct"));

    let restored = round_trip_owned(heap_ref, root)?;
    let attrs = restored.value().dir_attr();
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

    let mut content: SmallSet<FrozenValue> = SmallSet::new();
    content.insert_hashed(FrozenValue::testing_new_int(1).get_hashed()?);
    content.insert_hashed(FrozenValue::testing_new_int(2).get_hashed()?);
    content.insert_hashed(FrozenValue::testing_new_int(3).get_hashed()?);
    let root = heap.alloc_simple(SetGen(FrozenSetData::new(content)));

    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_frozen_set"));
    let restored = round_trip_owned(heap_ref, root)?;
    let set: &FrozenSet = restored.value().downcast_ref::<FrozenSet>().unwrap();
    assert_eq!(set.0.len(), 3);
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
    let rt_a_fv = heap.alloc_simple(FrozenRecordType {
        id: id_a,
        ty_record_data: Some(shared.clone()),
        fields: make_fields(),
    });
    let rt_b_fv = heap.alloc_simple(FrozenRecordType {
        id: id_b,
        ty_record_data: Some(shared),
        fields: make_fields(),
    });
    let root = heap.alloc_tuple(&[rt_a_fv, rt_b_fv]);

    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_frozen_record_type"));
    // SAFETY: heap_ref owns the arena hosting root.
    let owned = unsafe { OwnedFrozenValue::new(heap_ref, root) };
    let restored = round_trip_owned_frozen_value_pagable_ser_de_impl(&owned)?;

    let tuple: &crate::values::types::tuple::value::FrozenTuple = restored
        .value()
        .downcast_ref::<crate::values::types::tuple::value::FrozenTuple>()
        .unwrap();
    let content = tuple.content();
    let rt_a: &FrozenRecordType = content[0].downcast_ref::<FrozenRecordType>().unwrap();
    let rt_b: &FrozenRecordType = content[1].downcast_ref::<FrozenRecordType>().unwrap();

    assert_eq!(rt_a.id, id_a);
    assert_eq!(rt_b.id, id_b);
    for rt in [rt_a, rt_b] {
        let field_names: Vec<&str> = rt.fields.keys().map(String::as_str).collect();
        assert_eq!(field_names, vec!["x", "y"]);
        for (_, field) in rt.fields.iter() {
            assert!(field.default.is_none());
        }
    }

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

    assert!(
        Arc::ptr_eq(data_a, data_b),
        "pagable Arc dedup should round-trip the shared Arc<TyRecordData> as a single allocation",
    );

    Ok(())
}

#[test]
fn test_static_frozen_value_round_trip() -> crate::Result<()> {
    let heap = FrozenHeap::new();

    let none_fv = FrozenValue::new_none();
    let true_fv = FrozenValue::new_bool(true);
    let false_fv = FrozenValue::new_bool(false);
    let empty_tuple_fv = FrozenValue::new_empty_tuple();
    let static_str_fv = const_frozen_string!("static_test_str").to_frozen_value();

    let r0 = heap.alloc_simple(RefData {
        label: 1,
        target: none_fv,
    });
    let r1 = heap.alloc_simple(RefData {
        label: 2,
        target: true_fv,
    });
    let r2 = heap.alloc_simple(RefData {
        label: 3,
        target: false_fv,
    });
    let r3 = heap.alloc_simple(RefData {
        label: 4,
        target: empty_tuple_fv,
    });
    let r4 = heap.alloc_simple(RefData {
        label: 5,
        target: static_str_fv,
    });
    let root = heap.alloc_tuple(&[r0, r1, r2, r3, r4]);

    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_static"));
    let restored = round_trip_owned(heap_ref, root)?;
    let tuple: &crate::values::types::tuple::value::FrozenTuple = restored
        .value()
        .downcast_ref::<crate::values::types::tuple::value::FrozenTuple>()
        .unwrap();
    let content = tuple.content();

    let ref0: &RefData = content[0].downcast_ref::<RefData>().unwrap();
    assert_eq!(ref0.label, 1);
    assert!(ref0.target.is_none());
    assert_eq!(
        ref0.target.ptr_value().ptr_value_untagged(),
        none_fv.ptr_value().ptr_value_untagged(),
    );

    let ref1: &RefData = content[1].downcast_ref::<RefData>().unwrap();
    assert_eq!(ref1.label, 2);
    assert_eq!(ref1.target.unpack_bool(), Some(true));
    assert_eq!(
        ref1.target.ptr_value().ptr_value_untagged(),
        true_fv.ptr_value().ptr_value_untagged(),
    );

    let ref2: &RefData = content[2].downcast_ref::<RefData>().unwrap();
    assert_eq!(ref2.label, 3);
    assert_eq!(ref2.target.unpack_bool(), Some(false));
    assert_eq!(
        ref2.target.ptr_value().ptr_value_untagged(),
        false_fv.ptr_value().ptr_value_untagged(),
    );

    let ref3: &RefData = content[3].downcast_ref::<RefData>().unwrap();
    assert_eq!(ref3.label, 4);
    assert_eq!(
        ref3.target.ptr_value().ptr_value_untagged(),
        empty_tuple_fv.ptr_value().ptr_value_untagged(),
    );

    let ref4: &RefData = content[4].downcast_ref::<RefData>().unwrap();
    assert_eq!(ref4.label, 5);
    assert_eq!(ref4.target.unpack_str(), Some("static_test_str"));
    assert_eq!(
        ref4.target.ptr_value().ptr_value_untagged(),
        static_str_fv.ptr_value().ptr_value_untagged(),
    );

    Ok(())
}

starlark::globals_static!(
    PAGABLE_TEST_STATIC_GLOBALS = |globals| {
        globals.set(
            "global_value",
            SimpleData {
                flag: true,
                count: 17,
            },
        );
    }
);

starlark::methods_static!(
    PAGABLE_TEST_STATIC_METHODS = |methods| {
        methods.set_attribute(
            "method_value",
            SimpleData {
                flag: false,
                count: 23,
            },
            None,
        );
    }
);

fn assert_static_value_round_trip(
    static_fv: FrozenValue,
    label: usize,
) -> crate::Result<FrozenValue> {
    let heap = FrozenHeap::new();
    let root = heap.alloc_simple(RefData {
        label,
        target: static_fv,
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_static_heap_value"));

    let restored = round_trip_owned(heap_ref, root)?;
    let ref_data: &RefData = restored.value().downcast_ref().unwrap();
    assert_eq!(ref_data.label, label);
    assert_eq!(
        ref_data.target.ptr_value().ptr_value_untagged(),
        static_fv.ptr_value().ptr_value_untagged(),
    );
    Ok(ref_data.target)
}

#[test]
fn test_globals_static_heap_value_round_trip() -> crate::Result<()> {
    let static_fv = PAGABLE_TEST_STATIC_GLOBALS
        .globals()
        .get_frozen("global_value")
        .expect("static global should exist");

    let restored = assert_static_value_round_trip(static_fv, 8)?;
    let restored_data = restored
        .to_value()
        .downcast_ref::<SimpleData>()
        .expect("static global should still point to SimpleData");
    assert_eq!(restored_data.flag, true);
    assert_eq!(restored_data.count, 17);
    Ok(())
}

#[test]
fn test_methods_static_heap_value_round_trip() -> crate::Result<()> {
    let static_fv = PAGABLE_TEST_STATIC_METHODS
        .methods()
        .members()
        .find_map(|(name, value)| (name == "method_value").then_some(value))
        .expect("static method attribute should exist");

    assert_static_value_round_trip(static_fv, 9)?;
    Ok(())
}

/// Phantom type used only as the `T` of `StarlarkValueAsType<T>` in the
/// round-trip test below. Never allocated on a heap.
#[derive(
    Debug,
    Display,
    Allocative,
    ProvidesStaticType,
    NoSerialize,
    StarlarkPagable
)]
#[display("AsTypeRoundTripTestType")]
struct AsTypeRoundTripTestType;

#[starlark_value(type = "AsTypeRoundTripTestType")]
impl<'v> StarlarkValue<'v> for AsTypeRoundTripTestType {}

crate::declare_starlark_value_as_type!(AS_TYPE_RT_STATIC, AsTypeRoundTripTestType);

#[test]
fn test_starlark_value_as_type_round_trip() -> crate::Result<()> {
    let heap = FrozenHeap::new();
    let static_fv = AS_TYPE_RT_STATIC.to_frozen_value();
    let root = heap.alloc_simple(RefData {
        label: 7,
        target: static_fv,
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name(
        "test_starlark_value_as_type_round_trip",
    ));

    let restored = round_trip_owned(heap_ref, root)?;
    let ref_data: &RefData = restored.value().downcast_ref::<RefData>().unwrap();
    assert_eq!(ref_data.label, 7);
    assert_eq!(
        ref_data.target.ptr_value().ptr_value_untagged(),
        static_fv.ptr_value().ptr_value_untagged(),
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

#[starlark_value(type = "OuterArcValue")]
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

    let shared = Arc::new(InnerArcData {
        target: target_fv,
        label: 99,
    });

    let a = heap.alloc_simple(OuterArcValue {
        inner: shared.clone(),
        outer_label: 1,
        items: vec![10, 20, 30],
    });
    let b = heap.alloc_simple(OuterArcValue {
        inner: shared,
        outer_label: 2,
        items: vec![40, 50],
    });
    let root = heap.alloc_tuple(&[a, b]);

    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_with_starlark_context_arc"));
    // SAFETY: heap_ref owns the arena hosting root.
    let owned = unsafe { OwnedFrozenValue::new(heap_ref, root) };
    let restored = round_trip_owned_frozen_value_pagable_ser_de_impl(&owned)?;

    let tuple: &crate::values::types::tuple::value::FrozenTuple = restored
        .value()
        .downcast_ref::<crate::values::types::tuple::value::FrozenTuple>()
        .unwrap();
    let content = tuple.content();
    let outer_a: &OuterArcValue = content[0].downcast_ref::<OuterArcValue>().unwrap();
    let outer_b: &OuterArcValue = content[1].downcast_ref::<OuterArcValue>().unwrap();
    assert_eq!(outer_a.outer_label, 1);
    assert_eq!(outer_b.outer_label, 2);
    assert_eq!(outer_a.items, vec![10, 20, 30]);
    assert_eq!(outer_b.items, vec![40, 50]);

    assert_eq!(outer_a.inner.label, 99);
    assert_eq!(outer_b.inner.label, 99);
    let target_data: &SimpleData = outer_a.inner.target.downcast_ref::<SimpleData>().unwrap();
    assert_eq!(target_data.flag, true);
    assert_eq!(target_data.count, 314);
    assert_eq!(
        outer_a.inner.target.ptr_value().ptr_value_untagged(),
        outer_b.inner.target.ptr_value().ptr_value_untagged(),
    );

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

#[starlark_value(type = "ArcBlanketOuter")]
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

    let a = heap.alloc_simple(ArcBlanketOuter {
        inner: shared.clone(),
        outer_label: 1,
        items: vec![1, 2, 3],
    });
    let b = heap.alloc_simple(ArcBlanketOuter {
        inner: shared,
        outer_label: 2,
        items: vec![4, 5],
    });
    let root = heap.alloc_tuple(&[a, b]);

    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_arc_blanket"));
    // SAFETY: heap_ref owns the arena hosting root.
    let owned = unsafe { OwnedFrozenValue::new(heap_ref, root) };
    let restored = round_trip_owned_frozen_value_pagable_ser_de_impl(&owned)?;

    let tuple: &crate::values::types::tuple::value::FrozenTuple = restored
        .value()
        .downcast_ref::<crate::values::types::tuple::value::FrozenTuple>()
        .unwrap();
    let content = tuple.content();
    let outer_a: &ArcBlanketOuter = content[0].downcast_ref::<ArcBlanketOuter>().unwrap();
    let outer_b: &ArcBlanketOuter = content[1].downcast_ref::<ArcBlanketOuter>().unwrap();
    assert_eq!(outer_a.outer_label, 1);
    assert_eq!(outer_b.outer_label, 2);
    assert_eq!(outer_a.items, vec![1, 2, 3]);
    assert_eq!(outer_b.items, vec![4, 5]);

    assert_eq!(outer_a.inner.label, 7);
    assert_eq!(outer_b.inner.label, 7);
    let target_data: &SimpleData = outer_a.inner.target.downcast_ref::<SimpleData>().unwrap();
    assert_eq!(target_data.flag, true);
    assert_eq!(target_data.count, 271);
    assert_eq!(
        outer_a.inner.target.ptr_value().ptr_value_untagged(),
        outer_b.inner.target.ptr_value().ptr_value_untagged(),
    );

    assert!(
        Arc::ptr_eq(&outer_a.inner, &outer_b.inner),
        "Arc<T> blanket should preserve Arc identity across round-trip",
    );

    Ok(())
}

// ============================================================================
// StarlarkAny<T> / FrozenAnyValue<T> round-trip
// ============================================================================

/// Pure-data payload for `StarlarkAny` tests. `StarlarkPagable` derive so the
/// inner data round-trips; `register_starlark_any!` registers the typing
/// vtable entry needed for `StarlarkAny<AnyPayload>` to participate in ser/de.
#[derive(Debug, Clone, PartialEq, Eq, StarlarkPagable)]
struct AnyPayload {
    name: String,
    count: u32,
}

crate::register_starlark_any!(AnyPayload);
crate::register_any_array!(AnyPayload);

#[test]
fn test_starlark_any_round_trip() -> crate::Result<()> {
    let heap = FrozenHeap::new();
    let payload = AnyPayload {
        name: "hello".to_owned(),
        count: 7,
    };
    let root = heap.alloc_any_value(payload.clone()).to_frozen_value();
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_starlark_any"));

    let restored = round_trip_owned(heap_ref, root)?;
    let any_payload: &crate::values::any::StarlarkAny<AnyPayload> = restored
        .value()
        .downcast_ref::<crate::values::any::StarlarkAny<AnyPayload>>()
        .unwrap();
    assert_eq!(any_payload.0, payload);

    Ok(())
}

#[test]
fn test_starlark_any_multiple_values_round_trip() -> crate::Result<()> {
    let heap = FrozenHeap::new();
    let a = AnyPayload {
        name: "first".to_owned(),
        count: 1,
    };
    let b = AnyPayload {
        name: "second".to_owned(),
        count: 2,
    };
    let a_fv = heap.alloc_any_value(a.clone()).to_frozen_value();
    let b_fv = heap.alloc_any_value(b.clone()).to_frozen_value();
    let root = heap.alloc_tuple(&[a_fv, b_fv]);
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_starlark_any_multi"));

    let restored = round_trip_owned(heap_ref, root)?;
    let tuple: &crate::values::types::tuple::value::FrozenTuple = restored
        .value()
        .downcast_ref::<crate::values::types::tuple::value::FrozenTuple>()
        .unwrap();
    let content = tuple.content();
    let got_a: &crate::values::any::StarlarkAny<AnyPayload> = content[0]
        .downcast_ref::<crate::values::any::StarlarkAny<AnyPayload>>()
        .unwrap();
    let got_b: &crate::values::any::StarlarkAny<AnyPayload> = content[1]
        .downcast_ref::<crate::values::any::StarlarkAny<AnyPayload>>()
        .unwrap();
    assert_eq!(got_a.0, a);
    assert_eq!(got_b.0, b);

    Ok(())
}

// ============================================================================
// FrozenAnyArray<T> round-trip
//
// `AnyArray<T>` uses `alloc_raw_extra` for a trailing elements array — the
// elements live beyond the struct bounds. Its `AValue` impl
// (`AValueAnyArray<T>`) provides `starlark_serialize`/`starlark_deserialize`
// that walk `as_slice()` on the way out and reconstruct elements in-place on
// the way in (same pattern as `AValueList`).
// ============================================================================

#[test]
fn test_frozen_any_array_round_trip() -> crate::Result<()> {
    let heap = FrozenHeap::new();
    let items = vec![
        AnyPayload {
            name: "alpha".to_owned(),
            count: 10,
        },
        AnyPayload {
            name: "beta".to_owned(),
            count: 20,
        },
        AnyPayload {
            name: "gamma".to_owned(),
            count: 30,
        },
    ];
    let root = heap.alloc_any_array_value(&items).to_frozen_value();
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_frozen_any_array"));

    let restored = round_trip_owned(heap_ref, root)?;
    let any_array: &crate::values::types::any_array::AnyArray<AnyPayload> = restored
        .value()
        .downcast_ref::<crate::values::types::any_array::AnyArray<AnyPayload>>()
        .unwrap();
    assert_eq!(any_array.as_slice().len(), 3);
    assert_eq!(any_array.as_slice()[0], items[0]);
    assert_eq!(any_array.as_slice()[1], items[1]);
    assert_eq!(any_array.as_slice()[2], items[2]);

    Ok(())
}

#[test]
fn test_frozen_any_array_empty_round_trip() -> crate::Result<()> {
    let heap = FrozenHeap::new();
    let root = heap
        .alloc_any_array_value::<AnyPayload>(&[])
        .to_frozen_value();
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_frozen_any_array_empty"));

    let restored = round_trip_owned(heap_ref, root)?;
    let any_array: &crate::values::types::any_array::AnyArray<AnyPayload> = restored
        .value()
        .downcast_ref::<crate::values::types::any_array::AnyArray<AnyPayload>>()
        .unwrap();
    assert_eq!(any_array.as_slice().len(), 0);

    Ok(())
}

/// Host type holding an `AtomicFrozenAnyValueOption<AnyPayload>` field.
#[derive(Display, Allocative, ProvidesStaticType, NoSerialize, StarlarkPagable)]
#[display("AtomicHost({})", self.label)]
struct AtomicHost {
    label: String,
    #[allocative(skip)]
    option: crate::values::any::AtomicFrozenAnyValueOption<AnyPayload>,
}

impl std::fmt::Debug for AtomicHost {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AtomicHost")
            .field("label", &self.label)
            .field("option_is_some", &self.option.load_relaxed().is_some())
            .finish()
    }
}

starlark_simple_value!(AtomicHost);

#[starlark_value(type = "AtomicHost")]
impl<'v> StarlarkValue<'v> for AtomicHost {
    type Canonical = Self;
}

#[test]
fn test_atomic_frozen_any_value_option_some_round_trip() -> crate::Result<()> {
    let heap = FrozenHeap::new();
    let payload_fv = heap.alloc_any_value(AnyPayload {
        name: "target".to_owned(),
        count: 42,
    });
    let root = heap.alloc_simple(AtomicHost {
        label: "host_with_some".to_owned(),
        option: crate::values::any::AtomicFrozenAnyValueOption::new(Some(payload_fv)),
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name(
        "test_atomic_frozen_any_value_option_some",
    ));

    let restored = round_trip_owned(heap_ref, root)?;
    let host: &AtomicHost = restored.value().downcast_ref::<AtomicHost>().unwrap();
    assert_eq!(host.label, "host_with_some");

    let loaded = host.option.load_relaxed();
    let fv = loaded.expect("option should be Some after round-trip");
    assert_eq!(
        *fv.as_ref(),
        AnyPayload {
            name: "target".to_owned(),
            count: 42,
        }
    );

    Ok(())
}

#[test]
fn test_atomic_frozen_any_value_option_none_round_trip() -> crate::Result<()> {
    let heap = FrozenHeap::new();
    let root = heap.alloc_simple(AtomicHost {
        label: "host_with_none".to_owned(),
        option: crate::values::any::AtomicFrozenAnyValueOption::new(None),
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name(
        "test_atomic_frozen_any_value_option_none",
    ));

    let restored = round_trip_owned(heap_ref, root)?;
    let host: &AtomicHost = restored.value().downcast_ref::<AtomicHost>().unwrap();
    assert_eq!(host.label, "host_with_none");

    assert!(
        host.option.load_relaxed().is_none(),
        "option should be None after round-trip"
    );

    Ok(())
}

// ============================================================================
// StarlarkAnyComplex<T> round-trip
//
// `StarlarkAnyComplex<T>` is the GC-traceable sibling of `StarlarkAny<T>`.
// For pagable, we exercise the frozen-heap side: allocate
// `StarlarkAnyComplex<FrozenComplexPayload>` directly on a `FrozenHeap` via
// `alloc_simple` and round-trip.
// ============================================================================

#[derive(Debug, Allocative, ProvidesStaticType, StarlarkPagable)]
struct FrozenComplexPayload {
    label: String,
    numbers: Vec<u32>,
}

crate::register_starlark_any_complex!(frozen FrozenComplexPayload);

#[test]
fn test_starlark_any_complex_round_trip() -> crate::Result<()> {
    let heap = FrozenHeap::new();
    let root = heap.alloc_simple(crate::values::any_complex::StarlarkAnyComplex::new(
        FrozenComplexPayload {
            label: "complex".to_owned(),
            numbers: vec![1, 2, 3],
        },
    ));
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_starlark_any_complex"));

    let restored = round_trip_owned(heap_ref, root)?;
    let got: &crate::values::any_complex::StarlarkAnyComplex<FrozenComplexPayload> = restored
        .value()
        .downcast_ref::<crate::values::any_complex::StarlarkAnyComplex<FrozenComplexPayload>>()
        .unwrap();
    assert_eq!(got.value.label, "complex");
    assert_eq!(got.value.numbers, vec![1, 2, 3]);

    Ok(())
}
// ============================================================================
// `#[starlark_pagable_typetag]` round-trip: `Box<dyn Trait>` with a
// concrete impl that holds a `FrozenValue`.
// ============================================================================

#[crate::starlark_pagable_typetag]
trait TypetagPayload:
    pagable::PagableTagged + std::fmt::Debug + Allocative + Send + Sync + 'static
{
    fn label(&self) -> u32;
    fn target(&self) -> FrozenValue;
}

#[derive(Debug, Allocative, ProvidesStaticType, StarlarkPagable)]
struct TypetagImpl {
    target: FrozenValue,
    label: u32,
}

#[crate::starlark_pagable_typetag]
impl TypetagPayload for TypetagImpl {
    fn label(&self) -> u32 {
        self.label
    }
    fn target(&self) -> FrozenValue {
        self.target
    }
}

#[derive(
    Debug,
    Display,
    Allocative,
    ProvidesStaticType,
    NoSerialize,
    StarlarkPagable
)]
#[display("TypetagOuter({})", self.outer)]
struct TypetagOuter {
    #[starlark_pagable(pagable)]
    inner: Box<dyn TypetagPayload>,
    outer: u32,
}

starlark_simple_value!(TypetagOuter);

#[starlark_value(type = "TypetagOuter")]
impl<'v> StarlarkValue<'v> for TypetagOuter {
    type Canonical = Self;
}

#[test]
fn test_starlark_pagable_typetag_round_trip() -> crate::Result<()> {
    let heap = FrozenHeap::new();
    let target_fv = heap.alloc_simple(SimpleData {
        flag: true,
        count: 161,
    });

    let root = heap.alloc_simple(TypetagOuter {
        inner: Box::new(TypetagImpl {
            target: target_fv,
            label: 11,
        }),
        outer: 1,
    });

    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_typetag"));
    let restored = round_trip_owned(heap_ref, root)?;
    let outer: &TypetagOuter = restored.value().downcast_ref::<TypetagOuter>().unwrap();
    assert_eq!(outer.outer, 1);
    assert_eq!(outer.inner.label(), 11);
    let target_data: &SimpleData = outer.inner.target().downcast_ref::<SimpleData>().unwrap();
    assert_eq!(target_data.count, 161);

    Ok(())
}

#[test]
fn test_type_compiled_impl_as_starlark_value_round_trip() -> crate::Result<()> {
    use crate::typing::Ty;
    use crate::values::typing::type_compiled::compiled::DummyTypeMatcher;
    use crate::values::typing::type_compiled::compiled::TypeCompiledImplAsStarlarkValue;

    let heap = FrozenHeap::new();
    let original_ty = Ty::any();
    let root = heap.alloc_simple(
        TypeCompiledImplAsStarlarkValue::<DummyTypeMatcher>::new_for_test(
            DummyTypeMatcher,
            original_ty.clone(),
        ),
    );
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name(
        "test_type_compiled_impl_round_trip",
    ));

    let restored = round_trip_owned(heap_ref, root)?;
    let got: &TypeCompiledImplAsStarlarkValue<DummyTypeMatcher> = restored
        .value()
        .downcast_ref::<TypeCompiledImplAsStarlarkValue<DummyTypeMatcher>>()
        .unwrap();
    assert_eq!(got.ty_for_test(), &original_ty);
    let _: &DummyTypeMatcher = got.impl_for_test();

    Ok(())
}

#[test]
fn test_type_compiled_impl_with_is_str_round_trip() -> crate::Result<()> {
    use crate::typing::Ty;
    use crate::values::typing::type_compiled::compiled::TypeCompiledImplAsStarlarkValue;
    use crate::values::typing::type_compiled::matcher::TypeMatcher;
    use crate::values::typing::type_compiled::matchers::IsStr;

    let heap = FrozenHeap::new();
    let original_ty = Ty::string();
    let root = heap.alloc_simple(TypeCompiledImplAsStarlarkValue::<IsStr>::new_for_test(
        IsStr,
        original_ty.clone(),
    ));
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_type_compiled_is_str"));

    let restored = round_trip_owned(heap_ref, root)?;
    let got: &TypeCompiledImplAsStarlarkValue<IsStr> = restored
        .value()
        .downcast_ref::<TypeCompiledImplAsStarlarkValue<IsStr>>()
        .unwrap();
    assert_eq!(got.ty_for_test(), &original_ty);
    assert!(
        got.impl_for_test()
            .matches(const_frozen_string!("hi").to_value())
    );
    assert!(
        !got.impl_for_test()
            .matches(FrozenValue::new_bool(true).to_value())
    );

    Ok(())
}

#[test]
fn test_type_compiled_impl_with_is_int_round_trip() -> crate::Result<()> {
    use crate::typing::Ty;
    use crate::values::typing::type_compiled::compiled::TypeCompiledImplAsStarlarkValue;
    use crate::values::typing::type_compiled::matcher::TypeMatcher;
    use crate::values::typing::type_compiled::matchers::IsInt;

    let heap = FrozenHeap::new();
    let original_ty = Ty::int();
    let root = heap.alloc_simple(TypeCompiledImplAsStarlarkValue::<IsInt>::new_for_test(
        IsInt,
        original_ty.clone(),
    ));
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_type_compiled_is_int"));

    let restored = round_trip_owned(heap_ref, root)?;
    let got: &TypeCompiledImplAsStarlarkValue<IsInt> = restored
        .value()
        .downcast_ref::<TypeCompiledImplAsStarlarkValue<IsInt>>()
        .unwrap();
    assert_eq!(got.ty_for_test(), &original_ty);
    assert!(
        got.impl_for_test()
            .matches(FrozenValue::testing_new_int(42).to_value())
    );
    assert!(
        !got.impl_for_test()
            .matches(const_frozen_string!("x").to_value())
    );

    Ok(())
}

#[test]
fn test_type_compiled_impl_with_is_any_of_round_trip() -> crate::Result<()> {
    use crate::typing::Ty;
    use crate::values::typing::type_compiled::compiled::TypeCompiledImplAsStarlarkValue;
    use crate::values::typing::type_compiled::matcher::TypeMatcher;
    use crate::values::typing::type_compiled::matcher::TypeMatcherBox;
    use crate::values::typing::type_compiled::matchers::IsAnyOf;
    use crate::values::typing::type_compiled::matchers::IsInt;
    use crate::values::typing::type_compiled::matchers::IsStr;

    let inners = vec![TypeMatcherBox::new(IsStr), TypeMatcherBox::new(IsInt)];
    let heap = FrozenHeap::new();
    let original_ty = Ty::union2(Ty::string(), Ty::int());
    let root = heap.alloc_simple(TypeCompiledImplAsStarlarkValue::<IsAnyOf>::new_for_test(
        IsAnyOf(inners),
        original_ty.clone(),
    ));
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_type_compiled_is_any_of"));

    let restored = round_trip_owned(heap_ref, root)?;
    let got: &TypeCompiledImplAsStarlarkValue<IsAnyOf> = restored
        .value()
        .downcast_ref::<TypeCompiledImplAsStarlarkValue<IsAnyOf>>()
        .unwrap();
    assert_eq!(got.ty_for_test(), &original_ty);
    assert_eq!(got.impl_for_test().0.len(), 2);
    assert!(
        got.impl_for_test()
            .matches(const_frozen_string!("s").to_value())
    );
    assert!(
        got.impl_for_test()
            .matches(FrozenValue::testing_new_int(1).to_value())
    );
    assert!(
        !got.impl_for_test()
            .matches(FrozenValue::new_bool(true).to_value())
    );

    Ok(())
}

// ============================================================================
// `starlark_deserialize_field` error context
// ============================================================================

#[derive(Debug, Allocative)]
struct AlwaysFailDeserialize;

impl crate::pagable::StarlarkSerialize for AlwaysFailDeserialize {
    fn starlark_serialize(
        &self,
        _ctx: &mut dyn crate::pagable::StarlarkSerializeContext,
    ) -> crate::Result<()> {
        Ok(())
    }
}

impl crate::pagable::StarlarkDeserialize for AlwaysFailDeserialize {
    fn starlark_deserialize(
        _ctx: &mut dyn crate::pagable::starlark_deserialize::StarlarkDeserializeContext<'_>,
    ) -> crate::Result<Self> {
        Err(crate::Error::new_other(anyhow::anyhow!(
            "intentional test failure"
        )))
    }
}

#[derive(
    Debug,
    Display,
    Allocative,
    ProvidesStaticType,
    NoSerialize,
    StarlarkPagable
)]
#[display("FieldErrorTestData")]
struct FieldErrorTestData {
    good_field: bool,
    bad_field: AlwaysFailDeserialize,
}

starlark_simple_value!(FieldErrorTestData);

#[starlark_value(type = "FieldErrorTestData")]
impl<'v> StarlarkValue<'v> for FieldErrorTestData {
    type Canonical = Self;
}

#[test]
fn test_deserialize_field_error_includes_field_name() {
    let heap = FrozenHeap::new();
    let root = heap.alloc_simple(FieldErrorTestData {
        good_field: true,
        bad_field: AlwaysFailDeserialize,
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("test_field_error"));

    let result = round_trip_owned(heap_ref, root);

    let err = result.unwrap_err();
    let err_msg = format!("{:#}", err);
    assert!(
        err_msg.contains("deserializing field bad_field"),
        "error should mention the field name, got: {}",
        err_msg,
    );
}

#[starlark_derive::starlark_module]
fn register_foo(builder: &mut GlobalsBuilder) {
    fn foo() -> anyhow::Result<i32> {
        Ok(1)
    }
}

starlark::globals_static!(
    STATIC_GLOBALS = |static_globals| {
        static_globals.set("x", FrozenValue::new_none());
    }
);

#[test]
fn test_globals_roundtrip() {
    use pagable::PagableSerialize;
    let mut globals = GlobalsBuilder::new();
    STATIC_GLOBALS.populate(&mut globals);

    register_list(&mut globals);
    globals.namespace_no_docs("ns_hidden", |_| {});
    globals.namespace("ns", |globals| {
        globals.namespace_no_docs("nested_ns_hidden", |_| {});
        globals.set("x", FrozenValue::new_none());
        register_dict(globals);
    });

    let globals = globals.build_named(GlobalFrozenHeapName { name: "testing" });

    let mut serializer = pagable::testing::TestingSerializer::new();
    globals.pagable_serialize(&mut serializer).unwrap();
}

/// Round-trip an `OwnedFrozenValue` through `SerializerForPaging` +
/// `InMemoryPagableStorage` + `PagableDeserializerImpl` (the real concrete
/// impls used in production, not the testing ones).
fn round_trip_owned_frozen_value_pagable_ser_de_impl(
    owned: &OwnedFrozenValue,
) -> crate::Result<OwnedFrozenValue> {
    use std::any::TypeId;

    use pagable::PagableDeserialize;
    use pagable::context::PagableDeserializerImpl;
    use pagable::storage::handle::PagableStorageHandle;
    use pagable::storage::in_memory::InMemoryPagableStorage;
    use pagable::storage::support::SerializerForPaging;

    let backing = InMemoryPagableStorage::new();
    let storage = backing.handle();
    let handle = PagableStorageHandle::new(storage.clone());
    let session_ctx = storage.session_context();

    let mut ser = SerializerForPaging::new(session_ctx);
    owned
        .pagable_serialize(&mut ser)
        .map_err(crate::Error::new_other)?;
    let (data, arcs) = ser.finish();

    let top_key = {
        let mut finished = dashmap::DashMap::new();
        storage
            .page_out_item(data, arcs, &mut finished, session_ctx)
            .map_err(crate::Error::new_other)?
    };

    let top_data = storage
        .fetch_arc_or_data_blocking(&TypeId::of::<()>(), &top_key)
        .map_err(crate::Error::new_other)?
        .right()
        .expect("top-level key should return data, not a cached arc");

    let mut de = PagableDeserializerImpl::new(&top_data.data, &top_data.arcs, &handle);
    OwnedFrozenValue::pagable_deserialize(&mut de).map_err(crate::Error::new_other)
}

/// Same scenario as `test_cross_heap_frozen_value_round_trip` but routed
/// through `SerializerForPaging` + `InMemoryPagableStorage` +
/// `PagableDeserializerImpl`.
#[test]
fn test_cross_heap_frozen_value_round_trip_via_storage() -> crate::Result<()> {
    let dep_heap = FrozenHeap::new();
    let dep_fv = dep_heap.alloc_simple(SimpleData {
        flag: true,
        count: 77,
    });
    let dep_heap_ref = dep_heap.into_ref_named(TestHeapName::heap_name("dep_storage"));

    let main_heap = FrozenHeap::new();
    main_heap.add_reference(&dep_heap_ref);
    let ref_fv = main_heap.alloc_simple(RefData {
        label: 99,
        target: dep_fv,
    });
    let main_heap_ref = main_heap.into_ref_named(TestHeapName::heap_name("main_storage"));

    // SAFETY: `main_heap_ref` keeps the arena hosting `ref_fv` alive.
    let owned = unsafe { OwnedFrozenValue::new(main_heap_ref, ref_fv) };
    let restored = round_trip_owned_frozen_value_pagable_ser_de_impl(&owned)?;

    let undrop_headers = restored.owner().collect_undrop_headers_ordered();
    assert_eq!(undrop_headers.len(), 1);
    let ref_data: &RefData = undrop_headers[0].unpack().downcast_ref().unwrap();
    assert_eq!(ref_data.label, 99);
    let resolved: &SimpleData = ref_data
        .target
        .to_value()
        .downcast_ref::<SimpleData>()
        .expect("FrozenValue should resolve to SimpleData in dep heap");
    assert_eq!(resolved.flag, true);
    assert_eq!(resolved.count, 77);
    Ok(())
}

/// Storage-path counterpart to `test_with_starlark_context_arc_dedup_round_trip`.
/// `OuterArcValue` owns `Arc<InnerArcData>` whose inner holds a `FrozenValue`.
/// `PagableDeserializerImpl` reads the inner `Arc` body through its own
/// sub-deserializer, so a naive `seek(target.abs_pos)` would land in the
/// wrong stream; `ensure` instead seeks a deserializer reconstructed from
/// the owning heap's recipe in `HeapRecipeMap`.
#[test]
fn test_with_starlark_context_arc_dedup_round_trip_via_storage() {
    let heap = FrozenHeap::new();
    let target_fv = heap.alloc_simple(SimpleData {
        flag: true,
        count: 314,
    });

    let shared = Arc::new(InnerArcData {
        target: target_fv,
        label: 99,
    });

    let outer_a_fv = heap.alloc_simple(OuterArcValue {
        inner: shared.clone(),
        outer_label: 1,
        items: vec![10, 20, 30],
    });
    heap.alloc_simple(OuterArcValue {
        inner: shared,
        outer_label: 2,
        items: vec![40, 50],
    });

    let heap_ref = heap.into_ref_named(TestHeapName::heap_name(
        "test_with_starlark_context_arc_storage",
    ));

    // SAFETY: `heap_ref` keeps the arena hosting `outer_a_fv` alive.
    let owned = unsafe { OwnedFrozenValue::new(heap_ref, outer_a_fv) };
    let restored =
        round_trip_owned_frozen_value_pagable_ser_de_impl(&owned).expect("round-trip via storage");

    // Partial deser: only the root `OuterArcValue` (and its transitive deps)
    // is materialized. The second `OuterArcValue`, which is unreachable from
    // the root, is never allocated.
    let drop_headers = restored.owner().collect_drop_headers_ordered();
    assert_eq!(drop_headers.len(), 1);
    let outer_a: &OuterArcValue = drop_headers[0].unpack().downcast_ref().unwrap();
    assert_eq!(outer_a.outer_label, 1);
    assert_eq!(outer_a.inner.label, 99);
    let resolved: &SimpleData = outer_a
        .inner
        .target
        .to_value()
        .downcast_ref::<SimpleData>()
        .expect("FrozenValue inside Arc<InnerArcData> should resolve");
    assert_eq!(resolved.count, 314);
}

/// In D104180037 we unified `SameHeapPtr` and `CrossHeapPtr` into a single
/// `HeapPtr { heap_id, .. }`. Under partial deser, a shared `Arc<T>` body
/// encoded under `HA`'s context can be decoded under `HB`'s — without an
/// explicit `heap_id` the embedded `FrozenValue` would misroute.
///
/// `HA` owns the target; `HB` refs `HA` and shares the `Arc<InnerArcData>`.
/// Serializing `OFV(HB)` encodes the Arc body under `HA` (walked first as
/// `HB`'s ref); deser of `HB` then checks the `FrozenValue` still lands in
/// `HA`.
#[test]
fn test_cross_heap_arc_dedup_explicit_heap_id_round_trip() -> crate::Result<()> {
    use pagable::storage::handle::PagableStorageHandle;
    use pagable::storage::in_memory::InMemoryPagableStorage;

    // HA: SimpleData target + OuterArcValue_A holding Arc<InnerArcData>.
    let heap_a = FrozenHeap::new();
    let target_fv = heap_a.alloc_simple(SimpleData {
        flag: true,
        count: 271,
    });
    let shared = Arc::new(InnerArcData {
        target: target_fv,
        label: 42,
    });
    heap_a.alloc_simple(OuterArcValue {
        inner: shared.clone(),
        outer_label: 1,
        items: vec![10, 20, 30],
    });
    let heap_a_ref = heap_a.into_ref_named(TestHeapName::heap_name(
        "test_cross_heap_arc_dedup_explicit_heap_id_HA",
    ));

    // HB: refs HA, holds OuterArcValue_B with a clone of the same Arc.
    let heap_b = FrozenHeap::new();
    heap_b.add_reference(&heap_a_ref);
    let outer_b_fv = heap_b.alloc_simple(OuterArcValue {
        inner: shared,
        outer_label: 2,
        items: vec![40, 50],
    });
    let heap_b_ref = heap_b.into_ref_named(TestHeapName::heap_name(
        "test_cross_heap_arc_dedup_explicit_heap_id_HB",
    ));

    // SAFETY: heap_b_ref keeps the arena hosting outer_b_fv alive.
    let ofv_b = unsafe { OwnedFrozenValue::new(heap_b_ref, outer_b_fv) };

    // Serializing OFV(HB, …) walks HB.refs first inside `page_out_item`, so HA
    // — and the InnerArcData Arc body reachable through HA — is encoded under
    // HA's serialize context before HB's body is written.
    let backing = InMemoryPagableStorage::new();
    let handle = PagableStorageHandle::new(backing.handle());
    let key_b = ser_owned_frozen_value_into_storage(&backing, &ofv_b)?;

    drop(ofv_b);
    drop(heap_a_ref);

    // Deserialize HB. The Arc body bytes — produced under HA's serialize
    // context — are decoded inside HB's deserialize. The explicit `heap_id`
    // must still route the embedded FrozenValue into HA's arena.
    let restored_b = deser_owned_frozen_value_from_storage(&backing, &handle, &key_b)?;
    let outer_b: &OuterArcValue = restored_b
        .value()
        .downcast_ref::<OuterArcValue>()
        .expect("restored_b root is OuterArcValue");
    assert_eq!(outer_b.outer_label, 2);
    assert_eq!(outer_b.items, vec![40, 50]);
    assert_eq!(outer_b.inner.label, 42);

    // HB's owner refs HA. Partial deser will have materialized HA's
    // SimpleData on demand when the InnerArcData was decoded; OuterArcValue_A
    // is unreachable from HB's root and intentionally stays unmaterialized.
    let hb_refs: Vec<_> = restored_b.owner().refs().collect();
    assert_eq!(hb_refs.len(), 1, "HB should retain its ref to HA");
    let restored_ha = hb_refs[0].clone();
    let ha_undrop = restored_ha.collect_undrop_headers_ordered();
    assert_eq!(
        ha_undrop.len(),
        1,
        "HA's SimpleData should be materialized on demand via the Arc body's FrozenValue",
    );
    let target_data: &SimpleData = ha_undrop[0].unpack().downcast_ref().unwrap();
    assert_eq!(target_data.flag, true);
    assert_eq!(target_data.count, 271);

    // The core post-fix invariant: even though the Arc body was encoded under
    // HA and decoded under HB, the embedded FrozenValue resolves into HA's
    // arena — because the wire carries an explicit `heap_id: HA` instead of
    // a context-dependent SameHeapPtr.
    let target_addr = ha_undrop[0] as *const _ as usize;
    assert_eq!(
        outer_b.inner.target.ptr_value().ptr_value_untagged(),
        target_addr,
        "FrozenValue cached inside the shared Arc must resolve into HA \
         even though HB is the decoder",
    );

    Ok(())
}

/// Partial deserialization proper: allocate multiple values, only one is the
/// root. After the round-trip via the storage path, only the reachable values
/// are materialized in the arena.
#[test]
fn test_partial_deser_skips_unreachable_values() -> crate::Result<()> {
    let heap = FrozenHeap::new();
    let reachable_fv = heap.alloc_simple(SimpleData {
        flag: true,
        count: 1,
    });
    // These two are allocated but unreachable from the root.
    heap.alloc_simple(SimpleData {
        flag: false,
        count: 99,
    });
    heap.alloc_simple(SimpleData {
        flag: true,
        count: 100,
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("partial_skip"));

    // SAFETY: `heap_ref` keeps the arena alive.
    let owned = unsafe { OwnedFrozenValue::new(heap_ref, reachable_fv) };
    let restored = round_trip_owned_frozen_value_pagable_ser_de_impl(&owned)?;

    let undrop = restored.owner().collect_undrop_headers_ordered();
    // Only the reachable SimpleData is materialized; the other two are skipped.
    assert_eq!(undrop.len(), 1);
    let data: &SimpleData = undrop[0].unpack().downcast_ref().unwrap();
    assert_eq!(data.flag, true);
    assert_eq!(data.count, 1);
    Ok(())
}

/// Partial deser materializes values in demand-walk order, which may differ
/// from the original allocation order. Source order is [SimpleData(target),
/// RefData(root)] — A is allocated first, then B which references A. The
/// root (B) materializes first via `ensure_initialized`, then B's `target`
/// resolution materializes A. The restored arena ends up [B, A], not [A, B].
#[test]
fn test_partial_deser_materializes_in_demand_order() -> crate::Result<()> {
    let heap = FrozenHeap::new();
    let target_fv = heap.alloc_simple(SimpleData {
        flag: true,
        count: 42,
    });
    let root_fv = heap.alloc_simple(RefData {
        label: 7,
        target: target_fv,
    });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("demand_order"));

    // SAFETY: `heap_ref` keeps the arena alive.
    let owned = unsafe { OwnedFrozenValue::new(heap_ref, root_fv) };
    let restored = round_trip_owned_frozen_value_pagable_ser_de_impl(&owned)?;

    let undrop = restored.owner().collect_undrop_headers_ordered();
    assert_eq!(undrop.len(), 2);
    // Demand-walk order: root B (RefData) was materialized first, then its
    // target A (SimpleData). Original allocation order was the reverse.
    let first: &RefData = undrop[0]
        .unpack()
        .downcast_ref::<RefData>()
        .expect("first header should be RefData (materialized first as the root)");
    let second: &SimpleData = undrop[1]
        .unpack()
        .downcast_ref::<SimpleData>()
        .expect("second header should be SimpleData (materialized via root's target)");
    assert_eq!(first.label, 7);
    assert_eq!(second.count, 42);

    // The root's `target` FrozenValue resolves to the SimpleData allocation.
    assert_eq!(
        first.target.ptr_value().ptr_value_untagged(),
        undrop[1] as *const _ as usize,
    );
    Ok(())
}

/// Serialize an OFV into the given shared storage, returning the top
/// `DataKey`. Companion to `deser_owned_frozen_value_from_storage`.
fn ser_owned_frozen_value_into_storage(
    backing: &pagable::storage::in_memory::InMemoryPagableStorage,
    owned: &OwnedFrozenValue,
) -> crate::Result<pagable::DataKey> {
    use pagable::storage::support::SerializerForPaging;

    let storage = backing.handle();
    let session_ctx = storage.session_context();

    let mut ser = SerializerForPaging::new(session_ctx);
    owned
        .pagable_serialize(&mut ser)
        .map_err(crate::Error::new_other)?;
    let (data, arcs) = ser.finish();
    let mut finished = dashmap::DashMap::new();
    let key = storage
        .page_out_item(data, arcs, &mut finished, session_ctx)
        .map_err(crate::Error::new_other)?;
    Ok(key)
}

/// Deserialize an OFV from the given shared storage by its top `DataKey`.
fn deser_owned_frozen_value_from_storage(
    backing: &pagable::storage::in_memory::InMemoryPagableStorage,
    handle: &pagable::storage::handle::PagableStorageHandle,
    top_key: &pagable::DataKey,
) -> crate::Result<OwnedFrozenValue> {
    use std::any::TypeId;

    use pagable::PagableDeserialize;
    use pagable::context::PagableDeserializerImpl;

    let top_data = backing
        .handle()
        .fetch_arc_or_data_blocking(&TypeId::of::<()>(), top_key)
        .map_err(crate::Error::new_other)?
        .right()
        .expect("top-level key should return data, not a cached arc");
    let mut de = PagableDeserializerImpl::new(&top_data.data, &top_data.arcs, handle);
    OwnedFrozenValue::pagable_deserialize(&mut de).map_err(crate::Error::new_other)
}

/// Multi-heap, multi-`OwnedFrozenValue` partial-deser, with incremental
/// state checks between deserialization steps:
///
/// Setup
///   dep_heap: SimpleData_d1 (count=1), SimpleData_d2 (count=2)
///   heap_a refs dep_heap; values: RefData_a → d1
///   heap_b refs dep_heap; values: RefData_b → d2
///   OFV1 = RefData_a
///   OFV2 = RefData_b
///   OFV3 = SimpleData_d1 directly (subset of what OFV1 already materializes)
///
/// Step 1 — deser OFV1
///   Expected materialization: heap_a has [RefData_a]; dep_heap has [d1].
///   d2 is allocated on the wire but not reachable from OFV1 → not materialized.
///
/// Step 2 — deser OFV2 (same shared storage / session)
///   Expected materialization: heap_b has [RefData_b]; dep_heap now has [d1, d2].
///   d1 was already materialized in step 1 → its allocation is reused (fast
///   path), no second copy. heap_b is added via the storage's arc cache (or
///   freshly fetched, depending on order).
///
/// Step 3 — deser OFV3 (subset: just SimpleData_d1, no extra work)
///   Expected materialization: nothing new. d1 is already materialized in
///   dep_heap; OFV3 just produces an `OwnedFrozenValue` pointing at the
///   already-allocated slot.
#[test]
fn test_multi_ofv_shared_heap_incremental_partial_deser() -> crate::Result<()> {
    use pagable::storage::handle::PagableStorageHandle;
    use pagable::storage::in_memory::InMemoryPagableStorage;

    // ---- 1. Build the source heaps. ----
    let dep_heap = FrozenHeap::new();
    let d1_fv = dep_heap.alloc_simple(SimpleData {
        flag: true,
        count: 1,
    });
    let d2_fv = dep_heap.alloc_simple(SimpleData {
        flag: false,
        count: 2,
    });
    let dep_heap_ref = dep_heap.into_ref_named(TestHeapName::heap_name("incr_dep"));

    let heap_a = FrozenHeap::new();
    heap_a.add_reference(&dep_heap_ref);
    let a_fv = heap_a.alloc_simple(RefData {
        label: 11,
        target: d1_fv,
    });
    let heap_a_ref = heap_a.into_ref_named(TestHeapName::heap_name("incr_a"));

    let heap_b = FrozenHeap::new();
    heap_b.add_reference(&dep_heap_ref);
    let b_fv = heap_b.alloc_simple(RefData {
        label: 22,
        target: d2_fv,
    });
    let heap_b_ref = heap_b.into_ref_named(TestHeapName::heap_name("incr_b"));

    // SAFETY: each heap_ref keeps its arena alive.
    let ofv1 = unsafe { OwnedFrozenValue::new(heap_a_ref, a_fv) };
    let ofv2 = unsafe { OwnedFrozenValue::new(heap_b_ref.clone(), b_fv) };
    let ofv3 = unsafe { OwnedFrozenValue::new(dep_heap_ref.clone(), d1_fv) };

    // ---- 2. Serialize all three into a SHARED storage. ----
    let backing = InMemoryPagableStorage::new();
    let handle = PagableStorageHandle::new(backing.handle());
    let key1 = ser_owned_frozen_value_into_storage(&backing, &ofv1)?;
    let key2 = ser_owned_frozen_value_into_storage(&backing, &ofv2)?;
    let key3 = ser_owned_frozen_value_into_storage(&backing, &ofv3)?;

    // Drop the original OFVs so the only path to the values is through deser.
    drop(ofv1);
    drop(ofv2);
    drop(ofv3);
    drop(dep_heap_ref);
    drop(heap_b_ref);

    // ---- 3. Step 1: deser OFV1, then inspect the heap state. ----
    let restored_ofv1 = deser_owned_frozen_value_from_storage(&backing, &handle, &key1)?;
    let a_data: &RefData = restored_ofv1
        .value()
        .downcast_ref::<RefData>()
        .expect("ofv1 root is RefData");
    assert_eq!(a_data.label, 11);

    // heap_a has exactly one materialized value (the root).
    let a_undrop = restored_ofv1.owner().collect_undrop_headers_ordered();
    assert_eq!(
        a_undrop.len(),
        1,
        "step1: heap_a should hold only RefData_a"
    );

    // dep_heap (reachable via heap_a.refs) has exactly one materialized
    // value: d1 (the target of the root). d2 is unreached.
    let dep_after_1 = {
        let refs: Vec<_> = restored_ofv1.owner().refs().collect();
        assert_eq!(refs.len(), 1, "step1: heap_a refs only dep_heap");
        refs[0].clone()
    };
    let dep_undrop_after_1 = dep_after_1.collect_undrop_headers_ordered();
    assert_eq!(
        dep_undrop_after_1.len(),
        1,
        "step1: dep_heap should hold only d1"
    );
    let d1_after_1: &SimpleData = dep_undrop_after_1[0].unpack().downcast_ref().unwrap();
    assert_eq!(d1_after_1.count, 1);

    let d1_ptr_after_1 = dep_undrop_after_1[0] as *const _ as usize;

    // ---- 4. Step 2: deser OFV2 from the SAME storage. ----
    let restored_ofv2 = deser_owned_frozen_value_from_storage(&backing, &handle, &key2)?;
    let b_data: &RefData = restored_ofv2
        .value()
        .downcast_ref::<RefData>()
        .expect("ofv2 root is RefData");
    assert_eq!(b_data.label, 22);

    // heap_b has only its root.
    let b_undrop = restored_ofv2.owner().collect_undrop_headers_ordered();
    assert_eq!(
        b_undrop.len(),
        1,
        "step2: heap_b should hold only RefData_b"
    );

    // The dep_heap arc returned to OFV2 is the SAME allocation as OFV1's
    // (the storage's arc cache deduplicates `Arc<FrozenFrozenHeap>` by
    // `DataKey`; `FrozenHeapRef::eq` is `Arc::ptr_eq`).
    let dep_after_2 = {
        let refs: Vec<_> = restored_ofv2.owner().refs().collect();
        assert_eq!(refs.len(), 1);
        refs[0].clone()
    };
    assert_eq!(
        dep_after_1, dep_after_2,
        "step2: dep_heap must be the same Arc allocation as in step1"
    );

    // Now dep_heap holds [d1, d2] — d2 was just materialized by OFV2.
    let dep_undrop_after_2 = dep_after_2.collect_undrop_headers_ordered();
    assert_eq!(
        dep_undrop_after_2.len(),
        2,
        "step2: dep_heap should now hold d1 (from step1) and d2 (new from step2)"
    );
    // d1 must be at the SAME address as before (fast-path reuse, no re-alloc).
    let d1_ptr_after_2 = dep_undrop_after_2[0] as *const _ as usize;
    assert_eq!(
        d1_ptr_after_1, d1_ptr_after_2,
        "step2: d1 must remain at the same allocation (fast path hit, no re-materialize)"
    );

    // OFV1's target FrozenValue still resolves to the same d1 address.
    assert_eq!(
        a_data.target.ptr_value().ptr_value_untagged(),
        d1_ptr_after_1
    );
    // OFV2's target resolves to d2 (the newly-materialized slot).
    let b_target: &SimpleData = b_data
        .target
        .to_value()
        .downcast_ref::<SimpleData>()
        .expect("b.target is SimpleData");
    assert_eq!(b_target.count, 2);

    // ---- 5. Step 3: deser OFV3 — strict subset of OFV1's materialization. ----
    let restored_ofv3 = deser_owned_frozen_value_from_storage(&backing, &handle, &key3)?;
    // OFV3's value points directly at d1 in dep_heap.
    let d3_target: &SimpleData = restored_ofv3
        .value()
        .downcast_ref::<SimpleData>()
        .expect("ofv3 root is SimpleData");
    assert_eq!(d3_target.count, 1);

    // OFV3 owns the dep_heap arc directly; arc-cache dedup means it's the
    // same Arc allocation.
    let dep_after_3 = restored_ofv3.owner().clone();
    assert_eq!(
        dep_after_1, dep_after_3,
        "step3: dep_heap must remain the same Arc allocation"
    );
    let dep_undrop_after_3 = dep_after_3.collect_undrop_headers_ordered();
    assert_eq!(
        dep_undrop_after_3.len(),
        2,
        "step3: no new materializations — still just [d1, d2]"
    );
    // OFV3's resolved address matches d1.
    assert_eq!(
        restored_ofv3.value().ptr_value().ptr_value_untagged(),
        d1_ptr_after_1
    );

    Ok(())
}

/// Cross-thread cycle test: two values on the same heap reference each other.
/// Two threads simultaneously deserialize one value each. Without cross-thread
/// cycle detection, this deadlocks (thread A waits for B's value, B waits for A's).
/// With cycle detection, the wait-for graph detects the cycle and returns a
/// sentinel pointer to break it.
#[test]
fn test_cross_thread_cycle_does_not_deadlock() {
    use std::sync::Barrier;
    use std::time::Duration;

    use pagable::PagableDeserialize;
    use pagable::context::PagableDeserializerImpl;
    use pagable::storage::handle::PagableStorageHandle;
    use pagable::storage::in_memory::InMemoryPagableStorage;
    use pagable::storage::support::SerializerForPaging;

    use crate::environment::Module;
    use crate::eval::Evaluator;
    use crate::syntax::AstModule;
    use crate::syntax::Dialect;

    let code = r#"
a = []
b = []
a.append(b)
b.append(a)
"#;

    let ast = AstModule::parse("cross_thread.star", code.to_owned(), &Dialect::Extended).unwrap();
    let globals = GlobalsBuilder::new().build_named(GlobalFrozenHeapName {
        name: "cross_thread",
    });
    let frozen_module = Module::with_temp_heap(|module| {
        {
            let mut eval = Evaluator::new(&module);
            eval.eval_module(ast, &globals).unwrap();
        }
        module.freeze_named(TestHeapName::heap_name("cross_thread_cycle"))
    })
    .unwrap();

    let ofv_a = frozen_module.get("a").unwrap();
    let ofv_b = frozen_module.get("b").unwrap();

    let backing = InMemoryPagableStorage::new();
    let storage = backing.handle();
    let session_ctx = storage.session_context();

    // Serialize both OwnedFrozenValues.
    let ser_one = |ofv: &OwnedFrozenValue| -> crate::Result<pagable::DataKey> {
        let mut ser = SerializerForPaging::new(session_ctx);
        ofv.pagable_serialize(&mut ser)
            .map_err(crate::Error::new_other)?;
        let (data, arcs) = ser.finish();
        let mut finished = dashmap::DashMap::new();
        storage
            .page_out_item(data, arcs, &mut finished, session_ctx)
            .map_err(crate::Error::new_other)
    };
    let key_a = ser_one(&ofv_a).unwrap();
    let key_b = ser_one(&ofv_b).unwrap();

    let handle = Arc::new(PagableStorageHandle::new(storage.clone()));
    let barrier = Arc::new(Barrier::new(2));

    let deser_one = |key: pagable::DataKey,
                     handle: Arc<PagableStorageHandle>,
                     barrier: Arc<Barrier>,
                     storage: Arc<dyn pagable::storage::traits::PagableStorage>|
     -> std::thread::JoinHandle<crate::Result<()>> {
        std::thread::spawn(move || {
            let top_data = storage
                .fetch_data_blocking(&key)
                .map_err(crate::Error::new_other)?;

            // Synchronize so both threads enter deserialization at the same time.
            barrier.wait();

            let mut de = PagableDeserializerImpl::new(&top_data.data, &top_data.arcs, &handle);
            let _restored =
                OwnedFrozenValue::pagable_deserialize(&mut de).map_err(crate::Error::new_other)?;
            Ok(())
        })
    };

    let h_a = deser_one(key_a, handle.clone(), barrier.clone(), storage.clone());
    let h_b = deser_one(key_b, handle.clone(), barrier.clone(), storage.clone());

    // Use recv_timeout to detect deadlock.
    let (tx, rx) = std::sync::mpsc::channel();
    let tx2 = tx.clone();
    std::thread::spawn(move || {
        let r_a = h_a.join().expect("thread A panicked");
        tx.send(r_a).ok();
    });
    std::thread::spawn(move || {
        let r_b = h_b.join().expect("thread B panicked");
        tx2.send(r_b).ok();
    });

    rx.recv_timeout(Duration::from_secs(10))
        .expect("cross-thread cycle test deadlocked")
        .unwrap_or_else(|e| panic!("deser thread failed: {:#}", e));
}

// ---- Ser/deser micro-benchmarks ----

struct BenchResult {
    label: &'static str,
    count: usize,
    ser_us: u128,
    deser_us: u128,
    bytes: usize,
}

impl BenchResult {
    fn print(&self) {
        eprintln!(
            "  {:<30} n={:<6} ser={:>8}µs ({:>6}µs/v)  deser={:>8}µs ({:>6}µs/v)  bytes={:>8} ({:>4}B/v)",
            self.label,
            self.count,
            self.ser_us,
            self.ser_us / self.count as u128,
            self.deser_us,
            self.deser_us / self.count as u128,
            self.bytes,
            self.bytes / self.count,
        );
    }
}

fn bench_owned_frozen_value_round_trip(
    label: &'static str,
    heap_ref: FrozenHeapRef,
    root: FrozenValue,
    count: usize,
) -> crate::Result<BenchResult> {
    use std::any::TypeId;
    use std::time::Instant;

    use pagable::PagableDeserialize;
    use pagable::context::PagableDeserializerImpl;
    use pagable::storage::handle::PagableStorageHandle;
    use pagable::storage::in_memory::InMemoryPagableStorage;
    use pagable::storage::support::SerializerForPaging;

    let owned = unsafe { OwnedFrozenValue::new(heap_ref, root) };

    let backing = InMemoryPagableStorage::new();
    let storage = backing.handle();
    let handle = PagableStorageHandle::new(storage.clone());
    let session_ctx = storage.session_context();

    let ser_start = Instant::now();
    let mut ser = SerializerForPaging::new(session_ctx);
    owned
        .pagable_serialize(&mut ser)
        .map_err(crate::Error::new_other)?;
    let (data, arcs) = ser.finish();
    let ser_us = ser_start.elapsed().as_micros();
    let bytes = data.len();

    let top_key = {
        let mut finished = dashmap::DashMap::new();
        storage
            .page_out_item(data, arcs, &mut finished, session_ctx)
            .map_err(crate::Error::new_other)?
    };

    let top_data = storage
        .fetch_arc_or_data_blocking(&TypeId::of::<()>(), &top_key)
        .map_err(crate::Error::new_other)?
        .right()
        .expect("should be data");

    let deser_start = Instant::now();
    let mut de = PagableDeserializerImpl::new(&top_data.data, &top_data.arcs, &handle);
    let _restored =
        OwnedFrozenValue::pagable_deserialize(&mut de).map_err(crate::Error::new_other)?;
    let deser_us = deser_start.elapsed().as_micros();

    Ok(BenchResult {
        label,
        count,
        ser_us,
        deser_us,
        bytes,
    })
}

#[test]
fn bench_pagable_ser_deser_by_value_type() -> crate::Result<()> {
    eprintln!();
    eprintln!("=== Pagable ser/deser micro-benchmark ===");

    // SimpleData: primitive fields
    {
        let n = 5000;
        let heap = FrozenHeap::new();
        let mut root = FrozenValue::new_none();
        for i in 0..n {
            root = heap.alloc_simple(SimpleData {
                flag: i % 2 == 0,
                count: i,
            });
        }
        let heap_ref = heap.into_ref_named(TestHeapName::heap_name("bench_simple"));
        bench_owned_frozen_value_round_trip("SimpleData(bool,usize)", heap_ref, root, n)?.print();
    }

    // HeapData: Vec, String, Box
    {
        let n = 5000;
        let heap = FrozenHeap::new();
        let mut root = FrozenValue::new_none();
        for i in 0..n {
            root = heap.alloc_simple(HeapData {
                items: vec![i as u32; 10],
                label: format!("label_{i}"),
                boxed: Box::new(i as i64 * 100),
            });
        }
        let heap_ref = heap.into_ref_named(TestHeapName::heap_name("bench_heap_data"));
        bench_owned_frozen_value_round_trip("HeapData(Vec,String,Box)", heap_ref, root, n)?.print();
    }

    // FrozenStr: strings of various sizes
    for &str_len in &[10, 100, 1000] {
        let n = 2000;
        let heap = FrozenHeap::new();
        let s: String = "x".repeat(str_len);
        let mut root = FrozenValue::new_none();
        for _ in 0..n {
            root = heap.alloc_str(&s).to_frozen_value();
        }
        let heap_ref =
            heap.into_ref_named(TestHeapName::heap_name(&format!("bench_str_{str_len}")));
        let label = match str_len {
            10 => "FrozenStr(len=10)",
            100 => "FrozenStr(len=100)",
            1000 => "FrozenStr(len=1000)",
            _ => unreachable!(),
        };
        bench_owned_frozen_value_round_trip(label, heap_ref, root, n)?.print();
    }

    // Module eval: expressions producing ints and strings
    {
        use crate::environment::FrozenModule;
        use crate::environment::Module;
        use crate::eval::Evaluator;
        use crate::syntax::AstModule;
        use crate::syntax::Dialect;

        let mut lines = Vec::new();
        let n = 500;
        for i in 0..n {
            lines.push(format!("v{i} = {i} + 1"));
        }
        let code = lines.join("\n");

        let ast = AstModule::parse("bench_module.star", code, &Dialect::Extended)?;
        let globals =
            GlobalsBuilder::new().build_named(GlobalFrozenHeapName { name: "bench_eval" });
        let frozen_module = Module::with_temp_heap(|module| {
            {
                let mut eval = Evaluator::new(&module);
                eval.eval_module(ast, &globals).unwrap();
            }
            module.freeze_named(TestHeapName::heap_name("bench_module_eval"))
        })?;

        use std::any::TypeId;
        use std::time::Instant;

        use pagable::PagableDeserialize;
        use pagable::context::PagableDeserializerImpl;
        use pagable::storage::handle::PagableStorageHandle;
        use pagable::storage::in_memory::InMemoryPagableStorage;
        use pagable::storage::support::SerializerForPaging;

        let backing = InMemoryPagableStorage::new();
        let storage = backing.handle();
        let handle = PagableStorageHandle::new(storage.clone());
        let session_ctx = storage.session_context();

        let ser_start = Instant::now();
        let mut ser = SerializerForPaging::new(session_ctx);
        frozen_module
            .pagable_serialize(&mut ser)
            .map_err(crate::Error::new_other)?;
        let (data, arcs) = ser.finish();
        let ser_us = ser_start.elapsed().as_micros();
        let bytes = data.len();

        let top_key = {
            let mut finished = dashmap::DashMap::new();
            storage
                .page_out_item(data, arcs, &mut finished, session_ctx)
                .map_err(crate::Error::new_other)?
        };

        let top_data = storage
            .fetch_arc_or_data_blocking(&TypeId::of::<()>(), &top_key)
            .map_err(crate::Error::new_other)?
            .right()
            .expect("should be data");

        let deser_start = Instant::now();
        let mut de = PagableDeserializerImpl::new(&top_data.data, &top_data.arcs, &handle);
        let restored =
            FrozenModule::pagable_deserialize(&mut de).map_err(crate::Error::new_other)?;
        let deser_us = deser_start.elapsed().as_micros();

        // Access one value to confirm correctness
        let v0 = restored.get("v0")?.value().unpack_i32().unwrap();
        assert_eq!(v0, 1);

        BenchResult {
            label: "Module(500 int assigns)",
            count: n,
            ser_us,
            deser_us,
            bytes,
        }
        .print();
    }

    eprintln!("=== done ===");
    eprintln!();

    Ok(())
}

// ============================================================================
// Regression: concurrent page-in must not hash a not-yet-deserialized value.
//
// A `SmallMapFvData` is keyed by a `GateKey` whose deserialization blocks on a
// global gate. Thread A claims the key and blocks mid-deserialization; thread B
// then deserializes the map, which must hash that key. Before the fix, B got the
// not-yet-materialized (sentinel) pointer and hashing it panicked with
// "accessing a frozen value that has not been deserialized yet". With the fix B
// blocks until the key is materialized, then hashes the real value.
//
// The gate makes the cross-thread collision deterministic: B is only started
// once A has signalled it is blocked inside the key's deser, so the key is
// guaranteed in-progress when B reaches it.
// ============================================================================

/// Channels used by [`GateField`] to block a key's deserialization until the
/// test releases it.
struct GateChannels {
    /// Sent from inside the key's deser once this thread has claimed the key and
    /// is about to block.
    started: std::sync::mpsc::Sender<()>,
    /// The key's deser blocks on this until the test releases it.
    release: std::sync::mpsc::Receiver<()>,
}

static GATE: std::sync::Mutex<Option<GateChannels>> = std::sync::Mutex::new(None);

/// A unit field whose deserialization blocks on [`GATE`] (one-shot). Embedding it
/// in `GateKey` keeps that key's slot in-progress while another thread races to
/// hash it.
#[derive(Debug, Allocative)]
struct GateField;

impl crate::pagable::StarlarkSerialize for GateField {
    fn starlark_serialize(
        &self,
        _ctx: &mut dyn crate::pagable::StarlarkSerializeContext,
    ) -> crate::Result<()> {
        Ok(())
    }
}

impl crate::pagable::StarlarkDeserialize for GateField {
    fn starlark_deserialize(
        _ctx: &mut dyn crate::pagable::StarlarkDeserializeContext<'_>,
    ) -> crate::Result<Self> {
        // One-shot: only the first deserialization (the claimer) blocks.
        if let Some(ch) = GATE.lock().unwrap().take() {
            ch.started.send(()).ok();
            ch.release.recv().ok();
        }
        Ok(GateField)
    }
}

/// Hashable key whose deserialization blocks (via [`GateField`]).
#[derive(
    Debug,
    Display,
    Allocative,
    ProvidesStaticType,
    NoSerialize,
    StarlarkPagable
)]
#[display("GateKey({})", self.id)]
struct GateKey {
    gate: GateField,
    id: u32,
}

starlark_simple_value!(GateKey);

#[starlark_value(type = "GateKey")]
impl<'v> StarlarkValue<'v> for GateKey {
    type Canonical = Self;

    fn write_hash(&self, hasher: &mut crate::collections::StarlarkHasher) -> crate::Result<()> {
        use std::hash::Hasher;
        hasher.write_u32(self.id);
        Ok(())
    }

    fn equals(&self, other: crate::values::Value<'v>) -> crate::Result<bool> {
        Ok(other
            .downcast_ref::<GateKey>()
            .is_some_and(|o| o.id == self.id))
    }
}

/// Cross-thread regression. Thread A claims a `GateKey` and blocks inside its
/// deserialization (via the gate), so the key's slot stays in-progress. Thread B
/// then deserializes a `SmallMapFvData` keyed by it, which must hash that key.
/// Pre-fix B took the not-yet-materialized (sentinel) key and panicked hashing
/// it; with the fix B waits for the slot, then hashes the real value. The
/// `started`/`release` channels make the collision deterministic: B starts only
/// after A is confirmed blocked, and A is released only after B has reached the
/// key.
#[test]
fn test_concurrent_page_in_does_not_hash_sentinel_key() {
    use std::sync::mpsc;
    use std::time::Duration;

    use dupe::Dupe;
    use pagable::context::PagableDeserializerImpl;
    use pagable::storage::handle::PagableStorageHandle;
    use pagable::storage::in_memory::InMemoryPagableStorage;
    use pagable::storage::support::SerializerForPaging;
    use starlark_map::small_map::SmallMap;

    // Heap: a gated key, a value, and a map keyed by the gated key.
    let heap = FrozenHeap::new();
    let key_fv = heap.alloc_simple(GateKey {
        gate: GateField,
        id: 7,
    });
    let val_fv = heap.alloc_simple(SimpleData {
        flag: true,
        count: 42,
    });
    let mut entries = SmallMap::new();
    entries.insert_hashed(key_fv.get_hashed().unwrap(), val_fv);
    let map_fv = heap.alloc_simple(SmallMapFvData { entries });
    let heap_ref = heap.into_ref_named(TestHeapName::heap_name("gated_key"));

    // Two roots from the same heap: the key alone, and the map.
    // SAFETY: `heap_ref` owns the arena hosting both values.
    let ofv_key = unsafe { OwnedFrozenValue::new(heap_ref.dupe(), key_fv) };
    let ofv_map = unsafe { OwnedFrozenValue::new(heap_ref.dupe(), map_fv) };

    // Serialize each root through the production storage path.
    let backing = InMemoryPagableStorage::new();
    let storage = backing.handle();
    let session_ctx = storage.session_context();
    let ser_one = |ofv: &OwnedFrozenValue| -> pagable::DataKey {
        let mut ser = SerializerForPaging::new(session_ctx);
        ofv.pagable_serialize(&mut ser).unwrap();
        let (data, arcs) = ser.finish();
        let mut finished = dashmap::DashMap::new();
        storage
            .page_out_item(data, arcs, &mut finished, session_ctx)
            .unwrap()
    };
    let key_data = ser_one(&ofv_key);
    let map_data = ser_one(&ofv_map);

    // Install the gate so the key's deserialization blocks.
    let (started_tx, started_rx) = mpsc::channel();
    let (release_tx, release_rx) = mpsc::channel();
    *GATE.lock().unwrap() = Some(GateChannels {
        started: started_tx,
        release: release_rx,
    });

    let handle = Arc::new(PagableStorageHandle::new(storage.clone()));

    let deser_one = |key: pagable::DataKey,
                     handle: Arc<PagableStorageHandle>,
                     storage: Arc<dyn pagable::storage::traits::PagableStorage>|
     -> std::thread::JoinHandle<crate::Result<()>> {
        std::thread::spawn(move || {
            let top = storage
                .fetch_data_blocking(&key)
                .map_err(crate::Error::new_other)?;
            let mut de = PagableDeserializerImpl::new(&top.data, &top.arcs, &handle);
            OwnedFrozenValue::pagable_deserialize(&mut de).map_err(crate::Error::new_other)?;
            Ok(())
        })
    };

    // Thread A: deserialize the key. It claims the key and blocks inside GateField.
    let h_a = deser_one(key_data, handle.clone(), storage.clone());
    // Wait until A is blocked inside the key's deser (key is now in-progress).
    started_rx.recv().expect("key deser never started");

    // Thread B: deserialize the map, which must hash the (in-progress) key.
    let h_b = deser_one(map_data, handle.clone(), storage.clone());

    // Give B time to reach the key — pre-fix it hashes the sentinel and panics
    // here; post-fix it blocks on the slot — then release A so the key
    // materializes (waking a post-fix waiter).
    std::thread::sleep(Duration::from_millis(500));
    release_tx.send(()).ok();

    h_a.join().expect("thread A panicked").unwrap();
    match h_b.join() {
        Ok(r) => r.unwrap_or_else(|e| panic!("map deser failed: {e:#}")),
        // Pre-fix: B panicked hashing the sentinel key — re-raise so the test fails.
        Err(panic) => std::panic::resume_unwind(panic),
    }

    *GATE.lock().unwrap() = None;
}
