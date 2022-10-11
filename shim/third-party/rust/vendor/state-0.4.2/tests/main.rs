extern crate state;

use std::sync::{Arc, RwLock};

// Tiny structures to test that dropping works as expected.
struct DroppingStruct(Arc<RwLock<bool>>);
struct DroppingStructWrap(DroppingStruct);

impl Drop for DroppingStruct {
    fn drop(&mut self) {
        *self.0.write().unwrap() = true;
    }
}

// Ensure our DroppingStruct works as intended.
#[test]
fn test_dropping_struct() {
    let drop_flag = Arc::new(RwLock::new(false));
    let dropping_struct = DroppingStruct(drop_flag.clone());
    drop(dropping_struct);
    assert_eq!(*drop_flag.read().unwrap(), true);

    let drop_flag = Arc::new(RwLock::new(false));
    let dropping_struct = DroppingStruct(drop_flag.clone());
    let wrapper = DroppingStructWrap(dropping_struct);
    drop(wrapper);
    assert_eq!(*drop_flag.read().unwrap(), true);
}

mod container_tests {
    use super::{DroppingStruct, DroppingStructWrap};
    use super::state::Container;
    use std::sync::{Arc, RwLock};
    use std::thread;

    // We use one `CONTAINER` to get an implicit test since each `test` runs in
    // a different thread. This means we have to `set` different types in each
    // test if we want the `set` to succeed.
    static CONTAINER: Container = Container::new();

    #[test]
    fn simple_set_get() {
        assert!(CONTAINER.set(1u32));
        assert_eq!(*CONTAINER.get::<u32>(), 1);
    }

    #[test]
    fn dst_set_get() {
        assert!(CONTAINER.set::<[u32; 4]>([1, 2, 3, 4u32]));
        assert_eq!(*CONTAINER.get::<[u32; 4]>(), [1, 2, 3, 4]);
    }

    #[test]
    fn set_get_remote() {
        thread::spawn(|| {
            CONTAINER.set(10isize);
        }).join().unwrap();

        assert_eq!(*CONTAINER.get::<isize>(), 10);
    }

    #[test]
    fn two_put_get() {
        assert!(CONTAINER.set("Hello, world!".to_string()));

        let s_old = CONTAINER.get::<String>();
        assert_eq!(s_old, "Hello, world!");

        assert!(!CONTAINER.set::<String>("Bye bye!".into()));
        assert_eq!(CONTAINER.get::<String>(), "Hello, world!");
        assert_eq!(CONTAINER.get::<String>(), s_old);
    }

    #[test]
    fn many_puts_only_one_succeeds() {
        let mut threads = vec![];
        for _ in 0..1000 {
            threads.push(thread::spawn(|| {
                CONTAINER.set(10i64)
            }))
        }

        let results: Vec<bool> = threads.into_iter().map(|t| t.join().unwrap()).collect();
        assert_eq!(results.into_iter().filter(|&b| b).count(), 1);
        assert_eq!(*CONTAINER.get::<i64>(), 10);
    }

    // Ensure setting when already set doesn't cause a drop.
    #[test]
    fn test_no_drop_on_set() {
        let drop_flag = Arc::new(RwLock::new(false));
        let dropping_struct = DroppingStruct(drop_flag.clone());

        let _drop_flag_ignore = Arc::new(RwLock::new(false));
        let _dropping_struct_ignore = DroppingStruct(_drop_flag_ignore.clone());

        CONTAINER.set::<DroppingStruct>(dropping_struct);
        assert!(!CONTAINER.set::<DroppingStruct>(_dropping_struct_ignore));
        assert_eq!(*drop_flag.read().unwrap(), false);
    }

    // Ensure dropping a container drops its contents.
    #[test]
    fn drop_inners_on_drop() {
        let drop_flag_a = Arc::new(RwLock::new(false));
        let dropping_struct_a = DroppingStruct(drop_flag_a.clone());

        let drop_flag_b = Arc::new(RwLock::new(false));
        let dropping_struct_b = DroppingStructWrap(DroppingStruct(drop_flag_b.clone()));

        {
            let container = Container::new();
            container.set(dropping_struct_a);
            assert_eq!(*drop_flag_a.read().unwrap(), false);

            container.set(dropping_struct_b);
            assert_eq!(*drop_flag_a.read().unwrap(), false);
            assert_eq!(*drop_flag_b.read().unwrap(), false);
        }

        assert_eq!(*drop_flag_a.read().unwrap(), true);
        assert_eq!(*drop_flag_b.read().unwrap(), true);
    }
}

#[cfg(feature = "tls")]
mod container_tests_tls {
    use std::sync::{Arc, Barrier};
    use super::state::Container;
    use std::cell::Cell;
    use std::thread;

    // We use one `CONTAINER` to get an implicit test since each `test` runs in
    // a different thread. This means we have to `set` different types in each
    // test if we want the `set` to succeed.
    static CONTAINER: Container = Container::new();

    #[test]
    fn test_simple() {
        assert!(CONTAINER.try_get_local::<u32>().is_none());
        assert!(CONTAINER.set_local(|| 1u32));
        assert_eq!(*CONTAINER.get_local::<u32>(), 1);
    }

    #[test]
    fn test_double_put() {
        assert!(CONTAINER.set_local(|| 1i32));
        assert!(!CONTAINER.set_local(|| 1i32));
    }

    #[test]
    fn not_unique_when_sent() {
        assert!(CONTAINER.set_local(|| 1i64));
        let value = CONTAINER.get_local::<i64>();

        thread::spawn(move || {
            assert_eq!(*value, 1i64);
        }).join().expect("Panic.");
    }

    #[test]
    fn container_tls_really_is_tls() {
        const THREADS: usize = 50;

        let barriers = Arc::new((Barrier::new(THREADS), Barrier::new(THREADS)));
        assert!(CONTAINER.set_local(|| Cell::new(0u8)));

        let mut threads = vec![];
        for i in 1..THREADS {
            let barriers = barriers.clone();
            threads.push(thread::spawn(move || {
                barriers.0.wait();
                assert_eq!(CONTAINER.get_local::<Cell<u8>>().get(), 0);
                CONTAINER.get_local::<Cell<u8>>().set(i as u8);
                let v = CONTAINER.get_local::<Cell<u8>>().get();
                barriers.1.wait();
                v
            }));
        }

        barriers.0.wait();
        CONTAINER.get_local::<Cell<u8>>().set(0);
        barriers.1.wait();

        let vals = threads.into_iter().map(|t| t.join().unwrap()).collect::<Vec<_>>();
        for (i, val) in vals.into_iter().enumerate() {
            assert_eq!((i + 1) as u8, val);
        }

        assert_eq!(CONTAINER.get_local::<Cell<u8>>().get(), 0);
    }

    #[test]
    fn container_tls_really_is_tls_take_2() {
        thread::spawn(|| {
            assert!(CONTAINER.set_local(|| Cell::new(1i8)));
            CONTAINER.get_local::<Cell<i8>>().set(2);

            thread::spawn(|| {
                assert_eq!(CONTAINER.get_local::<Cell<i8>>().get(), 1);
            }).join().expect("inner join");
        }).join().expect("outer join");
    }
}

mod storage_tests {
    use super::DroppingStruct;
    use super::state::Storage;
    use std::sync::{Arc, RwLock};
    use std::thread;

    #[test]
    fn simple_put_get() {
        static STORAGE: Storage<u32> = Storage::new();

        assert!(STORAGE.set(10));
        assert_eq!(*STORAGE.get(), 10);
    }

    #[test]
    fn no_double_put() {
        static STORAGE: Storage<u32> = Storage::new();

        assert!(STORAGE.set(1));
        assert!(!STORAGE.set(5));
        assert_eq!(*STORAGE.get(), 1);
    }

    #[test]
    fn many_puts_only_one_succeeds() {
        static STORAGE: Storage<u32> = Storage::new();

        let mut threads = vec![];
        for _ in 0..1000 {
            threads.push(thread::spawn(|| {
                let was_set = STORAGE.set(10);
                assert_eq!(*STORAGE.get(), 10);
                was_set
            }))
        }

        let results: Vec<bool> = threads.into_iter().map(|t| t.join().unwrap()).collect();
        assert_eq!(results.into_iter().filter(|&b| b).count(), 1);
        assert_eq!(*STORAGE.get(), 10);
    }

    #[test]
    fn dst_set_get() {
        static STORAGE: Storage<[u32; 4]> = Storage::new();

        assert!(STORAGE.set([1, 2, 3, 4]));
        assert_eq!(*STORAGE.get(), [1, 2, 3, 4]);
    }

    // Ensure dropping a `Storage<T>` drops `T`.
    #[test]
    fn drop_inners_on_drop() {
        let drop_flag = Arc::new(RwLock::new(false));
        let dropping_struct = DroppingStruct(drop_flag.clone());

        {
            let storage = Storage::new();
            assert!(storage.set(dropping_struct));
            assert_eq!(*drop_flag.read().unwrap(), false);
        }

        assert_eq!(*drop_flag.read().unwrap(), true);
    }

    #[test]
    fn clone() {
        let storage: Storage<u32> = Storage::new();
        assert!(storage.try_get().is_none());

        let storage_clone = storage.clone();
        assert!(storage_clone.try_get().is_none());

        assert!(storage.set(10));
        let storage_clone = storage.clone();
        assert_eq!(*storage_clone.get(), 10);
    }
}

#[cfg(feature = "tls")]
mod storage_tests_tls {
    use super::state::LocalStorage;

    use std::thread;
    use std::cell::Cell;
    use std::sync::{Arc, Barrier};

    #[test]
    fn simple_put_get() {
        static STORAGE: LocalStorage<u32> = LocalStorage::new();

        assert!(STORAGE.set(|| 10));
        assert_eq!(*STORAGE.get(), 10);
    }

    #[test]
    fn no_double_put() {
        static STORAGE: LocalStorage<u32> = LocalStorage::new();

        assert!(STORAGE.set(|| 1));
        assert!(!STORAGE.set(|| 5));
        assert_eq!(*STORAGE.get(), 1);
    }

    #[test]
    fn many_puts_only_one_succeeds() {
        static STORAGE: LocalStorage<u32> = LocalStorage::new();

        let mut threads = vec![];
        for _ in 0..1000 {
            threads.push(thread::spawn(|| {
                let was_set = STORAGE.set(|| 10);
                assert_eq!(*STORAGE.get(), 10);
                was_set
            }))
        }

        let results: Vec<bool> = threads.into_iter().map(|t| t.join().unwrap()).collect();
        assert_eq!(results.into_iter().filter(|&b| b).count(), 1);
        assert_eq!(*STORAGE.get(), 10);
    }

    #[test]
    fn storage_tls_really_is_tls() {
        const THREADS: usize = 50;
        static STORAGE: LocalStorage<Cell<u8>> = LocalStorage::new();

        let barriers = Arc::new((Barrier::new(THREADS), Barrier::new(THREADS)));
        assert!(STORAGE.set(|| Cell::new(0)));

        let mut threads = vec![];
        for i in 1..50 {
            let barriers = barriers.clone();
            threads.push(thread::spawn(move || {
                barriers.0.wait();
                STORAGE.get().set(i);
                let val = STORAGE.get().get();
                barriers.1.wait();
                val
            }));
        }

        barriers.0.wait();
        STORAGE.get().set(0);
        barriers.1.wait();

        let vals = threads.into_iter().map(|t| t.join().unwrap()).collect::<Vec<_>>();
        for (i, val) in vals.into_iter().enumerate() {
            assert_eq!((i + 1) as u8, val);
        }

        assert_eq!(STORAGE.get().get(), 0);
    }

    #[test]
    fn storage_tls_really_is_tls_take_2() {
        static STORAGE: LocalStorage<Cell<u8>> = LocalStorage::new();

        thread::spawn(|| {
            assert!(STORAGE.set(|| Cell::new(1)));
            STORAGE.get().set(2);

            thread::spawn(|| {
                assert_eq!(STORAGE.get().get(), 1);
            }).join().expect("inner join");
        }).join().expect("outer join");
    }
}
