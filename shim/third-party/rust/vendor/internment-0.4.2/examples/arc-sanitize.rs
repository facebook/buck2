use internment::{ArcIntern, Intern, LocalIntern};

fn main() {
    println!("^^^^^^^^^^^^^^^^^^^^^^");
    Intern::new(0i64);
    LocalIntern::new(0i64);
    ArcIntern::new(0i64);
    arc_interner::ArcIntern::new(0i64);
    let mut threads = Vec::new();
    for _ in 1..10000 {
        threads.push(std::thread::spawn(|| {
            ArcIntern::new(1i64);
        }));
        threads.push(std::thread::spawn(|| {
            let _x = ArcIntern::new(1i64);
            std::thread::sleep(std::time::Duration::from_nanos(1));
        }));
    }
    for x in threads.into_iter() {
        x.join().unwrap();
    }
    println!("moving on");
    use std::thread;
    let mut thandles = vec![];
    for _i in 0..10 {
        thandles.push(thread::spawn(|| {
            for _i in 0..100_000 {
                let _interned1 = ArcIntern::new("foo".to_string());
                let _interned2 = ArcIntern::new("bar".to_string());
            }
        }));
    }
    for h in thandles.into_iter() {
        h.join().unwrap();
    }
    println!("and finally");
    #[derive(Eq, PartialEq, Hash)]
    pub struct TestStruct(String, u64);

    let mut thandles = vec![];
    for _i in 0..10 {
        thandles.push(thread::spawn(|| {
            for _i in 0..100_000 {
                let _interned1 = ArcIntern::new(TestStruct("foo".to_string(), 5));
                let _interned2 = ArcIntern::new(TestStruct("bar".to_string(), 10));
            }
        }));
    }
    for h in thandles.into_iter() {
        h.join().unwrap()
    }

    println!("vvvvvvvvvvvvvvvvvvvvv");
    println!("all good\n\n\n\n");
}
