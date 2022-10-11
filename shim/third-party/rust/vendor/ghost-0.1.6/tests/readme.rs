mod first {
    use ghost::phantom;

    #[phantom]
    struct MyPhantom<T: ?Sized>;

    #[test]
    fn test() {
        // Proof that MyPhantom behaves like PhantomData.
        let _: MyPhantom<u8> = MyPhantom::<u8>;
        assert_eq!(0, std::mem::size_of::<MyPhantom<u8>>());
    }

    // Proof that MyPhantom is not just a re-export of PhantomData.
    // If it were a re-export, these would be conflicting impls.
    trait Trait {}
    impl<T> Trait for std::marker::PhantomData<T> {}
    impl<T> Trait for MyPhantom<T> {}

    // Proof that MyPhantom is local to the current crate.
    impl<T> MyPhantom<T> {}
}

mod second {
    use ghost::phantom;

    #[phantom]
    #[derive(Copy, Clone, Default, Hash, PartialOrd, Ord, PartialEq, Eq, Debug)]
    struct Crazy<'a, V: 'a, T>
    where
        &'a V: IntoIterator<Item = T>;

    #[test]
    fn test() {
        let _ = Crazy::<'static, Vec<String>, &'static String>;

        // Lifetime elision.
        let crazy = Crazy::<Vec<String>, &String>;
        println!("{:?}", crazy);
    }
}
