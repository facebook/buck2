/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#[allow(unused)]
mod inner {
    use crate as pagable;
    // don't add any more use statements here, we want to make sure that the derive macro doesn't require any to be in scope

    #[derive(crate::Pagable, Eq, PartialEq, Debug)]
    pub(super) struct Test {
        xxx: String,
        yyy: String,
        #[pagable(flatten_serde)]
        flatten_serde: Serde,
    }

    #[derive(crate::Pagable, Eq, PartialEq, Debug)]
    enum TestEnum {
        Unit,
        Named { name1: String, name2: String },
        Unnamed(String, String),
    }

    #[derive(serde::Serialize, serde::Deserialize, Eq, PartialEq, Debug)]
    struct Serde(String);

    #[derive(crate::Pagable, Eq, PartialEq, Debug)]
    struct WithDiscard {
        #[pagable(discard = "Default::default()")]
        discard: String,
        keep: String,
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use crate::testing::TestingDeserializer;
        use crate::testing::TestingSerializer;
        use crate::traits::PagableDeserialize;
        use crate::traits::PagableSerialize;

        #[test]
        fn test_roundtrip() -> crate::Result<()> {
            let t1 = Test {
                xxx: "xxx".to_owned(),
                yyy: "yyy".to_owned(),
                flatten_serde: Serde("flatten".to_owned()),
            };
            let mut serializer = TestingSerializer::new();
            t1.pagable_serialize(&mut serializer)?;
            let (bytes, ptrs) = serializer.finish();
            let mut deserializer = TestingDeserializer::new(&bytes, ptrs);
            let t2 = Test::pagable_deserialize(&mut deserializer)?;
            assert_eq!(t1, t2);
            Ok(())
        }

        #[test]
        fn test_discard() -> crate::Result<()> {
            let t1 = WithDiscard {
                discard: "discard".to_owned(),
                keep: "keep".to_owned(),
            };
            let mut serializer = TestingSerializer::new();
            t1.pagable_serialize(&mut serializer)?;
            let (bytes, ptrs) = serializer.finish();
            let mut deserializer = TestingDeserializer::new(&bytes, ptrs);
            let t2 = WithDiscard::pagable_deserialize(&mut deserializer)?;
            assert_eq!(t2.discard, "");
            assert_eq!(t2.keep, "keep");
            Ok(())
        }
    }
}

mod asserts {
    use crate::traits::Pagable;
    use crate::traits::PagableDeserialize;
    use crate::traits::PagableSerialize;

    static_assertions::assert_impl_all!(super::inner::Test: PagableSerialize, PagableDeserialize<'static>);
    static_assertions::assert_impl_all!(super::inner::Test: Pagable);
}
