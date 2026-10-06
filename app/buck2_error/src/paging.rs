/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Paging support for [`Error`](crate::Error).
//!
//! An error pages out as its [`ErrorReport`], the same thing it is logged as, and pages back in
//! through the report's conversion. What survives is what the report carries: the rendered
//! message, tags, string tags, source location and so the category key. The context chain is
//! flattened into the message, so the paged-in error renders the same under `Debug` but not
//! under `Display`, and an action error is dropped.

use buck2_data::ErrorReport;
use pagable::PagableDeserialize;
use pagable::PagableDeserializer;
use pagable::PagableSerialize;
use pagable::PagableSerializer;
use prost::Message;

impl PagableSerialize for crate::Error {
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> pagable::Result<()> {
        ErrorReport::from(self)
            .encode_to_vec()
            .pagable_serialize(serializer)
    }
}

impl<'de> PagableDeserialize<'de> for crate::Error {
    fn pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> pagable::Result<Self> {
        let bytes = Vec::<u8>::pagable_deserialize(deserializer)?;
        Ok(ErrorReport::decode(bytes.as_slice())?.into())
    }
}

#[cfg(test)]
pub(crate) mod testing {
    use pagable::PagableDeserialize;
    use pagable::PagableSerialize;

    pub(crate) fn round_trip(error: &crate::Error) -> crate::Error {
        let mut serializer = pagable::testing::TestingSerializer::new();
        error.pagable_serialize(&mut serializer).unwrap();
        let bytes = serializer.finish();
        let mut deserializer = pagable::testing::TestingDeserializer::new(&bytes);
        crate::Error::pagable_deserialize(&mut deserializer).unwrap()
    }

    /// The `Debug` rendering without the backtrace `anyhow` appends under
    /// `RUST_BACKTRACE`, which is captured where the error is formatted and so
    /// never compares equal between two renderings.
    pub(crate) fn rendering(error: &crate::Error) -> String {
        let rendered = format!("{error:?}");
        match rendered.split_once("\n\nStack backtrace:") {
            Some((message, _backtrace)) => message.to_owned(),
            None => rendered,
        }
    }

    /// Everything a paged-in error keeps; see the module doc.
    pub(crate) fn assert_same_error(restored: &crate::Error, original: &crate::Error) {
        assert_eq!(rendering(restored), rendering(original));
        assert_eq!(restored.tags(), original.tags());
        assert_eq!(restored.string_tags(), original.string_tags());
        assert_eq!(restored.source_location(), original.source_location());
        assert_eq!(restored.category_key(), original.category_key());
    }
}

#[cfg(test)]
mod tests {
    use super::testing::assert_same_error;
    use super::testing::round_trip;
    use crate::ErrorTag;
    use crate::context_value::StarlarkContext;

    #[test]
    fn context_chain_round_trips() {
        let original = crate::buck2_error!(ErrorTag::Analysis, "analysis of `cell//pkg:t` failed")
            .context("while resolving a lazy operation")
            .string_tag("inner_string_tag")
            .tag([ErrorTag::Bxl])
            .context("while evaluating `main.bxl`")
            .string_tag("outer_string_tag");

        assert_same_error(&round_trip(&original), &original);
    }

    #[test]
    fn starlark_backtrace_round_trips() {
        let original = crate::buck2_error!(ErrorTag::StarlarkFail, "fail() was called")
            .context_for_starlark_backtrace(StarlarkContext {
                call_stack: "Traceback (most recent call last):\n  * main.bxl:3, in _impl\n"
                    .to_owned(),
                error_msg: "fail() was called".to_owned(),
                span: None,
            })
            .context("while evaluating `main.bxl`");

        assert_same_error(&round_trip(&original), &original);
    }
}
