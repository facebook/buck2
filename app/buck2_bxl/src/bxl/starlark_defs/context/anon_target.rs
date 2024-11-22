/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::rule_defs::provider::ty::abstract_provider::AbstractProvider;
use buck2_interpreter_for_build::attrs::StarlarkAttribute;
use buck2_interpreter_for_build::rule::RuleCallable;
use starlark::collections::SmallMap;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::typing::ParamIsRequired;
use starlark::typing::ParamSpec;
use starlark::util::ArcStr;
use starlark::values::dict::UnpackDictEntries;
use starlark::values::list::ListType;
use starlark::values::list::UnpackList;
use starlark::values::structs::StructRef;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::typing::StarlarkCallable;
use starlark::values::typing::StarlarkCallableChecked;
use starlark::values::typing::StarlarkCallableParamSpec;
use starlark::values::FrozenValue;
use starlark::values::StringValue;

use crate::bxl::starlark_defs::context::BxlContext;

struct BxlAnonCallbackParamSpec;

impl StarlarkCallableParamSpec for BxlAnonCallbackParamSpec {
    fn params() -> ParamSpec {
        ParamSpec::new_parts(
            [],
            [],
            None,
            [
                (
                    ArcStr::new_static("bxl_ctx"),
                    ParamIsRequired::Yes,
                    BxlContext::starlark_type_repr(),
                ),
                (
                    ArcStr::new_static("attrs"),
                    ParamIsRequired::Yes,
                    StructRef::starlark_type_repr(),
                ),
            ],
            None,
        )
        .unwrap()
    }
}

#[starlark_module]
pub(crate) fn register_anon_rule(globals: &mut GlobalsBuilder) {
    /// Create a new anonymous rule.
    fn anon_rule<'v>(
        #[starlark(require = named)] r#impl: StarlarkCallableChecked<
            'v,
            BxlAnonCallbackParamSpec,
            ListType<AbstractProvider>,
        >,
        #[starlark(require = named)] attrs: UnpackDictEntries<&'v str, &'v StarlarkAttribute>,
        #[starlark(require = named, default = "")] doc: &str,
        #[starlark(require = named, default = SmallMap::default())]
        artifact_promise_mappings: SmallMap<
            StringValue<'v>,
            StarlarkCallable<'v, (FrozenValue,), UnpackList<FrozenValue>>,
        >,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> anyhow::Result<RuleCallable<'v>> {
        RuleCallable::new_bxl_anon(
            StarlarkCallable::unchecked_new(r#impl.0),
            attrs,
            doc,
            artifact_promise_mappings,
            eval,
        )
        .map_err(Into::into)
    }
}
