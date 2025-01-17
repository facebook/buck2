/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use async_trait::async_trait;
use buck2_error::starlark_error::from_starlark_with_options;
use buck2_error::BuckErrorContext;
use buck2_futures::cancellation::CancellationContext;
use buck2_interpreter::dice::starlark_provider::with_starlark_eval_provider;
use buck2_interpreter::dice::starlark_provider::StarlarkEvalKind;
use buck2_interpreter::file_type::StarlarkFileType;
use buck2_interpreter::starlark_profiler::profiler::StarlarkProfilerOpt;
use dice::DiceComputations;
use dice::Key;
use indoc::indoc;
use starlark::environment::Globals;
use starlark::environment::Module;
use starlark::syntax::AstModule;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Tier0)]
enum CheckStarlarkStackSizeError {
    #[error("Error checking starlark stack size")]
    CheckStarlarkStackSizeError,
}

// In order to prevent non deterministic crashes
// we intentionally set off a starlark stack overflow, to make
// sure that starlark catches the overflow and reports an error
// before the native stack overflows
pub(crate) async fn check_starlark_stack_size(
    ctx: &mut DiceComputations<'_>,
) -> buck2_error::Result<()> {
    #[derive(Debug, derive_more::Display, Clone, Allocative, Eq, PartialEq, Hash)]
    struct StarlarkStackSizeChecker;

    #[async_trait]
    impl Key for StarlarkStackSizeChecker {
        type Value = buck2_error::Result<()>;

        async fn compute(
            &self,
            ctx: &mut DiceComputations,
            _cancellation: &CancellationContext,
        ) -> Self::Value {
            with_starlark_eval_provider(
                ctx,
                &mut StarlarkProfilerOpt::disabled(),
                &StarlarkEvalKind::Unknown("Check starlark stack size".into()),
                move |provider, _| {
                    let env = Module::new();
                    let (mut eval, _) = provider.make(&env)?;
                    let content = indoc!(
                        r#"
                                def f():
                                    f()
                                f()
                        "#
                    );
                    let ast = AstModule::parse(
                        "x.star",
                        content.to_owned(),
                        &StarlarkFileType::Bzl.dialect(false),
                    )
                    .map_err(|e| {
                        from_starlark_with_options(
                            e,
                            buck2_error::starlark_error::NativeErrorHandling::Unknown,
                            false,
                        )
                    })
                    .internal_error("Failed to parse check module")?;
                    match eval.eval_module(ast, &Globals::standard()) {
                        Err(e) if e.to_string().contains("Starlark call stack overflow") => Ok(()),
                        Err(p) => Err(from_starlark_with_options(
                            p,
                            buck2_error::starlark_error::NativeErrorHandling::Unknown,
                            false,
                        )
                        .into()),
                        Ok(_) => {
                            Err(CheckStarlarkStackSizeError::CheckStarlarkStackSizeError.into())
                        }
                    }
                },
            )
            .await?;
            Ok(())
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            match (x, y) {
                (Ok(x), Ok(y)) => x == y,
                _ => false,
            }
        }

        fn validity(x: &Self::Value) -> bool {
            x.is_ok()
        }
    }

    ctx.compute(&StarlarkStackSizeChecker).await?
}
