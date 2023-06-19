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

#[cfg(test)]
mod t {
    use std::collections::HashMap;
    use std::sync::atomic::AtomicUsize;
    use std::sync::atomic::Ordering;
    use std::sync::Arc;
    use std::thread::ScopedJoinHandle;
    use std::time::Duration;
    use std::time::Instant;

    use debugserver_types::*;
    use dupe::Dupe;

    use crate::assert::test_functions;
    use crate::debug::adapter::implementation::prepare_dap_adapter;
    use crate::debug::adapter::implementation::resolve_breakpoints;
    use crate::debug::DapAdapter;
    use crate::debug::DapAdapterClient;
    use crate::debug::DapAdapterEvalHook;
    use crate::debug::StepKind;
    use crate::environment::GlobalsBuilder;
    use crate::environment::Module;
    use crate::eval::Evaluator;
    use crate::eval::ReturnFileLoader;
    use crate::syntax::AstModule;
    use crate::syntax::Dialect;
    use crate::values::OwnedFrozenValue;
    use crate::wasm::is_wasm;

    #[derive(Debug)]
    struct Client {
        breakpoints_hit: Arc<AtomicUsize>,
    }

    impl Client {
        pub fn new(breakpoints_hit: Arc<AtomicUsize>) -> Self {
            Self { breakpoints_hit }
        }
    }

    impl DapAdapterClient for Client {
        fn event_stopped(&self) {
            println!("stopped!");
            self.breakpoints_hit.fetch_add(1, Ordering::SeqCst);
        }
    }

    struct BreakpointController {
        breakpoints_hit: Arc<AtomicUsize>,
    }

    impl BreakpointController {
        fn new() -> Self {
            Self {
                breakpoints_hit: Arc::new(AtomicUsize::new(0)),
            }
        }

        fn get_client(&self) -> Box<dyn DapAdapterClient> {
            Box::new(Client::new(self.breakpoints_hit.dupe()))
        }

        fn wait_for_eval_stopped(&self, breakpoint_count: usize, timeout: Duration) {
            let now = Instant::now();
            while self.breakpoints_hit.load(Ordering::SeqCst) != breakpoint_count {
                if now.elapsed() > timeout {
                    panic!("didn't hit expected breakpoint");
                }
                std::hint::spin_loop();
            }
        }
    }

    fn breakpoint(line: i64, condition: Option<&str>) -> SourceBreakpoint {
        SourceBreakpoint {
            column: None,
            condition: condition.map(|v| v.to_owned()),
            hit_condition: None,
            line,
            log_message: None,
        }
    }

    fn breakpoints_args(path: &str, lines: &[(i64, Option<&str>)]) -> SetBreakpointsArguments {
        SetBreakpointsArguments {
            breakpoints: Some(
                lines
                    .iter()
                    .map(|(line, condition)| breakpoint(*line, condition.as_deref()))
                    .collect(),
            ),
            lines: None,
            source: Source {
                adapter_data: None,
                checksums: None,
                name: None,
                origin: None,
                path: Some(path.to_owned()),
                presentation_hint: None,
                source_reference: None,
                sources: None,
            },
            source_modified: None,
        }
    }

    fn eval_with_hook(
        ast: AstModule,
        hook: impl DapAdapterEvalHook,
    ) -> anyhow::Result<OwnedFrozenValue> {
        let modules = HashMap::new();
        let loader = ReturnFileLoader { modules: &modules };
        let globals = GlobalsBuilder::extended().with(test_functions).build();
        let env = Module::new();
        let res = {
            let mut eval = Evaluator::new(&env);
            Box::new(hook).add_dap_hooks(&mut eval);
            eval.set_loader(&loader);
            eval.eval_module(ast, &globals)?
        };

        env.set("_", res);
        Ok(env
            .freeze()
            .expect("error freezing module")
            .get("_")
            .unwrap())
    }

    fn join_timeout<T>(waiting: ScopedJoinHandle<T>, timeout: Duration) -> T {
        let start = Instant::now();
        while !waiting.is_finished() {
            if start.elapsed() > timeout {
                panic!();
            }
        }
        waiting.join().unwrap()
    }

    static TIMEOUT: Duration = Duration::from_secs(10);

    #[test]
    fn test_breakpoint() -> anyhow::Result<()> {
        if is_wasm() {
            // `thread::scope` doesn't work in wasm.
            return Ok(());
        }

        let controller = BreakpointController::new();
        let (adapter, eval_hook) = prepare_dap_adapter(controller.get_client());
        let file_contents = "
x = [1, 2, 3]
print(x)
        ";
        std::thread::scope(|s| {
            let ast = AstModule::parse("test.bzl", file_contents.to_owned(), &Dialect::Extended)?;
            let breakpoints =
                resolve_breakpoints(&breakpoints_args("test.bzl", &[(3, None)]), &ast)?;
            adapter.set_breakpoints("test.bzl", &breakpoints)?;
            let eval_result =
                s.spawn(move || -> anyhow::Result<_> { eval_with_hook(ast, eval_hook) });
            controller.wait_for_eval_stopped(1, TIMEOUT);
            // TODO(cjhopman): we currently hit breakpoints on top-level statements twice (once for the gc bytecode, once for the actual statement).
            adapter.continue_()?;
            controller.wait_for_eval_stopped(2, TIMEOUT);

            adapter.continue_()?;

            join_timeout(eval_result, TIMEOUT)?;
            Ok(())
        })
    }

    #[test]
    fn test_breakpoint_with_failing_condition() -> anyhow::Result<()> {
        if is_wasm() {
            return Ok(());
        }

        let controller = BreakpointController::new();
        let (adapter, eval_hook) = prepare_dap_adapter(controller.get_client());
        let file_contents = "
x = [1, 2, 3]
print(x)
        ";
        std::thread::scope(|s| {
            let ast = AstModule::parse("test.bzl", file_contents.to_owned(), &Dialect::Extended)?;
            let breakpoints =
                resolve_breakpoints(&breakpoints_args("test.bzl", &[(3, Some("5 in x"))]), &ast)?;
            adapter.set_breakpoints("test.bzl", &breakpoints)?;
            let eval_result =
                s.spawn(move || -> anyhow::Result<_> { eval_with_hook(ast, eval_hook) });
            join_timeout(eval_result, TIMEOUT)?;
            Ok(())
        })
    }

    #[test]
    fn test_breakpoint_with_passing_condition() -> anyhow::Result<()> {
        if is_wasm() {
            return Ok(());
        }

        let controller = BreakpointController::new();
        let (adapter, eval_hook) = prepare_dap_adapter(controller.get_client());
        let file_contents = "
x = [1, 2, 3]
print(x)
        ";
        std::thread::scope(|s| {
            let ast = AstModule::parse("test.bzl", file_contents.to_owned(), &Dialect::Extended)?;
            let breakpoints =
                resolve_breakpoints(&breakpoints_args("test.bzl", &[(3, Some("2 in x"))]), &ast)?;
            adapter.set_breakpoints("test.bzl", &breakpoints)?;
            let eval_result =
                s.spawn(move || -> anyhow::Result<_> { eval_with_hook(ast, eval_hook) });
            controller.wait_for_eval_stopped(1, TIMEOUT);
            adapter.continue_()?;
            // TODO(cjhopman): we currently hit breakpoints on top-level statements twice (once for the gc bytecode, once for the actual statement).
            controller.wait_for_eval_stopped(2, TIMEOUT);
            adapter.continue_()?;

            join_timeout(eval_result, TIMEOUT)?;
            Ok(())
        })
    }

    #[test]
    fn test_step_over() -> anyhow::Result<()> {
        if is_wasm() {
            return Ok(());
        }

        let controller = BreakpointController::new();
        let (adapter, eval_hook) = prepare_dap_adapter(controller.get_client());
        let file_contents = "
def adjust(y):
    y[0] += 1
    y[1] += 1 # line 4
    y[2] += 1
x = [1, 2, 3]
adjust(x) # line 7
adjust(x)
print(x)
        ";
        std::thread::scope(|s| {
            let ast = AstModule::parse("test.bzl", file_contents.to_owned(), &Dialect::Extended)?;
            let breakpoints =
                resolve_breakpoints(&breakpoints_args("test.bzl", &[(7, None)]), &ast)?;
            adapter.set_breakpoints("test.bzl", &breakpoints)?;
            let eval_result =
                s.spawn(move || -> anyhow::Result<_> { eval_with_hook(ast, eval_hook) });
            controller.wait_for_eval_stopped(1, TIMEOUT);
            // TODO(cjhopman): we currently hit breakpoints on top-level statements twice (once for the gc bytecode, once for the actual statement).
            adapter.continue_()?;
            controller.wait_for_eval_stopped(2, TIMEOUT);

            assert_eq!("[1, 2, 3]", adapter.evaluate("x")?.result);
            adapter.step(StepKind::Over)?;
            controller.wait_for_eval_stopped(3, TIMEOUT);
            assert_eq!("[2, 3, 4]", adapter.evaluate("x")?.result);

            // TODO(cjhopman): we currently hit breakpoints on top-level statements twice (once for the gc bytecode, once for the actual statement).
            adapter.step(StepKind::Over)?;
            controller.wait_for_eval_stopped(4, TIMEOUT);
            adapter.step(StepKind::Over)?;
            controller.wait_for_eval_stopped(5, TIMEOUT);
            assert_eq!("[3, 4, 5]", adapter.evaluate("x")?.result);
            adapter.continue_()?;
            join_timeout(eval_result, TIMEOUT)?;
            Ok(())
        })
    }

    #[test]
    fn test_step_into() -> anyhow::Result<()> {
        if is_wasm() {
            return Ok(());
        }

        let controller = BreakpointController::new();
        let (adapter, eval_hook) = prepare_dap_adapter(controller.get_client());
        let file_contents = "
def adjust(y):
    y[0] += 1
    y[1] += 1 # line 4
    y[2] += 1
x = [1, 2, 3]
adjust(x) # line 7
adjust(x)
print(x)
        ";
        std::thread::scope(|s| {
            let ast = AstModule::parse("test.bzl", file_contents.to_owned(), &Dialect::Extended)?;
            let breakpoints =
                resolve_breakpoints(&breakpoints_args("test.bzl", &[(7, None)]), &ast)?;
            adapter.set_breakpoints("test.bzl", &breakpoints)?;
            let eval_result =
                s.spawn(move || -> anyhow::Result<_> { eval_with_hook(ast, eval_hook) });
            controller.wait_for_eval_stopped(1, TIMEOUT);
            // TODO(cjhopman): we currently hit breakpoints on top-level statements twice (once for the gc bytecode, once for the actual statement).
            adapter.continue_()?;
            controller.wait_for_eval_stopped(2, TIMEOUT);

            assert_eq!("[1, 2, 3]", adapter.evaluate("x")?.result);

            // into adjust
            adapter.step(StepKind::Into)?;
            controller.wait_for_eval_stopped(3, TIMEOUT);
            assert_eq!("[1, 2, 3]", adapter.evaluate("y")?.result);

            // into should go to next line
            adapter.step(StepKind::Into)?;
            controller.wait_for_eval_stopped(4, TIMEOUT);
            assert_eq!("[2, 2, 3]", adapter.evaluate("y")?.result);
            // two more intos should get us out of the function call
            adapter.step(StepKind::Into)?;
            controller.wait_for_eval_stopped(5, TIMEOUT);
            adapter.step(StepKind::Into)?;
            controller.wait_for_eval_stopped(6, TIMEOUT);
            assert_eq!("[2, 3, 4]", adapter.evaluate("x")?.result);

            // and once more back into the function
            adapter.step(StepKind::Into)?;
            controller.wait_for_eval_stopped(7, TIMEOUT);

            // TODO(cjhopman): unfortunately, gc being marked as statements causes us to need to step_into again.
            adapter.step(StepKind::Into)?;
            controller.wait_for_eval_stopped(8, TIMEOUT);

            assert_eq!("[2, 3, 4]", adapter.evaluate("y")?.result);

            adapter.continue_()?;
            join_timeout(eval_result, TIMEOUT)?;
            Ok(())
        })
    }

    #[test]
    fn test_step_out() -> anyhow::Result<()> {
        if is_wasm() {
            return Ok(());
        }

        let controller = BreakpointController::new();
        let (adapter, eval_hook) = prepare_dap_adapter(controller.get_client());
        let file_contents = "
def adjust(y):
    y[0] += 1
    y[1] += 1 # line 4
    y[2] += 1
x = [1, 2, 3]
adjust(x) # line 7
adjust(x)
print(x)
        ";
        std::thread::scope(|s| {
            let ast = AstModule::parse("test.bzl", file_contents.to_owned(), &Dialect::Extended)?;
            let breakpoints =
                resolve_breakpoints(&breakpoints_args("test.bzl", &[(4, None)]), &ast)?;
            adapter.set_breakpoints("test.bzl", &breakpoints)?;
            let eval_result =
                s.spawn(move || -> anyhow::Result<_> { eval_with_hook(ast, eval_hook) });
            // should break on the first time hitting line 4
            controller.wait_for_eval_stopped(1, TIMEOUT);
            assert_eq!("[2, 2, 3]", adapter.evaluate("y")?.result);

            // step out should take us to line 8
            adapter.step(StepKind::Out)?;
            controller.wait_for_eval_stopped(2, TIMEOUT);
            assert_eq!("[2, 3, 4]", adapter.evaluate("x")?.result);

            // step out should actually hit the breakpoint at 4 first (before getting out)
            adapter.step(StepKind::Out)?;
            controller.wait_for_eval_stopped(3, TIMEOUT);
            assert_eq!("[3, 3, 4]", adapter.evaluate("y")?.result);

            // step out should get out to the print
            adapter.step(StepKind::Out)?;
            controller.wait_for_eval_stopped(4, TIMEOUT);
            assert_eq!("[3, 4, 5]", adapter.evaluate("x")?.result);

            // one more out should be equivalent to continue
            adapter.step(StepKind::Out)?;
            join_timeout(eval_result, TIMEOUT)?;
            Ok(())
        })
    }
}
