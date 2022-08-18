/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ops::Range;

use derive_more::Display;
use events::truncate::truncate;

/// Pairs some output value with a span covering where the value came from. Unlike nom_locate, we drop
/// the references into the input so that this has a 'static lifetime. To be useful, it'll need to be
/// recombined with the input string.
#[derive(Debug, Display)]
#[display(fmt = "{}", value)]
pub struct Spanned<T> {
    pub position: Range<usize>,
    pub value: T,
}

impl<T> Spanned<T> {
    pub fn map_res<L, R, F: FnOnce(&T) -> Result<L, R>>(
        &self,
        func: F,
    ) -> Result<Spanned<L>, Spanned<R>> {
        self.span(func(&self.value))
    }

    pub fn span<O, E>(&self, res: Result<O, E>) -> Result<Spanned<O>, Spanned<E>> {
        match res {
            Ok(value) => Ok(Spanned {
                position: self.position.clone(),
                value,
            }),
            Err(value) => Err(Spanned {
                position: self.position.clone(),
                value,
            }),
        }
    }

    pub async fn async_map_res<
        'a,
        L,
        R,
        Fut: std::future::Future<Output = Result<L, R>>,
        F: FnOnce(&'a T) -> Fut + 'a,
    >(
        &'a self,
        func: F,
    ) -> Result<Spanned<L>, Spanned<R>> {
        let position = self.position.clone();
        match func(&self.value).await {
            Ok(value) => Ok(Spanned { position, value }),
            Err(value) => Err(Spanned { position, value }),
        }
    }

    pub fn into_map_res<L, R, F: FnOnce(T) -> Result<L, R>>(
        self,
        func: F,
    ) -> Result<Spanned<L>, Spanned<R>> {
        match func(self.value) {
            Ok(value) => Ok(Spanned {
                position: self.position,
                value,
            }),
            Err(value) => Err(Spanned {
                position: self.position,
                value,
            }),
        }
    }

    pub async fn async_into_map_res<
        L,
        R,
        Fut: std::future::Future<Output = Result<L, R>>,
        F: FnOnce(T) -> Fut,
    >(
        self,
        func: F,
    ) -> Result<Spanned<L>, Spanned<R>> {
        match func(self.value).await {
            Ok(value) => Ok(Spanned {
                position: self.position,
                value,
            }),
            Err(value) => Err(Spanned {
                position: self.position,
                value,
            }),
        }
    }

    // Constructs a 2-line string that prints the line where the span occurs and then a line below that identifying the span.
    pub fn get_err_context(&self, input: &str) -> String {
        // TODO(cjhopman): This assumes that the input is a single line, it should (maybe) handle multi-line inputs.
        // TODO(cjhopman): This should cut off the beginning and/or end of long lines and focus around the span.
        // TODO(cjhopman): Consider using annotate-snippets like we do in starlark.
        let (rest, end) = input.split_at(self.position.end);
        let (start, inner) = rest.split_at(self.position.start);

        let inner = truncate(inner, 80);

        let pointer = match inner.len() {
            l if l < 3 => format!("{}^", " ".repeat(start.len())),
            l => format!("{}^{}^", " ".repeat(start.len()), "-".repeat(l - 2)),
        };
        format!("\n    {}{}{}\n    {}\n", start, inner, end, pointer)
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_truncate_span_in_middle() {
        let error = "rdeps(set(fbcode//buck2/... fbsource//xplat/buck2/..., fbsource//fbobjc/buck2/...), fbcode//buck2/cli:buck2)";
        let span = Spanned {
            position: 0..error.len(),
            value: false,
        };
        let context = span.get_err_context(error);
        let context_lines: Vec<&str> = context.split('\n').collect();
        assert_eq!(
            context_lines,
            [
                "",
                "    rdeps(set(fbcode//buck2/... fbsour<<omitted>>ck2/...), fbcode//buck2/cli:buck2)",
                "    ^-----------------------------------------------------------------------------^",
                "",
            ]
        );
    }
}
