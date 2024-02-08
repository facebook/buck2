# Superconsole

## 0.2.0 (June 5, 2023)

The major change from the last release is that `State` has been deleted in favor
of `render` taking a reference to the components. In addition there were a
number of API adjustments, including:

- Add the type `Lines` wrapping `Vec<Line>`.
- Add `Span::new_colored` and `Span::new_colored_lossy`.
- Remove the `line!` macro and add more utilities to the `Line` type, making its
  internals private.
- Rename `x`/`y` to `width`/`height` where it makes sense.

## 0.1.0 (February 3, 2022)

- Initial version.
