#[doc(hidden)]
#[macro_export(local_inner_macros)]
macro_rules! _overload_binary {
    (+, $($t:tt)+) => (_overload_binary_internal!(Add, add, $($t)+););
    (-, $($t:tt)+) => (_overload_binary_internal!(Sub, sub, $($t)+););
    (*, $($t:tt)+) => (_overload_binary_internal!(Mul, mul, $($t)+););
    (/, $($t:tt)+) => (_overload_binary_internal!(Div, div, $($t)+););
    (%, $($t:tt)+) => (_overload_binary_internal!(Rem, rem, $($t)+););
    (&, $($t:tt)+) => (_overload_binary_internal!(BitAnd, bitand, $($t)+););
    (|, $($t:tt)+) => (_overload_binary_internal!(BitOr, bitor, $($t)+););
    (^, $($t:tt)+) => (_overload_binary_internal!(BitXor, bitxor, $($t)+););
    (<<, $($t:tt)+) => (_overload_binary_internal!(Shl, shl, $($t)+););
    (>>, $($t:tt)+) => (_overload_binary_internal!(Shr, shr, $($t)+););
}

#[doc(hidden)]
#[macro_export(local_inner_macros)]
macro_rules! _overload_binary_internal {
    ($op_trait:ident, $op_fn:ident, $li:ident, $lt:ty, $ri:ident, $rt:ty, $out:ty, $body:block) => (
        impl ops::$op_trait<$rt> for $lt {
            type Output = $out;
            fn $op_fn(self, $ri: $rt) -> Self::Output {
                let $li = self;
                $body
            }
        }
    );
}
