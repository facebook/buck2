#[doc(hidden)]
#[macro_export(local_inner_macros)]
macro_rules! _overload_unary {
    (-, $($t:tt)+) => (_overload_unary_internal!(Neg, neg, $($t)+););
    (!, $($t:tt)+) => (_overload_unary_internal!(Not, not, $($t)+););
}

#[doc(hidden)]
#[macro_export(local_inner_macros)]
macro_rules! _overload_unary_internal {
    ($op_trait:ident, $op_fn:ident, $i:ident, $t:ty, $out:ty, $body:block) => (        
        impl ops::$op_trait for $t {
            type Output = $out;
            fn $op_fn(self) -> Self::Output {
                let $i = self;
                $body
            }
        }
    );
}
