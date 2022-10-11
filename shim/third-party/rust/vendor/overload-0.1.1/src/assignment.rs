#[doc(hidden)]
#[macro_export(local_inner_macros)]
macro_rules! _overload_assignment {
    (+=, $($t:tt)+) => (_overload_assignment_internal!(AddAssign, add_assign, $($t)+););
    (-=, $($t:tt)+) => (_overload_assignment_internal!(SubAssign, sub_assign, $($t)+););
    (*=, $($t:tt)+) => (_overload_assignment_internal!(MulAssign, mul_assign, $($t)+););
    (/=, $($t:tt)+) => (_overload_assignment_internal!(DivAssign, div_assign, $($t)+););
    (%=, $($t:tt)+) => (_overload_assignment_internal!(RemAssign, rem_assign, $($t)+););
    (&=, $($t:tt)+) => (_overload_assignment_internal!(BitAndAssign, bitand_assign, $($t)+););
    (|=, $($t:tt)+) => (_overload_assignment_internal!(BitOrAssign, bitor_assign, $($t)+););
    (^=, $($t:tt)+) => (_overload_assignment_internal!(BitXorAssign, bitxor_assign, $($t)+););
    (<<=, $($t:tt)+) => (_overload_assignment_internal!(ShlAssign, shl_assign, $($t)+););
    (>>=, $($t:tt)+) => (_overload_assignment_internal!(ShrAssign, shr_assign, $($t)+););
}

#[doc(hidden)]
#[macro_export(local_inner_macros)]
macro_rules! _overload_assignment_internal {
    ($op_trait:ident, $op_fn:ident, $li:ident, $lt:ty, $ri:ident, $rt:ty, $body:block) => (        
        impl ops::$op_trait<$rt> for $lt {
            fn $op_fn(&mut self, $ri: $rt) {
                let $li = self;
                $body
            }
        }
    );
}
