macro_rules! generic_set {
    () => {
        /// Returns true if the set is empty
        #[inline]
        pub fn is_empty(&self) -> bool {
            self.len() == 0
        }
    };
}

impl crate::SetU64 {
    generic_set!();
}
impl crate::SetU32 {
    generic_set!();
}
impl crate::SetUsize {
    generic_set!();
}
impl<T: crate::Fits64> crate::Set64<T> {
    generic_set!();
}
