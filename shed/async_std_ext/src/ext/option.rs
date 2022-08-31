/// Utility to let us use async functions when on Option types.
use async_trait::async_trait;
use futures::Future;

#[async_trait]
pub trait AsyncOptionExt<T> {
    ///
    /// Same as 'Option#map()'
    ///
    /// ```
    ///
    /// # let mut rt = tokio::runtime::Runtime::new().unwrap();
    /// # rt.block_on(
    /// #    async {
    ///
    /// use async_std_ext::prelude::*;
    ///
    /// async fn add1(i: i32) -> i32 {
    ///     i + 1
    /// }
    ///
    /// let opt = Some(5);
    /// assert_eq!(opt.async_map(add1).await, Some(6));
    ///
    /// let opt = None;
    /// assert_eq!(opt.async_map(add1).await, None);
    ///
    /// #   }
    /// # )
    /// ```
    async fn async_map<U, F, FUT>(self, f: F) -> Option<U>
    where
        F: FnOnce(T) -> FUT + Send,
        FUT: Future<Output = U> + Send;

    ///
    /// Same as 'Option#and_then()'
    ///
    /// ```
    ///
    /// # let mut rt = tokio::runtime::Runtime::new().unwrap();
    /// # rt.block_on(
    /// #    async {
    ///
    /// use async_std_ext::prelude::*;
    ///
    /// async fn minus1(i: i32) -> Option<u32> {
    ///     if i - 1 >= 0 {
    ///         Some((i - 1) as u32)
    ///     } else {
    ///         None
    ///     }
    /// }
    ///
    /// let opt = Some(5);
    /// assert_eq!(opt.async_and_then(minus1).await, Some(4));
    ///
    /// let opt = Some(0);
    /// assert_eq!(opt.async_and_then(minus1).await, None);
    ///
    /// let opt = None;
    /// assert_eq!(opt.async_and_then(minus1).await, None)
    ///
    /// #   }
    /// # )
    /// ```
    async fn async_and_then<U, F: FnOnce(T) -> FUT, FUT>(self, f: F) -> Option<U>
    where
        F: FnOnce(T) -> FUT + Send,
        FUT: Future<Output = Option<U>> + Send;

    ///
    /// Same as 'Option#map_or()'
    ///
    /// ```
    ///
    /// # let mut rt = tokio::runtime::Runtime::new().unwrap();
    /// # rt.block_on(
    /// #    async {
    ///
    /// use async_std_ext::prelude::*;
    ///
    /// async fn add1(i: i32) -> i32 {
    ///     i + 1
    /// }
    ///
    /// let opt = Some(5);
    /// assert_eq!(opt.async_map_or(-1, add1).await, 6);
    ///
    /// let opt = None;
    /// assert_eq!(opt.async_map_or(-1, add1).await, -1);
    ///
    /// #   }
    /// # )
    /// ```
    async fn async_map_or<U, F, FUT>(self, default: U, f: F) -> U
    where
        U: Send,
        F: FnOnce(T) -> FUT + Send,
        FUT: Future<Output = U> + Send;
}

#[async_trait]
impl<T> AsyncOptionExt<T> for Option<T>
where
    T: Send,
{
    async fn async_map<U, F, FUT>(self, f: F) -> Option<U>
    where
        F: FnOnce(T) -> FUT + Send,
        FUT: Future<Output = U> + Send,
    {
        match self {
            Some(t) => Some(f(t).await),
            None => None,
        }
    }

    async fn async_and_then<U, F: FnOnce(T) -> FUT, FUT>(self, f: F) -> Option<U>
    where
        F: FnOnce(T) -> FUT + Send,
        FUT: Future<Output = Option<U>> + Send,
    {
        match self {
            Some(t) => f(t).await,
            None => None,
        }
    }

    async fn async_map_or<U, F, FUT>(self, default: U, f: F) -> U
    where
        U: Send,
        F: FnOnce(T) -> FUT + Send,
        FUT: Future<Output = U> + Send,
    {
        match self {
            Some(t) => f(t).await,
            None => default,
        }
    }
}
