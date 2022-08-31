use futures::future::Future;

pub async fn to_tonic<F, T>(fut: F) -> Result<tonic::Response<T>, tonic::Status>
where
    F: Future<Output = anyhow::Result<T>>,
{
    match fut.await {
        Ok(r) => Ok(tonic::Response::new(r)),
        Err(e) => Err(tonic::Status::unknown(format!("{:#}", e))),
    }
}
