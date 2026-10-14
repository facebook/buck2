/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Uploads rejected by a graceful GOAWAY, as sent by load balancers that
//! recycle connections. A rejected request was not processed and can be sent
//! again. The transport does that by itself only while the request body fits
//! in what it buffers for a replay. These uploads are bigger than that, so the
//! transport fails them with `UNAVAILABLE` and they rely on `retry` in
//! `upload_impl`.

use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use futures::stream;
use prost::bytes::Bytes;
use re_grpc_proto::build::bazel::remote::execution::v2::batch_update_blobs_response;
use tokio::net::TcpListener;
use tokio::net::TcpStream;
use tonic::transport::Endpoint;

use super::*;

/// How much request body the transport buffers to send a request again.
const REPLAY_BUFFER_SIZE: usize = 256 * 1024;

const BATCH_UPDATE_BLOBS: &str =
    "/build.bazel.remote.execution.v2.ContentAddressableStorage/BatchUpdateBlobs";
const WRITE: &str = "/google.bytestream.ByteStream/Write";

/// A server whose first connection takes more than `REPLAY_BUFFER_SIZE` of
/// the first request's body and then sends GOAWAY with last stream ID 0 and
/// NO_ERROR, so the request was not processed. Later connections serve
/// `BatchUpdateBlobs` and `ByteStream.Write`.
struct GoawayServer {
    channel: Channel,
    goaway_sent: Arc<AtomicBool>,
    /// Method and blob bytes of each request served after the GOAWAY.
    served: Arc<Mutex<Vec<(String, usize)>>>,
}

impl GoawayServer {
    async fn start() -> Self {
        let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        let goaway_sent = Arc::new(AtomicBool::new(false));
        let served = Arc::new(Mutex::new(Vec::new()));
        tokio::spawn({
            let goaway_sent = goaway_sent.clone();
            let served = served.clone();
            async move {
                let (tcp, _) = listener.accept().await.unwrap();
                tokio::spawn(reject_first_request(tcp, goaway_sent));
                loop {
                    let (tcp, _) = listener.accept().await.unwrap();
                    tokio::spawn(serve(tcp, served.clone()));
                }
            }
        });
        let channel = Endpoint::from_shared(format!("http://{addr}"))
            .unwrap()
            .connect_lazy();
        Self {
            channel,
            goaway_sent,
            served,
        }
    }

    fn served(&self) -> Vec<(String, usize)> {
        self.served.lock().unwrap().clone()
    }
}

/// Speaks just enough HTTP/2 to receive the first request and send a GOAWAY
/// that rejects it, which `h2`'s server can't do once it has seen the request.
async fn reject_first_request(mut tcp: TcpStream, goaway_sent: Arc<AtomicBool>) {
    const DATA: u8 = 0x0;
    const SETTINGS: u8 = 0x4;
    const PING: u8 = 0x6;
    const GOAWAY: u8 = 0x7;
    const WINDOW_UPDATE: u8 = 0x8;
    const ACK: u8 = 0x1;
    const MAX_WINDOW: u32 = (1 << 31) - 1;

    let mut preface = [0; 24];
    tcp.read_exact(&mut preface).await.unwrap();
    assert_eq!(&preface, b"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n");

    // Let the client send the whole body without waiting for window updates.
    let mut initial_window_size = vec![0x00, 0x04];
    initial_window_size.extend_from_slice(&MAX_WINDOW.to_be_bytes());
    write_frame(&mut tcp, SETTINGS, 0, &initial_window_size)
        .await
        .unwrap();
    write_frame(
        &mut tcp,
        WINDOW_UPDATE,
        0,
        &(MAX_WINDOW - 65_535).to_be_bytes(),
    )
    .await
    .unwrap();

    let mut body_received = 0;
    let mut header = [0; 9];
    // Read until the client closes the connection, closing it here first with
    // unread data would reset it instead.
    while tcp.read_exact(&mut header).await.is_ok() {
        let len = u32::from_be_bytes([0, header[0], header[1], header[2]]) as usize;
        let mut payload = vec![0; len];
        if tcp.read_exact(&mut payload).await.is_err() {
            break;
        }
        let written = match (header[3], header[4] & ACK) {
            (SETTINGS, 0) => write_frame(&mut tcp, SETTINGS, ACK, &[]).await,
            (PING, 0) => write_frame(&mut tcp, PING, ACK, &payload).await,
            (DATA, _) if body_received <= REPLAY_BUFFER_SIZE => {
                body_received += len;
                if body_received > REPLAY_BUFFER_SIZE {
                    // Last stream ID 0 and NO_ERROR.
                    goaway_sent.store(true, Ordering::SeqCst);
                    write_frame(&mut tcp, GOAWAY, 0, &[0; 8]).await
                } else {
                    Ok(())
                }
            }
            _ => Ok(()),
        };
        if written.is_err() {
            break;
        }
    }
}

/// Writes a frame for stream 0, the connection.
async fn write_frame(tcp: &mut TcpStream, kind: u8, flags: u8, payload: &[u8]) -> io::Result<()> {
    let mut frame = (payload.len() as u32).to_be_bytes()[1..].to_vec();
    frame.extend_from_slice(&[kind, flags, 0, 0, 0, 0]);
    frame.extend_from_slice(payload);
    tcp.write_all(&frame).await
}

async fn serve(tcp: TcpStream, served: Arc<Mutex<Vec<(String, usize)>>>) {
    let mut connection = h2::server::handshake(tcp).await.unwrap();
    while let Some(request) = connection.accept().await {
        let (request, mut respond) = request.unwrap();
        let served = served.clone();
        tokio::spawn(async move {
            let method = request.uri().path().to_owned();
            let mut body = request.into_body();
            let mut data = Vec::new();
            while let Some(chunk) = body.data().await {
                let chunk = chunk.unwrap();
                body.flow_control().release_capacity(chunk.len()).unwrap();
                data.extend_from_slice(&chunk);
            }
            let messages = grpc_messages(&data);
            let (blob_bytes, response) = match method.as_str() {
                BATCH_UPDATE_BLOBS => {
                    let request = BatchUpdateBlobsRequest::decode(messages[0]).unwrap();
                    let response = BatchUpdateBlobsResponse {
                        responses: request
                            .requests
                            .iter()
                            .map(|r| batch_update_blobs_response::Response {
                                digest: r.digest.clone(),
                                status: Some(Status::default()),
                            })
                            .collect(),
                    };
                    let blob_bytes = request.requests.iter().map(|r| r.data.len()).sum();
                    (blob_bytes, response.encode_to_vec())
                }
                WRITE => {
                    let blob_bytes = messages
                        .iter()
                        .map(|m| WriteRequest::decode(*m).unwrap().data.len())
                        .sum::<usize>();
                    let response = WriteResponse {
                        committed_size: blob_bytes as i64,
                    };
                    (blob_bytes, response.encode_to_vec())
                }
                _ => panic!("Unexpected call to {method}"),
            };
            served.lock().unwrap().push((method, blob_bytes));

            let headers = http::Response::builder()
                .header("content-type", "application/grpc")
                .body(())
                .unwrap();
            let mut stream = respond.send_response(headers, false).unwrap();
            stream.send_data(grpc_frame(&response), false).unwrap();
            let mut trailers = http::HeaderMap::new();
            trailers.insert("grpc-status", http::HeaderValue::from_static("0"));
            stream.send_trailers(trailers).unwrap();
        });
    }
}

/// Splits a gRPC body into its length-prefixed messages.
fn grpc_messages(mut body: &[u8]) -> Vec<&[u8]> {
    let mut messages = Vec::new();
    while !body.is_empty() {
        let len = u32::from_be_bytes(body[1..5].try_into().unwrap()) as usize;
        messages.push(&body[5..5 + len]);
        body = &body[5 + len..];
    }
    messages
}

fn grpc_frame(message: &[u8]) -> Bytes {
    let mut frame = vec![0];
    frame.extend_from_slice(&(message.len() as u32).to_be_bytes());
    frame.extend_from_slice(message);
    frame.into()
}

fn upload_request(blob: Vec<u8>) -> UploadRequest {
    UploadRequest {
        inlined_blobs_with_digest: Some(vec![InlinedBlobWithDigest {
            digest: TDigest {
                hash: "aa".to_owned(),
                size_in_bytes: blob.len() as i64,
                ..Default::default()
            },
            blob,
            ..Default::default()
        }]),
        ..Default::default()
    }
}

#[tokio::test]
async fn test_upload_batch_after_goaway() -> anyhow::Result<()> {
    let server = GoawayServer::start().await;
    let cas = &ContentAddressableStorageClient::new(server.channel.clone());
    let blob_size = 2 * REPLAY_BUFFER_SIZE;

    let upload = upload_impl(
        &InstanceName(None),
        upload_request(vec![1; blob_size]),
        None,
        2 * blob_size, // so the blob goes into a BatchUpdateBlobs request
        None,
        |req| async move { anyhow::Ok(cas.clone().batch_update_blobs(req).await?.into_inner()) },
        |_segments| async move {
            panic!("Not called");
        },
    );
    tokio::time::timeout(Duration::from_secs(60), upload).await??;

    assert!(server.goaway_sent.load(Ordering::SeqCst));
    assert_eq!(
        server.served(),
        [(BATCH_UPDATE_BLOBS.to_owned(), blob_size)]
    );
    Ok(())
}

#[tokio::test]
async fn test_upload_bytestream_after_goaway() -> anyhow::Result<()> {
    let server = GoawayServer::start().await;
    let bytestream = &ByteStreamClient::new(server.channel.clone());
    let blob_size = 2 * REPLAY_BUFFER_SIZE;

    let upload = upload_impl(
        &InstanceName(None),
        upload_request(vec![1; blob_size]),
        None,
        REPLAY_BUFFER_SIZE, // so the blob goes through ByteStream.Write
        None,
        |_req| async move {
            panic!("Not called");
        },
        |segments| async move {
            anyhow::Ok(
                bytestream
                    .clone()
                    .write(stream::iter(segments))
                    .await?
                    .into_inner(),
            )
        },
    );
    tokio::time::timeout(Duration::from_secs(60), upload).await??;

    assert!(server.goaway_sent.load(Ordering::SeqCst));
    assert_eq!(server.served(), [(WRITE.to_owned(), blob_size)]);
    Ok(())
}
