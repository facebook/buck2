/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;

use anyhow::Context;
use bytes::Bytes;
use http::HeaderMap;
use http::Method;
use http::Uri;
use hyper::Request;
use hyper::Response;
use hyper::StatusCode;

use crate::http::HttpError;

trait UriWithRedirect {
    fn with_redirect(&self, location: &Uri) -> anyhow::Result<Uri>;

    fn is_cross_host(&self, other: &Uri) -> bool;
}

impl UriWithRedirect for Uri {
    /// Converts this Uri into the redirect Uri by combining it with the URI
    /// obtained from the Location header of a response.
    fn with_redirect(&self, location: &Uri) -> anyhow::Result<Uri> {
        let mut redirected = Uri::builder();
        if let Some(scheme) = location.scheme().or_else(|| self.scheme()) {
            redirected = redirected.scheme(scheme.clone());
        };
        if let Some(authority) = location.authority().or_else(|| self.authority()) {
            redirected = redirected.authority(authority.clone());
        }
        if let Some(path_and_query) = location.path_and_query().or_else(|| self.path_and_query()) {
            redirected = redirected.path_and_query(path_and_query.clone());
        }
        redirected.build().context("Building redirected URI")
    }

    /// Returns whether this Uri is the same host as represented by 'other'.
    fn is_cross_host(&self, other: &Uri) -> bool {
        self.host() != other.host() || self.port_u16() != other.port_u16()
    }
}

/// Intermediate type to store aspects of a request we need to mutate during
/// redirect state machine.
/// This is mostly because hyper::Request cannot be cloned.
pub(super) struct PendingRequest {
    method: Method,
    uri: Uri,
    headers: HeaderMap,
    body: Bytes,
}

impl PendingRequest {
    pub(super) fn from_request(request: &Request<Bytes>) -> Self {
        Self {
            method: request.method().clone(),
            uri: request.uri().clone(),
            headers: request.headers().clone(),
            body: request.body().clone(),
        }
    }

    pub(super) fn to_request(&self) -> anyhow::Result<Request<Bytes>> {
        let mut builder = Request::builder()
            .method(self.method.clone())
            .uri(self.uri.clone());
        *builder
            .headers_mut()
            .expect("Request builder should not error here") = self.headers.clone();
        builder
            .body(self.body.clone())
            .context("building redirected request")
    }
}

/// A simple state machine that drives following redirects. Much of this is derived
/// from how [`reqwest` handles redirects](https://docs.rs/reqwest/latest/src/reqwest/redirect.rs.html#1-337)
/// as well as the [`follow-redirects`](https://github.com/srijs/rust-follow-redirects) crate.
/// Unfortunately, the latter is abandoned; until and unless it's maintained by someone
/// (preferably the hyper folks), let's roll our own.
pub(super) struct RedirectEngine<B> {
    processed_redirects: usize,
    max_redirects: usize,
    pending_request: PendingRequest,
    response: Response<B>,
}

impl<B> RedirectEngine<B> {
    pub(super) fn new(
        max_redirects: usize,
        pending_request: PendingRequest,
        response: Response<B>,
    ) -> Self {
        Self {
            processed_redirects: 0,
            max_redirects,
            pending_request,
            response,
        }
    }

    /// Handle any redirects we get in the course of sending the request (up to
    /// self.max_redirects).
    pub(super) async fn handle_redirects<S, F>(
        mut self,
        sender_func: S,
    ) -> Result<Response<B>, HttpError>
    where
        F: Future<Output = Result<Response<B>, HttpError>>,
        S: Fn(Request<Bytes>) -> F,
    {
        let initial_uri = self.pending_request.uri.clone();
        loop {
            if self.processed_redirects > self.max_redirects {
                return Err(HttpError::TooManyRedirects {
                    uri: initial_uri.to_string(),
                    max_redirects: self.max_redirects,
                });
            }
            if !self.should_redirect() {
                break;
            }
            tracing::debug!(
                "http: processing redirect request ({}) for {}",
                self.response.status(),
                self.pending_request.uri,
            );

            if let Some(redirect_request) = self
                .update_and_create_request()
                .map_err(HttpError::MutateRequest)?
            {
                self.response = sender_func(redirect_request).await?;
                self.processed_redirects += 1;
            } else {
                break;
            }
        }

        Ok(self.response)
    }

    /// Whether we should redirect this response.
    fn should_redirect(&self) -> bool {
        let got_redirect_status_code = matches!(
            self.response.status(),
            StatusCode::MOVED_PERMANENTLY
                | StatusCode::FOUND
                | StatusCode::SEE_OTHER
                | StatusCode::TEMPORARY_REDIRECT
                | StatusCode::PERMANENT_REDIRECT
        );

        got_redirect_status_code && self.extract_redirect_location_from_response().is_some()
    }

    /// Updates the request in place to send to the redirect location.
    fn update_and_create_request(&mut self) -> anyhow::Result<Option<Request<Bytes>>> {
        let redirect_location =
            if let Some(location) = self.extract_redirect_location_from_response() {
                location
            } else {
                return Ok(None);
            };

        let redirect_uri = self
            .pending_request
            .uri
            .clone()
            .with_redirect(&redirect_location)?;
        let is_cross_host = redirect_uri.is_cross_host(&self.pending_request.uri);
        self.pending_request.uri = redirect_uri;

        if is_cross_host {
            for sensitive_header in &[
                hyper::header::AUTHORIZATION,
                hyper::header::COOKIE,
                hyper::header::PROXY_AUTHORIZATION,
                hyper::header::WWW_AUTHENTICATE,
            ] {
                self.pending_request.headers.remove(sensitive_header);
            }
        }

        match self.response.status() {
            StatusCode::MOVED_PERMANENTLY | StatusCode::FOUND | StatusCode::SEE_OTHER => {
                for sensitive_header in &[
                    hyper::header::TRANSFER_ENCODING,
                    hyper::header::CONTENT_ENCODING,
                    hyper::header::CONTENT_TYPE,
                    hyper::header::CONTENT_LENGTH,
                ] {
                    self.pending_request.headers.remove(sensitive_header);
                }

                if self.pending_request.method == Method::HEAD {
                    self.pending_request.method = Method::GET;
                }
            }
            _ => {}
        }

        Some(self.pending_request.to_request()).transpose()
    }

    /// Extracts location header from the current response and tries to convert it
    /// to a URI.
    fn extract_redirect_location_from_response(&self) -> Option<Uri> {
        self.response
            .headers()
            .get(hyper::header::LOCATION)
            .and_then(|location| Uri::try_from(location.as_bytes()).ok())
    }
}

#[cfg(test)]
mod tests {
    use httptest::matchers::*;
    use httptest::responders;
    use httptest::Expectation;

    use super::*;
    use crate::http::secure_client::SecureHttpClient;
    use crate::http::tls_config_with_system_roots;
    use crate::http::HttpClient;

    #[tokio::test]
    async fn test_follows_redirects() -> anyhow::Result<()> {
        let test_server = httptest::Server::run();
        // Chain of two redirects /foo -> /bar -> /baz.
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/foo"))
                .times(1)
                .respond_with(
                    responders::status_code(302).append_header(http::header::LOCATION, "/bar"),
                ),
        );
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/bar"))
                .times(1)
                .respond_with(
                    responders::status_code(302).append_header(http::header::LOCATION, "/baz"),
                ),
        );
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/baz"))
                .times(1)
                .respond_with(responders::status_code(200)),
        );

        let client = SecureHttpClient::new(tls_config_with_system_roots()?, 10);
        let resp = client.get(&test_server.url_str("/foo")).await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }

    #[tokio::test]
    async fn test_head_changes_to_get_on_redirect() -> anyhow::Result<()> {
        let test_server = httptest::Server::run();
        // Chain of two redirects /foo -> /bar -> /baz.
        test_server.expect(
            Expectation::matching(request::method_path("HEAD", "/foo"))
                .times(1)
                .respond_with(
                    responders::status_code(302).append_header(http::header::LOCATION, "/bar"),
                ),
        );
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/bar"))
                .times(1)
                .respond_with(responders::status_code(200)),
        );

        let client = SecureHttpClient::new(tls_config_with_system_roots()?, 10);
        let resp = client.head(&test_server.url_str("/foo")).await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }

    #[tokio::test]
    async fn test_post_gets_redirected() -> anyhow::Result<()> {
        let test_server = httptest::Server::run();
        // Redirect /foo -> /bar
        test_server.expect(
            Expectation::matching(all_of![
                request::method_path("POST", "/foo"),
                request::body("Hello, world!"),
            ])
            .times(1)
            .respond_with(
                responders::status_code(307).append_header(http::header::LOCATION, "/bar"),
            ),
        );
        test_server.expect(
            Expectation::matching(all_of![
                request::method_path("POST", "/bar"),
                request::body("Hello, world!"),
                request::headers(not(contains(key(hyper::header::ORIGIN.as_str())))),
                request::headers(not(contains(key(hyper::header::AUTHORIZATION.as_str())))),
                request::headers(not(contains(key(hyper::header::WWW_AUTHENTICATE.as_str())))),
                request::headers(not(contains(key(hyper::header::COOKIE.as_str())))),
                request::headers(not(contains(key(
                    hyper::header::PROXY_AUTHORIZATION.as_str()
                )))),
            ])
            .times(1)
            .respond_with(responders::status_code(200)),
        );

        let client = SecureHttpClient::new(tls_config_with_system_roots()?, 10);
        let bytes = Bytes::from_static(b"Hello, world!");
        let resp = client
            .post(
                &test_server.url_str("/foo"),
                bytes,
                vec![("key".to_owned(), "value".to_owned())],
            )
            .await?;
        assert_eq!(200, resp.status().as_u16());

        Ok(())
    }

    #[tokio::test]
    async fn test_too_many_redirects_fails() -> anyhow::Result<()> {
        let test_server = httptest::Server::run();
        // Chain of three redirects /foo -> /bar -> /baz -> /boo.
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/foo"))
                .times(1)
                .respond_with(
                    responders::status_code(302).append_header(http::header::LOCATION, "/bar"),
                ),
        );
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/bar"))
                .times(1)
                .respond_with(
                    responders::status_code(302).append_header(http::header::LOCATION, "/baz"),
                ),
        );
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/baz"))
                .times(1)
                .respond_with(
                    responders::status_code(302).append_header(http::header::LOCATION, "/boo"),
                ),
        );
        test_server.expect(
            Expectation::matching(request::method_path("GET", "/boo"))
                .times(0)
                .respond_with(responders::status_code(200)),
        );

        let client = SecureHttpClient::new(tls_config_with_system_roots()?, 1);
        let url = test_server.url_str("/foo");
        let result = client.get(&url).await;
        if let HttpError::TooManyRedirects { uri, max_redirects } = result.as_ref().err().unwrap() {
            assert_eq!(url.to_owned(), *uri);
            assert_eq!(1, *max_redirects);
        } else {
            unreachable!(
                "Expected HttpError::TooManyRedirects, got {:?}",
                result.err().unwrap()
            );
        }

        Ok(())
    }
}
