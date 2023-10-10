/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::net::IpAddr;
use std::str::FromStr;

use anyhow::Context;
use http::uri::InvalidUri;
use http::uri::PathAndQuery;
use http::uri::Scheme;
use http::Uri;
use hyper_proxy::Intercept;
use hyper_proxy::Proxy;
use ipnetwork::IpNetwork;

/// Lookup environment variable and return string value. Checks first for uppercase
/// and falls back to lowercase if unset.
fn env_to_string(env: &'static str) -> anyhow::Result<Option<String>> {
    std::env::var_os(env)
        .or_else(|| std::env::var_os(env.to_lowercase()))
        .map(|s| s.into_string())
        .transpose()
        .map_err(|original| anyhow::anyhow!("Invalid utf8 string: '{:?}'", original))
}

fn noproxy_from_env(scheme: Scheme) -> anyhow::Result<Option<NoProxy>> {
    Ok(env_to_string("NO_PROXY")?.map(|no_proxy| NoProxy::new(scheme, no_proxy)))
}

/// Returns a hyper_proxy::Proxy struct that proxies connections to the uri at
/// $HTTPS_PROXY (or $https_proxy if the former is unset). Respects $NO_PROXY.
pub(super) fn https_proxy_from_env() -> anyhow::Result<Option<Proxy>> {
    if let Some(https_proxy) = env_to_string("HTTPS_PROXY")? {
        let uri: DefaultSchemeUri = https_proxy
            .parse()
            .with_context(|| format!("Invalid HTTPS_PROXY uri: {}", https_proxy))?;
        if let Some(no_proxy) = noproxy_from_env(Scheme::HTTPS)? {
            Ok(Some(Proxy::new(
                no_proxy.into_proxy_intercept(),
                uri.into(),
            )))
        } else {
            Ok(Some(Proxy::new(Intercept::Https, uri.into())))
        }
    } else {
        Ok(None)
    }
}

/// Returns a hyper_proxy::Proxy struct that proxies connections to the uri at
/// $HTTP_PROXY (or $http_proxy if the former is unset). Respects $NO_PROXY.
pub(super) fn http_proxy_from_env() -> anyhow::Result<Option<Proxy>> {
    if let Some(http_proxy) = env_to_string("HTTP_PROXY")? {
        let uri: DefaultSchemeUri = http_proxy
            .parse()
            .with_context(|| format!("Invalid HTTP_PROXY uri: {}", http_proxy))?;
        if let Some(no_proxy) = noproxy_from_env(Scheme::HTTP)? {
            Ok(Some(Proxy::new(
                no_proxy.into_proxy_intercept(),
                uri.into(),
            )))
        } else {
            Ok(Some(Proxy::new(Intercept::Http, uri.into())))
        }
    } else {
        Ok(None)
    }
}

/// A wrapped Uri that handles inserting a default scheme (http) if one is not present.
///
/// See https://everything.curl.dev/usingcurl/proxies/type for more information about
/// how curl treats default schemes for e.g. proxy env vars.
pub(super) struct DefaultSchemeUri(pub(super) Uri);

impl FromStr for DefaultSchemeUri {
    type Err = InvalidUri;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse::<Uri>().map(Self)
    }
}

impl From<DefaultSchemeUri> for Uri {
    fn from(default_scheme_uri: DefaultSchemeUri) -> Self {
        let mut parts = default_scheme_uri.0.into_parts();
        if parts.scheme.is_none() {
            parts.scheme = Some(Scheme::HTTP);
        }
        if parts.path_and_query.is_none() {
            parts.path_and_query = Some(PathAndQuery::from_static("/"));
        }
        Uri::from_parts(parts).expect("Got invalid uri from formerly valid uri")
    }
}

#[derive(Debug)]
struct Domain(String);

impl Domain {
    /// Returns whether this domain "matches" candidate according to Curl's rules
    /// for NO_PROXY.
    ///
    /// See https://github.com/curl/curl/issues/1208 for a bit of discussion about
    /// some of the particulars of subdomain matching.
    fn is_match<S: AsRef<str>>(&self, candidate: S) -> bool {
        let candidate = candidate.as_ref();
        // * unambiguously matches all domains.
        self.0 == "*"
            // Exact match
            || self.0 == candidate
            // .<domain> matches all subdomains, look for exact match
            || self.0.trim_start_matches('.') == candidate
            // Candidate suffixed by domain, only match if candidate is a subdomain of domain
            // Ex: domain=".facebook.com" matches "images.facebook.com" but not "www.thefacebook.com"
            || candidate.trim_end_matches(self.0.as_str().trim_start_matches('.')).ends_with('.')
    }
}

/// Wrapper for the parsed version of Curl's "no proxy" format from the standard
/// NO_PROXY / no_proxy environment variables.
///
/// Uses hyper-proxy's Intercept::Custom to drive matching logic for whether Uris
/// should be proxied or not.
///
/// Incorporates a scheme to proxy connections as well. Connections matching this
/// (scheme, no_proxy_spec) pair will *not* be proxied.
///
/// Matching logic derived from reqwest::NoProxy (e.g. [here](https://github.com/seanmonstar/reqwest/blob/master/src/proxy.rs#L467)).
pub(super) struct NoProxy {
    addresses: Vec<IpAddr>,
    networks: Vec<IpNetwork>,
    domains: Vec<Domain>,
    proxy_scheme: Scheme,
}

impl NoProxy {
    /// Constructs a new NoProxy struct. Connections made to a host matching the
    /// no_proxy spec with a scheme of `proxy_scheme` WILL NOT be proxied (e.g.
    /// requests to these hosts with `proxy_scheme` will go directly to the dest).
    pub(super) fn new<S: AsRef<str>>(proxy_scheme: Scheme, s: S) -> Self {
        let mut addresses = Vec::new();
        let mut networks = Vec::new();
        let mut domains = Vec::new();
        let s = s.as_ref();
        for entity in s.split(',').map(str::trim) {
            if let Ok(network) = entity.parse::<IpNetwork>() {
                networks.push(network);
            } else if let Ok(address) = entity.parse::<IpAddr>() {
                addresses.push(address);
            } else {
                domains.push(Domain(entity.to_owned()));
            }
        }

        Self {
            addresses,
            networks,
            domains,
            proxy_scheme,
        }
    }

    /// Returns whether `host` matches any of the NO_PROXY entities - i.e. that we
    /// should *not* proxy requests to this host.
    fn should_bypass_proxy_for_host<S: AsRef<str>>(&self, host: S) -> bool {
        let host = host.as_ref();
        if let Ok(host_address) = host.parse::<IpAddr>() {
            self.addresses
                .iter()
                .any(|address| host_address == *address)
                || self
                    .networks
                    .iter()
                    .any(|network| network.contains(host_address))
        } else {
            self.domains.iter().any(|domain| domain.is_match(host))
        }
    }

    /// Converts this NoProxy spec into a hyper_proxy::Intercept::Custom closure
    /// so it can be used to build a new hyper_proxy::Proxy.
    ///
    /// Note: There's a tricky bit of logic below. We explicitly *negate* the return
    /// condition of the closure because of the way hyper_proxy::Intercept::Custom's
    /// closure works; if it returns `true`, the connection is proxied.
    ///
    /// For NoProxy, we want to *negate* this logic - if a (scheme, host) pair
    /// match our NoProxy instance, we _don't want to proxy_. Therefore we negate
    /// the return conditions below.
    ///
    /// Some examples to clarify:
    ///
    /// NO_PROXY=".facebook.com" for HTTPS
    ///     does not proxy https://www.facebook.com
    ///     does not proxy https://images.facebook.com
    ///     does proxy https://www.thefacebook.com
    ///     does proxy http://www.thefacebook.com
    ///
    /// NO_PROXY="192.168.0.1" for HTTP
    ///     does not proxy http://192.168.0.1
    ///     does proxy https://192.168.0.1
    ///     does proxy http://192.168.0.2
    pub(super) fn into_proxy_intercept(self) -> Intercept {
        let should_proxy = move |scheme: Option<&str>, host: Option<&str>, _port: Option<u16>| {
            // IPv6 addresses are wrapped in [ ] so remove those for equality checks.
            let host = host.map(|h| h.trim_start_matches('[').trim_end_matches(']'));
            let should_bypass_proxy =
                host.map_or(false, |host| self.should_bypass_proxy_for_host(host));

            // Negation happens here - true means we're going to proxy the connection.
            !should_bypass_proxy
                && self.proxy_scheme.as_str() == scheme.unwrap_or(Scheme::HTTP.as_str())
        };
        should_proxy.into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn uri(s: &'static str) -> Uri {
        s.parse().unwrap()
    }

    #[test]
    fn test_domain_match() {
        let d = Domain(".facebook.com".to_owned());
        assert!(d.is_match("www.facebook.com"));
        assert!(!d.is_match("boofacebook.com"));

        let d = Domain("facebook.com".to_owned());
        assert!(d.is_match("facebook.com"));
        assert!(d.is_match("www.facebook.com"));

        let d = Domain("photos.facebook.com".to_owned());
        assert!(!d.is_match("facebook.com"));
        assert!(d.is_match("jpg.photos.facebook.com"));

        let d = Domain("*".to_owned());
        assert!(d.is_match("www.facebook.com"));
        assert!(d.is_match("facebook.com"));
    }

    #[test]
    fn test_noproxy_empty_string_does_not_match() {
        let noproxy = NoProxy::new(Scheme::HTTP, "");
        assert!(!noproxy.should_bypass_proxy_for_host("facebook.com"));
    }

    #[test]
    fn test_noproxy_matches_ip_address() {
        let noproxy = NoProxy::new(Scheme::HTTP, "192.168.0.1");
        assert!(noproxy.should_bypass_proxy_for_host("192.168.0.1"));
    }

    #[test]
    fn test_noproxy_matches_ip_network() {
        let noproxy = NoProxy::new(Scheme::HTTP, "192.168.0.0/16");
        assert!(noproxy.should_bypass_proxy_for_host("192.168.0.1"));
    }

    #[test]
    fn test_noproxy_matches_subdomain() {
        let noproxy = NoProxy::new(Scheme::HTTP, ".facebook.com");
        assert!(noproxy.should_bypass_proxy_for_host("images.facebook.com"));
    }

    #[test]
    fn test_noproxy_matches_multiple() {
        let noproxy = NoProxy::new(Scheme::HTTP, ".facebook.com, 192.168.0.0/24, 28.0.0.1");
        assert!(noproxy.should_bypass_proxy_for_host("images.facebook.com"));
        assert!(noproxy.should_bypass_proxy_for_host("192.168.0.1"));
        assert!(!noproxy.should_bypass_proxy_for_host("28.0.0.2"));
    }

    #[test]
    fn test_noproxy_intercept_does_not_proxy_for_ip_addr_match() {
        let noproxy = NoProxy::new(Scheme::HTTPS, "192.168.0.1");
        let intercept = noproxy.into_proxy_intercept();
        // DON'T proxy https connections to 192.168.0.1 because it's an IP match
        assert!(!intercept.matches(&uri("https://192.168.0.1/foo")));
        // DON'T proxy http connections to 192.168.0.1 because it's a different scheme
        assert!(!intercept.matches(&uri("http://192.168.0.1/foo")));
        // DO proxy https connections to 192.168.0.2 because no IP match and schemes match
        assert!(intercept.matches(&uri("https://192.168.0.2/bar")));
    }

    #[test]
    fn test_noproxy_intercept_does_not_proxy_for_ip_net_match() {
        let noproxy = NoProxy::new(Scheme::HTTPS, "192.168.0.0/24");
        let intercept = noproxy.into_proxy_intercept();
        // DON'T proxy https to 192.168.0.1 because IP and scheme match
        assert!(!intercept.matches(&uri("https://192.168.0.1/foo")));
        // DO proxy https to 192.168.1.1 because IP mismatch and scheme match
        assert!(intercept.matches(&uri("https://192.168.1.1/foo")));
        // DON'T proxy http to 192.168.0.1 because scheme mismatch
        assert!(!intercept.matches(&uri("http://192.168.0.1/foo")));
    }

    #[test]
    fn test_noproxy_intercept_does_not_proxy_for_domain_match() {
        let noproxy = NoProxy::new(Scheme::HTTPS, ".facebook.com");
        let intercept = noproxy.into_proxy_intercept();
        // DON'T proxy because scheme matches and is subdomain.
        assert!(!intercept.matches(&uri("https://www.facebook.com/foo/bar")));
        // DO proxy because scheme matches but domain is different.
        assert!(intercept.matches(&uri("https://www.thefacebook.com/foo/bar")));
        // DON'T proxy because scheme mismatch
        assert!(!intercept.matches(&uri("http://www.facebook.com/foo/bar")));
    }

    #[test]
    fn test_noproxy_intercept_does_not_proxy_for_scheme_mismatch() {
        let noproxy = NoProxy::new(Scheme::HTTP, ".facebook.com");
        let intercept = noproxy.into_proxy_intercept();
        assert!(!intercept.matches(&uri("https://www.facebook.com/foo/bar")));
    }
}
