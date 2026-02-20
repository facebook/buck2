/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use quick_xml::Reader;
use quick_xml::events::BytesStart;
use quick_xml::events::Event;
use starlark::environment::GlobalsBuilder;
use starlark::starlark_module;
use starlark::values::Heap;
use starlark::values::Value;
use starlark::values::dict::AllocDict;
use starlark::values::list::AllocList;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum XmlDecodeError {
    #[error("xml.decode: unexpected closing tag without matching open tag")]
    UnexpectedClosingTag,
    #[error("xml.decode: no root element found in XML input")]
    NoRootElement,
    #[error("xml.decode: {0}")]
    XmlError(String),
    #[error("xml.decode: invalid UTF-8 in XML content")]
    Utf8Error,
}

/// An XML element collected during parsing, before allocation on the Starlark heap.
struct XmlElement {
    tag: String,
    attrs: Vec<(String, String)>,
    text: String,
    children: Vec<XmlElement>,
}

impl XmlElement {
    fn alloc<'v>(self, heap: Heap<'v>) -> Value<'v> {
        let children: Vec<Value<'v>> = self.children.into_iter().map(|c| c.alloc(heap)).collect();

        let tag = heap.alloc_str(&self.tag).to_value();
        let attrs =
            heap.alloc(AllocDict(self.attrs.into_iter().map(|(k, v)| {
                (heap.alloc_str(&k).to_value(), heap.alloc_str(&v).to_value())
            })));
        let text = heap.alloc_str(&self.text).to_value();
        let children_val = heap.alloc(AllocList(children));

        heap.alloc(AllocDict([
            ("tag", tag),
            ("attrs", attrs),
            ("text", text),
            ("children", children_val),
        ]))
    }
}

fn parse_attrs(
    reader: &Reader<&[u8]>,
    e: &BytesStart,
) -> Result<Vec<(String, String)>, XmlDecodeError> {
    e.attributes()
        .map(|a| {
            let a = a.map_err(|e| XmlDecodeError::XmlError(e.to_string()))?;
            let key = String::from_utf8(a.key.as_ref().to_vec())
                .map_err(|_| XmlDecodeError::Utf8Error)?;
            let val = a
                .decode_and_unescape_value(reader.decoder())
                .map_err(|e| XmlDecodeError::XmlError(e.to_string()))?
                .into_owned();
            Ok((key, val))
        })
        .collect()
}

/// Parse an XML string into a tree of `XmlElement`s using quick-xml's streaming reader.
fn parse_xml(input: &str) -> Result<XmlElement, XmlDecodeError> {
    let mut reader = Reader::from_str(input);
    reader.config_mut().trim_text(true);

    let mut stack: Vec<XmlElement> = Vec::new();
    let mut root: Option<XmlElement> = None;

    loop {
        match reader
            .read_event()
            .map_err(|e| XmlDecodeError::XmlError(e.to_string()))?
        {
            Event::Start(e) => {
                let tag = String::from_utf8(e.name().as_ref().to_vec())
                    .map_err(|_| XmlDecodeError::Utf8Error)?;
                let attrs = parse_attrs(&reader, &e)?;
                stack.push(XmlElement {
                    tag,
                    attrs,
                    text: String::new(),
                    children: Vec::new(),
                });
            }
            Event::End(_) => {
                let elem = stack.pop().ok_or(XmlDecodeError::UnexpectedClosingTag)?;
                if let Some(parent) = stack.last_mut() {
                    parent.children.push(elem);
                } else {
                    root = Some(elem);
                }
            }
            Event::Empty(e) => {
                let tag = String::from_utf8(e.name().as_ref().to_vec())
                    .map_err(|_| XmlDecodeError::Utf8Error)?;
                let attrs = parse_attrs(&reader, &e)?;
                let elem = XmlElement {
                    tag,
                    attrs,
                    text: String::new(),
                    children: Vec::new(),
                };
                if let Some(parent) = stack.last_mut() {
                    parent.children.push(elem);
                } else {
                    root = Some(elem);
                }
            }
            Event::Text(e) => {
                let text = e
                    .unescape()
                    .map_err(|e| XmlDecodeError::XmlError(e.to_string()))?
                    .into_owned();
                if let Some(current) = stack.last_mut() {
                    current.text.push_str(&text);
                }
            }
            Event::CData(e) => {
                let text = String::from_utf8(e.into_inner().to_vec())
                    .map_err(|_| XmlDecodeError::Utf8Error)?;
                if let Some(current) = stack.last_mut() {
                    current.text.push_str(&text);
                }
            }
            Event::Eof => break,
            _ => {}
        }
    }

    root.ok_or(XmlDecodeError::NoRootElement)
}

#[starlark_module]
fn xml_members(globals: &mut GlobalsBuilder) {
    /// Decodes an XML string into a Starlark dict tree.
    ///
    /// Each XML element becomes a dict with four keys:
    /// - `"tag"`: the element name (string)
    /// - `"attrs"`: a dict of attribute name→value pairs
    /// - `"text"`: the direct text content of the element (string)
    /// - `"children"`: a list of child element dicts
    ///
    /// ```python
    /// doc = xml.decode('<root attr="1"><child>hello</child></root>')
    /// doc["tag"]  # "root"
    /// doc["attrs"]["attr"]  # "1"
    /// doc["children"][0]["text"]  # "hello"
    /// ```
    fn decode<'v>(
        #[starlark(require = pos)] x: &str,
        heap: Heap<'v>,
    ) -> starlark::Result<Value<'v>> {
        let root = parse_xml(x).map_err(buck2_error::Error::from)?;
        Ok(root.alloc(heap))
    }
}

pub(crate) fn register_xml(globals: &mut GlobalsBuilder) {
    globals.namespace("xml", xml_members);
}

#[cfg(test)]
mod tests {
    use starlark::assert::Assert;

    use crate::interpreter::functions::xml::register_xml;

    fn assert_with_xml() -> Assert<'static> {
        let mut a = Assert::new();
        a.globals_add(|gb| {
            gb.namespace("__internal__", register_xml);
        });
        a
    }

    #[test]
    fn test_xml_decode_simple() {
        assert_with_xml().eq("__internal__.xml.decode('<root/>')[\"tag\"]", "'root'");
    }

    #[test]
    fn test_xml_decode_attrs() {
        assert_with_xml().eq(
            "__internal__.xml.decode('<r a=\"1\" b=\"2\"/>')[\"attrs\"][\"a\"]",
            "'1'",
        );
    }

    #[test]
    fn test_xml_decode_text() {
        assert_with_xml().eq(
            "__internal__.xml.decode('<msg>hello world</msg>')[\"text\"]",
            "'hello world'",
        );
    }

    #[test]
    fn test_xml_decode_children() {
        assert_with_xml().eq(
            "len(__internal__.xml.decode('<root><a/><b/><c/></root>')[\"children\"])",
            "3",
        );
    }

    #[test]
    fn test_xml_decode_nested() {
        assert_with_xml().eq(
            "__internal__.xml.decode('<root><child>text</child></root>')[\"children\"][0][\"text\"]",
            "'text'",
        );
    }

    #[test]
    fn test_xml_decode_junit() {
        assert_with_xml().is_true(
            r#"
doc = __internal__.xml.decode('<testsuite name="suite1" tests="2"><testcase name="test1"/><testcase name="test2"><failure message="oops"/></testcase></testsuite>')
doc["tag"] == "testsuite" and doc["attrs"]["name"] == "suite1" and len(doc["children"]) == 2
"#,
        );
    }
}
