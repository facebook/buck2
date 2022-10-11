//! Possible ZIP compression methods.

use std::fmt;

#[allow(deprecated)]
/// Identifies the storage format used to compress a file within a ZIP archive.
///
/// Each file's compression method is stored alongside it, allowing the
/// contents to be read without context.
///
/// When creating ZIP files, you may choose the method to use with
/// [`zip::write::FileOptions::compression_method`]
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum CompressionMethod {
    /// Store the file as is
    Stored,
    /// Compress the file using Deflate
    #[cfg(any(
        feature = "deflate",
        feature = "deflate-miniz",
        feature = "deflate-zlib"
    ))]
    Deflated,
    /// Compress the file using BZIP2
    #[cfg(feature = "bzip2")]
    Bzip2,
    /// Unsupported compression method
    #[deprecated(since = "0.5.7", note = "use the constants instead")]
    Unsupported(u16),
}
#[allow(deprecated, missing_docs)]
/// All compression methods defined for the ZIP format
impl CompressionMethod {
    pub const STORE: Self = CompressionMethod::Stored;
    pub const SHRINK: Self = CompressionMethod::Unsupported(1);
    pub const REDUCE_1: Self = CompressionMethod::Unsupported(2);
    pub const REDUCE_2: Self = CompressionMethod::Unsupported(3);
    pub const REDUCE_3: Self = CompressionMethod::Unsupported(4);
    pub const REDUCE_4: Self = CompressionMethod::Unsupported(5);
    pub const IMPLODE: Self = CompressionMethod::Unsupported(6);
    #[cfg(any(
        feature = "deflate",
        feature = "deflate-miniz",
        feature = "deflate-zlib"
    ))]
    pub const DEFLATE: Self = CompressionMethod::Deflated;
    #[cfg(not(any(
        feature = "deflate",
        feature = "deflate-miniz",
        feature = "deflate-zlib"
    )))]
    pub const DEFLATE: Self = CompressionMethod::Unsupported(8);
    pub const DEFLATE64: Self = CompressionMethod::Unsupported(9);
    pub const PKWARE_IMPLODE: Self = CompressionMethod::Unsupported(10);
    #[cfg(feature = "bzip2")]
    pub const BZIP2: Self = CompressionMethod::Bzip2;
    #[cfg(not(feature = "bzip2"))]
    pub const BZIP2: Self = CompressionMethod::Unsupported(12);
    pub const LZMA: Self = CompressionMethod::Unsupported(14);
    pub const IBM_ZOS_CMPSC: Self = CompressionMethod::Unsupported(16);
    pub const IBM_TERSE: Self = CompressionMethod::Unsupported(18);
    pub const ZSTD_DEPRECATED: Self = CompressionMethod::Unsupported(20);
    pub const ZSTD: Self = CompressionMethod::Unsupported(93);
    pub const MP3: Self = CompressionMethod::Unsupported(94);
    pub const XZ: Self = CompressionMethod::Unsupported(95);
    pub const JPEG: Self = CompressionMethod::Unsupported(96);
    pub const WAVPACK: Self = CompressionMethod::Unsupported(97);
    pub const PPMD: Self = CompressionMethod::Unsupported(98);
}
impl CompressionMethod {
    /// Converts an u16 to its corresponding CompressionMethod
    #[deprecated(
        since = "0.5.7",
        note = "use a constant to construct a compression method"
    )]
    pub fn from_u16(val: u16) -> CompressionMethod {
        #[allow(deprecated)]
        match val {
            0 => CompressionMethod::Stored,
            #[cfg(any(
                feature = "deflate",
                feature = "deflate-miniz",
                feature = "deflate-zlib"
            ))]
            8 => CompressionMethod::Deflated,
            #[cfg(feature = "bzip2")]
            12 => CompressionMethod::Bzip2,

            v => CompressionMethod::Unsupported(v),
        }
    }

    /// Converts a CompressionMethod to a u16
    #[deprecated(
        since = "0.5.7",
        note = "to match on other compression methods, use a constant"
    )]
    pub fn to_u16(self) -> u16 {
        #[allow(deprecated)]
        match self {
            CompressionMethod::Stored => 0,
            #[cfg(any(
                feature = "deflate",
                feature = "deflate-miniz",
                feature = "deflate-zlib"
            ))]
            CompressionMethod::Deflated => 8,
            #[cfg(feature = "bzip2")]
            CompressionMethod::Bzip2 => 12,
            CompressionMethod::Unsupported(v) => v,
        }
    }
}

impl fmt::Display for CompressionMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Just duplicate what the Debug format looks like, i.e, the enum key:
        write!(f, "{:?}", self)
    }
}

#[cfg(test)]
mod test {
    use super::CompressionMethod;

    #[test]
    fn from_eq_to() {
        for v in 0..(::std::u16::MAX as u32 + 1) {
            #[allow(deprecated)]
            let from = CompressionMethod::from_u16(v as u16);
            #[allow(deprecated)]
            let to = from.to_u16() as u32;
            assert_eq!(v, to);
        }
    }

    fn methods() -> Vec<CompressionMethod> {
        let mut methods = Vec::new();
        methods.push(CompressionMethod::Stored);
        #[cfg(any(
            feature = "deflate",
            feature = "deflate-miniz",
            feature = "deflate-zlib"
        ))]
        methods.push(CompressionMethod::Deflated);
        #[cfg(feature = "bzip2")]
        methods.push(CompressionMethod::Bzip2);
        methods
    }

    #[test]
    fn to_eq_from() {
        fn check_match(method: CompressionMethod) {
            #[allow(deprecated)]
            let to = method.to_u16();
            #[allow(deprecated)]
            let from = CompressionMethod::from_u16(to);
            #[allow(deprecated)]
            let back = from.to_u16();
            assert_eq!(to, back);
        }

        for method in methods() {
            check_match(method);
        }
    }

    #[test]
    fn to_display_fmt() {
        fn check_match(method: CompressionMethod) {
            let debug_str = format!("{:?}", method);
            let display_str = format!("{}", method);
            assert_eq!(debug_str, display_str);
        }

        for method in methods() {
            check_match(method);
        }
    }
}
