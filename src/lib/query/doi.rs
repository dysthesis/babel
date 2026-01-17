use memchr::memchr;
use std::{borrow::Cow, iter};

use super::util::strip_prefix_ignore_ascii_case;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum DoiPrefixKind {
    None,
    /// Plain `doi:` prefix – percent signs are treated literally.
    Doi,
    /// HTTP/HTTPS/DX prefixes where percent-escapes must be valid.
    HttpLike,
    /// `urn:doi:` prefix – percent-escapes must be valid.
    Urn,
}

impl DoiPrefixKind {
    #[inline]
    fn requires_percent_validation(self) -> bool {
        matches!(self, DoiPrefixKind::HttpLike | DoiPrefixKind::Urn)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Doi<'a>(Cow<'a, str>);

impl<'a> Doi<'a> {
    #[inline]
    pub(crate) fn parse(input: &'a str) -> Option<Self> {
        DoiInput::new(input).parse()
    }

    #[inline]
    pub(crate) fn into_cow(self) -> Cow<'a, str> {
        self.0
    }

    #[inline]
    fn normalise(cow: Cow<'a, str>) -> Self {
        Self(cow).strip_trailing()
    }

    #[inline]
    #[cfg(test)]
    pub(crate) fn strip_trailing_borrowed(s: &'a str) -> Self {
        Self(Cow::Borrowed(s)).strip_trailing()
    }

    #[inline]
    fn strip_trailing(self) -> Self {
        let Self(s) = self;
        let res = match s {
            Cow::Borrowed(b) => Self::trim_borrowed(b),
            Cow::Owned(mut o) => {
                let new_len = Self::trim_len(o.as_bytes());
                if new_len < o.len() {
                    o.truncate(new_len);
                }
                Cow::Owned(o)
            }
        };

        Self(res)
    }

    #[inline]
    #[cfg(test)]
    pub fn inner(self) -> Cow<'a, str> {
        let Self(inner) = self;
        inner
    }

    #[inline]
    fn trim_borrowed(b: &'a str) -> Cow<'a, str> {
        let new_len = Self::trim_len(b.as_bytes());
        if new_len == b.len() {
            Cow::Borrowed(b)
        } else {
            Cow::Borrowed(&b[..new_len])
        }
    }
    fn is_punct(b: &u8) -> bool {
        matches!(b, b'.' | b',' | b')')
    }

    #[inline]
    fn trim_len(bytes: &[u8]) -> usize {
        iter::successors(Some(bytes), |s| {
            // There is one trailing punctuation
            if let Some(b'.' | b',' | b')') = s.last() {
                return Some(&s[..s.len() - 1]);
            }

            // There is one trailing percent-encoded punctuation
            if s.len() >= 3 {
                let chunk = match s.get(s.len() - 3..) {
                    Some(val) => val,
                    None => {
                        return Some(&[]);
                    }
                };

                let (percent, rest) = chunk.split_at(1);
                if percent[0] == b'%' && rest.iter().all(u8::is_ascii_hexdigit) {
                    let decoded = (Self::decode_hex(chunk[1]) << 4) | Self::decode_hex(chunk[2]);
                    if Self::is_punct(&decoded) {
                        return Some(&s[..s.len() - 3]);
                    }
                }
            }
            None
        })
        .last()
        .unwrap_or(&[])
        .len()
    }

    #[inline]
    const fn decode_hex(h: u8) -> u8 {
        match h {
            b'0'..=b'9' => h - b'0',
            b'a'..=b'f' => 10 + (h - b'a'),
            b'A'..=b'F' => 10 + (h - b'A'),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Copy)]
struct DoiInput<'a> {
    raw: &'a str,
}

impl<'a> DoiInput<'a> {
    #[inline]
    fn new(raw: &'a str) -> Self {
        Self { raw }
    }

    #[inline]
    fn parse(self) -> Option<Doi<'a>> {
        let (rest, prefix) = self.strip_wrappers();
        let validate_percent = prefix.requires_percent_validation();

        DoiStream::new(rest, validate_percent)
            .process()
            .map(Doi::normalise)
    }

    #[inline]
    fn strip_wrappers(self) -> (&'a str, DoiPrefixKind) {
        const WRAPPERS: [&str; 6] = [
            "doi:",
            "urn:doi:",
            "https://doi.org/",
            "http://doi.org/",
            "https://dx.doi.org/",
            "http://dx.doi.org/",
        ];

        for w in WRAPPERS {
            if let Some(rest) = strip_prefix_ignore_ascii_case(self.raw, w) {
                let kind = match w {
                    "doi:" => DoiPrefixKind::Doi,
                    "urn:doi:" => DoiPrefixKind::Urn,
                    _ => DoiPrefixKind::HttpLike,
                };
                return (rest, kind);
            }
        }

        (self.raw, DoiPrefixKind::None)
    }
}

/// Parser for DOI
#[derive(Debug)]
struct DoiStream<'a> {
    source: &'a str,
    validate_percent: bool,
    first_slash: Option<usize>,
    out: Option<Vec<u8>>,
    skip: u8,
}

impl<'a> DoiStream<'a> {
    #[inline]
    fn new(source: &'a str, validate_percent: bool) -> Self {
        Self {
            source,
            validate_percent,
            first_slash: None,
            out: None,
            skip: 0,
        }
    }

    #[inline]
    fn process(mut self) -> Option<Cow<'a, str>> {
        let bytes = self.source.as_bytes();

        let slash = memchr(b'/', bytes)?;
        if slash == 0 || slash + 1 == bytes.len() {
            return None;
        }
        self.first_slash = Some(slash);

        if self.validate_percent {
            if !bytes.is_ascii() {
                return self.process_per_byte(bytes);
            }
            return self.process_percent_fast(bytes);
        }

        if bytes.is_ascii()
            && !bytes
                .iter()
                .any(|b| b.is_ascii_control() || b.is_ascii_whitespace())
        {
            return Some(Cow::Borrowed(self.source));
        }

        self.process_per_byte(bytes)
    }

    #[inline]
    fn consume(&mut self, idx: usize, byte: u8) -> Result<(), ()> {
        if self.skip > 0 {
            self.skip -= 1;
            return Ok(());
        }

        match byte {
            b'/' => self.record_slash(idx),
            b'%' if self.validate_percent => self.record_percent(idx),
            _ => self.record_other(idx, byte),
        }
    }

    #[inline]
    fn record_slash(&mut self, idx: usize) -> Result<(), ()> {
        if self.first_slash.is_none() {
            self.first_slash = Some(idx);
        }
        if let Some(ref mut out) = self.out {
            out.push(b'/');
        }
        Ok(())
    }

    #[inline]
    fn record_other(&mut self, idx: usize, byte: u8) -> Result<(), ()> {
        if byte.is_ascii_control() || byte.is_ascii_whitespace() {
            return Err(());
        }
        if byte < 0x80 {
            if let Some(ref mut out) = self.out {
                out.push(byte);
            }
            return Ok(());
        }
        self.record_unicode(idx)
    }

    #[inline]
    fn process_per_byte(mut self, bytes: &[u8]) -> Option<Cow<'a, str>> {
        bytes
            .iter()
            .copied()
            .enumerate()
            .try_for_each(|(idx, byte)| self.consume(idx, byte))
            .ok()?;
        self.finish()
    }

    #[inline]
    fn process_percent_fast(mut self, bytes: &[u8]) -> Option<Cow<'a, str>> {
        let mut i = 0;
        while let Some(rel) = memchr(b'%', &bytes[i..]) {
            let percent_idx = i + rel;

            if bytes[i..percent_idx]
                .iter()
                .any(|b| b.is_ascii_control() || b.is_ascii_whitespace())
            {
                return None;
            }

            if let Some(ref mut out) = self.out {
                out.extend_from_slice(&bytes[i..percent_idx]);
            } else {
                self.ensure_out(percent_idx);
            }

            if percent_idx + 2 >= bytes.len() {
                return None;
            }
            let hi = bytes[percent_idx + 1];
            let lo = bytes[percent_idx + 2];
            if !(hi.is_ascii_hexdigit() && lo.is_ascii_hexdigit()) {
                return None;
            }
            let decoded = (Doi::decode_hex(hi) << 4) | Doi::decode_hex(lo);
            self.out.as_mut().unwrap().push(decoded);

            i = percent_idx + 3;
        }

        if bytes[i..]
            .iter()
            .any(|b| b.is_ascii_control() || b.is_ascii_whitespace())
        {
            return None;
        }

        if let Some(ref mut out) = self.out {
            out.extend_from_slice(&bytes[i..]);
        }

        self.finish()
    }

    #[inline]
    fn record_percent(&mut self, idx: usize) -> Result<(), ()> {
        let bytes = self.source.as_bytes();
        let hi = *bytes.get(idx + 1).ok_or(())?;
        let lo = *bytes.get(idx + 2).ok_or(())?;

        if !(hi.is_ascii_hexdigit() && lo.is_ascii_hexdigit()) {
            return Err(());
        }

        let decoded = (Doi::decode_hex(hi) << 4) | Doi::decode_hex(lo);
        let out = self.ensure_out(idx);
        out.push(decoded);
        self.skip = 2; // skip over the two hex digits already consumed
        Ok(())
    }

    #[inline]
    fn record_unicode(&mut self, idx: usize) -> Result<(), ()> {
        if self.validate_percent {
            return Err(());
        }

        let ch = self.source[idx..].chars().next().ok_or(())?;
        if ch.is_control() || ch.is_whitespace() {
            return Err(());
        }

        let out = self.ensure_out(idx);
        let mut buf = [0u8; 4];
        let n = ch.encode_utf8(&mut buf).len();
        out.extend_from_slice(&buf[..n]);
        self.skip = ch.len_utf8().saturating_sub(1) as u8;
        Ok(())
    }

    #[inline]
    fn ensure_out(&mut self, upto: usize) -> &mut Vec<u8> {
        self.out.get_or_insert_with(|| {
            let mut v = Vec::with_capacity(self.source.len());
            v.extend_from_slice(&self.source.as_bytes()[..upto]);
            v
        })
    }

    #[inline]
    fn finish(self) -> Option<Cow<'a, str>> {
        let slash = self.first_slash?;
        if slash == 0 || slash == self.source.len() - 1 {
            return None;
        }

        match self.out {
            None => Some(Cow::Borrowed(self.source)),
            Some(bytes) => String::from_utf8(bytes).ok().map(Cow::Owned),
        }
    }
}
