#![allow(unexpected_cfgs)]

use memchr::memchr;
use std::borrow::Cow;

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

/// A query to search for entries. It can be a full-text search, or a direct
/// identifier such as DOI or ISBN.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Query<'a> {
    /// A DOI identifier of an academic literature
    Doi(Cow<'a, str>),
    /// An ISBN identifier of a book.
    Isbn(Cow<'a, str>),
    /// A full text query over the title, author, synopsis, etc. of the entry.
    FullText(&'a str),
}

impl<'a> Query<'a> {
    /// Parse a text as a query.
    pub fn parse(from: &'a str) -> Query<'a> {
        let trimmed = from.trim();

        // Try to parse the identifier as an ISBN.
        let isbn_view = trimmed;
        if Isbn::parse(isbn_view).is_some() {
            return Query::Isbn(Cow::Borrowed(isbn_view));
        }

        // If it's not an ISBN, maybe it's a DOI.
        if let Some(doi) = parse_doi_like(trimmed) {
            return Query::Doi(doi);
        }

        // It must be a full text query, then.
        Query::FullText(from)
    }
}

#[inline]
fn strip_outer_wrappers(s: &str) -> Option<&str> {
    let bytes = s.as_bytes();
    if bytes.len() < 2 {
        return None;
    }

    match (bytes[0], bytes[bytes.len() - 1]) {
        (b'(', b')') | (b'[', b']') | (b'{', b'}') => Some(&s[1..s.len() - 1]),
        _ => None,
    }
}

fn parse_doi_like<'a>(s: &'a str) -> Option<Cow<'a, str>> {
    let (rest, prefix) = strip_doi_wrappers(s);
    let validate_percent = matches!(prefix, DoiPrefixKind::HttpLike | DoiPrefixKind::Urn);

    DoiStream::new(rest, validate_percent)
        .process()
        .map(strip_doi_trailing_decor_cow)
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
            let decoded = (decode_hex(hi) << 4) | decode_hex(lo);
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

        let decoded = (decode_hex(hi) << 4) | decode_hex(lo);
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

#[inline]
fn decode_hex(h: u8) -> u8 {
    match h {
        b'0'..=b'9' => h - b'0',
        b'a'..=b'f' => 10 + (h - b'a'),
        b'A'..=b'F' => 10 + (h - b'A'),
        _ => unreachable!(),
    }
}

#[inline]
fn strip_doi_trailing_decor_borrowed<'a>(s: &'a str) -> Cow<'a, str> {
    #[inline]
    fn trim_len(bytes: &[u8]) -> usize {
        let mut len = bytes.len();

        while len > 0 {
            match bytes[len - 1] {
                b'.' | b',' | b')' => {
                    len -= 1;
                    continue;
                }
                _ => {
                    if len >= 3 {
                        let trio = &bytes[len - 3..len];
                        if trio[0] == b'%'
                            && trio[1].is_ascii_hexdigit()
                            && trio[2].is_ascii_hexdigit()
                        {
                            let decoded = (decode_hex(trio[1]) << 4) | decode_hex(trio[2]);
                            if matches!(decoded, b'.' | b',' | b')') {
                                len -= 3;
                                continue;
                            }
                        }
                    }
                    break;
                }
            }
        }

        len
    }

    let new_len = trim_len(s.as_bytes());
    if new_len == s.len() {
        Cow::Borrowed(s)
    } else {
        Cow::Borrowed(&s[..new_len])
    }
}

#[inline]
fn strip_doi_trailing_decor_cow<'a>(s: Cow<'a, str>) -> Cow<'a, str> {
    match s {
        Cow::Borrowed(b) => strip_doi_trailing_decor_borrowed(b),
        Cow::Owned(mut o) => {
            let new_len = {
                let bytes = o.as_bytes();
                // Reuse the same trimming logic as the borrowed path.
                let mut len = bytes.len();
                while len > 0 {
                    match bytes[len - 1] {
                        b'.' | b',' | b')' => {
                            len -= 1;
                            continue;
                        }
                        _ => {
                            if len >= 3 {
                                let trio = &bytes[len - 3..len];
                                if trio[0] == b'%'
                                    && trio[1].is_ascii_hexdigit()
                                    && trio[2].is_ascii_hexdigit()
                                {
                                    let decoded = (decode_hex(trio[1]) << 4) | decode_hex(trio[2]);
                                    if matches!(decoded, b'.' | b',' | b')') {
                                        len -= 3;
                                        continue;
                                    }
                                }
                            }
                            break;
                        }
                    }
                }
                len
            };

            if new_len < o.len() {
                o.truncate(new_len);
            }
            Cow::Owned(o)
        }
    }
}

#[inline]
fn strip_prefix_ignore_ascii_case<'a>(s: &'a str, prefix: &str) -> Option<&'a str> {
    let s_bytes = s.as_bytes();
    let prefix_bytes = prefix.as_bytes();
    if s_bytes.len() < prefix_bytes.len() {
        return None;
    }
    if s_bytes[..prefix_bytes.len()]
        .iter()
        .zip(prefix_bytes.iter())
        .all(|(a, b)| a.eq_ignore_ascii_case(b))
    {
        Some(&s[prefix.len()..])
    } else {
        None
    }
}

#[inline]
fn strip_isbn_prefix(s: &str) -> &str {
    let rest = match strip_prefix_ignore_ascii_case(s, "isbn") {
        Some(r) => r,
        None => return s,
    };

    let offset = s.len() - rest.len();
    let mut idx = match rest.as_bytes() {
        [b'-', b'1', b'0' | b'3', b':', ..] => offset + 4,
        [b':', ..] | [b' ', ..] => offset + 1,
        _ => return s,
    };

    let bytes = s.as_bytes();
    while idx < bytes.len() && bytes[idx].is_ascii_whitespace() {
        idx += 1;
    }

    &s[idx..]
}

#[inline]
fn trim_trailing_and_validate_last(s: &str) -> Option<&str> {
    let bytes = s.as_bytes();
    let mut end = bytes.len();
    while end > 0 {
        let b = bytes[end - 1];
        if b.is_ascii_whitespace() || matches!(b, b'.' | b',' | b';' | b')' | b']' | b'}') {
            end -= 1;
            continue;
        }
        if !(b.is_ascii_digit() || b == b'X' || b == b'x') {
            return None;
        }
        return Some(&s[..end]);
    }
    None
}

#[inline]
fn collect_isbn_digits(s: &str) -> Option<([u8; 13], usize)> {
    if s.is_ascii() {
        return collect_isbn_digits_ascii(s.as_bytes());
    }

    collect_isbn_digits_unicode(s)
}

#[inline]
fn collect_isbn_digits_ascii(bytes: &[u8]) -> Option<([u8; 13], usize)> {
    let mut buf = [0u8; 13];
    let mut len = 0usize;

    for &b in bytes {
        match b {
            b'-' | b' ' | b'\t' | b'\n' | b'\r' => {}
            b'0'..=b'9' => {
                if len >= buf.len() {
                    return None;
                }
                buf[len] = b;
                len += 1;
            }
            b'x' | b'X' => {
                if len != 9 || len >= buf.len() {
                    return None;
                }
                buf[len] = b'X';
                len += 1;
            }
            _ => return None,
        }
    }

    Some((buf, len))
}

#[inline]
fn collect_isbn_digits_unicode(s: &str) -> Option<([u8; 13], usize)> {
    let mut buf = [0u8; 13];
    let mut len = 0usize;

    s.chars()
        .try_for_each(|ch| {
            if ch == '-' || ch.is_whitespace() {
                return Ok(());
            }
            if !ch.is_ascii() {
                return Err(());
            }

            let upper = ch.to_ascii_uppercase();
            let val = match upper {
                '0'..='9' => upper as u8,
                'X' => b'X',
                _ => return Err(()),
            };

            if len >= buf.len() || (val == b'X' && len != 9) {
                return Err(());
            }

            buf[len] = val;
            len += 1;
            Ok(())
        })
        .ok()?;

    Some((buf, len))
}

struct Isbn([u8; 13], usize);
impl Isbn {
    fn parse(input: &str) -> Option<Self> {
        let mut s = input.trim();

        while let Some(stripped) = strip_outer_wrappers(s) {
            s = stripped.trim();
        }

        s = strip_isbn_prefix(s);

        s = trim_trailing_and_validate_last(s)?;

        let (buf, len) = collect_isbn_digits(s)?;
        Self(buf, len).validate()
    }

    pub fn validate(self) -> Option<Self> {
        let Isbn(buf, size) = self;
        let res = Isbn(buf, size);
        match size {
            10 => res.validate_isbn_10(),
            13 => res.validate_isbn_13(),
            _ => None,
        }
    }

    fn validate_isbn_13(self) -> Option<Self> {
        let Isbn(buf, size) = self;
        let (rest, check) = buf.split_at(12);
        if rest.iter().all(u8::is_ascii_digit) {
            let sum: u8 = rest
                .iter()
                .map(|b| b - b'0')
                .enumerate()
                .map(|(i, d)| {
                    let w = if i % 2 == 0 { 1 } else { 3 };
                    d * w
                })
                .sum();

            let expected = (10 - (sum % 10)) % 10;

            if (check[0] - b'0') != expected {
                return None;
            }
            return Some(Isbn(buf, size));
        }

        None
    }

    fn validate_isbn_10(self) -> Option<Self> {
        let Isbn(buf, size) = self;
        let (rest, check) = buf.split_at(9);
        if rest.iter().all(u8::is_ascii_digit) {
            let sum: u32 = rest
                .iter()
                .map(|b| b - b'0')
                .enumerate()
                .map(|(i, d)| (d as u32) * (10 - i as u32))
                .sum();

            let expected = match (11 - (sum % 11)) % 11 {
                10 => 'X',
                other => char::from(b'0' + (other as u8)),
            };

            if (check[0] as char).to_ascii_uppercase() != expected {
                return None;
            }

            return Some(Self(buf, size));
        }
        None
    }
}

fn strip_doi_wrappers(s: &str) -> (&str, DoiPrefixKind) {
    const WRAPPERS: [&str; 6] = [
        "doi:",
        "urn:doi:",
        "https://doi.org/",
        "http://doi.org/",
        "https://dx.doi.org/",
        "http://dx.doi.org/",
    ];

    for w in WRAPPERS {
        if let Some(rest) = strip_prefix_ignore_ascii_case(s, w) {
            let kind = match w {
                "doi:" => DoiPrefixKind::Doi,
                "urn:doi:" => DoiPrefixKind::Urn,
                _ => DoiPrefixKind::HttpLike,
            };
            return (rest, kind);
        }
    }

    (s, DoiPrefixKind::None)
}

#[cfg(test)]
mod tests {
    #![allow(unexpected_cfgs)]

    use super::*;
    use percent_encoding::{AsciiSet, NON_ALPHANUMERIC, percent_decode_str, utf8_percent_encode};
    use proptest::prelude::*;
    use proptest::sample::select;
    use proptest::string::string_regex;

    // Case count used for property tests.
    const PROPTEST_CASES: u32 = 10_000;

    // Authoritative ISBN fixtures to decouple expectations from production helpers.
    const VALID_ISBN_CASES: &[(&str, &str)] = &[
        ("0-306-40615-2", "0306406152"),
        ("ISBN:0 85131 041 9", "0851310419"),
        ("ISBN-10: 0-19-852663-6", "0198526636"),
        ("978-0-306-40615-7", "9780306406157"),
        ("ISBN 978 1 4028 9462 6", "9781402894626"),
        ("9780306406157.", "9780306406157"),
        ("(9780306406157)", "9780306406157"),
        ("isbn:0-9752298-0-X", "097522980X"),
    ];

    const INVALID_ISBN_CASES: &[&str] = &[
        "0-306-40615-3",           // bad check digit for ISBN-10
        "978-0-306-40615-8",       // bad check digit for ISBN-13
        "ISBN:0 85131 041",        // too short
        "ISBN:978 1 4028 9462 66", // too long
        "ISBN:0-306-4061A-2",      // non-digit inside
        "ISBN:ABCDEFGHIJ",         // all letters
        "ISBN:978 0 306 40615 7X", // trailing junk
        "ISBN:  ",                 // empty after prefix
        "0-306-40615-2-",          // dangling separator
        "😀9780306406157",         // emoji prefix
    ];

    #[inline]
    fn strip_doi_trailing_decor<'a>(s: &'a str) -> Cow<'a, str> {
        strip_doi_trailing_decor_borrowed(s)
    }

    #[inline]
    fn strip_and_normalise_isbn_input(input: &str) -> Option<String> {
        let Isbn(digits, len) = Isbn::parse(input)?;
        let mut canonical = String::with_capacity(len);
        for &d in digits.iter().take(len) {
            canonical.push(char::from(d));
        }
        Some(canonical)
    }

    #[test]
    fn strip_outer_wrappers_handles_too_short() {
        assert_eq!(strip_outer_wrappers(""), None);
        assert_eq!(strip_outer_wrappers("("), None);
        assert_eq!(strip_outer_wrappers(")"), None);
    }

    #[test]
    fn valid_isbn_fixtures_parse_to_canonical() {
        for (input, canonical) in VALID_ISBN_CASES {
            match Query::parse(input) {
                Query::Isbn(raw) => {
                    let got = normalise_isbn_ref(raw.as_ref())
                        .expect("reference normaliser should accept fixture ISBN");
                    assert_eq!(got, *canonical, "input: {}", input);
                }
                other => panic!("expected ISBN for {:?}, got {:?}", input, other),
            }
        }
    }

    #[test]
    fn invalid_isbn_fixtures_rejected() {
        for input in INVALID_ISBN_CASES {
            if let Query::Isbn(parsed) = Query::parse(input) {
                panic!("unexpected ISBN parse for {input:?}, parsed {parsed:?}")
            }
        }
    }

    #[test]
    fn full_text_preserves_whitespace_and_punctuation() {
        let input = "  some loose text, not an id.  ";
        match Query::parse(input) {
            Query::FullText(s) => assert_eq!(s, input),
            other => panic!("expected FullText, got {:?}", other),
        }
    }

    #[test]
    fn isbn_trailing_punct_outside_wrappers_is_rejected() {
        let input = "(9780306406157).";
        let normalised = strip_and_normalise_isbn_input(input);
        assert!(normalised.is_none());
    }

    #[test]
    fn doi_percent_encoded_unicode_is_decoded() {
        let doi_name = "10.1234/ümlaut";
        let encoded = percent_encode_doi(doi_name);
        let url = format!("https://doi.org/{}", encoded);

        match Query::parse(&url) {
            Query::Doi(parsed) => assert_eq!(parsed.as_ref(), doi_name),
            other => panic!("expected DOI, got {:?}", other),
        }
    }

    #[test]
    fn doi_prefix_without_percent_is_borrowed() {
        match Query::parse("https://doi.org/10.1000/xyz123") {
            Query::Doi(Cow::Borrowed(s)) => assert_eq!(s, "10.1000/xyz123"),
            other => panic!("expected borrowed DOI, got {:?}", other),
        }
    }

    #[test]
    fn doi_prefix_with_percent_is_owned() {
        match Query::parse("urn:doi:10.1234/%C3%BCmlaut") {
            Query::Doi(Cow::Owned(s)) => assert_eq!(s, "10.1234/ümlaut"),
            other => panic!("expected owned decoded DOI, got {:?}", other),
        }
    }

    /// Check that two query inputs are semantically equivalent
    fn eq_query(left: &str, right: &str) -> bool {
        match (Query::parse(left), Query::parse(right)) {
            (Query::Isbn(left), Query::Isbn(right)) => {
                normalise_isbn_ref(left.as_ref()) == normalise_isbn_ref(right.as_ref())
            }
            (Query::Doi(left), Query::Doi(right)) => {
                // If Query::parse returns a percent-encoded DOI in some cases,
                // decode here.
                let left_decoded = percent_decode(left.as_ref()).to_ascii_lowercase();
                let right_decoded = percent_decode(right.as_ref()).to_ascii_lowercase();
                left_decoded == right_decoded
            }
            (Query::FullText(a), Query::FullText(b)) => a == b,
            _ => false,
        }
    }

    #[test]
    fn eq_query_handles_doi_variants() {
        assert!(eq_query(
            "https://doi.org/10.1/FOO%2Fbar",
            "doi:10.1/foo/bar"
        ));
    }

    #[test]
    fn full_text_eq_query_match() {
        assert!(eq_query("plain text", "plain text"));
    }

    #[test]
    fn eq_query_mismatch_variants() {
        assert!(!eq_query("10.1/foo", "plain text"));
    }

    fn toggle_ascii_case(s: &str) -> String {
        s.chars()
            .map(|c| {
                if c.is_ascii_lowercase() {
                    c.to_ascii_uppercase()
                } else if c.is_ascii_uppercase() {
                    c.to_ascii_lowercase()
                } else {
                    c
                }
            })
            .collect()
    }

    fn isbn_pair_same_value() -> impl Strategy<Value = (String, String)> {
        isbn().prop_flat_map(|(_input, canonical)| {
            let mask1 = prop::collection::vec(any::<bool>(), canonical.len().saturating_sub(1));
            let mask2 = prop::collection::vec(any::<bool>(), canonical.len().saturating_sub(1));

            (mask1, mask2).prop_map(move |(m1, m2)| {
                let a = intersperse_with_mask(&canonical, '-', &m1);
                let b = intersperse_with_mask(&canonical, ' ', &m2);
                (format!("ISBN:{a}"), format!("{b}."))
            })
        })
    }

    /// Generate an ISO-style DOI name (`<prefix>/<suffix>`).
    /// Allows any Unicode scalar that is not control, whitespace, or `/`,
    /// matching `looks_like_doi_name` without extra filtering.
    fn doi_name_iso() -> impl Strategy<Value = String> {
        fn doi_segment() -> impl Strategy<Value = String> {
            // 1..=64 chars, exclude all Unicode control/format/etc (General_Category C),
            // whitespace, and slash.
            string_regex(r#"[^\p{C}\s/]{1,64}"#).expect("valid regex")
        }

        (doi_segment(), doi_segment()).prop_map(|(p, s)| format!("{}/{}", p, s))
    }

    const DOI_PATH_ENCODE_SET: &AsciiSet = &NON_ALPHANUMERIC.remove(b'/');

    /// Percent-encode a DOI name to include DOI HTTP URI or URN
    fn percent_encode_doi(doi: &str) -> String {
        utf8_percent_encode(doi, DOI_PATH_ENCODE_SET).to_string()
    }

    fn percent_decode(s: &str) -> String {
        percent_decode_str(s).decode_utf8_lossy().into_owned()
    }

    fn normalise_isbn_ref(input: &str) -> Option<String> {
        strip_and_normalise_isbn_input(input)
    }

    /// Strategy for generating arbitrary DOI
    fn doi() -> impl Strategy<Value = (String, String)> {
        doi_name_iso().prop_flat_map(|doi| {
            let enc = percent_encode_doi(&doi);
            let candidates: Vec<(String, String)> = vec![
                (doi.clone(), doi.clone()),
                (format!("doi:{}", doi), doi.clone()),
                (format!("https://doi.org/{}", enc), doi.clone()),
                (format!("http://doi.org/{}", enc), doi.clone()),
                (format!("https://dx.doi.org/{}", enc), doi.clone()),
                (format!("http://dx.doi.org/{}", enc), doi.clone()),
                (format!("urn:doi:{}", enc), doi.clone()),
            ];

            select(candidates).prop_flat_map(|(base, canonical)| {
                let puncts = vec![
                    String::new(),
                    ".".to_string(),
                    ",".to_string(),
                    ")".to_string(),
                ];
                select(puncts).prop_map(move |p| (format!("{base}{p}"), canonical.clone()))
            })
        })
    }

    /// Generate a string that is *not* DOI
    fn invalid_doi_input() -> impl Strategy<Value = String> {
        doi_name_iso().prop_flat_map(|doi_name| {
            let enc = percent_encode_doi(&doi_name);
            let prefix = doi_name.split('/').next().unwrap().to_string();

            let no_slash = doi_name.replace('/', ""); // removes required delimiter
            let empty_suffix = format!("{}/", prefix); // missing suffix
            let whitespace = doi_name.replacen("/", " /", 1); // introduces whitespace
            let bad_percent_url = format!("https://doi.org/{}%ZZ", enc); // invalid percent escape
            let bad_percent_urn = format!("urn:doi:{}%G", enc); // invalid percent escape
            let just_prefix = "doi:".to_string();

            prop_oneof![
                Just(no_slash),
                Just(empty_suffix),
                Just(whitespace),
                Just(bad_percent_url),
                Just(bad_percent_urn),
                Just(just_prefix),
            ]
        })
    }

    fn isbn10_check_digit(first9: &[u8; 9]) -> char {
        let sum: u32 = first9
            .iter()
            .enumerate()
            .map(|(i, &d)| (d as u32) * (10 - i as u32))
            .sum();

        let c = (11 - (sum % 11)) % 11;
        match c {
            10 => 'X',
            v => char::from(b'0' + (v as u8)),
        }
    }

    fn isbn13_check_digit(first12: &[u8; 12]) -> u8 {
        let sum: u32 = first12
            .iter()
            .enumerate()
            .map(|(i, &d)| {
                let w = if i % 2 == 0 { 1 } else { 3 };
                (d as u32) * (w as u32)
            })
            .sum();

        ((10 - (sum % 10)) % 10) as u8
    }

    fn intersperse_with_mask(digits: &str, sep: char, mask: &[bool]) -> String {
        let mut out = String::with_capacity(digits.len() + mask.len());
        for (i, ch) in digits.chars().enumerate() {
            out.push(ch);
            if i < mask.len() && mask[i] {
                out.push(sep);
            }
        }
        out
    }

    fn isbn10() -> impl Strategy<Value = (String, String)> {
        prop::collection::vec(0u8..10, 9)
            .prop_map(|v| {
                let mut a = [0u8; 9];
                a.copy_from_slice(&v);
                a
            })
            .prop_flat_map(|first9| {
                let check = isbn10_check_digit(&first9);

                let mut canonical = String::with_capacity(10);
                for d in first9 {
                    canonical.push(char::from(b'0' + d));
                }
                canonical.push(check);

                let hyphen_mask = prop::collection::vec(any::<bool>(), 9);
                let space_mask = prop::collection::vec(any::<bool>(), 9);

                let body = prop_oneof![
                    Just(canonical.clone()),
                    hyphen_mask.clone().prop_map({
                        let c = canonical.clone();
                        move |m| intersperse_with_mask(&c, '-', &m)
                    }),
                    space_mask.clone().prop_map({
                        let c = canonical.clone();
                        move |m| intersperse_with_mask(&c, ' ', &m)
                    }),
                ];

                let prefix = prop_oneof![
                    Just(String::new()),
                    Just("ISBN:".to_string()),
                    Just("isbn:".to_string()),
                    Just("ISBN-10:".to_string()),
                    Just("ISBN ".to_string()),
                ];

                (prefix, body).prop_map(move |(p, b)| (format!("{p}{b}"), canonical.clone()))
            })
    }

    fn isbn13() -> impl Strategy<Value = (String, String)> {
        let prefix = prop_oneof![Just([9u8, 7u8, 8u8]), Just([9u8, 7u8, 9u8])];

        (prefix, prop::collection::vec(0u8..10, 9))
            .prop_map(|(pfx, rest)| {
                let mut first12 = [0u8; 12];
                first12[0..3].copy_from_slice(&pfx);
                for (i, d) in rest.into_iter().enumerate() {
                    first12[3 + i] = d;
                }
                first12
            })
            .prop_flat_map(|first12| {
                let check = isbn13_check_digit(&first12);

                let mut canonical = String::with_capacity(13);
                for d in first12 {
                    canonical.push(char::from(b'0' + d));
                }
                canonical.push(char::from(b'0' + check));

                let hyphen_mask = prop::collection::vec(any::<bool>(), 12);
                let space_mask = prop::collection::vec(any::<bool>(), 12);

                let body = prop_oneof![
                    Just(canonical.clone()),
                    hyphen_mask.clone().prop_map({
                        let c = canonical.clone();
                        move |m| intersperse_with_mask(&c, '-', &m)
                    }),
                    space_mask.clone().prop_map({
                        let c = canonical.clone();
                        move |m| intersperse_with_mask(&c, ' ', &m)
                    }),
                ];

                let prefix = prop_oneof![
                    Just(String::new()),
                    Just("ISBN:".to_string()),
                    Just("isbn:".to_string()),
                    Just("ISBN-13:".to_string()),
                    Just("ISBN ".to_string()),
                ];

                (prefix, body).prop_map(move |(p, b)| (format!("{p}{b}"), canonical.clone()))
            })
    }

    fn isbn() -> impl Strategy<Value = (String, String)> {
        prop_oneof![isbn10(), isbn13()]
    }

    fn invalid_isbn_input() -> impl Strategy<Value = String> {
        isbn().prop_flat_map(|(_valid_input, canonical)| {
            let mut wrong_check = canonical.clone();

            let last = wrong_check.pop().unwrap();

            let replacement: proptest::sample::Select<char> = match canonical.len() {
                10 => {
                    let mut options = vec!['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'X'];
                    options.retain(|&c| c != last);
                    select(options)
                }
                13 => {
                    let mut options = vec!['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
                    options.retain(|&c| c != last);
                    select(options)
                }
                _ => unreachable!(),
            };
            let wrong_check_strategy = replacement.prop_map(move |c| {
                let mut s = wrong_check.clone();
                s.push(c);
                s
            });

            // Invalid length variants.
            let too_short = Just(canonical[..canonical.len() - 1].to_string());
            let too_long = Just(format!("{}0", canonical));

            // Introduce a non-digit in the middle.
            let with_letter = Just({
                let mut s = canonical.clone();
                let i = s.len() / 2;
                s.replace_range(i..=i, "A");
                s
            });

            let decorate = |s: String| {
                prop_oneof![
                    Just(s.clone()),
                    Just(format!("ISBN:{s}")),
                    Just(format!("ISBN {s}.")),
                    Just(format!("({s})")),
                ]
            };

            prop_oneof![
                wrong_check_strategy.prop_flat_map(decorate),
                too_short.prop_flat_map(decorate),
                too_long.prop_flat_map(decorate),
                with_letter.prop_flat_map(decorate),
            ]
        })
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(PROPTEST_CASES))]
        /// Test that any arbitrary DOI is correctly identified as DOI
        #[test]
        fn parse_valid_doi((input, expected) in doi()) {
            match Query::parse(&input) {
                Query::Doi(doi_str) => {
                    let got = percent_decode(doi_str.as_ref());
                    let exp = strip_doi_trailing_decor(&percent_decode(&expected)).into_owned();
                    prop_assert_eq!(got.to_ascii_lowercase(), exp.to_ascii_lowercase());
                }
                other => {
                    prop_assert!(false, "expected Query::Doi, got: {:?}", core::mem::discriminant(&other));
                }
            }
        }

        /// Test that any other string that is not a DOI is not identified as
        /// a DOI.
        #[test]
        fn dont_parse_invalid_doi(input in invalid_doi_input()) {
            match Query::parse(&input) {
                Query::Doi(doi_str) => {
                    prop_assert!(
                        false,
                        "parser accepted invalid DOI input; input={:?}, parsed={:?}",
                        input,
                        doi_str
                    );
                }
                _ => { prop_assert!(true); }
            }
        }

        #[test]
        fn parse_valid_isbn((input, expected_canonical) in isbn()) {
            match Query::parse(&input) {
                Query::Isbn(isbn_str) => {
                    let got = normalise_isbn_ref(isbn_str.as_ref())
                        .expect("parser returned Isbn but reference normaliser rejected it");
                    prop_assert_eq!(got, expected_canonical);
                }
                other => {
                    prop_assert!(false, "expected Query::Isbn, got: {:?}", core::mem::discriminant(&other));
                }
            }
        }

        #[test]
        fn dont_parse_invalid_isbn(input in invalid_isbn_input()) {
            match Query::parse(&input) {
                Query::Isbn(isbn_str) => {
                    prop_assert!(
                        false,
                        "parser accepted invalid ISBN input; input={:?}, parsed={:?}",
                        input,
                        isbn_str
                    );
                }
                _ => prop_assert!(true),
            }
        }

        #[test]
        fn isbn_semantic_idempotence((input, canonical) in isbn()) {
            let q1 = Query::parse(&input);

            let q2 = Query::parse(&canonical);

            match (q1, q2) {
                (Query::Isbn(a), Query::Isbn(b)) => {
                    let na = normalise_isbn_ref(a.as_ref()).unwrap();
                    let nb = normalise_isbn_ref(b.as_ref()).unwrap();
                    prop_assert_eq!(na, nb);
                }
                _ => prop_assert!(false, "expected both ISBN"),
            }
        }

        #[test]
        fn isbn_formatting_stability((a, b) in isbn_pair_same_value()) {
            prop_assert!(eq_query(&a, &b));
        }

        #[test]
        fn doi_ascii_case_insensitive((input, expected) in doi()) {
            let toggled = toggle_ascii_case(&input);

            match (Query::parse(&input), Query::parse(&toggled)) {
                (Query::Doi(a), Query::Doi(b)) => {
                    let da = percent_decode(a.as_ref()).to_ascii_lowercase();
                    let db = percent_decode(b.as_ref()).to_ascii_lowercase();
                    prop_assert_eq!(da.clone(), db);

                    let exp = strip_doi_trailing_decor(&percent_decode(&expected)).into_owned().to_ascii_lowercase();
                    prop_assert_eq!(da, exp);
                }
                _ => prop_assert!(false, "expected DOI both times"),
            }
        }
    }
}
