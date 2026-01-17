use super::util::strip_prefix_ignore_ascii_case;

#[derive(Clone, Copy)]
pub(crate) struct IsbnInput<'a> {
    pub(crate) raw: &'a str,
}

impl<'a> IsbnInput<'a> {
    #[inline]
    pub(crate) fn new(raw: &'a str) -> Self {
        Self { raw }
    }

    #[inline]
    pub(crate) fn body(self) -> Option<&'a str> {
        let mut s = self.raw.trim();

        while let Some(stripped) = Self::strip_outer_wrappers(s) {
            s = stripped.trim();
        }

        let s = Self::strip_prefix(s);
        Self::trim_trailing_and_validate_last(s)
    }

    #[inline]
    pub(crate) fn strip_outer_wrappers(s: &'a str) -> Option<&'a str> {
        let bytes = s.as_bytes();
        if bytes.len() < 2 {
            return None;
        }

        match (bytes[0], bytes[bytes.len() - 1]) {
            (b'(', b')') | (b'[', b']') | (b'{', b'}') => Some(&s[1..s.len() - 1]),
            _ => None,
        }
    }

    #[inline]
    fn strip_prefix(s: &'a str) -> &'a str {
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

        let skip_whitespace = bytes
            .get(idx..)
            .unwrap_or(&[])
            .iter()
            .take_while(|b| (*b).is_ascii_whitespace())
            .count();

        idx += skip_whitespace;

        &s[idx..]
    }

    #[inline]
    fn trim_trailing_and_validate_last(s: &'a str) -> Option<&'a str> {
        let bytes = s.as_bytes();

        let end = bytes
            .iter()
            // find the first character from the right that isn't a whitespace,
            // punctuation, or delimiter.
            .rposition(|&b| {
                !(b.is_ascii_whitespace() || matches!(b, b'.' | b',' | b';' | b')' | b']' | b'}'))
            })
            .map(|i| i + 1)?; // convert last index to exclusive end

        let b = bytes.get(end - 1)?;
        if b.is_ascii_digit() || *b == b'X' || *b == b'x' {
            Some(&s[..end])
        } else {
            None
        }
    }
}

#[derive(Clone, Copy)]
pub(crate) struct Isbn(pub(crate) [u8; 13], pub(crate) usize);

impl Isbn {
    #[inline]
    pub(crate) fn parse(input: &str) -> Option<Self> {
        let body = IsbnInput::new(input).body()?;
        Isbn::collect(body)?.validate()
    }

    #[inline]
    fn collect(body: &str) -> Option<Self> {
        if body.is_ascii() {
            return Self::collect_ascii(body.as_bytes());
        }

        Self::collect_unicode(body)
    }

    #[inline]
    fn collect_ascii(bytes: &[u8]) -> Option<Self> {
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

        Some(Self(buf, len))
    }

    #[inline]
    fn collect_unicode(s: &str) -> Option<Self> {
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

        Some(Self(buf, len))
    }

    #[inline]
    fn validate(self) -> Option<Isbn> {
        match self.1 {
            10 => self.validate_isbn_10(),
            13 => self.validate_isbn_13(),
            _ => None,
        }
    }

    #[inline]
    fn validate_isbn_13(self) -> Option<Isbn> {
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

            if (check[0] - b'0') == expected {
                return Some(Isbn(buf, size));
            }
        }

        None
    }

    #[inline]
    fn validate_isbn_10(self) -> Option<Isbn> {
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

            if (check[0] as char).to_ascii_uppercase() == expected {
                return Some(Isbn(buf, size));
            }
        }
        None
    }
}
