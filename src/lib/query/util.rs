#[inline]
pub(crate) fn strip_prefix_ignore_ascii_case<'a>(s: &'a str, prefix: &str) -> Option<&'a str> {
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
