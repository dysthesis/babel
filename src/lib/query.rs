/// A query to search for entries. It can be a full-text search, or a direct
/// identifier such as DOI or ISBN.
pub enum Query<'a> {
    /// A DOI identifier of an academic literature
    Doi(&'a str),
    /// An ISBN identifier of a book.
    Isbn(&'a str),
    /// A full text query over the title, author, synopsis, etc. of the entry.
    FullText(&'a str),
}

impl<'a> Query<'a> {
    pub fn parse(from: &'a str) -> Query<'a> {
        todo!(
            "Parse a string to a query; check if it's a literal DOI or ISBN, otherwise it's a full-text search"
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use percent_encoding::{AsciiSet, NON_ALPHANUMERIC, percent_decode_str, utf8_percent_encode};
    use proptest::prelude::*;

    /// Predicate to check that a character is printable in DOI, which excludes
    /// control characters, whitespace, delimiters.
    fn doi_printable_char(c: char) -> bool {
        !c.is_control() && c != '/' && !c.is_whitespace()
    }

    /// Generate an ISO-style OOI name as a string, in the form of
    /// `<prefix>/<suffix>`.
    fn doi_name_iso() -> impl Strategy<Value = String> {
        // generate a vector of...
        let seg = prop::collection::vec(
            any::<char>().prop_filter("printable DOI char", |c| doi_printable_char(*c)), // ...printable characters...
            1..=64, // ...that is 1 to 64 charcters long
        )
        .prop_map(|v| v.into_iter().collect::<String>());

        (seg.clone(), seg).prop_map(|(p, s)| format!("{}/{}", p, s))
    }

    const DOI_PATH_ENCODE_SET: &AsciiSet = &NON_ALPHANUMERIC.remove(b'/');

    /// Percent-encode a DOI name to include DOI HTTP URI or URN
    fn percent_encode_doi(doi: &str) -> String {
        utf8_percent_encode(doi, DOI_PATH_ENCODE_SET).to_string()
    }

    fn percent_decode(s: &str) -> String {
        percent_decode_str(s).decode_utf8_lossy().into_owned()
    }

    /// Strategy for generating arbitrary DOI
    fn doi() -> impl Strategy<Value = (String, String)> {
        doi_name_iso().prop_flat_map(|doi| {
            let enc = percent_encode_doi(&doi);

            prop_oneof![
                Just((doi.clone(), doi.clone())),
                Just((format!("doi:{}", doi), doi.clone())),
                Just((format!("https://doi.org/{}", enc), doi.clone())),
                Just((format!("urn:doi:{}", enc), doi.clone())),
            ]
        })
    }

    /// End-to-end decoding of DOI
    fn strip_and_decode_doi_input(input: &str) -> Option<String> {
        let s = input.trim();
        let s = s.trim_end_matches(|c: char| matches!(c, '.' | ',' | ';' | ')' | ']' | '}'));

        let rest = if let Some(r) = s.strip_prefix("doi:").or_else(|| s.strip_prefix("DOI:")) {
            r
        } else if let Some(r) = s
            .strip_prefix("https://doi.org/")
            .or_else(|| s.strip_prefix("http://doi.org/"))
            .or_else(|| s.strip_prefix("https://dx.doi.org/"))
            .or_else(|| s.strip_prefix("http://dx.doi.org/"))
        {
            r
        } else if let Some(r) = s
            .strip_prefix("urn:doi:")
            .or_else(|| s.strip_prefix("URN:DOI:"))
        {
            r
        } else {
            s
        };

        let decoded = percent_decode(rest);
        if looks_like_doi_name(&decoded) {
            Some(decoded)
        } else {
            None
        }
    }

    /// For the inverse-test case to filter out possibly valid DOIs
    fn looks_like_doi_name(s: &str) -> bool {
        // DOI syntax is prefix + "/" + suffix, both non-empty.
        let mut it = s.splitn(2, '/');
        let prefix = it.next().unwrap_or("");
        let suffix = it.next().unwrap_or("");
        if prefix.is_empty() || suffix.is_empty() {
            return false;
        }
        // ISO 26324 constrains to printable characters
        !prefix.chars().any(|c| c.is_control() || c.is_whitespace())
            && !suffix.chars().any(|c| c.is_control() || c.is_whitespace())
    }

    /// Generate a string that is *not* DOI
    fn invalid_doi_input() -> impl Strategy<Value = String> {
        doi_name_iso()
            .prop_flat_map(|doi_name| {
                let enc = percent_encode_doi(&doi_name);

                let no_slash = doi_name.replace('/', ""); // removes required delimiter
                let empty_suffix = format!("{}/", doi_name.split('/').next().unwrap());
                let whitespace = doi_name.replacen("/", " /", 1); // introduces whitespace
                let bad_pct_url = format!("https://doi.org/{}%ZZ", enc); // invalid percent escape
                let bad_pct_urn = format!("urn:doi:{}%G", enc); // invalid percent escape
                let just_prefix = "doi:".to_string();

                prop_oneof![
                    Just(no_slash),
                    Just(empty_suffix),
                    Just(whitespace),
                    Just(bad_pct_url),
                    Just(bad_pct_urn),
                    Just(just_prefix),
                ]
            })
            .prop_filter("must not be a valid DOI input", |s| {
                strip_and_decode_doi_input(s).is_none()
            })
    }

    proptest! {
        /// Test that any arbitrary DOI is correctly identified as DOI
        #[test]
        fn parse_valid_doi((input, expected) in doi()) {
            match Query::parse(&input) {
                Query::Doi(doi_str) => {
                    let got = percent_decode(doi_str);

                    prop_assert_eq!(got.to_ascii_lowercase(), expected.to_ascii_lowercase());
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
        fn parse_isbn(s in "\\PC*") {
            todo!()
        }

        fn dont_parse_invalid_isbn(s in "\\PC*") {
            todo!()
        }
    }
}
