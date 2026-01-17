use std::{hint::black_box, time::Duration};

use babel::query::Query;
use criterion::{BatchSize, BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};

// Valid DOI representations, including HTTP/URN prefixes, percent-encoded UTF-8,
// and trailing punctuation that should be stripped.
const DOI_VALID: &[&str] = &[
    "10.1000/xyz123",
    "doi:10.1000/xyz123",
    "urn:doi:10.1000/xyz123",
    "https://doi.org/10.1000/xyz123",
    "http://dx.doi.org/10.1000/xyz123",
    "https://doi.org/10.1234/%C3%BCmlaut",
    "doi:10.5555/foo.bar)",
];

// Prefixed DOIs without percent escapes to isolate prefix-handling overhead.
const DOI_PREFIX_PLAIN: &[&str] = &[
    "https://doi.org/10.1000/xyz123",
    "http://dx.doi.org/10.1000/xyz123",
    "urn:doi:10.1000/xyz123",
];

// Prefixed DOIs that require percent-decoding.
const DOI_PREFIX_PERCENT: &[&str] = &[
    "https://doi.org/10.1234/%C3%BCmlaut",
    "urn:doi:10.1234/%C3%BCmlaut",
];

// Inputs that should *not* parse as DOI (bad percent escapes, missing slash,
// etc.).
const DOI_INVALID: &[&str] = &[
    "10.1000",                  // missing suffix
    "10-1000/abc",              // wrong delimiter
    "https://doi.org/10.1/%ZZ", // bad percent encoding
    "urn:doi:10.1/%G0",         // bad percent encoding
    "doi:",                     // empty
    "not a doi",
];

// Canonical and decorated ISBN-10 variants.
const ISBN10_VALID: &[&str] = &[
    "0306406152",
    "0-306-40615-2",
    "ISBN:0 306 40615 2",
    "ISBN-10:0306406152",
    "(0-306-40615-2)",
];

// Canonical and decorated ISBN-13 variants (978/979 prefixes).
const ISBN13_VALID: &[&str] = &[
    "9780306406157",
    "978-0-306-40615-7",
    "ISBN:978 0 306 40615 7",
    "9791234567896",
    "(979-1-234-56789-6)",
];

// Inputs that should be rejected as ISBN (bad check digit, length, noise).
const ISBN_INVALID: &[&str] = &[
    "0-306-40615-3",            // bad check digit ISBN-10
    "978-0-306-40615-8",        // bad check digit ISBN-13
    "ISBN:0 306 40615",         // too short
    "ISBN:978 0 306 40615 700", // too long
    "ISBN:0-306-4061A-2",       // non-digit
    "ISBN:abcdefghij",          // letters only
    "isbn:9780306406157X",      // junk suffix
    "(9780306406157).",         // trailing punctuation outside wrapper
];

// Full-text strings that should never be parsed as DOI/ISBN.
const FULL_TEXT: &[&str] = &[
    "just some loose text, not an id.",
    "  spaced text with commas, periods, and ISBN-like 978 hints that should stay text ",
    "Long paragraph: Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.",
];

// Mixed realistic workload to approximate production traffic.
const MIXED: &[&str] = &[
    // DOIs
    "10.1000/xyz123",
    "https://doi.org/10.1234/%C3%BCmlaut",
    "doi:10.1/foo/bar",
    // ISBN-10
    "0-306-40615-2",
    "ISBN:0306406152",
    // ISBN-13
    "978-0-306-40615-7",
    "9791234567896",
    // Invalids
    "doi:",
    "9780306406157X",
    "https://doi.org/10.1/%ZZ",
    // Full text
    "plain search terms about something else",
    "another sentence with numbers 123456 but not identifiers",
];

fn bench_config() -> Criterion {
    Criterion::default()
        .sample_size(200)
        .warm_up_time(Duration::from_secs(3))
        .measurement_time(Duration::from_secs(8))
        .noise_threshold(0.02)
        .confidence_level(0.99)
}

fn bench_group_inputs(c: &mut Criterion, name: &str, inputs: &[&str]) {
    let mut group = c.benchmark_group(name);
    for &input in inputs {
        group.bench_with_input(BenchmarkId::new("parse", input), input, |b, s| {
            b.iter(|| black_box(Query::parse(black_box(s))))
        });
    }
    group.finish();
}

fn doi_valid(c: &mut Criterion) {
    bench_group_inputs(c, "doi_valid", DOI_VALID);
}

fn doi_prefixed_plain(c: &mut Criterion) {
    bench_group_inputs(c, "doi_prefixed_plain", DOI_PREFIX_PLAIN);
}

fn doi_prefixed_percent(c: &mut Criterion) {
    bench_group_inputs(c, "doi_prefixed_percent", DOI_PREFIX_PERCENT);
}

fn doi_invalid(c: &mut Criterion) {
    bench_group_inputs(c, "doi_invalid", DOI_INVALID);
}

fn isbn10_valid(c: &mut Criterion) {
    bench_group_inputs(c, "isbn10_valid", ISBN10_VALID);
}

fn isbn13_valid(c: &mut Criterion) {
    bench_group_inputs(c, "isbn13_valid", ISBN13_VALID);
}

fn isbn_invalid(c: &mut Criterion) {
    bench_group_inputs(c, "isbn_invalid", ISBN_INVALID);
}

fn full_text(c: &mut Criterion) {
    bench_group_inputs(c, "full_text", FULL_TEXT);
}

fn mixed_batch(c: &mut Criterion) {
    let mut group = c.benchmark_group("mixed_batch");
    group.throughput(Throughput::Elements(MIXED.len() as u64));
    group.bench_function("parse_all", |b| {
        b.iter_batched(
            || (),
            |_| {
                for &s in MIXED {
                    black_box(Query::parse(black_box(s)));
                }
            },
            BatchSize::SmallInput,
        )
    });
    group.finish();
}

criterion_group! {
    name = benches;
    config = bench_config();
    targets =
        doi_valid,
        doi_prefixed_plain,
        doi_prefixed_percent,
        doi_invalid,
        isbn10_valid,
        isbn13_valid,
        isbn_invalid,
        full_text,
        mixed_batch,
}
criterion_main!(benches);
