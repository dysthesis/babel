use crate::query::Query;

mod google_books;
mod openalex;

pub const PROVIDERS: &[Box<dyn Provider + Send + Sync>] = &[];

pub trait Provider {
    fn query(&self, query: Query) -> String;
}
