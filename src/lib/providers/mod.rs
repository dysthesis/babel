use crate::query::Query;

mod google_books;
mod openalex;

pub trait Provider {
    fn query(&self, query: Query) -> String;
}
