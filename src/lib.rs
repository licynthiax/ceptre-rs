#![allow(unused)]
pub mod parser;
pub mod preprocess;

#[derive(Debug)]
pub enum Error {
    Parse,
    IllFormed,
}

pub(crate) fn all_ok_or<I, E>(
    vec: impl Iterator<Item = std::result::Result<I, E>>,
) -> std::result::Result<Vec<I>, E> {
    let mut o = Vec::new();
    for i in vec {
        o.push(i?);
    }
    Ok(o)
}
