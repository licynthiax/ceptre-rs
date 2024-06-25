pub mod parser;
pub mod preprocess;

#[derive(Debug)]
pub enum Error {
    Parse,
    IllFormed,
}

pub type Result<O> = std::result::Result<O, Error>;

pub fn parse(file: &str) -> Result<(preprocess::Sigma, Vec<preprocess::Program>)> {
    let tops = parser::ceptre_parse(file)?;
    preprocess::process(tops)
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
