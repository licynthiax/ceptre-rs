use ceptre::parser::*;
use ceptre::preprocess::*;

fn main() -> Result<(), ceptre::Error> {
    let tops = ceptre_parse("./crawler.cep")?;
    let (_, _) = dbg!(process(tops)?);
    Ok(())
}
