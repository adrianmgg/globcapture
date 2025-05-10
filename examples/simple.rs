// use globcapture::Pattern;

fn main() -> miette::Result<()> {
    let arg = std::env::args().nth(1).expect("no arg provided");
    globcapture::parse::parse(&arg)?;
    Ok(())
}
