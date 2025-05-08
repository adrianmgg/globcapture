// use globcapture::Pattern;

fn main() -> miette::Result<()> {
    globcapture::parse::parse("(a|b(c|d))")?;
    Ok(())
}
