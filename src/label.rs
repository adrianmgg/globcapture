use std::{convert::Infallible, fmt::Debug};

pub trait Label: PartialEq + Debug + Clone {
    type Error;
    fn parse_label(text: &'_ str) -> Result<Self, Self::Error>;
}

impl Label for String {
    type Error = Infallible;

    fn parse_label(text: &'_ str) -> Result<Self, Self::Error> {
        Ok(text.to_owned())
    }
}

impl Label for Box<str> {
    type Error = Infallible;

    fn parse_label(text: &'_ str) -> Result<Self, Self::Error> {
        Ok(text.into())
    }
}
