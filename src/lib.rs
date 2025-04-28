use std::ops::{Range, RangeInclusive};

use label::Label;

pub mod label;
pub mod lex;
pub mod parse;

#[derive(Debug)]
pub enum Pattern<L: Label> {
    Single(PatternPart<L>),
    Choice(PatternChoice<L>),
    Concat(Vec<Self>),
}

#[derive(Debug)]
pub struct PatternChoice<L: Label> {
    pub choices: Vec<Pattern<L>>,
}

#[derive(Debug)]
pub enum PatternPart<L: Label> {
    Literal(String),
    CharClass(PatternPartCharClass),
    Choice(PatternPartChoice<L>),
    Concat(Vec<Self>),
    Repeat { item: Box<Self>, count: Range<u64> },
    Capture(PatternPartCapture<L>),
}

#[derive(Debug)]
pub struct PatternPartCharClass(pub Vec<RangeInclusive<char>>);

#[derive(Debug)]
pub struct PatternPartChoice<L: Label> {
    pub choices: Vec<PatternPart<L>>,
}

#[derive(Debug)]
pub struct PatternPartCapture<L: Label> {
    pub item: Box<PatternPart<L>>,
    pub label: L,
}

impl<L: Label> Pattern<L> {
    pub fn parse(source: &'_ str) -> Result<Self, parse::Error<L::Error>> {
        let mut lexer = logos::Lexer::new(source);
        parse::parse(&mut lexer)
    }
}
