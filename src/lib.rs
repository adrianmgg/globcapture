#![allow(unused)]

use std::ops::{Range, RangeInclusive};

use label::Label;

pub mod engine;
pub mod label;
mod lex;
pub mod parse;
mod parse_util;

// impl<L: Label> Pattern<L> {
//     pub fn parse(source: &'_ str) -> Result<Self, parse::Error<L::Error>> {
//         let mut lexer = logos::Lexer::new(source);
//         parse::parse(&mut lexer)
//     }
// }
