use std::ops::{Deref, Range};

use winnow::{
    LocatingSlice, Parser, RecoverableParser as _,
    combinator::{alt, cut_err, dispatch, empty, eof, fail, peek, preceded, repeat, terminated},
    stream::Recoverable,
    token::{any, literal},
};

use crate::{
    parse::{ParseError, ctx},
    parse_util::{resume_after_cut, resume_after_nooption},
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum TokenKind {
    // paired open/close tokens
    OpenGroup,
    CloseGroup,
    OpenLayerGroup,
    CloseLayerGroup,
    OpenCharClass,
    CloseCharClass,
    OpenRepeatCount,
    CloseRepeatCount,
    OpenLabel,
    CloseLabel,

    // tokens that can mean multiple things depending on their context, but which we will lex context-independently and worry about later
    Asterisk,
    QuestionMark,
    Plus,
    Comma,
    Hyphen,
    Caret,

    PathSeparator,
    GroupChoiceSeparator,

    // plain simple single characters (+ escaped ones)
    PlainCharacter,
    EscapedPlainCharacter,

    // errors
    Unexpected,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Token<'source> {
    pub(crate) kind: TokenKind,
    pub(crate) raw: &'source str,
    pub(crate) span: Range<usize>,
}

impl PartialEq<TokenKind> for Token<'_> {
    fn eq(&self, other: &TokenKind) -> bool {
        self.kind == *other
    }
}

// yes, this is backwards.
// see: https://github.com/winnow-rs/winnow/discussions/752
impl winnow::stream::Location for crate::lex::Token<'_> {
    fn previous_token_end(&self) -> usize {
        self.span.start
    }
    
    fn current_token_start(&self) -> usize {
        self.span.start - 1
    }
}

impl<'i, I, E> Parser<I, &'i Token<'i>, E> for TokenKind
where
    E: winnow::error::ParserError<I>,
    I: winnow::stream::Stream<Slice = &'i [Token<'i>]>
        + winnow::stream::StreamIsPartial
        + winnow::stream::Compare<TokenKind>,
{
    fn parse_next(&mut self, input: &mut I) -> winnow::Result<&'i Token<'i>, E> {
        literal(*self).parse_next(input).map(|t| &t[0])
    }
}

impl winnow::stream::ContainsToken<&'_ Token<'_>> for TokenKind {
    #[inline(always)]
    fn contains_token(&self, token: &'_ Token<'_>) -> bool {
        *self == token.kind
    }
}

impl winnow::stream::ContainsToken<&'_ Token<'_>> for &'_ [TokenKind] {
    #[inline]
    fn contains_token(&self, token: &'_ Token<'_>) -> bool {
        self.iter().any(|t| *t == token.kind)
    }
}

impl<const LEN: usize> winnow::stream::ContainsToken<&'_ Token<'_>> for &'_ [TokenKind; LEN] {
    #[inline]
    fn contains_token(&self, token: &'_ Token<'_>) -> bool {
        self.iter().any(|t| *t == token.kind)
    }
}

impl<const LEN: usize> winnow::stream::ContainsToken<&'_ Token<'_>> for [TokenKind; LEN] {
    #[inline]
    fn contains_token(&self, token: &'_ Token<'_>) -> bool {
        self.iter().any(|t| *t == token.kind)
    }
}

type LInput<'i> = Recoverable<LocatingSlice<&'i str>, ParseError>;

type LResult<O> = winnow::ModalResult<O, ParseError>;

fn token<'i>(input: &mut LInput<'i>) -> LResult<Token<'i>> {
    dispatch! {peek(any);
        '(' => preceded('(', alt((
            '@'.value(TokenKind::OpenLayerGroup),
            empty.value(TokenKind::OpenGroup),
        ))).with_taken(),
        ')' => ')'.value(TokenKind::CloseGroup).with_taken(),
        '@' => alt((
            "@)".value(TokenKind::CloseLayerGroup).with_taken(),
            resume_after_nooption(
                cut_err(fail).context(ctx().msg("'@' not included in a '(@...@)' group must be escaped").help("try inserting a '\\' before the '@'")),
                '@'.value(TokenKind::Unexpected).with_taken(),
            ),
        )),
        '/' => '/'.value(TokenKind::PathSeparator).with_taken(),
        '|' => '|'.value(TokenKind::GroupChoiceSeparator).with_taken(),
        '[' => '['.value(TokenKind::OpenCharClass).with_taken(),
        ']' => ']'.value(TokenKind::CloseCharClass).with_taken(),
        '{' => '{'.value(TokenKind::OpenRepeatCount).with_taken(),
        '}' => '}'.value(TokenKind::CloseRepeatCount).with_taken(),
        '<' => '<'.value(TokenKind::OpenLabel).with_taken(),
        '>' => '>'.value(TokenKind::CloseLabel).with_taken(),
        '*' => '*'.value(TokenKind::Asterisk).with_taken(),
        '?' => '?'.value(TokenKind::QuestionMark).with_taken(),
        '+' => '+'.value(TokenKind::Plus).with_taken(),
        ',' => ','.value(TokenKind::Comma).with_taken(),
        '-' => '-'.value(TokenKind::Hyphen).with_taken(),
        '^' => '^'.value(TokenKind::Caret).with_taken(),
        '\\' => preceded('\\',
            resume_after_nooption(
                cut_err(any.value(TokenKind::EscapedPlainCharacter)).context(ctx().msg("end of input reached, but was waiting for the character following a backslash")),
                empty.value(TokenKind::Unexpected)
            ).with_taken()
        ),
        _ => any.value(TokenKind::PlainCharacter).with_taken(),
    }
    .with_span()
    .map(|((kind, raw), span)| Token { kind, raw, span })
    .parse_next(input)
}

pub(crate) fn lex(input: LocatingSlice<&str>) -> (Option<Vec<Token<'_>>>, Vec<ParseError>) {
    let (_input, val, problems): (_, Option<Vec<_>>, Vec<_>) =
        terminated(repeat(0.., token), cut_err(eof)).recoverable_parse(input);
    (val, problems)
}
