use std::sync::Arc;

use byteyarn::Yarn;
use miette::SourceSpan;
use vec1::Vec1;

use crate::{
    engine,
    label::Label,
    lex::TokenKind,
    parse_util::{resume_after_cut, resume_after_nooption},
};

use winnow::{
    LocatingSlice,
    combinator::{
        alt, cut_err, dispatch, empty, eof, fail, opt, peek, preceded, repeat, repeat_till,
        separated, separated_pair, terminated,
    },
    error::{AddContext, ErrMode, FromRecoverableError, StrContext, StrContextValue},
    prelude::*,
    stream::{Location, Recoverable, Stream, TokenSlice},
    token::{any, take_while},
};

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("failed to parse pattern")]
pub struct Error {
    #[source_code]
    pub src: Arc<String>,
    #[related]
    pub problems: Vec<Problem>,
}

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("{}", message.as_deref().unwrap_or("something went wrong"))]
pub struct Problem {
    #[source_code]
    pub src: Arc<String>,

    #[label("{}", label.as_deref().unwrap_or("here"))]
    pub span: SourceSpan,

    pub message: Option<String>,

    pub label: Option<String>,

    #[help]
    pub help: Option<String>,
}

// type ErrorStr<'source> = Error<&'source str>;
//
// impl ErrorStr<'_> {
//     pub fn to_owned(self) -> Error<String> {
//         Error {
//             src: self.src.to_owned(),
//             problems: self.problems,
//         }
//     }
// }

#[derive(Debug, Default, Clone)]
pub(crate) struct ParseError {
    message: Option<String>,
    span: Option<SourceSpan>,
    label: Option<String>,
    help: Option<String>,
}

impl<I: Stream> winnow::error::ParserError<I> for ParseError {
    type Inner = Self;

    fn from_input(_input: &I) -> Self {
        Self {
            label: None,
            message: None,
            span: None,
            help: None,
        }
    }

    fn into_inner(self) -> winnow::Result<Self::Inner, Self> {
        Ok(self)
    }
}

impl<I: Stream + winnow::stream::Location> FromRecoverableError<I, Self> for ParseError {
    fn from_recoverable_error(
        token_start: &<I as Stream>::Checkpoint,
        _err_start: &<I as Stream>::Checkpoint,
        input: &I,
        mut e: Self,
    ) -> Self {
        e.span = e.span.or_else(|| {
            Some(
                ((input.current_token_start() - input.offset_from(token_start))
                    ..=input.current_token_start())
                    .into(),
            )
        });
        e
    }
}

impl<I: Stream + winnow::stream::Location> FromRecoverableError<I, ErrMode<Self>> for ParseError {
    fn from_recoverable_error(
        token_start: &<I as Stream>::Checkpoint,
        err_start: &<I as Stream>::Checkpoint,
        input: &I,
        e: ErrMode<Self>,
    ) -> Self {
        match e {
            ErrMode::Incomplete(_) => Self::default(),
            ErrMode::Backtrack(mut err) | ErrMode::Cut(mut err) => {
                ParseError::from_recoverable_error(token_start, err_start, input, err)
            }
        }
    }
}

impl<I: Stream + winnow::stream::Location> AddContext<I, Context> for ParseError {
    fn add_context(
        mut self,
        input: &I,
        token_start: &<I as Stream>::Checkpoint,
        ctx: Context,
    ) -> Self {
        self.message = ctx.message.or(self.message);
        self.label = ctx.label.or(self.label);
        self.help = ctx.help.or(self.help);
        if ctx.span_here {
            self.span = Some(
                ((input.current_token_start() - input.offset_from(token_start))
                    ..=input.current_token_start())
                    .into(),
            )
        }
        self
    }
}

// based on kdl-rs's KdlParseContext
#[derive(Debug, Clone, Default, Eq, PartialEq)]
pub(crate) struct Context {
    message: Option<String>,
    label: Option<String>,
    help: Option<String>,
    span_here: bool,
}

impl Context {
    pub(crate) fn msg(mut self, txt: impl AsRef<str>) -> Self {
        self.message = Some(txt.as_ref().to_string());
        self
    }

    pub(crate) fn label(mut self, txt: impl AsRef<str>) -> Self {
        self.label = Some(txt.as_ref().to_string());
        self
    }

    pub(crate) fn help(mut self, txt: impl AsRef<str>) -> Self {
        self.help = Some(txt.as_ref().to_string());
        self
    }

    pub(crate) fn span_here(mut self) -> Self {
        self.span_here = true;
        self
    }
}

pub(crate) fn ctx() -> Context {
    Default::default()
}

type PInput<'i> = Recoverable<TokenSlice<'i, crate::lex::Token<'i>>, ParseError>;

type PResult<'source, O> = winnow::ModalResult<O, ParseError>;

pub fn parse(source: &str) -> Result<(), Error> {
    let input = LocatingSlice::new(source);
    let (tokens, mut problems) = crate::lex::lex(input);
    dbg!(&tokens);
    let mut val = None;
    if let Some(tokens) = tokens {
        let tokens = TokenSlice::new(&tokens);
        let (_, val_, parse_problems) = nq::<String>.recoverable_parse(tokens);
        val = val_;
        problems.extend(parse_problems);
    }
    if let (Some(val), true) = (val, problems.is_empty()) {
        dbg!(val);
        Ok(())
    } else {
        let src = Arc::<String>::new(source.into());
        Err(dbg!(Error {
            src: src.clone(),
            problems: problems
                .into_iter()
                .map(|e| Problem {
                    src: src.clone(),
                    span: e.span.unwrap_or_else(|| (0usize..0usize).into()),
                    message: e.message,
                    label: e.label,
                    help: e.help,
                })
                .collect(),
        }))
    }
}

// fn layer_single<'i, L: Label>(input: &mut PInput<'i>) -> PResult<'i, engine::LayerSingle<L>> {
//     nq::<L>
//         .map(|nq| engine::LayerSingle {
//             query: todo!(),
//             attr_filter: None,
//         })
//         .parse_next(input)
// }

fn nq<'i, L: Label>(input: &mut PInput<'i>) -> PResult<'i, engine::NameQuery<L>> {
    repeat(0.., nq_item)
        .map(|parts| engine::NameQuery { parts })
        .parse_next(input)
}

fn nq_item<'i, L: Label>(input: &mut PInput<'i>) -> PResult<'i, engine::NQItem<L>> {
    (nq_no_repeat, opt(repeat_count))
        .map(|(content, count)| match count {
            None => engine::NQItem::Simple(content),
            Some(count) => engine::NQItem::Repeat { content, count },
        })
        .parse_next(input)
}

fn nq_no_repeat<'i, L: Label>(input: &mut PInput<'i>) -> PResult<'i, engine::NQNoRepeat<L>> {
    alt((
        preceded(
            TokenKind::OpenGroup,
            resume_after_nooption(
                cut_err(terminated(
                    nq_group_inner,
                    TokenKind::CloseGroup
                        .context(ctx().msg("group not closed (missing closing ')')")),
                )),
                repeat_till::<_, _, (), _, _, _, _>(
                    0..,
                    any,
                    alt((TokenKind::CloseGroup.void(), eof.void())),
                )
                .map(|_| engine::NQGroup {
                    choices: Vec1::new(engine::NameQuery { parts: Vec::new() }),
                    label: None,
                }),
            ),
        )
        .map(engine::NQNoRepeat::Group),
        preceded(
            TokenKind::OpenCharClass,
            resume_after_nooption(
                terminated(nq_charclass_inner, TokenKind::CloseCharClass),
                repeat_till::<_, _, (), _, _, _, _>(0.., any, TokenKind::CloseGroup).map(|_| {
                    engine::NQCharClass {
                        choices: Vec::new(),
                        inverted: false,
                    }
                }),
            ),
        )
        .map(engine::NQNoRepeat::CharClass),
        plain_char_or_escaped.map(|c| {
            engine::NQNoRepeat::Literal(engine::NQLiteral {
                value: Yarn::from_char(c),
            })
        }),
    ))
    .parse_next(input)
}

fn label<'i, L: Label>(input: &mut PInput<'i>) -> PResult<'i, L> {
    preceded(
        TokenKind::OpenLabel,
        cut_err(terminated(label_content, TokenKind::CloseLabel)),
    )
    .parse_next(input)
}

fn label_content<'i, L: Label>(input: &mut PInput<'i>) -> PResult<'i, L> {
    let start = input.checkpoint();
    let text: String = repeat(0.., plain_char_or_escaped).parse_next(input)?;
    // TODO wait do i need to rewind to the start or is that the caller's responsibility
    match L::parse_label(&text) {
        Ok(label) => Ok(label),
        // (just gonna piggyback off of from_recoverable_error since that already handles calculating the span)
        Err(err) => Err(FromRecoverableError::from_recoverable_error(
            &start,
            &start,
            input,
            ErrMode::Cut(ParseError {
                message: Some(format!("{}", err)),
                span: None,
                label: None,
                help: None,
            }),
        )),
    }
}

fn nq_group_inner<'i, L: Label>(input: &mut PInput<'i>) -> PResult<'i, engine::NQGroup<L>> {
    (
        opt(label),
        separated(1.., nq, TokenKind::GroupChoiceSeparator),
    )
        .map(|(label, choices)| engine::NQGroup {
            label,
            choices: Vec1::try_from_vec(choices).unwrap_or_else(|_| unreachable!()),
        })
        .parse_next(input)
}

fn nq_charclass_inner<'i>(input: &mut PInput<'i>) -> PResult<'i, engine::NQCharClass> {
    (
        opt(TokenKind::Caret.value(true)).map(|o| o.unwrap_or(false)),
        repeat(0.., charclass_item),
    )
        .map(|(inverted, choices)| engine::NQCharClass { inverted, choices })
        .parse_next(input)
}

// TODO make manually grabbing the chars by index from raw not required
fn charclass_item_char<'i>(input: &mut PInput<'i>) -> PResult<'i, char> {
    plain_char_or_escaped.parse_next(input)
}

fn charclass_item<'i>(input: &mut PInput<'i>) -> PResult<'i, engine::CharClassItem> {
    alt((
        (
            charclass_item_char,
            preceded(TokenKind::Hyphen, cut_err(charclass_item_char)),
        )
            .map(|(a, b)| engine::CharClassItem(a..=b)),
        charclass_item_char.map(|c| engine::CharClassItem(c..=c)),
    ))
    .parse_next(input)
}

fn repeat_count<'i>(input: &mut PInput<'i>) -> PResult<'i, engine::RepeatCount> {
    preceded(
        TokenKind::OpenRepeatCount,
        resume_after_nooption(
            cut_err(terminated(
                alt((
                    TokenKind::Asterisk.value((0, None)),
                    TokenKind::Plus.value((1, None)),
                    TokenKind::QuestionMark.value((0, Some(1))),
                    separated_pair(dec_uint, TokenKind::Comma, opt(dec_uint)),
                    preceded(TokenKind::Comma, dec_uint).map(|hi| (0, Some(hi))),
                    fail.context(ctx().msg("expected '*', '+', '?', or explicit min,max")),
                )),
                TokenKind::CloseRepeatCount,
            )),
            repeat_till::<_, _, (), _, _, _, _>(0.., any, TokenKind::CloseRepeatCount)
                .value((0, Some(0))),
        ),
    )
    .map(|(lo, hi)| engine::RepeatCount { lo, hi })
    .parse_next(input)
}

fn plain_char_or_escaped<'i>(input: &mut PInput<'i>) -> PResult<'i, char> {
    alt((
        TokenKind::PlainCharacter.map(|t| t.raw.chars().nth(0).unwrap()),
        TokenKind::EscapedPlainCharacter.map(|t| t.raw.chars().nth(1).unwrap()),
    ))
    .parse_next(input)
}

fn dec_uint<'i>(input: &mut PInput<'i>) -> PResult<'i, u64> {
    // TODO ideally optimize this to not collect to a String
    take_while(1.., |t: &'i crate::lex::Token<'_>| {
        t.kind == TokenKind::PlainCharacter
            && t.raw.len() == 1
            && matches!(t.raw.chars().nth(0).unwrap(), '0'..='9')
    })
    .map(|x: &'i [crate::lex::Token<'_>]| x.iter().map(|t| t.raw).collect())
    .verify_map(|s: String| u64::from_str_radix(&s, 10).ok())
    .parse_next(input)
}
