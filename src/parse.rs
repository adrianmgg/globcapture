use std::sync::Arc;

use byteyarn::Yarn;
use miette::SourceSpan;
use vec1::Vec1;

use crate::{
    engine,
    label::Label,
    parse_util::resume_after_cut,
};

use winnow::{
    LocatingSlice,
    ascii::dec_uint,
    combinator::{
        alt, cut_err, dispatch, empty, fail, opt, peek, preceded, repeat, repeat_till, separated,
        separated_pair, terminated,
    },
    error::{AddContext, ErrMode, FromRecoverableError, StrContext, StrContextValue},
    prelude::*,
    stream::{Location, Recoverable, Stream},
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
struct ParseError {
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
                    ..input.current_token_start())
                    .into(),
            )
        });
        e
    }
}

impl<I: Stream + winnow::stream::Location> FromRecoverableError<I, ErrMode<Self>> for ParseError {
    fn from_recoverable_error(
        _token_start: &<I as Stream>::Checkpoint,
        _err_start: &<I as Stream>::Checkpoint,
        _input: &I,
        e: ErrMode<Self>,
    ) -> Self {
        match e {
            ErrMode::Incomplete(_) => Self::default(),
            ErrMode::Backtrack(err) | ErrMode::Cut(err) => err,
        }
    }
}

impl<I: Stream> AddContext<I, Context> for ParseError {
    fn add_context(
        mut self,
        _input: &I,
        _token_start: &<I as Stream>::Checkpoint,
        ctx: Context,
    ) -> Self {
        self.message = ctx.message.or(self.message);
        self.label = ctx.label.or(self.label);
        self.help = ctx.help.or(self.help);
        self
    }
}

// based on kdl-rs's KdlParseContext
#[derive(Debug, Clone, Default, Eq, PartialEq)]
struct Context {
    message: Option<String>,
    label: Option<String>,
    help: Option<String>,
}

impl Context {
    fn msg(mut self, txt: impl AsRef<str>) -> Self {
        self.message = Some(txt.as_ref().to_string());
        self
    }

    fn label(mut self, txt: impl AsRef<str>) -> Self {
        self.label = Some(txt.as_ref().to_string());
        self
    }

    fn help(mut self, txt: impl AsRef<str>) -> Self {
        self.help = Some(txt.as_ref().to_string());
        self
    }
}

fn ctx() -> Context {
    Default::default()
}

type PInput<'i> = Recoverable<LocatingSlice<&'i str>, ParseError>;

type PResult<'source, O> = winnow::ModalResult<O, ParseError>;

pub fn parse<'source>(source: &'source str) -> Result<(), Error> {
    let input = LocatingSlice::new(source);
    let (_, val, problems) = nq::<String>.recoverable_parse(input);
    if let (Some(val), true) = (val, problems.is_empty()) {
        dbg!(val);
        Ok(())
    } else {
        let src = Arc::<String>::new(source.into());
        Err(Error {
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
        })
    }
}

fn nq<'i, L: Label>(input: &mut PInput<'i>) -> PResult<'i, engine::NameQuery<L>> {
    repeat(1.., nq_item)
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
    dispatch! {peek(any);
        '(' => resume_after_cut(
            preceded('(', cut_err(terminated(nq_group_inner, ')'))).map(engine::NQNoRepeat::Group),
            // TODO should we make the resume recover skip "/)"s?
            repeat_till(1.., alt((('\\', any).void(), any.void())), ')').map(|((), _)| ()),
        )
        .verify_map(|g|
            g.or_else(|| Some(engine::NQNoRepeat::Group(engine::NQGroup {
                choices: Vec1::new(engine::NameQuery { parts: Vec::new() }),
                label: None,
            })))
        ),
        // '(' => preceded('(', cut_err(terminated(nq_group_inner, ')'))).map(engine::NQNoRepeat::Group),
        '[' => preceded('[', cut_err(terminated(nq_charclass_inner, ']'))).map(engine::NQNoRepeat::CharClass),
        '\\' => preceded('\\', any).map(|c| engine::NQNoRepeat::Literal(engine::NQLiteral { value: Yarn::from_char(c) })),
        ')' | ']' | '{' | '}' | '|' => fail,
        _ => any.map(|c| engine::NQNoRepeat::Literal(engine::NQLiteral { value: Yarn::from_char(c) })),
    }
    .parse_next(input)
}

fn label<'i, L: Label>(input: &mut PInput<'i>) -> PResult<'i, L> {
    preceded('<', cut_err(terminated(label_content, '>'))).parse_next(input)
}

fn label_content<'i, L: Label>(input: &mut PInput<'i>) -> PResult<'i, L> {
    let start = input.checkpoint();
    let text: String = repeat(
        0..,
        dispatch! {peek(any);
            '\\' => preceded('\\', any),
            '>' => fail,
            _ => any,
        },
    )
    .parse_next(input)?;
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
    (opt(label), separated(1.., nq, '|'))
        .map(|(label, choices)| engine::NQGroup {
            label,
            choices: Vec1::try_from_vec(choices).unwrap_or_else(|_| unreachable!()),
        })
        .parse_next(input)
}

fn nq_charclass_inner<'i>(input: &mut PInput<'i>) -> PResult<'i, engine::NQCharClass> {
    (
        opt('^'.value(true)).map(|o| o.unwrap_or_else(|| false)),
        repeat(0.., charclass_item),
    )
        .map(|(inverted, choices)| engine::NQCharClass { inverted, choices })
        .parse_next(input)
}

fn charclass_item_char<'i>(input: &mut PInput<'i>) -> PResult<'i, char> {
    // alt((preceded('\\', any), any))
    dispatch! {peek(any);
        '\\' => preceded('\\', any),
        '^' | '-' | '[' | ']' => fail
            .context(ctx().msg("special characters inside character class must be backslash-escaped")),
        _ => any,
    }
    .parse_next(input)
}

fn charclass_item<'i>(input: &mut PInput<'i>) -> PResult<'i, engine::CharClassItem> {
    alt((
        (
            charclass_item_char,
            preceded('-', cut_err(charclass_item_char)),
        )
            .map(|(a, b)| engine::CharClassItem(a..=b)),
        charclass_item_char.map(|c| engine::CharClassItem(c..=c)),
    ))
    .parse_next(input)
}

fn repeat_count<'i>(input: &mut PInput<'i>) -> PResult<'i, engine::RepeatCount> {
    preceded(
        '{',
        resume_after_cut(
            cut_err(terminated(
                dispatch! {peek(any);
                    '*' => '*'.value((0, None)),
                    '+' => '+'.value((1, None)),
                    '?' => '?'.value((0, Some(1))),
                    '0'..='9' => separated_pair(dec_uint, ",", opt(dec_uint)),
                    ',' => preceded(',', dec_uint).map(|hi| (0, Some(hi))),
                    _ => fail
                        .context(ctx().msg("expected '*', '+', '?', or explicit min,max")),
                },
                "}",
            )),
            repeat_till(1.., alt((('\\', any).void(), any.void())), '}').map(|((), _)| ()),
        ),
    )
    .map(|o| o.unwrap_or((0, Some(0))))
    .map(|(lo, hi)| engine::RepeatCount { lo, hi })
    .parse_next(input)
}
