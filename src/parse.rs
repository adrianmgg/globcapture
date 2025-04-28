use logos::Lexer;

use crate::{
    Pattern, PatternPart,
    label::Label,
    lex::{self, Token},
};

#[derive(Debug, thiserror::Error)]
pub enum Error<LabelErr> {
    #[error(transparent)]
    Lexer(lex::Error),
    #[error(r#"failed converting {label:?} to a label"#)]
    LabelConversion {
        #[source]
        source: LabelErr,
        label: String,
    },
    #[error(r#"choice separator ("|") encountered while not inside a choice"#)]
    ChoiceSepOutsideChoice,
    #[error(r#"character class has hyphen in the wrong place"#)]
    CharClassThruMisplaced,
    #[error("missing label on capture")]
    CaptureMissingLabel,
    #[error("multiple labels on capture")]
    CaptureMultipleLabels,
}

struct ParseState<L: Label> {
    levels: Vec<Pattern<L>>,
    cur_level: Vec<PatternPart<L>>,
    cur_literal: String,
}

impl<L: Label> ParseState<L> {
    fn new() -> Self {
        Self {
            levels: Vec::new(),
            cur_level: Vec::new(),
            cur_literal: String::new(),
        }
    }

    fn is_mid_level(&self) -> bool {
        !(self.cur_level.is_empty() && self.cur_literal.is_empty())
    }

    fn put_literal_char(&mut self, c: char) {
        self.cur_literal.push(c);
    }

    fn finish_literal_maybe(&mut self) {
        if !self.cur_literal.is_empty() {
            self.cur_level
                .push(PatternPart::Literal(std::mem::take(&mut self.cur_literal)));
        }
    }

    fn finish_level(&mut self) {
        self.finish_literal_maybe();
        let mut parts = std::mem::take(&mut self.cur_level);
        let part = if parts.len() == 1 {
            parts.pop().unwrap()
        } else {
            PatternPart::Concat(parts)
        };
        self.levels.push(Pattern::Single(part));
    }

    fn finish_level_maybe(&mut self) {
        if self.is_mid_level() {
            self.finish_level();
        }
        assert!(!self.is_mid_level());
    }

    fn put_level_part(&mut self, part: PatternPart<L>) {
        self.finish_literal_maybe();
        self.cur_level.push(part);
    }

    fn put_level(&mut self, level: Pattern<L>) {
        self.finish_level_maybe();
        self.levels.push(level);
    }

    fn finish(mut self) -> Pattern<L> {
        if self.levels.is_empty() {
            self.finish_literal_maybe();
            if self.cur_level.len() == 1 {
                Pattern::Single(self.cur_level.pop().unwrap())
            } else {
                Pattern::Single(PatternPart::Concat(self.cur_level))
            }
        } else {
            self.finish_level_maybe();
            Pattern::Concat(self.levels)
        }
    }
}

pub(crate) fn parse<L: Label>(lex: &mut Lexer<'_, Token>) -> Result<Pattern<L>, Error<L::Error>> {
    let mut state = ParseState::new();
    for tok in lex.by_ref() {
        match tok.map_err(Error::Lexer)? {
            Token::ChoiceEnd => {
                unreachable!("end token received by parser, but lexer should never emit that token")
            }
            Token::ChoiceSep => return Err(Error::ChoiceSepOutsideChoice),
            Token::PathSep => {
                state.finish_level();
            }
            Token::CharClass(char_cls_toks) => {
                state.put_level_part(PatternPart::CharClass(parse_char_cls(char_cls_toks)?));
            }
            Token::Capture(capture_toks) => {
                state.put_level_part(PatternPart::Capture(parse_capture(capture_toks)?));
            }
            Token::Choice(tokens) => {
                let choice = parse_choice(tokens, state.is_mid_level())?;
                match choice {
                    ChoiceParseResult::IntraLevel(choice) => {
                        state.put_level_part(PatternPart::Choice(choice))
                    }
                    ChoiceParseResult::InterLevel(choice) => {
                        assert!(!state.is_mid_level());
                        state.put_level(Pattern::Choice(choice));
                    }
                }
            }
            Token::LiteralChar(c) => {
                state.put_literal_char(c);
            }
        }
    }
    Ok(state.finish())
}

fn parse_char_cls<LabelErr>(
    tokens: Vec<lex::CharClsTok>,
) -> Result<crate::PatternPartCharClass, Error<LabelErr>> {
    let mut parts = Vec::new();
    #[derive(Debug)]
    enum State {
        Nothing,
        Start(char),
        StartAndThru(char),
    }
    let mut state = State::Nothing;
    for tok in tokens {
        match tok {
            lex::CharClsTok::End => {
                unreachable!("end token received by parser, but lexer should never emit that token")
            }
            lex::CharClsTok::Thru => match state {
                State::Nothing => return Err(Error::CharClassThruMisplaced),
                State::Start(c) => state = State::StartAndThru(c),
                State::StartAndThru(_) => return Err(Error::CharClassThruMisplaced),
            },
            lex::CharClsTok::Char(cur) => match state {
                State::Nothing => state = State::Start(cur),
                State::Start(prev_start) => {
                    parts.push(prev_start..=prev_start);
                    state = State::Start(cur);
                }
                State::StartAndThru(start) => {
                    parts.push(start..=cur);
                    state = State::Nothing;
                }
            },
        }
    }
    match state {
        State::Nothing => {}
        State::Start(c) => parts.push(c..=c),
        State::StartAndThru(_) => return Err(Error::CharClassThruMisplaced),
    }
    Ok(crate::PatternPartCharClass(parts))
}

fn parse_capture<L: Label>(
    tokens: Vec<lex::CaptureTok>,
) -> Result<crate::PatternPartCapture<L>, Error<L::Error>> {
    let mut label = None;
    let mut parts = Vec::new();
    let mut cur_literal = String::new();

    for tok in tokens {
        match tok {
            lex::CaptureTok::End => {
                unreachable!("end token received by parser, but lexer should never emit that token")
            }
            lex::CaptureTok::Label(label_str) => match label {
                Some(_) => return Err(Error::CaptureMultipleLabels),
                None => match L::parse_label(&label_str) {
                    Err(err) => {
                        return Err(Error::LabelConversion {
                            source: err,
                            label: label_str,
                        });
                    }
                    Ok(l) => label = Some(l),
                },
            },
            lex::CaptureTok::CharClass(char_cls_toks) => {
                if !cur_literal.is_empty() {
                    parts.push(PatternPart::Literal(std::mem::take(&mut cur_literal)));
                }
                parts.push(PatternPart::CharClass(parse_char_cls(char_cls_toks)?));
            }
            lex::CaptureTok::Char(c) => cur_literal.push(c),
        }
    }
    if !cur_literal.is_empty() {
        parts.push(PatternPart::Literal(std::mem::take(&mut cur_literal)));
    }

    let label = match label {
        Some(label) => label,
        None => return Err(Error::CaptureMissingLabel),
    };
    Ok(crate::PatternPartCapture {
        item: Box::new(PatternPart::Concat(parts)),
        label,
    })
}

enum ChoiceParseResult<L: Label> {
    InterLevel(crate::PatternChoice<L>),
    IntraLevel(crate::PatternPartChoice<L>),
}

fn parse_choice<L: Label>(
    tokens: Vec<Token>,
    starting_mid_level: bool,
) -> Result<ChoiceParseResult<L>, Error<L::Error>> {
    todo!()
}
