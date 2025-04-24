use logos::{Lexer, Logos};
use sublexer_helper::{
    self as sublex, SubLexerToken, sublexer_callback, sublexer_callback_advanced,
};

#[derive(Logos, Debug, Clone)]
#[logos(error = Error)]
pub(crate) enum Token {
    #[token("/")]
    PathSep,
    #[token("[", |lex| map_sublex_err_variant(sublexer_callback(lex), || Error::UnclosedCharClass))]
    CharClass(Vec<CharClsTok>),
    #[token("{", |lex| map_sublex_err_variant(sublexer_callback(lex), || Error::UnclosedCapture))]
    Capture(Vec<CaptureTok>),
    #[token("(", (|lex| map_sublex_err_variant(sublexer_callback_advanced::<SLTChoice, _, _>(lex), || Error::UnclosedChoice)))]
    Choice(Vec<Self>),
    #[token(")")]
    ChoiceEnd,
    #[token("|")]
    ChoiceSep,
    #[regex(".", single_char_cb, priority = 1)]
    #[regex(r"\\.", backslash_escaped_char_cb)]
    LiteralChar(char),
}

fn map_sublex_err_variant<T, F: FnOnce() -> Error>(
    result: Result<T, sublex::Error<Error>>,
    eof_err: F,
) -> Result<T, Error> {
    result.map_err(|err| match err {
        sublexer_helper::Error::SublexerFailed(err) => err,
        sublexer_helper::Error::EOFBeforeClose => eof_err(),
    })
}

#[derive(Logos, Debug, Clone, Copy)]
#[logos(error = Error)]
pub(crate) enum CharClsTok {
    #[token("]")]
    End,
    #[token("-")]
    Thru,
    #[regex(".", single_char_cb, priority = 1)]
    #[regex(r"\\.", backslash_escaped_char_cb)]
    Char(char),
}

#[derive(Logos, Debug, Clone)]
#[logos(error = Error)]
pub(crate) enum CaptureTok {
    #[token("}")]
    End,
    #[regex(r":[a-zA-Z0-9_-]", label_str_cb)]
    Label(String),
    #[token("[", |lex| map_sublex_err_variant(sublexer_callback(lex), || Error::UnclosedCharClass))]
    CharClass(Vec<CharClsTok>),
    #[regex(".", single_char_cb, priority = 1)]
    #[regex(r"\\.", backslash_escaped_char_cb)]
    Char(char),
}

#[derive(thiserror::Error, Debug, PartialEq, Clone, Default)]
pub enum Error {
    #[default]
    #[error("invalid token")]
    InvalidToken,
    #[error(r#"unclosed character class ( "[" missing matching "]" )"#)]
    UnclosedCharClass,
    #[error(r#"unclosed capture ( "{{" missing matching "}}" )"#)]
    UnclosedCapture,
    #[error(r#"unclosed choice ( "(" missing matching ")" )"#)]
    UnclosedChoice,
}

fn backslash_escaped_char_cb<'source, T: Logos<'source, Source = str>>(
    lex: &mut Lexer<'source, T>,
) -> char {
    let mut chars = lex.slice().chars();
    assert_eq!(chars.next(), Some('\\'));
    let c = chars.next().unwrap();
    assert_eq!(chars.next(), None);
    c
}

fn single_char_cb<'source, T: Logos<'source, Source = str>>(lex: &mut Lexer<'source, T>) -> char {
    let mut chars = lex.slice().chars();
    let c = chars.next().unwrap();
    assert_eq!(chars.next(), None);
    c
}

fn label_str_cb<'source, T: Logos<'source, Source = str>>(lex: &mut Lexer<'source, T>) -> String {
    let mut chars = lex.slice().chars();
    assert_eq!(chars.next(), Some(':'));
    chars.collect()
}

impl SubLexerToken for CharClsTok {
    fn is_end(t: &Self) -> bool {
        matches!(t, Self::End)
    }
}

impl SubLexerToken for CaptureTok {
    fn is_end(t: &Self) -> bool {
        matches!(t, Self::End)
    }
}

struct SLTChoice;
impl SubLexerToken<Token> for SLTChoice {
    fn is_end(t: &Token) -> bool {
        matches!(t, Token::ChoiceEnd)
    }
}

mod sublexer_helper {
    use logos::{Lexer, Logos};

    pub trait SubLexerToken<Token = Self> {
        fn is_end(t: &Token) -> bool;
    }

    #[derive(Debug, thiserror::Error)]
    pub enum Error<E> {
        SublexerFailed(#[from] E),
        EOFBeforeClose,
    }

    /*
    we have the simple and _advanced versions of this because
    with the simple version (SLT trait implemented by the sub-token type itself) everything can just be inferred,
     (so can be used as just `#[token(..., sublexer_callback)]`)
    whereas in the complex case (SLT trait implemented on something else) types need to be specified
     (so e.g. `#[token(..., (sublexer_callback_advanced::<Foo, _, _>))]`)

    as for why the advanced case is necessary, it's needed when one token type has multiple subgroup terminators used in different contexts
      e.g. if need both `(` and `[` sub-seqs containing same token type T,
           T shouldn't close a `(` with a `]` or close a `[` with a `)`,
           so 2 distinct is_end implementations need to exist for T in 2 different contexts,
           so can make 2 unit structs implementing the paren and bracket cases seperately.
    */

    pub fn sublexer_callback_advanced<'source, SLT, ParentTok, SubTok>(
        lex: &mut Lexer<'source, ParentTok>,
    ) -> Result<Vec<SubTok>, Error<SubTok::Error>>
    where
        ParentTok: Logos<'source, Extras: Clone> + Clone,
        SubTok: Logos<
                'source,
                Extras: From<ParentTok::Extras> + Into<ParentTok::Extras>,
                Source = ParentTok::Source,
            >,
        SLT: SubLexerToken<SubTok>,
    {
        let mut sublex = lex.clone().morph::<SubTok>();
        let mut tokens = Vec::new();
        loop {
            match sublex.next() {
                None => return Err(Error::EOFBeforeClose),
                Some(Err(err)) => return Err(Error::SublexerFailed(err)),
                Some(Ok(tok)) if SLT::is_end(&tok) => break,
                Some(Ok(tok)) => tokens.push(tok),
            }
        }
        *lex = sublex.morph();
        Ok(tokens)
    }

    #[inline(always)]
    pub fn sublexer_callback<'source, SubTok, ParentTok>(
        lex: &mut Lexer<'source, ParentTok>,
    ) -> Result<Vec<SubTok>, Error<SubTok::Error>>
    where
        ParentTok: Logos<'source, Extras: Clone> + Clone,
        SubTok: Logos<
                'source,
                Extras: From<ParentTok::Extras> + Into<ParentTok::Extras>,
                Source = ParentTok::Source,
            >,
        SubTok: SubLexerToken<SubTok>,
    {
        sublexer_callback_advanced::<SubTok, ParentTok, SubTok>(lex)
    }
}
