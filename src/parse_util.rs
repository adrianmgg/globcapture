use winnow::{
    ModalResult, Parser,
    combinator::trace,
    error::{ErrMode, FromRecoverableError, ParserError},
    stream::{Recover, Stream},
};

/*

resume_after_cut is mostly just a copy of a function from kdl-rs
    https://github.com/kdl-org/kdl-rs/blob/439aa63bfc595432e6dc6b0fdd708c364628ab18/src/v2_parser.rs#L206-L255
, albeit with a bit of porting to our more recent targeted version of winnow

 */

pub(crate) fn resume_after_cut<Input, Output, Error, ParseNext, ParseRecover>(
    mut parser: ParseNext,
    mut recover: ParseRecover,
) -> impl Parser<Input, Option<Output>, ErrMode<Error>>
where
    Input: Stream + Recover<Error>,
    Error: FromRecoverableError<Input, Error> + ParserError<Input>,
    ParseNext: Parser<Input, Output, ErrMode<Error>>,
    ParseRecover: Parser<Input, (), ErrMode<Error>>,
{
    trace("resume_after_cut", move |input: &mut Input| {
        resume_after_cut_inner(&mut parser, &mut recover, input)
    })
}

fn resume_after_cut_inner<P, R, I, O, E>(
    parser: &mut P,
    recover: &mut R,
    i: &mut I,
) -> ModalResult<Option<O>, E>
where
    P: Parser<I, O, ErrMode<E>>,
    R: Parser<I, (), ErrMode<E>>,
    I: Stream,
    I: Recover<E>,
    E: FromRecoverableError<I, E>,
{
    let token_start = i.checkpoint();
    let mut err = match parser.parse_next(i) {
        Ok(o) => {
            return Ok(Some(o));
        }
        Err(ErrMode::Incomplete(e)) => return Err(ErrMode::Incomplete(e)),
        Err(ErrMode::Backtrack(e)) => return Err(ErrMode::Backtrack(e)),
        Err(ErrMode::Cut(err)) => err,
    };
    let err_start = i.checkpoint();
    if recover.parse_next(i).is_ok() {
        if let Err(err_) = i.record_err(&token_start, &err_start, err) {
            err = err_;
        } else {
            return Ok(None);
        }
    }

    i.reset(&err_start);
    err = FromRecoverableError::from_recoverable_error(&token_start, &err_start, i, err);
    // TODO is wrapping this in Cut(...) the way we should be handling this?
    Err(ErrMode::Cut(err))
}

/*

this one's a rewrite of winnow's resume_after in which the recover has the same result type as the main parser, rather than returning an option

*/

pub(crate) fn resume_after_nooption<P, R, I, O, E>(
    mut parser: P,
    mut recover: R,
) -> impl Parser<I, O, E>
where
    P: Parser<I, O, E>,
    R: Parser<I, O, E>,
    I: Stream,
    I: Recover<E>,
    E: ParserError<I> + FromRecoverableError<I, E>,
{
    trace("resume_after_nooption", move |input: &mut I| {
        resume_after_nooption_inner(&mut parser, &mut recover, input)
    })
}

fn resume_after_nooption_inner<P, R, I, O, E>(
    parser: &mut P,
    recover: &mut R,
    i: &mut I,
) -> Result<O, E>
where
    P: Parser<I, O, E>,
    R: Parser<I, O, E>,
    I: Stream,
    I: Recover<E>,
    E: ParserError<I> + FromRecoverableError<I, E>,
{
    let token_start = i.checkpoint();
    let mut err = match parser.parse_next(i) {
        Ok(o) => {
            return Ok(o);
        }
        Err(e) if e.is_incomplete() => return Err(e),
        Err(err) => err,
    };
    let err_start = i.checkpoint();
    if let Ok(recov) = recover.parse_next(i) {
        if let Err(err_) = i.record_err(&token_start, &err_start, err) {
            err = err_;
        } else {
            return Ok(recov);
        }
    }

    i.reset(&err_start);
    err = FromRecoverableError::from_recoverable_error(&token_start, &err_start, i, err);
    Err(err)
}
