// TODO rename this module maybe?

use std::ops::RangeInclusive;

use byteyarn::Yarn;
use vec1::Vec1;

use crate::label::Label;

// ###########
// # pattern #
// ###########

#[derive(Debug)]
pub struct Pattern<L: Label> {
    layers: Vec<Layer<L>>,
}

// #########
// # layer #
// #########

#[derive(Debug)]
pub(crate) struct LayerSingle<L: Label> {
    pub(crate) query: NameQuery<L>,
    pub(crate) attr_filter: Option<Vec1<AttrFilter>>,
}

#[derive(Debug)]
pub(crate) struct LayerGroup<L: Label> {
    pub(crate) choices: Vec1<Layer<L>>,
}

#[derive(Debug)]
pub(crate) enum Layer<L: Label> {
    Single(LayerSingle<L>),
    Group {
        content: LayerGroup<L>,
        repeat_count: Option<RepeatCount>,
    },
}

// ##############
// # name query #
// ##############

#[derive(Debug)]
pub(crate) struct NQLiteral {
    pub(crate) value: Yarn,
}

#[derive(Debug)]
pub(crate) struct NQGroup<L: Label> {
    pub(crate) choices: Vec1<NameQuery<L>>,
    pub(crate) label: Option<L>,
}

#[derive(Debug)]
pub(crate) struct NQCharClass {
    pub(crate) choices: Vec<CharClassItem>,
    pub(crate) inverted: bool,
}

#[derive(Debug)]
pub(crate) enum NQItem<L: Label> {
    Simple(NQNoRepeat<L>),
    Repeat {
        content: NQNoRepeat<L>,
        count: RepeatCount,
    },
}

#[derive(Debug)]
pub(crate) enum NQNoRepeat<L: Label> {
    Literal(NQLiteral),
    Group(NQGroup<L>),
    CharClass(NQCharClass),
}

#[derive(Debug)]
pub(crate) struct NameQuery<L: Label> {
    pub(crate) parts: Vec<NQItem<L>>,
}

// ###############
// # other stuff #
// ###############

#[derive(Debug)]
pub(crate) struct RepeatCount {
    pub(crate) lo: u64,
    // TODO make this u64 explicitly non-zero
    pub(crate) hi: Option<u64>,
}

#[derive(Debug)]
pub(crate) struct CharClassItem(pub(crate) RangeInclusive<char>);

#[derive(Debug)]
pub(crate) struct AttrFilter {
    // as of yet unimplemented
}
