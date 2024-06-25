use crate::all_ok_or;
use crate::preprocess::*;
use pest::{error::Error, Parser};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "ceptre.pest"]
pub struct CeptreParser;

pub fn ceptre_parse(file: &str) -> Result<Vec<Top>, crate::Error> {
    let file = std::fs::read_to_string(file).unwrap();
    let ceptre = CeptreParser::parse(Rule::tops, &file)
        .map_err(|_| crate::Error::Parse)?
        .next()
        .unwrap();

    use pest::iterators::Pair;

    fn parse_tops(pair: Pair<Rule>) -> Vec<Top> {
        match pair.as_rule() {
            Rule::tops => pair.into_inner().map(|pair| parse_top(pair)).collect(),
            _ => unreachable!(),
        }
    }

    fn parse_top(pair: Pair<Rule>) -> Top {
        match pair.as_rule() {
            Rule::top => {
                let mut pairs = pair.into_inner();
                let first_pair = pairs.next().unwrap();
                match first_pair.as_rule() {
                    Rule::annote_syn => {
                        let mut syn_inner = first_pair.into_inner();
                        let first_pair = syn_inner.next().unwrap();
                        let (syn, annote) = match first_pair.as_rule() {
                            Rule::annote => {
                                let mut annote_inner = first_pair.into_inner();
                                let annote = annote_inner.next().unwrap().as_str().to_string();
                                let syn = parse_syn(syn_inner.next().unwrap());
                                (syn, Some(Annote::new(annote)))
                            }
                            Rule::syn => {
                                let syn = parse_syn(first_pair);
                                (syn, None)
                            }
                            _ => unreachable!(),
                        };
                        Top::Decl(syn, annote)
                    }
                    Rule::stage => {
                        let ident = pairs.next().unwrap().as_str().to_string();
                        let tops = pairs.map(parse_top).collect();
                        Top::Stage(ident, tops)
                    }
                    Rule::context => {
                        let ident = pairs.next().unwrap().as_str().to_string();
                        let op_syn = pairs
                            .next()
                            .map(|syn| parse_syn(syn.into_inner().next().unwrap()));
                        Top::Context(ident, op_syn)
                    }
                    Rule::special => {
                        let special = parse_special(first_pair);
                        Top::Special(special)
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }

    fn parse_special(pair: Pair<Rule>) -> Special {
        let mut special_inner = pair.into_inner().next().unwrap();
        match special_inner.as_rule() {
            Rule::trace => {
                let mut special_inner = special_inner.into_inner();

                let limit = special_inner.next().unwrap();
                let limit = match limit.as_rule() {
                    Rule::wildcard => None,
                    Rule::number => Some(
                        limit
                            .as_str()
                            .parse::<i32>()
                            .map_err(|_| crate::Error::Parse)
                            .unwrap(),
                    ),
                    _ => unreachable!(),
                };

                let name = special_inner.next().unwrap().as_str().to_string();
                let context = parse_atomic(special_inner.next().unwrap());

                Special::Trace {
                    name,
                    limit,
                    context,
                }
            }
            Rule::builtin => {
                let mut special_inner = special_inner.into_inner();
                let builtin = special_inner.next().unwrap();
                let builtin = match builtin.as_rule() {
                    Rule::nat => Builtin::Nat,
                    Rule::nat_zero => Builtin::Zero,
                    Rule::nat_succ => Builtin::Succ,
                    _ => unreachable!(),
                };
                let name = special_inner.next().unwrap().as_str().to_string();
                Special::Builtin(name, builtin)
            }
            Rule::interactive => Special::Interactive(
                special_inner
                    .into_inner()
                    .next()
                    .unwrap()
                    .as_str()
                    .to_string(),
            ),
            _ => unreachable!(),
        }
    }

    fn parse_syn(pair: Pair<Rule>) -> Syn {
        use pest::pratt_parser::{Assoc, Op as POp, PrattParser};

        let pratt = PrattParser::new()
            .op(POp::infix(Rule::comma, Assoc::Right))
            .op(POp::infix(Rule::colon, Assoc::Right))
            .op(POp::infix(Rule::larrow, Assoc::Left) | POp::infix(Rule::llolli, Assoc::Left))
            .op(POp::infix(Rule::rlolli, Assoc::Right) | POp::infix(Rule::rarrow, Assoc::Right))
            .op(POp::infix(Rule::star, Assoc::Right))
            .op(POp::prefix(Rule::bang) | POp::prefix(Rule::dollar))
            .op(POp::infix(Rule::unify, Assoc::Right))
            .op(POp::infix(Rule::differ, Assoc::Right));

        fn parse_atomics(ats: impl Iterator<Item = Atomic>) -> Vec<Atomic> {
            let mut atomics = Vec::new();
            for atomic in ats {
                match atomic {
                    Atomic::App(ats) => {
                        let ats = parse_atomics(ats.into_iter());
                        atomics.push(Atomic::App(ats));
                    }
                    Atomic::Ident(_) | Atomic::Wildcard | Atomic::Number(_) => atomics.push(atomic),
                    _ => unreachable!(), // ditto
                }
            }
            atomics
        }

        pratt
            .map_primary(|primary| {
                let primary = match primary.as_rule() {
                    Rule::atomics => {
                        let mut inner = primary.into_inner();
                        if inner.len() == 1 {
                            Primary::Atomic(parse_atomic(inner.next().unwrap()))
                        } else {
                            let inner = inner.map(parse_atomic);
                            Primary::Atomics(parse_atomics(inner))
                        }
                    }
                    _ => unreachable!(),
                };
                Syn::Primary(primary)
            })
            .map_prefix(|op, rhs| Syn::Prefix {
                op: parse_op(op, None),
                rhs: Box::new(rhs),
            })
            .map_postfix(|lhs, op| Syn::Postfix {
                lhs: Box::new(lhs),
                op: parse_op(op, None),
            })
            .map_infix(|mut lhs, op, mut rhs| Syn::Infix {
                op: parse_op(op, Some((&mut lhs, &mut rhs))),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
            .parse(pair.into_inner())
    }

    fn parse_op(pair: Pair<Rule>, syns: Option<(&mut Syn, &mut Syn)>) -> Op {
        match pair.as_rule() {
            Rule::comma => Op::Comma,
            Rule::colon => Op::Colon,
            Rule::larrow => {
                if let Some((lhs, rhs)) = syns {
                    std::mem::swap(lhs, rhs);
                }
                Op::Arrow
            }
            Rule::rarrow => Op::Arrow,
            Rule::llolli => {
                if let Some((lhs, rhs)) = syns {
                    std::mem::swap(lhs, rhs);
                }
                Op::Lolli
            }
            Rule::rlolli => Op::Lolli,
            Rule::star => Op::Star,
            Rule::bang => Op::Bang,
            Rule::dollar => Op::Dollar,
            Rule::unify => Op::Unify,
            Rule::differ => Op::Differ,
            _ => unreachable!(),
        }
    }

    fn parse_atomic(pair: Pair<Rule>) -> Atomic {
        match pair.as_rule() {
            Rule::atomic => {
                let inner = pair.into_inner().next().unwrap();
                parse_atomic(inner)
            }
            Rule::ident => Atomic::Ident(pair.as_str().to_string()),
            Rule::braces => {
                let syn_pair = pair.into_inner().next().unwrap();
                Atomic::Braces(Box::new(parse_syn(syn_pair)))
            }
            Rule::empty_braces => Atomic::EmptyBraces,
            Rule::unit => Atomic::Unit,
            Rule::parens => {
                let atomics = pair.into_inner().next().unwrap();
                Atomic::App(atomics.into_inner().map(parse_atomic).collect())
            }
            Rule::wildcard => Atomic::Wildcard,
            Rule::pred => Atomic::Pred,
            Rule::number => {
                let num: i32 = pair.as_str().parse().unwrap();
                Atomic::Number(num)
            }
            _ => unreachable!(),
        }
    }

    Ok(parse_tops(ceptre))
}

#[derive(Debug, Clone)]
pub enum Top {
    Decl(Syn, Option<Annote>),    // something.
    Stage(String, Vec<Top>),      // stage x {decl1, ..., decln}
    Context(String, Option<Syn>), // Context of string * syn option, // context x {t}
    Special(Special),             // Special of string * syn list   // #whatever t1...tn
}

#[derive(Debug, Clone)]
pub enum Special {
    Trace {
        name: String,
        limit: Option<i32>,
        context: Atomic,
    },
    Builtin(String, crate::preprocess::Builtin),
    Interactive(String),
}

#[derive(Debug, Clone)]
pub enum Syn {
    Primary(Primary),
    Infix {
        lhs: Box<Syn>,
        op: Op,
        rhs: Box<Syn>,
    },
    Prefix {
        op: Op,
        rhs: Box<Syn>,
    },
    Postfix {
        lhs: Box<Syn>,
        op: Op,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Colon,
    Lolli,
    Arrow,
    Star,
    Comma,
    Unify,
    Differ,
    Bang,
    Dollar,
}

#[derive(Debug, Clone)]
pub enum Primary {
    Atomic(Atomic),
    Atomics(Vec<Atomic>),
}

#[derive(Debug, Clone)]
pub enum Atomic {
    EmptyBraces,
    Unit,
    Braces(Box<Syn>),
    App(Vec<Atomic>),
    Ident(String),
    Wildcard,
    Pred,
    Number(i32),
}
