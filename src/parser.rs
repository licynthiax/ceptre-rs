use crate::*;
use pest::{error::Error, Parser};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "ceptre.pest"]
pub struct CeptreParser;

pub fn ceptre_parse(file: &str) -> Result<Vec<Top>, Box<Error<Rule>>> {
    // Vec<Top>
    let file = std::fs::read_to_string(file).unwrap();
    let ceptre = CeptreParser::parse(Rule::tops, &file)?.next().unwrap();

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
                                let annote = annote_inner.nth(1).unwrap().to_string();
                                let syn = parse_syn(dbg!(syn_inner.next().unwrap()));
                                (syn, Some(Annote { annote }))
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
                        let ident = pairs.next().unwrap();
                        let tops = parse_tops(pairs.next().unwrap());
                        Top::Stage(ident.to_string(), tops)
                    }
                    Rule::context => {
                        let ident = pairs.next().unwrap();
                        let syn = pairs.next().map(parse_syn);
                        Top::Context(ident.to_string(), syn)
                    }
                    Rule::hashident => {
                        let ident = first_pair.as_str();
                        let atomics = pairs.map(parse_atomic).collect();
                        Top::Special(ident.to_string(), atomics)
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }

    fn parse_syn(pair: Pair<Rule>) -> Syn {
        use pest::pratt_parser::{Assoc, Op as POp, PrattParser};

        let pratt = PrattParser::new()
            .op(POp::infix(Rule::comma, Assoc::Right))
            .op(POp::infix(Rule::colon, Assoc::Right))
            .op(POp::infix(Rule::larrow, Assoc::Left) | POp::infix(Rule::llolli, Assoc::Left))
            .op(POp::infix(Rule::rlolli, Assoc::Right)
                | POp::infix(Rule::rarrow, Assoc::Right)
                | POp::postfix(Rule::rlolli))
            .op(POp::infix(Rule::star, Assoc::Right))
            .op(POp::prefix(Rule::bang) | POp::prefix(Rule::dollar))
            .op(POp::infix(Rule::unify, Assoc::Right))
            .op(POp::infix(Rule::differ, Assoc::Right));

        pratt
            .map_primary(|primary| {
                fn parse_primary(pair: Pair<Rule>) -> Syn {
                    match pair.as_rule() {
                        Rule::primary => {
                            let mut inner = pair.into_inner();
                            parse_primary(inner.next().unwrap())
                        }
                        Rule::stage => {
                            let ident = pair.into_inner().nth(1).unwrap();
                            Syn::Primary(Primary::Stage(ident.as_str().to_string()))
                        }
                        Rule::atomic => {
                            let atomics = pair.into_inner().map(parse_atomic).collect();
                            Syn::Primary(Primary::Atomic(atomics))
                        }
                        Rule::syn => parse_syn(pair),
                        _ => unreachable!(),
                    }
                }
                parse_primary(primary)
            })
            .map_prefix(|op, rhs| Syn::Prefix {
                op: parse_op(op),
                rhs: Box::new(rhs),
            })
            .map_postfix(|lhs, op| Syn::Postfix {
                lhs: Box::new(lhs),
                op: parse_op(op),
            })
            .map_infix(|lhs, op, rhs| Syn::Infix {
                lhs: Box::new(lhs),
                op: parse_op(op),
                rhs: Box::new(rhs),
            })
            .parse(pair.into_inner())
    }

    fn parse_op(pair: Pair<Rule>) -> Op {
        match pair.as_rule() {
            Rule::comma => Op::Comma,
            Rule::colon => Op::Colon,
            Rule::larrow => Op::LArrow,
            Rule::llolli => Op::LLolli,
            Rule::rlolli => Op::RLolli,
            Rule::rarrow => Op::RArrow,
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
            Rule::braces_syn => {
                let syn_pair = pair.into_inner().next().unwrap();
                Atomic::Braces(parse_syn(syn_pair))
            }
            Rule::empty_braces => Atomic::EmptyBraces,
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

#[derive(Debug)]
pub enum Top {
    Decl(Syn, Option<Annote>),    // something.
    Stage(String, Vec<Top>),      // stage x {decl1, ..., decln}
    Context(String, Option<Syn>), // Context of string * syn option, // context x {t}
    Special(String, Vec<Atomic>), // Special of string * syn list   // #whatever t1...tn
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Op {
    Colon,
    RLolli,
    LLolli,
    RArrow,
    LArrow,
    Star,
    Comma,
    Unify,
    Differ,
    Bang,
    Dollar,
}

#[derive(Debug)]
pub enum Primary {
    Stage(String),
    Atomic(Vec<Atomic>),
}

#[derive(Debug)]
pub enum Atomic {
    EmptyBraces,
    Braces(Syn),
    Ident(String),
    Wildcard,
    Pred,
    Number(i32),
}
