#![allow(unused)]
mod parser;
pub use crate::parser::*;

pub struct Ceptre {
    header: Vec<Decl>,
    builtin: Vec<(String, Builtin)>,
    rules: Vec<BwdRule>,
}

struct Decl {
    ident: String,
    classifier: Classifier,
    annote: Option<Annote>,
}

#[derive(Debug)]
pub struct Annote {
    annote: String,
}

enum Classifier {
    Type,
    Tp(Vec<String>, String),
    Pred(PredClass, Vec<Term>),
}

enum PredClass {
    Prop,
    Bwd,
    Sense,
    Act,
}

enum Term {
    Fn(String, Vec<Term>),
    Var(i32),
    SLit(String),
    ILit(i32),
}

enum Builtin {
    Nat,
    Zero,
    Succ,
}

struct BwdRule {
    name: String,
    pivars: i32,
    head: (String, Vec<Term>),
    subgoals: Vec<Atom>,
}

struct Atom {
    mode: Mode,
    pred: String,
    terms: Vec<Term>,
}

enum Mode {
    Pers,
    Lin,
}
