use crate::parser::*;
use crate::Error;

enum CSyn {
    Stage(Stage),
    Rule(Rule),
    None(Syn),
    Error(Top),
    Ctx(Context),               // (* named ctx *)
    Prog(Option<i32>, Context), // limit, initial phase & initial ctx *),
    Decl(Decl),
    Bwd(BwdRule),
    Builtin(String, Builtin),
    StageMode(String, Nondet),
}

fn process(tops: Vec<Top>) -> (Sigma, Vec<Program>) {
    let mut header: Vec<Decl> = Vec::new();
    let mut bwds: Vec<BwdRule> = Vec::new();
    let mut contexts: Vec<Context> = Vec::new();
    let mut stages: Vec<Stage> = Vec::new();
    let mut links: Vec<StageRule> = Vec::new();
    let mut progs: Vec<Program> = Vec::new();
    let mut builtins: Vec<(String, Builtin)> = Vec::new();

    for top in tops {
        match extract_top(top).unwrap() {
            CSyn::Stage(stage) => stages.push(stage),
            CSyn::Rule(rule) => {
                if let Some(link) = rule_to_stagerule(rule) {
                    links.push(link);
                }
            }
            CSyn::Ctx(context) => contexts.push(context),
            CSyn::Prog(limit, (name, ctx)) => {
                if stages.iter().any(|stage| stage.name == name) {
                    progs.push(Program {
                        init_stage: name,
                        init_state: ctx,
                    });
                }
            }
            CSyn::Decl(decl) => header.push(decl),
            CSyn::Bwd(bwd) => bwds.push(bwd),
            CSyn::Builtin(name, builtin) => builtins.push((name, builtin)),
            CSyn::StageMode(name, mode) => {
                for stage in stages.iter_mut().filter(|stage| stage.name == name) {
                    stage.nondet = mode;
                }
            }
            CSyn::None(_) | CSyn::Error(_) => {}
        }
    }
    let sigma = Sigma {
        header,
        builtins,
        rules: bwds,
        stages,
        links,
    };
    (sigma, progs)
}

fn rule_to_stagerule(rule: Rule) -> Option<StageRule> {
    let not_stage_atom = |atom: &Atom| atom.pred != "stage";
    let mut lhs = rule.lhs.into_iter().skip_while(not_stage_atom);
    let mut rhs = rule.rhs.into_iter().skip_while(not_stage_atom);
    if let (Some(l_atom), Some(r_atom)) = (lhs.next(), rhs.next()) {
        if l_atom.terms.len() == 1 && r_atom.terms.len() == 1 {
            if let (Term::Fn(pre_stage, lt), Term::Fn(post_stage, rt)) =
                (&l_atom.terms[0], &r_atom.terms[0])
            {
                if lt.is_empty() && rt.is_empty() {
                    let lhs = lhs.collect();
                    let rhs = rhs.collect();
                    return Some(StageRule {
                        name: rule.name,
                        pivars: rule.pivars,
                        lhs,
                        rhs,
                        pre_stage: pre_stage.clone(),
                        post_stage: post_stage.clone(),
                        ann: rule.ann,
                    });
                }
            }
        }
    }
    None
}

fn extract_top(top: Top) -> Result<CSyn, Error> {
    let csyn = match top {
        Top::Stage(name, tops) => CSyn::Stage(match extract_stage(name, tops) {
            Ok(stage) => stage,
            Err(e) => return Err(e),
        }),
        Top::Decl(syn, annote) => match syn {
            Syn::Primary(primary) => todo!(), // genuinely idk what happens here
            Syn::Infix { lhs, op, rhs } => match op {
                Op::Colon => {
                    // ascribe
                    let ident = match *lhs {
                        Syn::Primary(Primary::Atomic(atomics)) => {
                            let a = &atomics[0];
                            match a {
                                Atomic::Ident(s) => s.clone(),
                                _ => return Err(Error::IllFormed),
                            }
                        }
                        _ => todo!(),
                    };
                    let classifier = match *rhs {
                        Syn::Primary(p) => match p {
                            Primary::Stage(_) => todo!(),
                            Primary::Atomic(_) => todo!(),
                        },
                        Syn::Infix { lhs, op, rhs } => todo!(),
                        Syn::Prefix { op, rhs } => todo!(),
                        Syn::Postfix { lhs, op } => todo!(),
                    };
                    let decl = Decl {
                        ident,
                        classifier,
                        annote,
                    };
                }
                _ => return Err(Error::IllFormed), // ?
            },
            Syn::Prefix { op, rhs } => todo!(),
            Syn::Postfix { lhs, op } => todo!(),
        },
        Top::Stage(_, _) => todo!(),
        Top::Context(_, _) => todo!(),
        Top::Special(special) => match special {
            Special::Trace(_) => todo!(),
            Special::Builtin(_) => todo!(),
            Special::Interactive(_) => todo!(),
        },
    };
    Ok(csyn)
}

fn extract_stage(name: String, tops: Vec<Top>) -> Result<Stage, Error> {
    tops.first()
        .ok_or(Error::IllFormed)
        .and_then(|top| match top {
            Top::Stage(name, rules_syntax) => {
                let rules = todo!();
                Ok(Stage {
                    name: name.to_string(),
                    nondet: Nondet::Random,
                    body: rules,
                })
            }
            _ => Err(Error::IllFormed),
        })
}

fn extract_trace(atomics: &[Atomic]) {}
fn extract_builtin(atomics: &[Atomic]) {}

pub struct Program {
    init_stage: String,
    init_state: Vec<Atom>,
}

pub struct Stage {
    name: String,
    nondet: Nondet,
    body: Vec<Rule>,
}

pub struct StageRule {
    name: String,
    pivars: i32,
    lhs: Vec<Atom>,
    rhs: Vec<Atom>,
    pre_stage: String,
    post_stage: String,
    ann: Option<Annote>,
}

pub struct Rule {
    name: String,
    pivars: i32,
    lhs: Vec<Atom>,
    rhs: Vec<Atom>,
    ann: Option<Annote>,
}

#[derive(Copy, Clone)]
enum Nondet {
    Random,
    Interactive,
    Ordered,
}

pub struct Sigma {
    header: Vec<Decl>,
    builtins: Vec<(String, Builtin)>,
    rules: Vec<BwdRule>,
    stages: Vec<Stage>,
    links: Vec<StageRule>,
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

impl Annote {
    pub fn new(annote: String) -> Self {
        Self { annote }
    }
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
    head: String,
    head_terms: Vec<Term>,
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

type Context = (String, Vec<Atom>);
