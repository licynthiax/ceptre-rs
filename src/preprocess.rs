use crate::parser::*;
use crate::{all_ok_or, Error};

type Result<O> = std::result::Result<O, Error>;

#[derive(Debug)]
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

pub fn process(tops: Vec<Top>) -> Result<(Sigma, Vec<Program>)> {
    let mut header: Vec<Decl> = Vec::new();
    let mut bwds: Vec<BwdRule> = Vec::new();
    let mut contexts: Vec<Context> = Vec::new();
    let mut stages: Vec<Stage> = Vec::new();
    let mut links: Vec<StageRule> = Vec::new();
    let mut progs: Vec<Program> = Vec::new();
    let mut builtins: Vec<(String, Builtin)> = Vec::new();

    let mut gen_count = 0;
    let mut gensym = || {
        let name = "`anon".to_string() + &gen_count.to_string();
        gen_count += 1;
        name
    };

    for top in tops {
        match extract_top(&header, &contexts, top, &mut gensym)? {
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
    Ok((sigma, progs))
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

fn extract_top(
    header: &[Decl],
    contexts: &[Context],
    top: Top,
    mut gensym: impl FnMut() -> String,
) -> Result<CSyn> {
    let csyn = match top {
        Top::Stage(name, tops) => CSyn::Stage(extract_stage(header, name, tops, gensym)?),
        Top::Context(name, syn) => {
            CSyn::Ctx((name, extract_context(contexts, header, syn.as_ref())?))
        }
        Top::Special(special) => match special {
            Special::Trace {
                name,
                limit,
                context,
            } => {
                let context = match context {
                    Atomic::EmptyBraces => Vec::new(),
                    Atomic::Braces(syn) => extract_context(contexts, header, Some(&syn))?,
                    Atomic::Ident(name) => match contexts.iter().find(|(n, _)| n == &name) {
                        Some((name, ctx)) => ctx.clone(),
                        None => return Err(Error::IllFormed),
                    },
                    _ => return Err(Error::IllFormed),
                };
                CSyn::Prog(limit, (name, context))
            }
            Special::Builtin(name, builtin) => CSyn::Builtin(name, builtin),
            Special::Interactive(name) => CSyn::StageMode(name, Nondet::Interactive),
        },
        // this is awful bc it's just ugly plus theres a bunch of clones of stuff that feels
        // like it should be unnecessary but are, in fact, necessary :/
        Top::Decl(syn, annote) => match &syn {
            // id: lhs -o rhs
            Syn::Infix {
                ref lhs,
                op: Op::Colon,
                ref rhs,
            } => {
                let is_id = matches![**lhs, Syn::Primary(Primary::Atomic(Atomic::Ident(_)))];
                let is_lolli = matches![**rhs, Syn::Infix { op: Op::Lolli, .. }];

                if is_id && is_lolli {
                    CSyn::Rule(decl_to_rule(header, Top::Decl(syn, annote), gensym)?)
                } else {
                    extract_decl(header, syn, annote, gensym)?
                }
            }
            // lolli
            Syn::Infix {
                lhs,
                op: Op::Lolli,
                rhs,
            } => {
                let syn = Syn::Infix {
                    lhs: Box::new(Syn::Primary(Primary::Atomic(Atomic::Ident(gensym())))),
                    op: Op::Lolli,
                    rhs: Box::new(Syn::Infix {
                        lhs: lhs.clone(),
                        op: Op::Lolli,
                        rhs: rhs.clone(),
                    }),
                };
                extract_top(header, contexts, Top::Decl(syn, annote.clone()), gensym)?
            }
            _ => extract_decl(header, syn, annote, gensym)?,
        },
    };
    Ok(csyn)
}

fn extract_decl(
    header: &[Decl],
    syn: Syn,
    annote: Option<Annote>,
    mut gensym: impl FnMut() -> String,
) -> Result<CSyn> {
    let mut wild = 0;

    match &syn {
        Syn::Infix {
            ref lhs,
            op: Op::Colon,
            ref rhs,
        } => match rhs.as_ref() {
            Syn::Primary(primary) => {
                extract_primary(&mut wild, lhs.as_ref(), primary, annote, header)
            }
            Syn::Infix { op: Op::Arrow, .. } => {
                let name = get_ident(lhs.as_ref())?;
                Ok(CSyn::Bwd(extract_bwd(&mut 0, name, &syn, &mut Vec::new())?))
            }
            _ => Err(Error::IllFormed),
        },
        _ => extract_decl(
            header,
            Syn::Infix {
                lhs: Box::new(Syn::Primary(Primary::Atomic(Atomic::Ident(gensym())))),
                op: Op::Colon,
                rhs: Box::new(syn),
            },
            annote,
            gensym,
        ),
    }
}

fn extract_primary(
    wild: &mut i32,
    lhs: &Syn,
    primary: &Primary,
    annote: Option<Annote>,
    header: &[Decl],
) -> Result<CSyn> {
    if let Primary::Atomics(ref atomics) = primary {
        if atomics.is_empty() {
            return Err(Error::IllFormed);
        } else if atomics.len() != 1 {
            let name = get_ident(lhs)?;
            return Ok(CSyn::Bwd(extract_bwd(
                &mut 0,
                name,
                &Syn::Primary(primary.clone()),
                &mut Vec::new(),
            )?));
        }
    }

    let name = match primary {
        Primary::Atomic(atomic) => match atomic {
            Atomic::Ident(name) => name.clone(),
            Atomic::Pred => "pred".to_string(),
            _ => return Err(Error::IllFormed),
        },
        _ => return Err(Error::IllFormed),
    };

    if name == "type" {
        let ident = get_ident(lhs)?.to_string();
        return Ok(CSyn::Decl(Decl {
            ident,
            classifier: Classifier::Type,
            annote,
        }));
    }

    let predclass = if name == "bwd" {
        Some(PredClass::Bwd)
    } else if name == "sense" {
        Some(PredClass::Sense)
    } else if name == "action" {
        Some(PredClass::Act)
    } else if name == "pred" {
        Some(PredClass::Prop)
    } else {
        None
    };

    if let Some(class) = predclass {
        let data = get_first_atomic(lhs)?;
        if let Term::Fn(ident, args) = extract_term(wild, &data)? {
            return Ok(CSyn::Decl(Decl {
                ident,
                classifier: Classifier::Pred(class, args),
                annote,
            }));
        } else {
            return Err(Error::IllFormed);
        }
    } else {
        let decl = header.iter().find(|decl| decl.ident == name);
        if let Some(decl) = decl {
            match &decl.classifier {
                Classifier::Type => {
                    let data = get_first_atomic(lhs)
                        .or(get_ident(lhs).map(|ident| Atomic::Ident(ident.to_string())))?;
                    if let Term::Fn(ident, args) = extract_term(wild, &data)? {
                        let idents = all_ok_or(args.into_iter().map(|a| {
                            if let Term::Fn(name, _) = a {
                                Ok(name)
                            } else {
                                Err(Error::IllFormed)
                            }
                        }))?;
                        return Ok(CSyn::Decl(Decl {
                            ident,
                            classifier: Classifier::Tp(idents, name),
                            annote,
                        }));
                    } else {
                        return Err(Error::IllFormed);
                    }
                }
                Classifier::Pred(PredClass::Bwd, arg_tps) => {
                    let name = get_ident(lhs)?;
                    return Ok(CSyn::Bwd(extract_bwd(
                        &mut 0,
                        name,
                        &Syn::Primary(primary.clone()),
                        &mut Vec::new(),
                    )?));
                }
                _ => return Err(Error::IllFormed),
            }
        }
    }
    Err(Error::IllFormed)
}

fn extract_stage(
    header: &[Decl],
    name: String,
    tops: Vec<Top>,
    mut gensym: impl FnMut() -> String,
) -> Result<Stage> {
    let rules = tops
        .into_iter()
        .map(|top| decl_to_rule(header, top, &mut gensym));
    let mut body = all_ok_or(rules)?;
    Ok(Stage {
        name: name.to_string(),
        nondet: Nondet::Random,
        body,
    })
}

fn decl_to_rule(header: &[Decl], top: Top, mut gensym: impl FnMut() -> String) -> Result<Rule> {
    let mut wild: i32 = 0;

    match top {
        Top::Decl(
            Syn::Infix {
                op: Op::Colon,
                lhs,
                rhs,
            },
            annote,
        ) => match (lhs.as_ref(), rhs.as_ref()) {
            (
                Syn::Primary(Primary::Atomic(Atomic::Ident(name))),
                Syn::Infix {
                    op: Op::Lolli,
                    rhs,
                    lhs,
                },
            ) => {
                let (lhs, rhs, pivars) =
                    extract_infix_atoms(&mut wild, lhs.as_ref(), rhs.as_ref())?;
                Ok(Rule {
                    name: name.to_string(),
                    pivars,
                    lhs,
                    rhs,
                    ann: annote,
                })
            }
            _ => Err(Error::IllFormed),
        },
        Top::Decl(
            Syn::Infix {
                op: Op::Lolli,
                lhs,
                rhs,
            },
            annote,
        ) => {
            let name = gensym();
            let (lhs, rhs, pivars) = extract_infix_atoms(&mut wild, lhs.as_ref(), rhs.as_ref())?;
            Ok(Rule {
                name: name.to_string(),
                pivars,
                lhs,
                rhs,
                ann: annote,
            })
        }
        _ => Err(Error::IllFormed), // TODO: should probably print the top for debugging but i can't be
                                    // bothered rn
    }
}

fn extract_bwd(wild: &mut i32, name: &str, syn: &Syn, subgoals: &mut Vec<Atom>) -> Result<BwdRule> {
    match syn {
        Syn::Infix {
            lhs,
            op: Op::Arrow,
            rhs,
        } => {
            subgoals.push(extract_atom(wild, lhs, None)?);
            extract_bwd(wild, name, syn, subgoals)
        }
        Syn::Primary(Primary::Atomics(_)) | Syn::Primary(Primary::Atomic(Atomic::Ident(_))) => {
            let mut head = extract_atom(wild, syn, None)?;
            match head.mode {
                Mode::Lin => head.mode = Mode::Pers,
                Mode::Pers => return Err(Error::IllFormed),
            }

            use std::collections::BTreeSet;
            let mut vars = BTreeSet::new();
            for term in head.terms.iter() {
                if let Term::Var(name) = term {
                    vars.insert(name);
                }
            }
            for atom in subgoals.iter() {
                for term in atom.terms.iter() {
                    if let Term::Var(name) = term {
                        vars.insert(name);
                    }
                }
            }
            let var_count = vars.len() as i32;

            Ok(BwdRule {
                name: name.to_string(),
                pivars: var_count,
                head: head.pred,
                head_terms: head.terms,
                subgoals: subgoals.clone(),
            })
        }
        _ => Err(Error::IllFormed),
    }
}

// returns lhs and rhs atoms
fn extract_infix_atoms(
    wild: &mut i32,
    lhs: &Syn,
    rhs: &Syn,
) -> Result<(Vec<Atom>, Vec<Atom>, i32)> {
    let mut rhs_atoms = Vec::new();

    // recursively grab all atoms on lhs
    fn extract_lhs(wild: &mut i32, lhs: &Syn, rhs_atoms: &mut Vec<Atom>) -> Result<Vec<Atom>> {
        let mut lhs_atoms = Vec::new();
        match lhs {
            Syn::Infix {
                lhs,
                op: Op::Star,
                rhs,
            } => {
                lhs_atoms.push(extract_atom(wild, lhs, Some(rhs_atoms))?);
                lhs_atoms.append(&mut extract_lhs(wild, rhs, rhs_atoms)?);
            }
            _ => lhs_atoms.push(extract_atom(wild, lhs, Some(rhs_atoms))?),
        };
        Ok(lhs_atoms)
    }
    let lhs_atoms = extract_lhs(wild, lhs, &mut rhs_atoms)?;

    fn extract_rhs(wild: &mut i32, rhs: &Syn) -> Result<Vec<Atom>> {
        let mut rhs_atoms = Vec::new();
        match rhs {
            Syn::Infix {
                lhs,
                op: Op::Star,
                rhs,
            } => {
                rhs_atoms.push(extract_atom(wild, lhs, None)?);
                rhs_atoms.append(&mut extract_rhs(wild, rhs)?);
            }
            _ => {
                let atom = extract_atom(wild, rhs, None)?;
                rhs_atoms.push(atom);
            }
        };
        Ok(rhs_atoms)
    }
    rhs_atoms.append(&mut extract_rhs(wild, rhs)?);

    use std::collections::BTreeSet;
    let mut vars = BTreeSet::new();
    for atom in lhs_atoms.iter().chain(rhs_atoms.iter()) {
        for term in atom.terms.iter() {
            if let Term::Var(name) = term {
                vars.insert(name);
            }
        }
    }
    let var_count = vars.len() as i32;
    Ok((lhs_atoms, rhs_atoms, var_count))
}

fn extract_term(wild: &mut i32, atomic: &Atomic) -> Result<Term> {
    let term = match atomic {
        Atomic::Ident(s) => {
            if let Some(c) = s.chars().next() {
                if c.is_uppercase() {
                    Term::Var(s.to_string())
                } else {
                    Term::Fn(s.to_string(), Vec::new())
                }
            } else {
                return Err(Error::IllFormed);
            }
        }
        Atomic::Wildcard => {
            let w = wild.to_string();
            *wild += 1;
            Term::Var("_X".to_string() + &w)
        }
        Atomic::Number(i) => Term::ILit(*i),
        Atomic::App(atomics) => {
            let mut atomics = atomics.iter();
            if let Some(Atomic::Ident(name)) = atomics.next() {
                Term::Fn(
                    name.clone(),
                    all_ok_or(atomics.map(|a| extract_term(wild, a)))?,
                )
            } else {
                return Err(Error::IllFormed);
            }
        }
        _ => return Err(Error::IllFormed),
    };
    Ok(term)
}

fn extract_atom(wild: &mut i32, syn: &Syn, rhs: Option<&mut Vec<Atom>>) -> Result<Atom> {
    let atom = match syn {
        Syn::Primary(Primary::Atomics(atomics)) => {
            let mut atomics = atomics.iter();
            if let Some(Atomic::Ident(name)) = atomics.next() {
                let terms = all_ok_or(atomics.map(|a| extract_term(wild, a)))?;
                Atom {
                    mode: Mode::Lin,
                    pred: name.to_string(),
                    terms,
                }
            } else {
                return Err(Error::IllFormed);
            }
        }
        Syn::Primary(Primary::Atomic(Atomic::Ident(name))) => Atom {
            mode: Mode::Lin,
            pred: name.clone(),
            terms: Vec::new(),
        },
        Syn::Prefix {
            op: Op::Dollar,
            rhs: syn,
        } => match rhs {
            Some(rhs) => {
                let atom = extract_atom(wild, syn, Some(rhs))?;
                match atom.mode {
                    Mode::Pers => return Err(Error::IllFormed),
                    Mode::Lin => {
                        rhs.push(atom.clone());
                        atom
                    }
                }
            }
            None => {
                let atom = extract_atom(wild, syn, None)?;
                match atom.mode {
                    Mode::Pers => return Err(Error::IllFormed),
                    Mode::Lin => atom,
                }
            }
        },
        Syn::Prefix {
            op: Op::Bang,
            rhs: syn,
        } => extract_atom(wild, syn, rhs).and_then(|a| match a.mode {
            Mode::Pers => Err(Error::IllFormed),
            Mode::Lin => Ok(Atom {
                mode: Mode::Pers,
                pred: a.pred,
                terms: a.terms,
            }),
        })?,
        _ => return Err(Error::IllFormed),
    };
    Ok(atom)
}

fn extract_context(contexts: &[Context], header: &[Decl], syn: Option<&Syn>) -> Result<Vec<Atom>> {
    match syn {
        Some(syn) => match syn {
            Syn::Primary(Primary::Atomic(Atomic::Ident(name))) => {
                match contexts.iter().find(|(id, _)| id == name) {
                    None => header
                        .iter()
                        .find(|decl| {
                            decl.ident == *name
                                && match &decl.classifier {
                                    Classifier::Pred(_, terms) => terms.is_empty(),
                                    _ => false,
                                }
                        })
                        .ok_or(Error::IllFormed)
                        .and_then(|_| extract_ground_atom(syn))
                        .map(|a| vec![a]),
                    Some((_, atoms)) => Ok(atoms.clone()),
                }
            }
            Syn::Infix {
                lhs,
                op: Op::Comma,
                rhs,
            } => {
                let mut lctx = extract_context(contexts, header, Some(lhs.as_ref()))?;
                let mut rctx = extract_context(contexts, header, Some(rhs.as_ref()))?;
                lctx.append(&mut rctx);
                Ok(lctx)
            }
            _ => Ok(vec![extract_ground_atom(syn)?]),
        },
        None => Ok(Vec::new()),
    }
}

fn extract_ground_atom(syn: &Syn) -> Result<Atom> {
    match syn {
        Syn::Primary(Primary::Atomic(Atomic::Ident(name))) => Ok(Atom {
            mode: Mode::Lin,
            pred: name.clone(),
            terms: Vec::new(),
        }),
        Syn::Primary(Primary::Atomics(atomics)) => {
            let mut atomics = atomics.iter();
            if let Some(Atomic::Ident(name)) = atomics.next() {
                let terms = all_ok_or(atomics.map(|a| extract_term(&mut 0, a)))?;
                Ok(Atom {
                    mode: Mode::Lin,
                    pred: name.to_string(),
                    terms,
                })
            } else {
                Err(Error::IllFormed)
            }
        }
        Syn::Prefix { op: Op::Bang, rhs } => {
            let a = extract_ground_atom(rhs)?;
            match a.mode {
                Mode::Pers => Err(Error::IllFormed),
                Mode::Lin => Ok(Atom {
                    mode: Mode::Pers,
                    pred: a.pred,
                    terms: a.terms,
                }),
            }
        }
        _ => Err(Error::IllFormed),
    }
}

fn get_ident(syn: &Syn) -> Result<&str> {
    if let Syn::Primary(Primary::Atomic(Atomic::Ident(id))) = syn {
        Ok(id)
    } else {
        Err(Error::IllFormed)
    }
}

fn get_first_atomic(syn: &Syn) -> Result<Atomic> {
    if let Syn::Primary(Primary::Atomics(atomics)) = syn {
        atomics.first().ok_or(Error::IllFormed).cloned()
    } else {
        Err(Error::IllFormed)
    }
}

#[derive(Debug)]
pub struct Program {
    pub init_stage: String,
    pub init_state: Vec<Atom>,
}

#[derive(Debug)]
pub struct Stage {
    name: String,
    nondet: Nondet,
    body: Vec<Rule>,
}

#[derive(Debug)]
pub struct StageRule {
    name: String,
    pivars: i32,
    lhs: Vec<Atom>,
    rhs: Vec<Atom>,
    pre_stage: String,
    post_stage: String,
    ann: Option<Annote>,
}

#[derive(Debug)]
pub struct Rule {
    name: String,
    pivars: i32,
    lhs: Vec<Atom>,
    rhs: Vec<Atom>,
    ann: Option<Annote>,
}

#[derive(Debug, Copy, Clone)]
enum Nondet {
    Random,
    Interactive,
    Ordered,
}

#[derive(Debug)]
pub struct Sigma {
    pub header: Vec<Decl>,
    pub builtins: Vec<(String, Builtin)>,
    pub rules: Vec<BwdRule>,
    pub stages: Vec<Stage>,
    pub links: Vec<StageRule>,
}

#[derive(Debug)]
pub struct Decl {
    pub ident: String,
    pub classifier: Classifier,
    annote: Option<Annote>,
}

#[derive(Debug, Clone)]
pub struct Annote {
    annote: String,
}

impl Annote {
    pub fn new(annote: String) -> Self {
        Self { annote }
    }
}

#[derive(Debug)]
pub enum Classifier {
    Type,
    Tp(Vec<String>, String),
    Pred(PredClass, Vec<Term>),
}

#[derive(Debug)]
pub enum PredClass {
    Prop,
    Bwd,
    Sense,
    Act,
}

#[derive(Debug, Clone)]
pub enum Term {
    Fn(String, Vec<Term>),
    Var(String),
    SLit(String),
    ILit(i32),
}

// this should go somewhere else
#[derive(Debug, Clone, Copy)]
pub enum Builtin {
    Nat,
    Zero,
    Succ,
}

#[derive(Debug)]
pub struct BwdRule {
    name: String,
    pivars: i32,
    head: String,
    head_terms: Vec<Term>,
    subgoals: Vec<Atom>,
}

#[derive(Debug, Clone)]
pub struct Atom {
    mode: Mode,
    pred: String,
    terms: Vec<Term>,
}

#[derive(Debug, Copy, Clone)]
enum Mode {
    Pers,
    Lin,
}

type Context = (String, Vec<Atom>);
