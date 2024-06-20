//! Another precedence parser known as Pratt parsing was first described by Vaughan Pratt 
//! in the 1973 paper "Top down operator precedence",[3] based on recursive descent. 
//! -- Wikipedia

//! TODO: record only on recursive entry points (implement an alternative tagging strategy)

use std::fmt::{Debug, Error};
use std::{marker::PhantomData, ops::RangeBounds};

#[derive(Debug)]
pub struct Parser<O, E, X, F: for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, O), (usize, E)>>(F, PhantomData<(O, E, X)>);

impl<OA, E, X, F> Parser<OA, E, X, F> where 
    F: for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, OA), (usize, E)>
{
    pub fn then<OB>(self, other: Parser<OB, E, X, impl for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, OB), (usize, E)>>) 
        -> Parser<(OA, OB), E, X, impl for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, (OA, OB)), (usize, E)>> 
    {
        let Parser(a, _) = self;
        let Parser(b, _) = other;
        Parser::new(move |s: &str, p: usize, x: &mut X| {
            let (p, a) = a(s, p, x)?;
            let (p, b) = b(s, p, x)?;
            Ok((p, (a, b)))
        })
    }
    pub fn ignore_then<OB>(self, other: Parser<OB, E, X, impl for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, OB), (usize, E)>>) 
        -> Parser<OB, E, X, impl for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, OB), (usize, E)>> 
    {
        let Parser(a, _) = self;
        let Parser(b, _) = other;
        Parser::new(move |s: &str, p: usize, x: &mut X| {
            let (p, _) = a(s, p, x)?;
            let (p, b) = b(s, p, x)?;
            Ok((p, b))
        })
    }
    pub fn then_ignore<OB>(self, other: Parser<OB, E, X, impl for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, OB), (usize, E)>>) 
        -> Parser<OA, E, X, impl for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, OA), (usize, E)>> 
    {
        let Parser(a, _) = self;
        let Parser(b, _) = other;
        Parser::new(move |s: &str, p: usize, x: &mut X| {
            let (p, a) = a(s, p, x)?;
            let (p, _) = b(s, p, x)?;
            Ok((p, a))
        })
    }
    pub fn and<OB>(self, other: Parser<OB, E, X, impl for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, OB), (usize, E)>>) 
        -> Parser<OB, E, X, impl for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, OB), (usize, E)>> 
    {
        let Parser(a, _) = self;
        let Parser(b, _) = other;
        Parser::new(move |s: &str, p: usize, x: &mut X| {
            let (_, _) = a(s, p, x)?;
            let (p, b) = b(s, p, x)?;
            Ok((p, b))
        })
    }
    pub fn map<OB>(self, f: impl Fn(&mut X, OA) -> OB) 
        -> Parser<OB, E, X, impl for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, OB), (usize, E)>> 
    {
        let Parser(a, _) = self;
        Parser::new(move |s: &str, p: usize, x: &mut X| {
            let (p, a) = a(s, p, x)?;
            Ok((p, f(x, a)))
        })
    }
    pub fn repeat<OB>(self, range: impl RangeBounds<usize>, init: impl Fn(&mut X) -> OB, fold: impl Fn(&mut X, OB, OA) -> OB)
        -> Parser<OB, E, X, impl for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, OB), (usize, E)>>
    {
        let Parser(a, _) = self;
        Parser::new(move |s: &str, mut p: usize, x: &mut X| {
            let mut pass = false;
            let mut value = init(x);
            for i in 0.. {
                pass |= range.contains(&i);
                if pass && !range.contains(&i) { return Ok((p, value)) }
                let result = a(s, p, x);
                if range.contains(&i) && result.is_err() {
                    return Ok((p, value))
                }
                let (q, next) = result?;
                value = fold(x, value, next);
                p = q;
            }
            Ok((p, value))
        })
    }
    pub fn pad(self)
        -> Parser<OA, E, X, impl for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, OA), (usize, E)>>
    {
        let Parser(a, _) = self;
        Parser::new(move |s: &str, p: usize, x: &mut X| {
            let d = s[p.min(s.len())..].char_indices().find_map(|(i, s)| (!s.is_whitespace()).then(|| i)).unwrap_or(0);
            let (p, a) = a(s, p + d, x)?;
            let d = s[p..].char_indices().find_map(|(i, s)| (!s.is_whitespace()).then(|| i)).unwrap_or(0);
            Ok((p + d, a))
        })   
    }
}

pub trait Merge<X> {
    fn merge(self, x: &mut X, other: Self) -> Self;
}

impl<O, EA, X, F> Parser<O, EA, X, F> where
    F: for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, O), (usize, EA)>
{
    pub fn or<EB>(self, other: Parser<O, EB, X, impl for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, O), (usize, EB)>>) 
        -> Parser<O, (EA, EB), X, impl for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, O), (usize, (EA, EB))>> 
    {
        let Parser(a, _) = self;
        let Parser(b, _) = other;
        Parser::new(move |s: &str, p: usize, x: &mut X| {
            let (pa, a) = match a(s, p, x) {
                Err(e) => e,
                Ok(x) => return Ok(x),
            };
            let (pb, b) = match b(s, p, x) {
                Err(e) => e,
                Ok(x) => return Ok(x),
            };
            Err((pa.max(pb), (a, b)))
        })
    }
    pub fn or_merge(self, other: Parser<O, EA, X, impl for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, O), (usize, EA)>>) 
        -> Parser<O, EA, X, impl for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, O), (usize, EA)>> where
        EA: Merge<X>
    {
        let Parser(a, _) = self;
        let Parser(b, _) = other;
        Parser::new(move |s: &str, p: usize, x: &mut X| {
            let (pa, a) = match a(s, p, x) {
                Err(e) => e,
                Ok(x) => return Ok(x),
            };
            let (pb, b) = match b(s, p, x) {
                Err(e) => e,
                Ok(x) => return Ok(x),
            };
            Err((pa.max(pb), a.merge(x, b)))
        })
    }
    pub fn err<EB>(self, f: impl Fn(&mut X, EA) -> EB)
        -> Parser<O, EB, X, impl for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, O), (usize, EB)>> 
    {
        let Parser(a, _) = self;
        Parser::new(move |s: &str, p: usize, x: &mut X| {
            a(s, p, x).map_err(|(i, e)| (i, f(x, e)))
        })
    }
}

pub trait Memorize<O, E> {
    fn visit<const N: u16>(&mut self, _progress: usize) -> bool { false }
    fn record<const N: u16>(&mut self, _progress: usize, _result: Result<(usize, O), (usize, E)>) {}
    fn replay<const N: u16>(&mut self, _progress: usize) -> Option<Result<(usize, O), (usize, E)>> { None }
}

pub trait Visit {
    fn visited() -> Self;
}

pub trait Rest<X> {
    fn with_rest(x: &mut X, input: &str) -> Self;
}

impl<O, E, X, F> Parser<O, E, X, F> where 
    F: for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, O), (usize, E)>,
    X: Memorize<O, E>,
    O: Clone + Debug,
    E: Clone + Debug,
{
    // memorized parse
    pub fn label<const LABEL: u16>(self) -> Parser<O, E, X, impl for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, O), (usize, E)>> 
        where E: Visit
    {
        Parser::new(move |input: &str, progress: usize, x: &mut X| {
            if let Some(r) = x.replay::<LABEL>(progress) { return r; }
            if x.visit::<LABEL>(progress) { return Err((progress, E::visited())); }
            let result = self.0(input, progress, x);
            x.record::<LABEL>(progress, result.clone());
            result
        })
    }
    // debug print
    pub fn debug(self, message: &'static str) -> Parser<O, E, X, impl for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, O), (usize, E)>> {
        Parser::new(move |input: &str, progress: usize, x: &mut X| {
            let result = self.0(input, progress, x);
            println!("{progress:>4}: RULE '{message}'\t {result:?}");
            result
        })
    }
    // end a parsing process, force parser to read until file end
    pub fn end(self) -> Parser<O, E, X, impl for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, O), (usize, E)>>
        where E: Rest<X>
    {
        Parser::new(move |input: &str, progress: usize, x: &mut X| {
            let result = self.0(input, progress, x)?;
            if result.0 < input.len() {
                Err((result.0, E::with_rest(x, &input[result.0..])))
            }
            else {
                Ok(result)
            }
        })
    }
}

impl<O, E, X, F> Parser<O, E, X, F> where
    F: for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, O), (usize, E)>,
{
    pub fn new(x: F) -> Self {
        Self(x, PhantomData)
    }
    pub fn boxed(self) -> Parser<O, E, X, Box<dyn for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, O), (usize, E)>>> {
        let Parser(x, _) = self;
        let x = Box::new(x) as Box<dyn for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, O), (usize, E)>>;
        // we can do this with clear conscience, because self will never contain an portion of extension or string (pointless programming)
        Parser::new(unsafe { std::mem::transmute(x) })
    }
    pub fn release(self) -> impl for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, O), (usize, E)> {
        self.0
    }
    pub fn parse<'a>(&self, input: &'a str, extension: &'a mut X) -> Result<(usize, O), (usize, E)>  {
        self.0(input, 0, extension)
    }
    pub fn delayed(f: fn() -> Self) -> Parser<O, E, X, impl for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, O), (usize, E)>> {
        Parser(move |input, progress, x| f().0(input, progress, x), PhantomData)
    }
}

pub fn token<X>(t: &'static str) -> Parser<(), (), X, impl for<'a> Fn(&'a str, usize, &'a mut X) -> Result<(usize, ()), (usize, ())>> {
    Parser::new(move |s: &str, p: usize, _: &mut X| {
        pub fn prefix(xs: &[u8], ys: &[u8]) -> usize {
            xs.iter().zip(ys)
              .take_while(|(x, y)| x == y)
              .count()
        }
        if s.starts_with(t) {
            Ok((p + t.len(), ()))
        }
        else {
            Err((p + prefix(s[p..].as_bytes(), t.as_bytes()), ()))
        }
    })
}

use thiserror::Error;

#[derive(Clone, Copy, Error)]
pub enum CalcErr<'a> {
    #[error("no number matched")]
    NoNumberMatched,
    #[error("invalid token, expect {expect:?}, matched {matched:?}")]
    InvalidToken {
        expect: &'static str,
        matched: &'a str,
    },
    #[error("{0}\n{1}")]
    ManyError(&'a CalcErr<'a>, &'a CalcErr<'a>),
    #[error("recursive visit")]
    Visited,
    #[error("the parser cannot reach the end, the rest part: \"{0}\"")]
    Rest(&'a str),
}

impl<'a> Rest<CalcMemory<'a>> for CalcErr<'a> {
    fn with_rest(x: &mut CalcMemory<'a>, input: &str) -> Self {
        Self::Rest(x.bump.alloc_str(input))
    }
}

impl<'a> Visit for CalcErr<'a> {
    fn visited() -> Self { CalcErr::Visited }
}

impl<'a> Debug for CalcErr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CalcExp<'a> {
    Add(&'a CalcExp<'a>, &'a CalcExp<'a>),
    Mul(&'a CalcExp<'a>, &'a CalcExp<'a>),
    Sub(&'a CalcExp<'a>, &'a CalcExp<'a>),
    Div(&'a CalcExp<'a>, &'a CalcExp<'a>),
    Int(i64),
}

impl<'a> std::fmt::Display for CalcExp<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Debug)]
pub struct CalcMemory<'a> {
    bump: &'a Bump,
    visited: bumpalo::collections::Vec<'a, (u16, usize)>,
    recorded: bumpalo::collections::Vec<'a, (u16, usize, &'a u8)>,
}

impl<'a, O, E> Memorize<O, E> for CalcMemory<'a> where
    O: Clone, 
    E: Clone
{
    fn visit<const N: u16>(&mut self, progress: usize) -> bool {
        if self.recorded.iter().any(|(x, y, _)| (*x, *y) == (N, progress)) { return false }
        if self.visited.contains(&(N, progress)) { return true }
        self.visited.push((N, progress));
        false
    }
    fn record<const N: u16>(&mut self, progress: usize, result: Result<(usize, O), (usize, E)>) {
        self.recorded.push((N, progress, unsafe{std::mem::transmute(self.bump.alloc(result))}));
    }
    fn replay<const N: u16>(&mut self, progress: usize) -> Option<Result<(usize, O), (usize, E)>> {
        self.recorded.iter().find_map(|(x, y, res)| ((*x, *y) == (N, progress))
            .then(|| unsafe{std::mem::transmute::<_, &Result<(usize, O), (usize, E)>>(*res as &u8)}.clone()))
    }
}

use bumpalo::Bump;

impl<'a> Merge<CalcMemory<'a>> for CalcErr<'a> {
    fn merge(self, x: &mut CalcMemory<'a>, other: Self) -> Self {
        CalcErr::ManyError(x.bump.alloc(self), x.bump.alloc(other))
    }
}

pub fn number<'a>() -> Parser<CalcExp<'a>, CalcErr<'a>, CalcMemory<'a>, impl for<'b> Fn(&'b str, usize, &'b mut CalcMemory<'a>) -> Result<(usize, CalcExp<'a>), (usize, CalcErr<'a>)>> {
    Parser::new(move |s: &str, p: usize, _: &mut CalcMemory<'a>| {
        let d = s[p..].char_indices().map_while(|(i, c)| {
            c.is_ascii_digit().then_some(i+c.len_utf8())
        }).last();
        if d.is_none() { return Err((p, CalcErr::NoNumberMatched)) }
        let d = d.unwrap();
        Ok((p+d, CalcExp::Int(s[p..p+d].parse::<i64>().unwrap())))
    })
}

pub fn calculator_token<'a>(t: &'static str) -> Parser<(), CalcErr<'a>, CalcMemory<'a>, impl for<'b> Fn(&'b str, usize, &'b mut CalcMemory<'a>) -> Result<(usize, ()), (usize, CalcErr<'a>)>> {
    Parser::new(move |s: &str, p: usize, bump: &mut CalcMemory<'a>| {
        pub fn prefix(xs: &[u8], ys: &[u8]) -> usize {
            xs.iter().zip(ys)
              .take_while(|(x, y)| x == y)
              .count()
        }
        if s[p..].starts_with(t) {
            Ok((p + t.len(), ()))
        }
        else {
            Err((p + prefix(s.as_bytes(), t.as_bytes()), CalcErr::InvalidToken { expect: t, matched: bump.bump.alloc_str(&s[p..(p+t.len()).min(s.len())]) }))
        }
    }).pad()
}

// this will expand into a huge segement of code
pub fn calculator<'a>() -> Parser<CalcExp<'a>, CalcErr<'a>, CalcMemory<'a>, impl for<'b> Fn(&'b str, usize, &'b mut CalcMemory<'a>) -> Result<(usize, CalcExp<'a>), (usize, CalcErr<'a>)>> {
    fn mul<'a>() -> Parser<CalcExp<'a>, CalcErr<'a>, CalcMemory<'a>, impl for<'b> Fn(&'b str, usize, &'b mut CalcMemory<'a>) -> Result<(usize, CalcExp<'a>), (usize, CalcErr<'a>)>> {
        let f0 = number().map(|x, y| x.bump.alloc(y))
            .then_ignore(calculator_token("/")).then(Parser::delayed(mul).map(|x, y| x.bump.alloc(y)))
            .map(|_, y| CalcExp::Div(y.0, y.1)).pad();
        let f1 = number().map(|x, y| x.bump.alloc(y))
            .then_ignore(calculator_token("*")).then(Parser::delayed(mul).map(|x, y| x.bump.alloc(y)))
            .map(|_, y| CalcExp::Mul(y.0, y.1)).pad();
        f0.or_merge(f1).or_merge(number()).label::<0>().boxed().pad()
    }
    fn add<'a>() -> Parser<CalcExp<'a>, CalcErr<'a>, CalcMemory<'a>, impl for<'b> Fn(&'b str, usize, &'b mut CalcMemory<'a>) -> Result<(usize, CalcExp<'a>), (usize, CalcErr<'a>)>> {
        let a0 = Parser::delayed(mul).map(|x, y| x.bump.alloc(y))
            .then_ignore(calculator_token("+")).then(Parser::delayed(add).map(|x, y| x.bump.alloc(y)))
            .map(|_, y| CalcExp::Add(y.0, y.1)).pad();
        let a1 = Parser::delayed(mul).map(|x, y| x.bump.alloc(y))
            .then_ignore(calculator_token("-")).then(Parser::delayed(add).map(|x, y| x.bump.alloc(y)))
            .map(|_, y| CalcExp::Sub(y.0, y.1)).pad();
        a0.or_merge(a1).or_merge(mul()).label::<1>().boxed().pad()
    }
    add()
}

#[test]
pub fn test_calculator() {
    let bump = Bump::new();
    let bump = &bump;
    let calc = calculator().end().boxed();
    let mut memory = CalcMemory { bump, visited: bumpalo::collections::Vec::new_in(bump), recorded: bumpalo::collections::Vec::new_in(bump) };
    println!("{:?}", calc.parse("1 /2+4*3* /4", &mut memory).unwrap_err());
}
