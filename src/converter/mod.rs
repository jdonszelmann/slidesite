use std::num::ParseIntError;
use convert_case::{Case, Casing};
use crate::parser;
use thiserror::Error;

mod ast;
pub use ast::*;

#[derive(Debug, Error)]
pub enum ConversionError {
    #[error("couldn't parse as int: {0}")]
    ParseInt(#[from] ParseIntError)
}

type Result<T> = std::result::Result<T, ConversionError>;

pub fn convert(inp: parser::Program) -> Result<Program> {
    Ok(Program {
        title: convert_slidestring(inp.title)?,
        themes: inp.themes.into_iter().map(convert_theme).collect::<Result<_>>()?,
        slides: convert_slides(inp.slides)?,
        templates: inp.templates.into_iter().map(convert_template).collect::<Result<_>>()?,
    })
}

fn convert_atom(a: parser::Atom) -> Result<Atom> {
    Ok(match a {
        parser::Atom::Identifier(i) => Atom::Identifier(i),
        parser::Atom::Number(n) => Atom::Number(n.parse()?),
        parser::Atom::String(s) => Atom::String(Box::new(convert_slidestring(*s)?)),
    })
}

fn convert_expr(e: parser::Expression) -> Result<Expression> {
    Ok(match e {
        parser::Expression::Atom(a) => Expression::Atom(convert_atom(a)?),
        parser::Expression::Neg(n) => Expression::Mul(Box::new(convert_expr(*n)?), Box::new(Expression::Atom(Atom::Number(-1)))),
        parser::Expression::Sub(a, b) => Expression::Sub(Box::new(convert_expr(*a)?), Box::new(convert_expr(*b)?)),
        parser::Expression::Mul(a, b) => Expression::Mul(Box::new(convert_expr(*a)?), Box::new(convert_expr(*b)?)),
        parser::Expression::Div(a, b) => Expression::Div(Box::new(convert_expr(*a)?), Box::new(convert_expr(*b)?)),
        parser::Expression::Add(a, b) => Expression::Add(Box::new(convert_expr(*a)?), Box::new(convert_expr(*b)?)),
    })
}

fn convert_stringchar(c: parser::StringCharacter) -> Result<StringCharacter> {
    Ok(match c {
        parser::StringCharacter::Char(c) => StringCharacter::Char(c),
        parser::StringCharacter::Expr(e) => StringCharacter::Expr(convert_expr(e)?),
    })
}

fn convert_slidestring(string: parser::SlideString) -> Result<SlideString> {
    Ok(match string {
        parser::SlideString::Complex(c) => SlideString(c.into_iter().map(convert_stringchar).collect::<Result<_>>()?),
        parser::SlideString::Simple(s) => SlideString(s.chars().map(StringCharacter::Char).collect()),
    })
}

fn convert_template(theme: parser::Template) -> Result<Template> {
    Ok(Template {
        name: convert_slidestring(theme.name)?,
        body: convert_body(&theme.body)?
    })
}

fn convert_theme(theme: parser::Theme) -> Result<Theme> {
    Ok(Theme {
        name: convert_slidestring(theme.name)?,
        body: vec![]
    })
}

fn convert_slides(slides: Vec<parser::Slide>) -> Result<Vec<Slide>> {
    let mut res = Vec::new();

    for i in &slides {
        res.push(Slide {
            title: convert_slidestring(i.title.clone())?,
            body: convert_body(&i.body)?,
        });
    }

    Ok(res)
}

pub fn convert_body(body: &[parser::SlideStmt]) -> Result<Vec<SlideStmt>> {
    let mut res = Vec::new();

    for i in body {
        res.push(convert_stmt(i)?);
    }

    Ok(res)
}

pub fn convert_ident_or_number(i: &parser::IdentifierOrNumber) -> Result<Atom> {
    Ok(match i {
        parser::IdentifierOrNumber::Identifier(i) => Atom::Identifier(i.clone()),
        parser::IdentifierOrNumber::Number(n) => Atom::Number(n.parse()?),
    })
}

pub fn convert_stmt(stmt: &parser::SlideStmt) -> Result<SlideStmt> {
    Ok(match stmt {
        parser::SlideStmt::String(s) => SlideStmt::String(convert_slidestring(s.clone())?),
        parser::SlideStmt::Block(b) => SlideStmt::Block(convert_body(b)?),
        parser::SlideStmt::Column(b) => SlideStmt::Column(convert_body(b)?),
        parser::SlideStmt::ListItem(l) => SlideStmt::ListItem(Box::new(convert_stmt(l)?)),
        parser::SlideStmt::EnumItem(ident, stmt) => SlideStmt::EnumItem(convert_ident_or_number(ident)?, Box::new(convert_stmt(stmt)?)),
        parser::SlideStmt::Marked(m, s) => SlideStmt::Marked(m.clone(), Box::new(convert_stmt(s)?)),
        parser::SlideStmt::Insert(i) => SlideStmt::Insert(i.clone()),
    })
}