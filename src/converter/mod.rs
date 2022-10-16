use std::num::ParseIntError;
use crate::parser;
use thiserror::Error;

mod ast;

pub use ast::*;
use crate::parser::{FunctionStatement, TopLevel};

#[derive(Debug, Error)]
pub enum ConversionError {
    #[error("couldn't parse as int: {0}")]
    ParseInt(#[from] ParseIntError),
}

type Result<T> = std::result::Result<T, ConversionError>;

pub fn convert(inp: parser::Program) -> Result<Program> {
    let (statements, types) = convert_toplevels(inp.statements)?;
    Ok(Program {
        statements,
        types,
    })
}

fn convert_atom(a: parser::Atom) -> Result<Expression> {
    Ok(match a {
        parser::Atom::Identifier(i) => Expression::Identifier(i),
        parser::Atom::Number(n) => Expression::Number(n.parse()?),
        parser::Atom::String(s) => Expression::String(Box::new(convert_slidestring(*s)?)),
        parser::Atom::Tuple(t) => Expression::Tuple(t.into_iter().map(convert_expr).collect::<Result<_>>()?),
        parser::Atom::Struct(name, assignments) => Expression::StructInstance {
            name,
            assignments: assignments.into_iter().map(|(n, e)| Ok((n, convert_expr(e)?))).collect::<Result<_>>()?,
        },
        parser::Atom::Function(f) => {
            Expression::Function(Function {
                name: None,
                signature: convert_signature(f.signature)?,
                body: convert_function_body(f.body)?,
            })
        }
    })
}

fn convert_signature(s: parser::FunctionSignature) -> Result<FunctionSignature> {
    Ok(FunctionSignature {
        params: s.params,
        ret: s.ret,
    })
}

fn convert_function_body(s: parser::FunctionBody) -> Result<FunctionBody> {
    Ok(FunctionBody {
        stmts: s.stmts.into_iter().map(convert_function_stmt).collect::<Result<_>>()?,
        ret_expr: s.ret_expr.map(convert_expr).transpose()?.map(Box::new),
    })
}

fn convert_function_stmt(s: parser::FunctionStatement) -> Result<Statement> {
    Ok(match s {
        FunctionStatement::Let(name, expr) => Statement::Let {
            name,
            value: convert_expr(expr)?,
        }
    })
}

fn convert_expr(e: parser::Expression) -> Result<Expression> {
    Ok(match e {
        parser::Expression::Atom(a) => convert_atom(a)?,
        parser::Expression::Neg(n) => Expression::Mul(Box::new(convert_expr(*n)?), Box::new(Expression::Number(-1))),
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

fn convert_toplevels(toplevels: Vec<parser::TopLevel>) -> Result<(Vec<Statement>, Vec<TypeDef>)> {
    let mut statements = Vec::new();
    let mut types = Vec::new();

    for toplevel in toplevels {
        statements.push(match toplevel {
            TopLevel::Slide(i) => Statement::Slide {
                title: convert_slidestring(i.title.clone())?,
                body: convert_body(i.body)?,
                identifier: None,
            },
            TopLevel::Theme(t) => Statement::Let {
                name: t.name,
                value: Expression::StructInstance {
                    name: "Theme".to_string(),
                    assignments: vec![],
                },
            },
            TopLevel::Template(t) => Statement::Let {
                name: t.name,
                value: Expression::SlideBody(convert_body(t.body)?),
            },
            TopLevel::TypeDef(t) => {
                types.push(convert_typedef(t)?);
                continue;
            }
            TopLevel::Let(name, value) => Statement::Let {
                name,
                value: convert_expr(value)?,
            },
            TopLevel::Title(s) => Statement::Title(convert_slidestring(s)?),
            TopLevel::Function(f) => Statement::Let {
                name: f.name.clone(),
                value: Expression::Function(Function {
                    name: Some(f.name),
                    signature: convert_signature(f.signature)?,
                    body: convert_function_body(f.body)?
                }),
            }
        })
    }

    Ok((statements, types))
}

pub fn convert_field(field: parser::Field) -> Result<Field> {
    Ok(Field {
        name: field.name,
        default: field.default.map(convert_expr).transpose()?,
        field_type: field.field_type,
    })
}

pub fn convert_typedef(typedef: parser::TypeDef) -> Result<TypeDef> {
    Ok(match typedef {
        parser::TypeDef::Enum { name, variants } => TypeDef::Enum {
            name,
            variants: variants.into_iter().map(convert_typedef).collect::<Result<_>>()?,
        },
        parser::TypeDef::Struct { name, fields } => TypeDef::Struct {
            name,
            fields: fields.into_iter().map(convert_field).collect::<Result<_>>()?,
        }
    })
}

pub fn convert_body(body: Vec<parser::SlideStmt>) -> Result<Vec<SlideStmt>> {
    let mut res = Vec::new();

    for i in body {
        res.push(convert_stmt(i)?);
    }

    Ok(res)
}

pub fn convert_ident_or_number(i: parser::IdentifierOrNumber) -> Result<Expression> {
    Ok(match i {
        parser::IdentifierOrNumber::Identifier(i) => Expression::Identifier(i.clone()),
        parser::IdentifierOrNumber::Number(n) => Expression::Number(n.parse()?),
    })
}

pub fn convert_stmt(stmt: parser::SlideStmt) -> Result<SlideStmt> {
    Ok(match stmt {
        parser::SlideStmt::String(s) => SlideStmt::String(convert_slidestring(s)?),
        parser::SlideStmt::Block(b) => SlideStmt::Block(convert_body(b)?),
        parser::SlideStmt::Column(b) => SlideStmt::Column(convert_body(b)?),
        parser::SlideStmt::ListItem(l) => SlideStmt::ListItem(Box::new(convert_stmt(*l)?)),
        parser::SlideStmt::EnumItem(ident, stmt) => SlideStmt::EnumItem(Box::new(convert_ident_or_number(ident)?), Box::new(convert_stmt(*stmt)?)),
        parser::SlideStmt::Marked(m, s) => SlideStmt::Marked(m.clone(), Box::new(convert_stmt(*s)?)),
        parser::SlideStmt::Insert(i) => SlideStmt::Insert(i.clone()),
        parser::SlideStmt::Let(name, value) => SlideStmt::Let(name, convert_expr(value)?)
    })
}