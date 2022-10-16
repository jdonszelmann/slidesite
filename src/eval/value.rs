use crate::converter::Function;
use crate::eval::Scope;
use itertools::Itertools;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub enum Value {
    Number(i64),
    String(String),
    Tuple(Vec<Value>),
    Struct(HashMap<String, Value>),
    Function(Function, Scope),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(i) => write!(f, "{i}"),
            Value::String(i) => write!(f, "{i}"),
            Value::Tuple(i) if i.len() != 1 => {
                write!(f, "({})", i.iter().map(ToString::to_string).join(","))
            }
            Value::Tuple(i) => {
                write!(f, "({},)", i[0])
            }
            Value::Struct(_) => todo!(),
            Value::Function(
                Function {
                    name: Some(name), ..
                },
                _,
            ) => write!(f, "<function {name}>"),
            Value::Function(_, _) => write!(f, "<unnamed function>"),
        }
    }
}

#[derive(Debug)]
pub enum SlideStmt {
    String(String),
    Block(Vec<SlideStmt>),
    Column(Vec<SlideStmt>),
    ListItem(Box<SlideStmt>),
    EnumItem(i64, Box<SlideStmt>),
    Marked(String, Box<SlideStmt>),
    Insert(String),
}

#[derive(Debug)]
pub struct Slide {
    pub title: String,
    pub identifier: String,
    pub theme: Option<Theme>,

    pub body: Vec<SlideStmt>,
}

#[derive(Debug)]
pub struct Theme {
    pub name: String,
    pub body: Vec<SlideStmt>,
}

#[derive(Debug)]
pub struct SlideShow {
    pub title: Option<String>,
    pub slides: Vec<Slide>,
}
