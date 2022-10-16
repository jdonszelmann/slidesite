use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use itertools::Itertools;

#[derive(Debug, Clone)]
pub enum Value {
    Number(i64),
    String(String),
    Tuple(Vec<Value>),
    Struct(HashMap<String, Value>)
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(i) => write!(f, "{i}"),
            Value::String(i) => write!(f, "{i}"),
            Value::Tuple(i) if i.len() != 1 => {
                write!(f, "({})", i.into_iter().map(ToString::to_string).join(","))
            },
            Value::Tuple(i) => {
                write!(f, "({},)", i[0])
            }
            Value::Struct(_) => todo!(),
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
    pub body: Vec<SlideStmt>
}

#[derive(Debug)]
pub struct SlideShow {
    pub title: Option<String>,
    pub slides: Vec<Slide>,
}
