use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub enum Value {
    Number(i64),
    String(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
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

    pub body: Vec<SlideStmt>,
}

#[derive(Debug)]
pub struct Theme {
    pub name: String,
    pub body: Vec<SlideStmt>
}

#[derive(Debug)]
pub struct Template {
    pub name: String,
    pub body: Vec<SlideStmt>,
}

#[derive(Debug)]
pub struct SlideShow {
    pub title: String,
    pub themes: Vec<Theme>,
    pub slides: Vec<Slide>,
    pub templates: Vec<Template>,
}
