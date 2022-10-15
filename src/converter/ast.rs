
#[derive(Debug, Clone)]
pub enum Atom {
    Identifier(String),
    Number(i64),
    String(Box<SlideString>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Atom(Atom),
    Neg(Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum StringCharacter {
    Char(char),
    Expr(Expression),
}

#[derive(Debug, Clone)]
pub struct SlideString(pub Vec<StringCharacter>);

#[derive(Debug)]
pub enum SlideStmt {
    String(SlideString),
    Block(Vec<SlideStmt>),
    Column(Vec<SlideStmt>),
    ListItem(Box<SlideStmt>),
    EnumItem(Atom, Box<SlideStmt>),
    Marked(String, Box<SlideStmt>),
    Insert(String),
}

#[derive(Debug)]
pub struct Slide {
    pub title: SlideString,
    pub body: Vec<SlideStmt>,
}

#[derive(Debug)]
pub struct Theme {
    pub name: SlideString,
    pub body: Vec<SlideStmt>
}

#[derive(Debug)]
pub struct Template {
    pub name: SlideString,
    pub body: Vec<SlideStmt>,
}

#[derive(Debug)]
pub struct Program {
    pub title: SlideString,
    pub themes: Vec<Theme>,
    pub slides: Vec<Slide>,
    pub templates: Vec<Template>,
}
