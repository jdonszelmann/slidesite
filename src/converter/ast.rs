
#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(String),
    Number(i64),
    String(Box<SlideString>),
    SlideBody(Vec<SlideStmt>),
    Tuple(Vec<Expression>),
    StructInstance {
        name: String,
        assignments: Vec<(String, Expression)>
    },

    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Call(Box<Expression>, Vec<Box<Expression>>),
}

#[derive(Debug, Clone)]
pub enum StringCharacter {
    Char(char),
    Expr(Expression),
}

#[derive(Debug, Clone)]
pub struct SlideString(pub Vec<StringCharacter>);

#[derive(Debug, Clone)]
pub enum SlideStmt {
    String(SlideString),
    Block(Vec<SlideStmt>),
    Column(Vec<SlideStmt>),
    ListItem(Box<SlideStmt>),
    EnumItem(Box<Expression>, Box<SlideStmt>),
    Marked(String, Box<SlideStmt>),
    Insert(String),
    Let(String, Expression),
}

#[derive(Debug)]
pub struct Slide {}

#[derive(Debug)]
pub struct Theme {
    pub name: SlideString,
    pub body: Vec<SlideStmt>,
}

#[derive(Debug)]
pub struct Template {
    pub name: SlideString,
    pub body: Vec<SlideStmt>,
}

#[derive(Debug)]
pub enum Statement {
    Slide {
        title: SlideString,
        body: Vec<SlideStmt>,
        identifier: Option<String>,
    },
    Let {
        name: String,
        value: Expression,
    },
    Title(SlideString),
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub types: Vec<TypeDef>,
}

#[derive(Debug)]
pub struct Field {
    pub name: String,
    pub default: Option<Expression>,
    pub field_type: String,
}

#[derive(Debug)]
pub enum TypeDef {
    Enum {
        name: String,
        variants: Vec<TypeDef>,
    },
    Struct {
        name: String,
        fields: Vec<Field>,
    },
}
