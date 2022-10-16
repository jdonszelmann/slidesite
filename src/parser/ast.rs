
#[derive(Debug, Clone)]
pub enum IdentifierOrNumber {
    Identifier(String),
    Number(String),
}

#[derive(Debug, Clone)]
pub enum Atom {
    Identifier(String),
    Number(String),
    String(Box<SlideString>),
    Tuple(Vec<Expression>),
    Struct(String, Vec<(String, Expression)>),
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
pub enum SlideString {
    Complex(Vec<StringCharacter>),
    Simple(String),
}

#[derive(Debug, Clone)]
pub enum SlideStmt {
    String(SlideString),
    Block(Vec<SlideStmt>),
    Column(Vec<SlideStmt>),
    ListItem(Box<SlideStmt>),
    EnumItem(IdentifierOrNumber, Box<SlideStmt>),
    Marked(String, Box<SlideStmt>),
    Insert(String),
    Let(String, Expression),
}

#[derive(Debug)]
pub struct Slide {
    pub title: SlideString,
    pub body: Vec<SlideStmt>,
}

#[derive(Debug)]
pub struct Template {
    pub name: String,
    pub body: Vec<SlideStmt>,
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

#[derive(Debug)]
pub struct Theme {
    pub name: String,
}

#[derive(Debug)]
pub enum TopLevel {
    Slide(Slide),
    Theme(Theme),
    Template(Template),
    TypeDef(TypeDef),
    Let(String, Expression),
    Title(SlideString),
}

#[derive(Debug)]
pub struct Program {
    pub title: SlideString,
    pub statements: Vec<TopLevel>,
}
