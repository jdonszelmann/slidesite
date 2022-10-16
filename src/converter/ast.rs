#[derive(Debug, Clone)]
pub struct FunctionBody {
    pub stmts: Vec<Statement>,
    pub ret_expr: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub params: Vec<(String, String)>,
    pub ret: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Option<String>,
    pub signature: FunctionSignature,
    pub body: FunctionBody,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(String),
    Number(i64),
    String(Box<SlideString>),
    SlideBody(Vec<SlideStmt>),
    Tuple(Vec<Expression>),
    StructInstance {
        name: String,
        assignments: Vec<(String, Expression)>,
    },
    Function(Function),

    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
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

#[derive(Debug, Clone)]
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
