use std::fmt::{Display, Formatter};
use itertools::Itertools;

#[derive(Debug, Clone)]
pub struct FunctionBody {
    pub stmts: Vec<Statement>,
    pub ret_expr: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub params: Vec<(String, TypeName)>,
    pub ret: Option<TypeName>,
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
        name: TypeName,
        assignments: Vec<(String, Expression)>,
    },
    Function(Function),

    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Index(Box<Expression>, Box<Expression>),
    Attr(Box<Expression>, String),
    TupleProject(Box<Expression>, u64),
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
    Let(String, Option<TypeName>, Expression),
}

#[derive(Debug)]
pub struct Slide {}

#[derive(Debug)]
pub struct Theme {
    pub name: SlideString,
    pub body: Vec<SlideStmt>,
}

#[derive(Debug, Clone)]
pub enum TypeName {
    Instantiation(String, Vec<TypeName>),
    Tuple(Vec<TypeName>),
    Int,
    String,
}

impl Display for TypeName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeName::Instantiation(i, args) if args.len() == 0 => write!(f, "{i}"),
            TypeName::Instantiation(i, args) => {
                write!(f, "{i}<{}>", args.iter().map(ToString::to_string).join(","))
            },
            TypeName::Tuple(t) if t.len() == 1 => write!(f, "({},)", t[0]),
            TypeName::Tuple(t) => write!(f, "({})", t.into_iter().map(ToString::to_string).join(",")),
            TypeName::Int => write!(f, "int"),
            TypeName::String => write!(f, "string"),
        }
    }
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
        ty: Option<TypeName>,
        expr: Expression,
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
    pub field_type: TypeName,
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
        generics: Vec<String>,
    },
    Trait {
        name: String,
        items: Vec<Statement>,
        generics: Vec<String>,
        stubs: Vec<FunctionStub>,
    },
    Impl {
        trait_name: Option<TypeName>,
        name: TypeName,
        instantiated_generics: Vec<String>,
        body: Vec<Statement>,
    },
}

#[derive(Debug, Clone)]
pub struct FunctionStub {
    pub name: String,
    pub signature: FunctionSignature,
}
