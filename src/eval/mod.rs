use crate::converter;
use crate::converter::{Expression, Function, Program, SlideString, Statement, StringCharacter};
use crate::eval::value::{Slide, SlideShow, SlideStmt, Value};
use convert_case::{Case, Casing};
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;
use thiserror::Error;

pub mod value;

#[derive(Debug, Error)]
pub enum EvalError {
    #[error("Expected a number for the index of an enum item, but found {0}")]
    NotANumberInEnum(Value),

    #[error("variable was never declared")]
    UndeclaredVariable(String),

    #[error("variable was declared but not yet defined")]
    UndefinedVariable(String),

    #[error("you cannot mark a {0:?}")]
    MarkedInvalidStatment(converter::SlideStmt),
}

type Result<T> = std::result::Result<T, EvalError>;

pub fn eval_ast(ast: Program) -> Result<SlideShow> {
    let mut e = Evaluator::new();
    let mut scope = Scope::new();
    let slides = e.eval_stmts(ast.statements, &mut scope)?;

    Ok(SlideShow {
        title: e.title,
        slides,
    })
}

pub struct Scope {
    bindings: HashMap<String, Rc<RefCell<Option<Value>>>>,
}

impl Debug for Scope {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<scope object>")
    }
}

impl Clone for Scope {
    fn clone(&self) -> Self {
        Scope {
            bindings: self.bindings.clone(),
        }
    }
}

pub struct UndefinedVariable(Rc<RefCell<Option<Value>>>);
impl UndefinedVariable {
    pub fn define(self, value: Value) {
        *self.0.borrow_mut() = Some(value);
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            bindings: Default::default(),
        }
    }

    pub fn declare_variable(&mut self, name: String) -> UndefinedVariable {
        let value = Rc::new(RefCell::new(None));
        self.bindings.insert(name, Rc::clone(&value));
        UndefinedVariable(value)
    }

    pub fn lookup_variable(&self, name: &str) -> Result<Value> {
        let value = self
            .bindings
            .get(name)
            .ok_or_else(|| EvalError::UndeclaredVariable(name.to_string()))?
            .borrow()
            .as_ref()
            .ok_or_else(|| EvalError::UndefinedVariable(name.to_string()))?
            .clone();

        Ok(value)
    }
}

struct Evaluator {
    title: Option<String>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self { title: None }
    }

    pub fn eval_string_char(&mut self, c: StringCharacter, scope: &mut Scope) -> Result<String> {
        Ok(match c {
            StringCharacter::Char(c) => c.to_string(),
            StringCharacter::Expr(e) => self.eval_expr(e, scope)?.to_string(),
        })
    }

    pub fn eval_string(&mut self, string: SlideString, scope: &mut Scope) -> Result<String> {
        string
            .0
            .into_iter()
            .map(|c| self.eval_string_char(c, scope))
            .collect::<Result<_>>()
    }

    pub fn eval_expr(&mut self, e: Expression, scope: &mut Scope) -> Result<Value> {
        Ok(match e {
            Expression::Identifier(name) => scope.lookup_variable(&name)?,
            Expression::Number(n) => Value::Number(n),
            Expression::String(s) => Value::String(self.eval_string(*s, scope)?),
            Expression::Tuple(v) => Value::Tuple(
                v.into_iter()
                    .map(|e| self.eval_expr(e, scope))
                    .collect::<Result<_>>()?,
            ),
            Expression::Sub(_, _) => todo!(),
            Expression::Mul(_, _) => todo!(),
            Expression::Div(_, _) => todo!(),
            Expression::Add(_, _) => todo!(),
            Expression::Call(_, _) => todo!(),
            Expression::Index(_, _) => todo!(),
            Expression::Attr(_, _) => todo!(),
            Expression::TupleProject(_, _) => todo!(),
            Expression::StructInstance {
                name: _,
                assignments,
            } => Value::Struct(
                assignments
                    .into_iter()
                    .map(|(n, e)| Ok((n, self.eval_expr(e, scope)?)))
                    .collect::<Result<_>>()?,
            ),
            Expression::SlideBody(_) => todo!(),
            Expression::Function(f @ Function { .. }) => Value::Function(f, scope.clone()),
        })
    }

    fn eval_let(&mut self, name: String, value: Expression, scope: &mut Scope) -> Result<()> {
        let var = scope.declare_variable(name);
        let value = self.eval_expr(value, scope)?;
        var.define(value);

        Ok(())
    }

    fn set_title(&mut self, title: String) {
        if let Some(ref i) = self.title {
            eprintln!("overwriting title from {i} to {title}");
        }

        self.title = Some(title)
    }

    fn eval_stmts(
        &mut self,
        statements: Vec<converter::Statement>,
        scope: &mut Scope,
    ) -> Result<Vec<Slide>> {
        let mut ctr = HashMap::new();
        let mut slides = Vec::new();

        for stmt in statements.into_iter() {
            match stmt {
                converter::Statement::Slide {
                    body,
                    identifier,
                    title,
                } => {
                    let title = self.eval_string(title, scope)?;

                    let identifier = identifier.unwrap_or_else(|| match ctr.entry(title.clone()) {
                        Entry::Vacant(v) => {
                            v.insert(1);

                            title.clone().to_case(Case::Kebab)
                        }
                        Entry::Occupied(mut o) => {
                            *o.get_mut() += 1;

                            format!("{} {}", title, o.get()).to_case(Case::Kebab)
                        }
                    });

                    slides.push(Slide {
                        title,
                        identifier,
                        theme: None,
                        body: self.eval_body(body, scope)?,
                    });
                }
                Statement::Let { name, expr: value , ..} => {
                    println!("{:?} {:?} {:?}", name, value, scope);

                    self.eval_let(name, value, scope)?
                }
                Statement::Title(t) => {
                    let title = self.eval_string(t, scope)?;
                    self.set_title(title)
                }
            }
        }

        Ok(slides)
    }

    pub fn eval_body(
        &mut self,
        body: Vec<converter::SlideStmt>,
        scope: &mut Scope,
    ) -> Result<Vec<SlideStmt>> {
        let mut res = Vec::new();

        for i in body {
            if let Some(i) = self.eval_slide_stmt(i, scope)? {
                res.push(i);
            }
        }

        Ok(res)
    }

    pub fn eval_slide_stmt(
        &mut self,
        stmt: converter::SlideStmt,
        scope: &mut Scope,
    ) -> Result<Option<SlideStmt>> {
        Ok(Some(match stmt {
            converter::SlideStmt::String(s) => SlideStmt::String(self.eval_string(s, scope)?),
            converter::SlideStmt::Block(b) => SlideStmt::Block(self.eval_body(b, scope)?),
            converter::SlideStmt::Column(b) => SlideStmt::Column(self.eval_body(b, scope)?),
            converter::SlideStmt::ListItem(l) => SlideStmt::ListItem(Box::new(
                self.eval_slide_stmt(*l, scope)?
                    .expect("slide stmt in list *never* evaluates to None"),
            )),
            converter::SlideStmt::EnumItem(atom, stmt) => SlideStmt::EnumItem(
                assert_number(self.eval_expr(*atom, scope)?)?,
                Box::new(
                    self.eval_slide_stmt(*stmt, scope)?
                        .expect("slide stmt in enum *never* evaluates to None"),
                ),
            ),
            converter::SlideStmt::Marked(m, s) => SlideStmt::Marked(
                m,
                Box::new(
                    self.eval_slide_stmt(*s.clone(), scope)?
                        .ok_or(EvalError::MarkedInvalidStatment(*s))?,
                ),
            ),
            converter::SlideStmt::Insert(i) => SlideStmt::Insert(i),
            converter::SlideStmt::Let(name, _, value) => {
                self.eval_let(name, value, scope)?;
                return Ok(None);
            }
        }))
    }
}

pub fn assert_number(v: Value) -> Result<i64> {
    if let Value::Number(v) = v {
        Ok(v)
    } else {
        Err(EvalError::NotANumberInEnum(v))
    }
}
