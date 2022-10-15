use std::collections::HashMap;
use convert_case::{Case, Casing};
use crate::converter::{Atom, Expression, Program, SlideString, StringCharacter};
use crate::eval::value::{SlideShow, SlideStmt, Theme, Value, Slide, Template};
use thiserror::Error;
use crate::converter;

pub mod value;

#[derive(Debug, Error)]
pub enum EvalError {
    #[error("Expected a number for the index of an enum item, but found {0}")]
    NotANumberInEnum(Value)
}

type Result<T> = std::result::Result<T, EvalError>;

pub fn eval_ast(ast: Program) -> Result<SlideShow> {
    let mut e = Evaluator::new();

    Ok(SlideShow {
        title: e.eval_string(ast.title)?,
        themes: ast.themes.into_iter().map(|t| e.eval_theme(t)).collect::<Result<_>>()?,
        slides: e.eval_slides(ast.slides)?,
        templates: ast.templates.into_iter().map(|t| e.eval_template(t)).collect::<Result<_>>()?,
    })
}

struct Evaluator {}

impl Evaluator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn eval_theme(&mut self, c: converter::Theme) -> Result<Theme> {
        Ok(Theme {
            name: self.eval_string(c.name)?,
            body: vec![],
        })
    }

    pub fn eval_template(&mut self, c: converter::Template) -> Result<Template> {
        Ok(Template {
            name: self.eval_string(c.name)?,
            body: vec![],
        })
    }

    pub fn eval_string_char(&mut self, c: StringCharacter) -> Result<String> {
        Ok(match c {
            StringCharacter::Char(c) => c.to_string(),
            StringCharacter::Expr(e) => self.eval_expr(e)?.to_string()
        })
    }

    pub fn eval_string(&mut self, string: SlideString) -> Result<String> {
        string.0.into_iter()
            .map(|c| self.eval_string_char(c))
            .collect::<Result<_>>()
    }

    pub fn eval_expr(&mut self, c: Expression) -> Result<Value> {
        todo!()
    }

    fn eval_slides(&mut self, slides: Vec<converter::Slide>) -> Result<Vec<Slide>> {
        let mut ctr = HashMap::new();
        let mut pre_ctr = HashMap::new();
        let mut res = Vec::new();
        let mut titles = Vec::new();

        for i in &slides {
            let title = self.eval_string(i.title.clone())?;
            titles.push(title.clone());
            *pre_ctr.entry(title).or_insert(0) += 1;
        }

        for (slide, title) in slides.into_iter().zip(titles) {
            let identifier = if pre_ctr.get(&title).expect("must be inserted") > &1 {
                let mut count = ctr.entry(title.clone()).or_insert(0);
                *count += 1;


                format!("{} {}", title, count).to_case(Case::Kebab)
            } else {
                title.clone().to_case(Case::Kebab)
            };

            res.push(Slide {
                title,
                identifier,
                body: self.eval_body(slide.body)?,
            });
        }

        Ok(res)
    }

    pub fn eval_body(&mut self, body: Vec<converter::SlideStmt>) -> Result<Vec<SlideStmt>> {
        let mut res = Vec::new();

        for i in body {
            res.push(self.eval_stmt(i)?);
        }

        Ok(res)
    }

    pub fn eval_atom(&mut self, i: converter::Atom) -> Result<Value> {
        Ok(match i {
            Atom::Identifier(_) => todo!(),
            Atom::Number(n) => Value::Number(n),
            Atom::String(s) => Value::String(self.eval_string(*s)?)
        })
    }

    pub fn eval_stmt(&mut self, stmt: converter::SlideStmt) -> Result<SlideStmt> {
        Ok(match stmt {
            converter::SlideStmt::String(s) => SlideStmt::String(self.eval_string(s)?),
            converter::SlideStmt::Block(b) => SlideStmt::Block(self.eval_body(b)?),
            converter::SlideStmt::Column(b) => SlideStmt::Column(self.eval_body(b)?),
            converter::SlideStmt::ListItem(l) => SlideStmt::ListItem(Box::new(self.eval_stmt(*l)?)),
            converter::SlideStmt::EnumItem(atom, stmt) => SlideStmt::EnumItem(assert_number(self.eval_atom(atom)?)?, Box::new(self.eval_stmt(*stmt)?)),
            converter::SlideStmt::Marked(m, s) => SlideStmt::Marked(m.clone(), Box::new(self.eval_stmt(*s)?)),
            converter::SlideStmt::Insert(i) => SlideStmt::Insert(i.clone()),
        })
    }
}

pub fn assert_number(v: Value) -> Result<i64> {
    if let Value::Number(v) = v {
        Ok(v)
    } else {
        Err(EvalError::NotANumberInEnum(v))
    }
}
