use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::converter::Program;
use thiserror::Error;

pub enum Type {
    Int,

    Template,
    ListItem,
    EnumItem,

}

struct InnerObject {
    ty: Type,
    parents: Vec<Object>,
    scope: HashMap<String, Object>,
}

pub struct Object(Rc<RefCell<Object>>);

pub enum TypeTerm {
    Constructor {
        name: Type,
        generics: Vec<TypeTerm>,
    },
    Variable(usize)
}

pub enum Constraint {
    Equal(TypeTerm, TypeTerm),
}

pub struct Solver {
    constraints: Vec<Constraint>
}

pub struct ConstraintContext {
    scope: HashMap<String, Object>,
    solver: Solver,
}

#[derive(Error, Debug)]
pub enum TypeError {

}

pub fn typecheck(ast: &Program) -> Result<(), TypeError> {


    Ok(())
}

