use std::fmt::{Display, Formatter};
use itertools::Itertools;
use crate::converter::{Program, TypeName};
use thiserror::Error;
use typed_arena::Arena;
use generate_constraints::ConstraintContext;
use crate::typechecker::solve::{ResolvedType, Solver};
use derivative::Derivative;

mod find_types;
mod generate_constraints;
mod solve;


#[derive(Clone, Debug, Eq, Derivative)]
#[derivative(Hash, PartialEq)]
pub enum Type {
    Int,
    String,
    Tuple(Vec<TypeVar>),
    Struct{
        name: String,
        type_params: Vec<TypeVar>,
    },
    Function {
        #[derivative(Hash="ignore")]
        #[derivative(PartialEq="ignore")]
        name: String,
        generics: Vec<TypeVar>,
        arguments: Vec<TypeVar>,
        return_type: TypeVar,
    },
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub struct TypeVar(usize);

#[derive(Clone, Debug)]
pub enum TypeTerm {
    Type(Type),
    Variable(TypeVar),
    FieldAccess(TypeVar, String),
}

impl From<Type> for TypeTerm {
    fn from(t: Type) -> Self {
        Self::Type(t)
    }
}

impl From<TypeVar> for TypeTerm {
    fn from(v: TypeVar) -> Self {
        Self::Variable(v)
    }
}

#[derive(Debug, Clone)]
pub enum Constraint {
    Equal(TypeVar, TypeTerm),
}

#[derive(Error, Debug)]
pub enum TypeError {
    #[error("type {0} defined more than once")]
    DuplicateDefinition(String),

    #[error("no type named {0} in scope")]
    TypeDefinitionNotFound(String),

    #[error("can't instantiate other types than structs")]
    NotAStruct,

    #[error("in the instantiation of {struct_name} you did not assign a value to field {field}")]
    StructFieldNotAssigned {
        struct_name: TypeName,
        field: String
    },

    #[error("use of variable {0} before definition")]
    NotYetDefined(String),

    #[error("only structs or enums can be instantiated with generic type parameters")]
    CantInstantiate,

    #[error("trying to access {0}.{1} but {0} has no such field")]
    NoSuchField(ResolvedType, String)
}

pub type Result<T> = std::result::Result<T, TypeError>;

pub fn typecheck(ast: &Program) -> Result<()> {
    let types = find_types::find_types(ast)?;

    let mut constraints = ConstraintContext::new(types);
    constraints.generate_constraints(ast)?;

    let arena = Arena::new();
    Solver::new(constraints, &arena).solve()?;

    Ok(())
}
