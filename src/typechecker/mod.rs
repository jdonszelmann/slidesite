use crate::converter::Program;
use thiserror::Error;
// use generate_constraints::ConstraintContext;
// use crate::typechecker::solve::Solver;

mod find_types;
mod generate_constraints;
mod solve;


#[derive(Clone, Debug)]
pub enum Type {
    Int,
    String,
    Tuple(Vec<TypeTerm>),
    Struct{
        name: String,
        type_params: Vec<TypeTerm>,
    },
    Function {
        generics: Vec<Type>,
        arguments: Vec<Type>,
        return_type: Box<Type>,
    },

    Template,
    ListItem,
    EnumItem,
}

#[derive(Copy, Clone, Debug)]
pub struct TypeVar(usize);

#[derive(Clone, Debug)]
pub enum TypeTerm {
    Constructor { name: Type, generics: Vec<TypeTerm> },
    Variable(TypeVar),
}

impl From<Type> for TypeTerm {
    fn from(t: Type) -> Self {
        Self::Constructor {name: t, generics: vec![]}
    }
}

impl From<TypeVar> for TypeTerm {
    fn from(v: TypeVar) -> Self {
        Self::Variable(v)
    }
}

#[derive(Debug)]
pub enum Constraint {
    Equal(TypeTerm, TypeTerm),
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
        struct_name: String,
        field: String
    },

    #[error("use of variable {0} before definition")]
    NotYetDefined(String),
}

pub type Result<T> = std::result::Result<T, TypeError>;

pub fn typecheck(ast: &Program) -> Result<()> {
    // let types = find_types::find_types(ast)?;
    //
    // let mut constraints = ConstraintContext::new(types);
    // constraints.generate_constraints(ast)?;
    //
    // Solver::new(constraints).solve()?;

    Ok(())
}
