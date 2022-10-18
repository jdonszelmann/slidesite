use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Deref, DerefMut};
use itertools::Itertools;
use typed_arena::Arena;
use crate::parser::Field;
use crate::typechecker::{Constraint, Type, TypeTerm, TypeVar};
use crate::typechecker::find_types::TypeScope;
use crate::typechecker::generate_constraints::ConstraintContext;
use crate::typechecker::Result;
use crate::TypeError;

#[derive(Debug, Clone)]
pub enum ResolvedType {
    Int,
    String,
    Tuple(Vec<ResolvedType>),
    Struct{
        name: String,
        type_params: Vec<ResolvedType>,
    },
    Function {
        name: String,
        generics: Vec<ResolvedType>,
        arguments: Vec<ResolvedType>,
        return_type: Box<ResolvedType>,
    }
}

impl Display for ResolvedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedType::Int => write!(f, "int"),
            ResolvedType::String => write!(f, "string"),
            ResolvedType::Tuple(t) if t.len() != 1 => write!(f, "({})", t.into_iter().map(ToString::to_string).join(",")),
            ResolvedType::Tuple(t) => write!(f, "(<unknown>,)"),
            ResolvedType::Struct { name, type_params } if type_params.len() > 0 => {
                write!(f, "{name}<{}>", type_params.iter().map(ToString::to_string).join(","))
            }
            ResolvedType::Struct { name, .. } => {
                write!(f, "{name} {{}}")
            }
            ResolvedType::Function { name, generics, arguments, return_type } => {
                write!(f, "{name}({}) -> {return_type}", arguments.into_iter().map(ToString::to_string).join(","))
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TypeRef {
    Variable(TypeVar),
    Type(Type)
}

pub struct TypeNode<'solve> {
    parent: Cell<Option<&'solve TypeNode<'solve>>>,
    ty: TypeRef,
}

impl Debug for TypeNode<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(i) = self.parent.get() {
            write!(f, "{:?}-->{:?}", self.ty, i)
        } else {
            write!(f, "{:?}", self.ty)
        }
    }
}

pub struct Solver<'src, 'solve> {
    constraints: Vec<Constraint>,
    scope: TypeScope<'src>,

    arena: &'solve Arena<TypeNode<'solve>>,
    nodes: RefCell<HashMap<TypeRef, &'solve TypeNode<'solve>>>,
    pending_field_accesses: RefCell<HashMap<TypeVar, Vec<(String, TypeVar)>>>,
}

impl<'src, 'solve> Solver<'src, 'solve> {
    pub fn new(constraints: ConstraintContext<'src>, arena: &'solve Arena<TypeNode<'solve>>) -> Self {
        Self {
            constraints: constraints.constraints.into_inner(),
            // TODO: we now assume there is a single type scope. With modules this may not be true!
            scope: constraints.scope,
            arena,
            nodes: Default::default(),
            pending_field_accesses: Default::default()
        }
    }

    pub fn new_type_node(&self, ty: TypeRef) -> &'solve TypeNode {
        self.arena.alloc(TypeNode {
            parent: Cell::new(None),
            ty,
        })
    }

    fn find_root<'a>(&'a self, ty: &'solve TypeNode<'solve>) -> &'solve TypeNode<'solve> where 'a: 'solve {
        if let Some(i) = ty.parent.get() {
            let parent = self.find_root(i);
            ty.parent.set(Some(parent));
            parent
        } else{
            ty
        }
    }

    fn find<'a>(&'a self, ty: TypeRef) -> &'solve TypeNode where 'a: 'solve {
        let leaf = *self.nodes.borrow_mut().entry(ty.clone())
            .or_insert_with(|| self.new_type_node(ty));

        self.find_root(leaf)
    }

    fn resolve_field_access(&self, ty: ResolvedType, field: String) -> Result<Type> {
        self.scope.access_field(ty, field)
    }

    fn resolve<'a>(&'a self, var: TypeVar) -> Option<ResolvedType> where 'a: 'solve {
        if let TypeRef::Type(ref t) = self.find(TypeRef::Variable(var)).ty {
            Some(match t {
                Type::Int => ResolvedType::Int,
                Type::String => ResolvedType::String,
                Type::Tuple(v) => ResolvedType::Tuple(v.iter().map(|i| self.resolve(*i)).collect::<Option<_>>()?),
                Type::Struct { name, type_params } => {
                    ResolvedType::Struct {
                        name: name.clone(),
                        type_params: type_params.iter().map(|i| self.resolve(*i)).collect::<Option<_>>()?,
                    }
                }
                Type::Function {name,  generics, arguments, return_type } => {
                    ResolvedType::Function {
                        name: "".to_string(),
                        generics: generics.iter().map(|i| self.resolve(*i)).collect::<Option<_>>()?,
                        arguments: arguments.iter().map(|i| self.resolve(*i)).collect::<Option<_>>()?,
                        return_type: Box::new(self.resolve(*return_type)?)
                    }
                }
            })
        } else {
            None
        }
    }

    fn process_equal<'a>(&'a self, var: TypeVar, term: TypeTerm) -> Result<()> where 'a: 'solve {
        let (a, b) = match term {
            TypeTerm::Type(t) => {
                let a = self.find(TypeRef::Variable(var));
                let b = self.find(TypeRef::Type(t));

                (a, b)
            }
            TypeTerm::Variable(v) => {
                let a = self.find(TypeRef::Variable(var));
                let b = self.find(TypeRef::Variable(v));

                (a, b)
            }
            TypeTerm::FieldAccess(e, field) => {
                if let Some(t) = self.resolve(e) {
                    let resolved_var = self.resolve_field_access(t, field)?;

                    let a = self.find(TypeRef::Type(resolved_var));
                    let b = self.find(TypeRef::Variable(var));

                    (a, b)
                } else {
                    println!("DLAY evaluation of {e:?}.{field:?}=={var:?} until {e:?} is known");

                    self.pending_field_accesses
                        .borrow_mut()
                        .entry(e)
                        .or_insert_with(Vec::new)
                        .push((field, var));

                    return Ok(());
                }
            }
        };


        let resolved = match (&a.ty, &b.ty) {
            (TypeRef::Type(x), TypeRef::Type(y)) => {
                todo!();
                None
            }
            (TypeRef::Type(t), TypeRef::Variable(v)) => {
                b.parent.set(Some(a));
                Some(*v)
            }
            (TypeRef::Variable(v), TypeRef::Type(t)) => {
                a.parent.set(Some(b));
                Some(*v)
            }
            (TypeRef::Variable(x), TypeRef::Variable(y)) => {
                b.parent.set(Some(a));
                None
            }
        };

        if let Some(var) = resolved {
            if let Some(ty) = self.resolve(var) {
                println!("RSLV {var:?} to {ty}");
            }
        }

        Ok(())
    }

    pub fn solve<'a>(&'a mut self) -> Result<()> where 'a: 'solve {

        for i in self.constraints.clone() {
            println!("EVAL {:?}", i);

            if let Constraint::Equal(var, term) = i {
                self.process_equal(var, term)?;
            }
        }

        unimplemented!();

        Ok(())
    }
}