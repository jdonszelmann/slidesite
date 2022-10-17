use std::borrow::Cow;
use std::cell::Cell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::converter::{Field, Program, TypeDef};
use crate::parser::TypeName;
use crate::typechecker::{Type, Result};
use crate::TypeError;

#[derive(Clone)]
pub enum TypeInfo<'src> {
    Struct {
        fields: &'src [Field],
        type_definition: Type,
        generics: Vec<String>,
    },
    Int,
    String,
    Tuple {
        nested_infos: Vec<TypeInfo<'src>>,
    },
}

impl<'src> TypeInfo<'src> {
    pub fn type_definition(&self) -> Result<Cow<Type>> {
        Ok(match self {
            TypeInfo::Struct { type_definition, .. } => Cow::Borrowed(type_definition),
            TypeInfo::Int => Cow::Owned(Type::Int),
            TypeInfo::String => Cow::Owned(Type::String),
            TypeInfo::Tuple { nested_infos } => {
                Cow::Owned(Type::Tuple(
                    nested_infos
                        .iter()
                        .map(|i| {
                            Ok(i.type_definition()?.into_owned().into())
                        })
                        .collect::<Result<_>>()?
                ))
            }
        })
    }
}

pub struct TypeScope<'src> {
    bindings: HashMap<String, TypeInfo<'src>>,
}

impl<'src> TypeScope<'src> {
    fn declare(&mut self, name: &str, ty: TypeInfo<'src>) -> Result<()> {
        if self.bindings.insert(name.to_string(), ty).is_some() {
            Err(TypeError::DuplicateDefinition(name.to_string()))
        } else {
            Ok(())
        }
    }

    pub fn find(&self, name: &str, args: Vec<TypeInfo>) -> Result<&TypeInfo> {
        self.bindings.get(name)
            .ok_or_else(|| TypeError::TypeDefinitionNotFound(name.to_string()))
    }

    pub fn new() -> Self {
        Self {
            bindings: Default::default(),
        }
    }
}

fn convert_typedef<'src>(def: &'src TypeDef, _scope: &mut TypeScope<'src>) -> (&'src str, TypeInfo<'src>) {
    match def {
        TypeDef::Enum { .. } => todo!(),
        TypeDef::Struct { name, fields, generics } => (
            name,
            TypeInfo::Struct {
                fields,
                type_definition: Type::Struct {
                    name: name.to_string(),
                    type_params: vec![],
                },
                generics: generics.clone()
            }
        ),
        TypeDef::Trait { .. } => todo!(),
        TypeDef::Impl { .. } => todo!(),
    }
}

pub fn find_types(program: &Program) -> Result<TypeScope> {
    let mut scope = TypeScope::new();
    find_types_program(program, &mut scope)?;

    Ok(scope)
}

pub fn find_types_program<'src>(Program { types, .. }: &'src Program, scope: &mut TypeScope<'src>) -> Result<()> {
    for i in types {
        let (name, ty) = convert_typedef(i, scope);
        scope.declare(name, ty)?;
    }

    Ok(())
}
