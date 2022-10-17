use std::borrow::Cow;
use std::collections::HashMap;
use crate::converter::{Field, ImplItem, Program, TypeDef, TypeName, FunctionStub};
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

struct TraitInfo<'src> {
    fields: HashMap<String, ImplItem>,
    stubs: &'src [FunctionStub],
    generics: &'src [String],
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

pub enum ImplementationRequirement {

}

pub struct ImplementationInfo<'src> {
    creates_ty_vars: Vec<String>,
    requirements: Vec<ImplementationRequirement>,
    members: &'src [ImplItem],
    trait_name: Option<&'src TypeName>,
}

pub struct TypeScope<'src> {
    types: HashMap<String, TypeInfo<'src>>,
    traits: HashMap<String, TraitInfo<'src>>,
    impls: HashMap<&'src TypeName, Vec<ImplementationInfo<'src>>>,
}

impl<'src> TypeScope<'src> {
    fn declare(&mut self, name: &str, ty: TypeInfo<'src>) -> Result<()> {
        if self.types.insert(name.to_string(), ty).is_some() {
            Err(TypeError::DuplicateDefinition(name.to_string()))
        } else {
            Ok(())
        }
    }

    fn declare_trait(&mut self, name: &str, ty: TraitInfo<'src>) -> Result<()> {
        if self.traits.insert(name.to_string(), ty).is_some() {
            Err(TypeError::DuplicateDefinition(name.to_string()))
        } else {
            Ok(())
        }
    }

    fn add_impl(&mut self, name: &'src TypeName, imp: ImplementationInfo<'src>) -> Result<()> {
        // TODO: check for overlapping impls here?
        self.impls.entry(name).or_insert_with(Vec::new).push(imp);

        Ok(())
    }

    pub fn find(&self, name: &str, _args: Vec<TypeInfo>) -> Result<&TypeInfo> {
        // TODO: handle args?
        self.types.get(name)
            .ok_or_else(|| TypeError::TypeDefinitionNotFound(name.to_string()))
    }

    pub fn new() -> Self {
        Self {
            types: Default::default(),
            traits: Default::default(),
            impls: Default::default(),
        }
    }
}

fn convert_typedef<'src>(def: &'src TypeDef, scope: &mut TypeScope<'src>) -> Result<()> {
    match def {
        TypeDef::Enum { .. } => todo!(),
        TypeDef::Struct { name, fields, generics } => scope.declare(
            name,
            TypeInfo::Struct {
                fields,
                type_definition: Type::Struct {
                    name: name.to_string(),
                    type_params: vec![],
                },
                generics: generics.clone()
            }
        )?,
        TypeDef::Trait { name, items, generics, stubs } => {
            scope.declare_trait(name, TraitInfo {
                generics,
                fields: items.iter().map(|i| (i.name().to_string(), i.clone())).collect(),
                stubs,
            })?;
        },
        TypeDef::Impl { trait_name, name, instantiated_generics, body } => {
            scope.add_impl(name, ImplementationInfo {
                creates_ty_vars: instantiated_generics.clone(),
                requirements: vec![],
                members: body,
                trait_name: trait_name.as_ref()
            })?;
        },
    }

    Ok(())
}

pub fn find_types(program: &Program) -> Result<TypeScope> {
    let mut scope = TypeScope::new();
    find_types_program(program, &mut scope)?;

    Ok(scope)
}

pub fn find_types_program<'src>(Program { types, .. }: &'src Program, scope: &mut TypeScope<'src>) -> Result<()> {
    for i in types {
        convert_typedef(i, scope)?;
    }

    Ok(())
}
