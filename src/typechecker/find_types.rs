use std::collections::HashMap;
use std::rc::Rc;
use crate::converter::{Field, Program, TypeDef};
use crate::typechecker::{Type, Result};
use crate::TypeError;

pub enum TypeInfo<'src> {
    Struct {
        traits: Vec<String>,
        fields: &'src [Field],
        type_definition: Type,
    },
}

impl<'src> TypeInfo<'src> {
    pub fn type_definition(&self) -> &Type {
        match self {
            TypeInfo::Struct { type_definition, .. } => type_definition
        }
    }
}

pub struct TypeScope<'src> {
    bindings: HashMap<String, Rc<TypeInfo<'src>>>,
}

impl<'src> TypeScope<'src> {
    fn declare(&mut self, name: &str, ty: TypeInfo<'src>) -> Result<()> {
        if self.bindings.insert(name.to_string(), Rc::new(ty)).is_some() {
            Err(TypeError::DuplicateDefinition(name.to_string()))
        } else {
            Ok(())
        }
    }

    pub fn find(&self, name: &str) -> Result<Rc<TypeInfo>> {
        self.bindings.get(name).ok_or_else(|| TypeError::TypeDefinitionNotFound(name.to_string())).cloned()
    }

    pub fn new() -> Self {
        Self {
            bindings: Default::default(),
        }
    }
}

// fn convert_typedef<'src>(def: &'src TypeDef, _scope: &mut TypeScope<'src>) -> (&'src str, TypeInfo<'src>) {
//     match def {
//         TypeDef::Enum { .. } => todo!(),
//         TypeDef::Struct { name, fields } => (
//             name,
//             TypeInfo::Struct {
//                 traits: vec![],
//                 fields,
//                 type_definition: Type::Struct {
//                     name: name.to_string(),
//                     type_params: vec![],
//                 },
//             }
//         ),
//     }
// }

// pub fn find_types(program: &Program) -> Result<TypeScope> {
//     let mut scope = TypeScope::new();
//     find_types_program(program, &mut scope)?;
//
//     Ok(scope)
// }

// pub fn find_types_program<'src>(Program { types, .. }: &'src Program, scope: &mut TypeScope<'src>) -> Result<()> {
//     for i in types {
//         let (name, ty) = convert_typedef(i, scope);
//         scope.declare(name, ty)?;
//     }
//
//     Ok(())
// }
