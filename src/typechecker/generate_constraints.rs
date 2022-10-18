use std::cell::Cell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::converter::{Program, Statement, Expression, Field, Function, SlideString, StringCharacter};
use crate::{typechecker, TypeError};
use crate::typechecker::{Constraint, Type, TypeTerm, TypeVar};
use crate::typechecker::find_types::{TypeInfo, TypeScope};
use crate::typechecker::Result;

// pub struct ConstraintContext<'src> {
//     pub constraints: Vec<Constraint>,
//     scope: TypeScope<'src>,
//     var_ctr: Cell<usize>,
// }
//
// pub struct Scope<'src> {
//     type_vars: HashMap<&'src str, TypeVar>,
// }
//
// impl<'src> Scope<'src> {
//     pub fn new() -> Self {
//         Self {
//             type_vars: Default::default()
//         }
//     }
//
//     pub fn lookup(&self, name: &str) -> Result<TypeVar> {
//         self.type_vars.get(name)
//             .ok_or_else(|| TypeError::NotYetDefined(name.to_string()))
//             .copied()
//     }
//
//     pub fn define(&mut self, name: &'src str, var: TypeVar) {
//         self.type_vars.insert(name, var);
//     }
//
//     pub fn child(&self) -> Self {
//         // TODO: maybe we can be more efficient than cloning
//         Self {
//             type_vars: self.type_vars.clone()
//         }
//     }
// }
//
// impl<'src> ConstraintContext<'src> {
//     pub fn new(scope: TypeScope<'src>) -> Self {
//         Self {
//             constraints: vec![],
//             scope,
//             var_ctr: Cell::new(0),
//         }
//     }
//
//     fn new_type_var(&self) -> TypeVar {
//         let val = self.var_ctr.get();
//         self.var_ctr.set(val + 1);
//         TypeVar(val)
//     }
//
//     fn convert_typename(&self, name: &str) -> Result<Rc<TypeInfo>> {
//         self.scope.find(name)
//     }
//
//     fn add_constraint(&mut self, c: Constraint) {
//         self.constraints.push(c)
//     }
//
//     fn create_equal(&self, a: impl Into<TypeTerm>, b: impl Into<TypeTerm>) -> Constraint {
//         Constraint::Equal(a.into(), b.into())
//     }
//
//     fn equal(&mut self, a: impl Into<TypeTerm>, b: impl Into<TypeTerm>) {
//         self.add_constraint(self.create_equal(a, b))
//     }
//
//     pub fn generate_constraints(&mut self, Program { statements, .. }: &'src Program) -> typechecker::Result<()> {
//         let mut scope = Scope::new();
//         // TODO: also check type defs (in program) since struct default expressions also generate constraints.
//
//         for stmt in statements {
//             self.statement(stmt, &mut scope)?;
//         }
//
//         Ok(())
//     }
//
//     pub fn statement(&mut self, stmt: &'src Statement, scope: &mut Scope<'src>) -> typechecker::Result<()> {
//         match stmt {
//             Statement::Slide { .. } => {}
//             Statement::Let { name, expr } => {
//                 let var = self.new_type_var();
//                 scope.define(name, var);
//                 let ty = self.expression(expr, scope)?;
//
//                 self.equal(var, ty)
//             }
//             Statement::Title(s) => {
//                 self.string(s, scope)?;
//             }
//         }
//
//         Ok(())
//     }
//
//     pub fn string(&mut self, s: &'src SlideString, scope: &mut Scope<'src>) -> Result<()> {
//         for i in &s.0 {
//             if let StringCharacter::Expr(e) = i {
//                 self.expression(e, scope)?;
//             }
//         }
//
//         Ok(())
//     }
//
//     pub fn expression(&mut self, stmt: &'src Expression, scope: &mut Scope<'src>) -> Result<TypeTerm> {
//         Ok(match stmt {
//             Expression::Identifier(name) => {
//                 let ty = scope.lookup(name)?;
//
//                 ty.into()
//             },
//             Expression::Number(_) => Type::Int.into(),
//             Expression::String(s) => {
//                 self.string(s.as_ref(), scope)?;
//                 Type::String.into()
//             },
//             Expression::SlideBody(_) => todo!(),
//             Expression::Tuple(t) => {
//                 Type::Tuple(
//                     t.into_iter()
//                         .map(|e| {
//                             let ty = self.expression(e, scope)?;
//                             let var = self.new_type_var();
//                             self.equal(ty, var);
//
//                             Ok(var.into())
//                         })
//                         .collect::<Result<_>>()?
//                 ).into()
//             }
//             Expression::StructInstance { name: struct_name, assignments } => {
//                 let types: HashMap<_, _> = assignments
//                     .into_iter()
//                     .map(|(name, expr)| Ok((name, self.expression(expr, scope)?)))
//                     .collect::<Result<_>>()?;
//
//
//                 let ty = self.convert_typename(struct_name)?;
//                 if let TypeInfo::Struct { traits: _, fields, type_definition, .. } = ty.as_ref() {
//                     let mut constraints = Vec::new();
//                     let result = type_definition.clone();
//
//                     for Field{ name: field_name, field_type, default } in *fields {
//                         if let Some(assigned_type) = types.get(field_name) {
//                             let expected_type = self.convert_typename(field_type)?;
//
//                             constraints.push(self.create_equal(expected_type.type_definition().clone(), assigned_type.clone()));
//                         } else if default.is_some() {
//                             // the default expression can be assigned and all is good
//                         } else {
//                             println!("{:?}", assignments);
//                             return Err(TypeError::StructFieldNotAssigned {
//                                 struct_name: struct_name.to_string(),
//                                 field: field_name.to_string()
//                             })
//                         }
//                     }
//
//                     for c in constraints {
//                         self.add_constraint(c);
//                     }
//
//                     result.into()
//                 } else {
//                     unimplemented!()
//                 }
//             },
//             Expression::Function(Function{ signature, body, .. }) => {
//                 let arguments = signature.params.iter()
//                     .map(|(name, ty)| Ok((name, self.convert_typename(ty)?)))
//                     .collect::<Result<Vec<_>>>()?;
//
//                 let mut argument_types = Vec::new();
//                 let mut constraints = Vec::new();
//
//                 let mut body_scope = scope.child();
//                 for (name, info) in arguments {
//                     let tv = self.new_type_var();
//                     constraints.push(self.create_equal(tv, info.type_definition().clone()));
//
//                     body_scope.define(name, tv);
//
//                     argument_types.push(info.type_definition().clone())
//                 }
//
//                 for c in constraints {
//                     self.add_constraint(c);
//                 }
//
//                 for stmt in &body.stmts {
//                     self.statement(stmt, &mut body_scope)?;
//                 }
//
//                 let ret_ty = body.ret_expr
//                     .as_ref()
//                     .map(|e| self.expression(e, &mut body_scope))
//                     .unwrap_or_else(|| Ok(Type::Tuple(vec![]).into()))?;
//
//                 let expected_ret_ty = signature.ret
//                     .as_ref()
//                     .map(|t| Ok(self.convert_typename(t)?.type_definition().clone()))
//                     .transpose()?
//                     .unwrap_or_else(|| Type::Tuple(vec![]).into());
//
//                 self.equal(ret_ty, expected_ret_ty);
//
//                 Type::Function {
//                     generics: vec![],
//                     arguments: argument_types,
//                     return_type: Box::new(Type::Int)
//                 }.into()
//             },
//             Expression::Sub(_, _) => todo!(),
//             Expression::Mul(_, _) => todo!(),
//             Expression::Div(_, _) => todo!(),
//             Expression::Add(_, _) => todo!(),
//             Expression::Call(_, _) => todo!(),
//         })
//     }
// }