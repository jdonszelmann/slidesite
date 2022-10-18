use std::borrow::BorrowMut;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use crate::converter::{Program, Statement, Expression, Field, Function, SlideString, StringCharacter, TypeName};
use crate::{typechecker, TypeError};
use crate::typechecker::{Constraint, Type, TypeTerm, TypeVar};
use crate::typechecker::find_types::{TypeInfo, TypeScope};
use crate::typechecker::Result;

pub struct ConstraintContext<'src> {
    pub constraints: RefCell<Vec<Constraint>>,
    pub scope: TypeScope<'src>,
    var_ctr: Cell<usize>,
}

pub struct Scope<'src> {
    type_vars: HashMap<&'src str, TypeVar>,
}

impl<'src> Scope<'src> {
    pub fn new() -> Self {
        Self {
            type_vars: Default::default()
        }
    }

    pub fn lookup(&self, name: &str) -> Result<TypeVar> {
        self.type_vars.get(name)
            .ok_or_else(|| TypeError::NotYetDefined(name.to_string()))
            .copied()
    }

    pub fn define(&mut self, name: &'src str, var: TypeVar) {
        self.type_vars.insert(name, var);
    }

    pub fn child(&self) -> Self {
        // TODO: maybe we can be more efficient than cloning
        Self {
            type_vars: self.type_vars.clone()
        }
    }
}

impl<'src> ConstraintContext<'src> {
    pub fn new(scope: TypeScope<'src>) -> Self {
        Self {
            constraints: Default::default(),
            scope,
            var_ctr: Cell::new(0),
        }
    }

    fn new_type_var(&self) -> TypeVar {
        let val = self.var_ctr.get();
        self.var_ctr.set(val + 1);
        TypeVar(val)
    }

    fn new_type_var_for(&self, var: impl Into<TypeTerm>) -> TypeVar {
        match var.into() {
            TypeTerm::Variable(v) => v,
            o => {
                let var = self.new_type_var();
                self.equal(var, o);

                var
            }
        }
    }

    pub fn type_definition(&self, info: TypeInfo) -> Result<Type> {
        Ok(match info {
            TypeInfo::Struct { type_definition, .. } => type_definition,
            TypeInfo::Int => Type::Int,
            TypeInfo::String => Type::String,
            TypeInfo::Tuple { nested_infos } => {
                Type::Tuple(
                    nested_infos
                        .iter()
                        .map(|i| {
                            let ty = self.type_definition(i.clone())?;
                            let var = self.new_type_var();
                            self.equal(var, ty);

                            Ok(var)
                        })
                        .collect::<Result<_>>()?
                )
            }
        })
    }

    fn convert_typename(&self, name: &TypeName) -> Result<TypeInfo> {
        Ok(match name {
            TypeName::Constructor(name, args) => {
                let args: Vec<_> = args.into_iter()
                    .map(|i| self.convert_typename(i).map(|i| i))
                    .collect::<Result<_>>()?;
                self.scope.find(name, args)?.clone()
            }
            TypeName::Tuple(t) => TypeInfo::Tuple {
                nested_infos: t.into_iter()
                    .map(|i| {
                        self.convert_typename(i)
                    })
                    .collect::<Result<_>>()?,
            },
            TypeName::Int => TypeInfo::Int,
            TypeName::String => TypeInfo::String,
        })
    }

    fn add_constraint(&self, c: Constraint) {
        self.constraints.borrow_mut().push(c)
    }

    fn create_equal(&self, a: TypeVar, b: impl Into<TypeTerm>) -> Constraint {
        Constraint::Equal(a, b.into())
    }

    fn equal(&self, a: TypeVar, b: impl Into<TypeTerm>) {
        self.add_constraint(self.create_equal(a, b))
    }

    pub fn generate_constraints(&mut self, Program { statements, .. }: &'src Program) -> typechecker::Result<()> {
        let mut scope = Scope::new();
        // TODO: also check type defs (in program) since struct default expressions also generate constraints.

        for stmt in statements {
            self.statement(stmt, &mut scope)?;
        }

        Ok(())
    }

    pub fn statement(&mut self, stmt: &'src Statement, scope: &mut Scope<'src>) -> typechecker::Result<()> {
        match stmt {
            Statement::Slide { .. } => {}
            Statement::Let { name, ty: expected_ty, expr } => {
                let var = self.new_type_var();
                scope.define(name, var);
                let ty = self.expression(expr, scope)?;

                self.equal(var, ty);

                if let Some(i) = expected_ty {
                    let ty = self.convert_typename(i)?;
                    let ty = self.type_definition(ty.clone())?;
                    self.equal(var, ty);
                }
            }
            Statement::Title(s) => {
                self.string(s, scope)?;
            }
        }

        Ok(())
    }

    pub fn string(&mut self, s: &'src SlideString, scope: &mut Scope<'src>) -> Result<()> {
        for i in &s.0 {
            if let StringCharacter::Expr(e) = i {
                self.expression(e, scope)?;
            }
        }

        Ok(())
    }

    pub fn expression(&mut self, stmt: &'src Expression, scope: &mut Scope<'src>) -> Result<TypeTerm> {
        Ok(match stmt {
            Expression::Identifier(name) => {
                let ty = scope.lookup(name)?;

                ty.into()
            }
            Expression::Number(_) => Type::Int.into(),
            Expression::String(s) => {
                self.string(s.as_ref(), scope)?;
                Type::String.into()
            }
            Expression::SlideBody(_) => todo!(),
            Expression::Tuple(t) => {
                Type::Tuple(
                    t.into_iter()
                        .map(|e| {
                            let ty = self.expression(e, scope)?;
                            let var = self.new_type_var();
                            self.equal(var, ty);

                            Ok(var.into())
                        })
                        .collect::<Result<_>>()?
                ).into()
            }
            Expression::StructInstance { name: struct_name, assignments } => {
                let types: HashMap<_, _> = assignments
                    .into_iter()
                    .map(|(name, expr)| Ok((name, self.expression(expr, scope)?)))
                    .collect::<Result<_>>()?;


                let ty = self.convert_typename(struct_name)?;
                if let TypeInfo::Struct { fields, type_definition, .. } = &ty {
                    let mut constraints = Vec::new();
                    let result = type_definition.clone();

                    for Field { name: field_name, field_type, default } in *fields {
                        if let Some(assigned_type) = types.get(field_name) {
                            let expected_type = self.convert_typename(field_type)?;

                            let var = self.new_type_var_for(assigned_type.clone());

                            constraints.push(self.create_equal(
                                var,
                                self.type_definition(expected_type.clone())?.clone(),
                            ));
                        } else if default.is_some() {
                            // the default expression can be assigned and all is good
                        } else {
                            println!("{:?}", assignments);
                            return Err(TypeError::StructFieldNotAssigned {
                                struct_name: struct_name.clone(),
                                field: field_name.to_string(),
                            });
                        }
                    }

                    for c in constraints {
                        self.add_constraint(c);
                    }

                    result.into()
                } else {
                    unimplemented!()
                }
            }
            Expression::Function(Function { signature, body, name }) => {
                let arguments = signature.params.iter()
                    .map(|(name, ty)| Ok((name, self.convert_typename(ty)?)))
                    .collect::<Result<Vec<_>>>()?;

                let mut argument_types = Vec::new();
                let mut constraints = Vec::new();

                let mut body_scope = scope.child();
                for (name, info) in arguments {
                    let tv = self.new_type_var();
                    constraints.push(self.create_equal(tv, self.type_definition(info.clone())?.clone()));

                    body_scope.define(name, tv);

                    let ty = self.type_definition(info.clone())?;
                    argument_types.push(self.new_type_var_for(ty.clone()));
                }

                for c in constraints {
                    self.add_constraint(c);
                }

                for stmt in &body.stmts {
                    self.statement(stmt, &mut body_scope)?;
                }

                let ret_ty = body.ret_expr
                    .as_ref()
                    .map(|e| self.expression(e, &mut body_scope))
                    .unwrap_or_else(|| Ok(Type::Tuple(vec![]).into()))?;

                let expected_ret_ty = signature.ret
                    .as_ref()
                    .map(|t| Ok(self.type_definition(self.convert_typename(t)?.clone())?.clone()))
                    .transpose()?
                    .unwrap_or_else(|| Type::Tuple(vec![]).into());

                let var = self.new_type_var_for(ret_ty);
                self.equal(var, expected_ret_ty);

                Type::Function {
                    name: name.clone().unwrap_or("<anonymous function>".to_string()),
                    generics: vec![],
                    arguments: argument_types,
                    return_type: self.new_type_var_for(Type::Int),
                }.into()
            }
            Expression::Sub(_, _) => todo!(),
            Expression::Mul(_, _) => todo!(),
            Expression::Div(_, _) => todo!(),
            Expression::Add(_, _) => todo!(),
            Expression::Call(f, args) => {
                let func_var = self.expression(f, scope)?;

                let arg_vars = args.into_iter()
                    .map(|i| {
                        let ty = self.expression(i, scope)?;
                        Ok(self.new_type_var_for(ty))
                    })
                    .collect::<Result<Vec<_>>>()?;
                let ret_var = self.new_type_var();

                let func_type = Type::Function {
                    name: "<unknown>".to_string(),
                    generics: vec![],
                    arguments: arg_vars,
                    return_type: ret_var
                };

                let var = self.new_type_var_for(func_var);
                self.equal(var, func_type);

                ret_var.into()
            },
            Expression::Index(_, _) => todo!(),
            Expression::Attr(e, v) => {
                let et = self.expression(e, scope)?;
                let var = self.new_type_var_for(et);

                TypeTerm::FieldAccess(var, v.to_string())
            },
            Expression::TupleProject(_, _) => todo!(),
        })
    }
}