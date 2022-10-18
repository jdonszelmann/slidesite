use crate::typechecker::Constraint;
// use crate::typechecker::generate_constraints::ConstraintContext;
use crate::typechecker::Result;
//
// pub struct Solver {
//     constraints: Vec<Constraint>,
// }
//
// impl Solver {
//     pub fn new(constraints: ConstraintContext) -> Self {
//         Self {
//             constraints: constraints.constraints
//         }
//     }
//
//     pub fn solve(&self) -> Result<()> {
//         for i in &self.constraints {
//             println!("{:?}", i);
//         }
//         todo!()
//     }
// }