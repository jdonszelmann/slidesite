use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::error::Simple;
use chumsky::prelude::{choice, empty, end, filter, just, recursive, take_until, Recursive};
use chumsky::text::TextParser;
use chumsky::{text, Parser};
use std::io::{Cursor, Write};
use std::rc::Rc;

mod ast;
pub use ast::*;

trait Dy<A, B, C> {
    fn dy(self) -> Rc<dyn Parser<A, B, Error = C>>;
}

impl<A: Clone, B, C, T> Dy<A, B, C> for T
where
    T: Parser<A, B, Error = C> + 'static,
{
    fn dy(self) -> Rc<dyn Parser<A, B, Error = C>> {
        Rc::new(self)
    }
}

fn char_to_string(c: char) -> StringCharacter {
    StringCharacter::Char(c)
}

fn parser() -> impl Parser<char, Program, Error = Simple<char>> {
    let comment = just("//").then(take_until(just('\n'))).padded().dy();
    let op = |c| just(c).padded();

    let bare_name = filter(|c: &char| !['"', '\'', '\n', '\r', '{', '}', ';'].contains(c))
        .repeated()
        .map(|i| SlideString::Simple(String::from_iter(i).trim().to_string()))
        .dy();

    let number = text::int(10).dy();
    let identifier = text::ident().dy();

    let identifier_or_number = choice((
        number.clone().map(IdentifierOrNumber::Number),
        identifier.clone().map(IdentifierOrNumber::Identifier),
    ))
    .dy();

    let mut expression = Recursive::<_, _, Simple<char>>::declare();

    let statement_terminator = choice((just(';').to(()), just('\n').to(()), end())).dy();

    let assignment = identifier
        .clone()
        .padded()
        .then_ignore(op('='))
        .then(expression.clone())
        .dy();

    let assignment_comma_seq = assignment
        .clone()
        .padded()
        .padded_by(comment.clone().repeated())
        .separated_by(op(','))
        .allow_trailing()
        .padded()
        .dy();

    let assignment_seq = assignment
        .clone()
        .padded()
        .padded_by(comment.clone().repeated())
        .separated_by(statement_terminator.clone())
        .allow_trailing()
        .padded()
        .dy();

    let let_assignment = text::keyword("let").ignore_then(assignment.clone()).dy();

    let interpolation = expression
        .clone()
        .padded()
        .delimited_by(just('{'), just('}'))
        .dy();

    let escape = choice((
        just("\\\"").to(StringCharacter::Char('\"')),
        just("\\\'").to(StringCharacter::Char('\'')),
        just("\\\\").to(StringCharacter::Char('\\')),
        just("\\{").to(StringCharacter::Char('{')),
        just("\\}").to(StringCharacter::Char('}')),
        interpolation.map(StringCharacter::Expr),
    ))
    .dy();

    let single_quote_string_char =
        choice((escape.clone(), filter(|c| *c != '\'').map(char_to_string))).dy();
    let double_quote_string_char =
        choice((escape, filter(|c| *c != '\"').map(char_to_string))).dy();

    let single_quote_string = just('\'')
        .ignore_then(single_quote_string_char.repeated())
        .then_ignore(just('\''))
        .collect::<Vec<StringCharacter>>()
        .map(SlideString::Complex)
        .dy();

    let double_quote_string = just('"')
        .ignore_then(double_quote_string_char.repeated())
        .then_ignore(just('"'))
        .collect::<Vec<StringCharacter>>()
        .map(SlideString::Complex)
        .dy();

    let string = choice((single_quote_string, double_quote_string)).dy();

    let long_tuple = expression
        .clone()
        .separated_by(op(','))
        .at_least(2)
        .allow_trailing()
        .padded()
        .delimited_by(just('('), just(')'))
        .map(Atom::Tuple)
        .dy();

    let unit = just('(')
        .padded()
        .then(just(')'))
        .padded()
        .to(Atom::Tuple(vec![]))
        .dy();
    let one_tuple = op('(')
        .ignore_then(expression.clone())
        .then_ignore(op(','))
        .then_ignore(op(')'))
        .map(|i| Atom::Tuple(vec![i]))
        .dy();

    let tuple = choice((unit, one_tuple, long_tuple)).dy();

    let struct_instantiation = identifier
        .clone()
        .padded()
        .then(
            assignment_comma_seq
                .clone()
                .padded()
                .padded_by(comment.clone().repeated())
                .delimited_by(just('{'), just('}')),
        )
        .map(|(name, assignments)| Atom::Struct(name, assignments))
        .dy();

    let mut function_expr = Recursive::<_, _, Simple<char>>::declare();

    let atom = choice((
        struct_instantiation,
        number.map(Atom::Number),
        identifier.clone().map(Atom::Identifier),
        string.clone().map(|i| Atom::String(Box::new(i))),
        tuple,
        function_expr.clone().map(Atom::Function),
    ))
    .dy();

    let atom_expr = choice((
        atom.map(Expression::Atom),
        expression
            .clone()
            .padded()
            .delimited_by(just('('), just(')'))
            .padded(),
    ))
    .dy();

    let unary = op('-')
        .repeated()
        .then(atom_expr)
        .foldr(|_op, rhs| Expression::Neg(Box::new(rhs)))
        .dy();

    let product = unary
        .clone()
        .then(
            op('*')
                .to(Expression::Mul as fn(_, _) -> _)
                .or(op('/').to(Expression::Div as fn(_, _) -> _))
                .then(unary)
                .repeated(),
        )
        .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)))
        .dy();

    let sum = product
        .clone()
        .then(
            op('+')
                .to(Expression::Add as fn(_, _) -> _)
                .or(op('-').to(Expression::Sub as fn(_, _) -> _))
                .then(product)
                .repeated(),
        )
        .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)))
        .dy();

    expression.define(sum.padded());

    let name = choice((string.clone(), bare_name.clone())).padded().dy();

    let local_comment = comment.clone();
    let stmtseq = recursive(|stmtseq| {
        let slidestmt = |do_delimited: bool| {
            {
                let seq = stmtseq.clone();

                let repeat = if do_delimited { 1 } else { 0 };

                let bare_block = seq
                    .padded()
                    .padded_by(local_comment.clone().repeated())
                    .delimited_by(just('{'), just('}'));

                let column = text::keyword("column")
                    .padded()
                    .then(bare_block.clone())
                    .map(|(_, block)| SlideStmt::Column(block));

                let block = bare_block.map(SlideStmt::Block);

                recursive(|_stmt| {
                    let item_content = choice((
                        block.clone(),
                        string.clone().map(SlideStmt::String),
                        bare_name.clone().map(SlideStmt::String),
                    ));

                    let insert = text::keyword("insert")
                        .padded()
                        .then(identifier.clone())
                        .map(|(_, i)| SlideStmt::Insert(i));

                    let list_item = choice((just("-"), just("*")))
                        .padded()
                        .then(item_content.clone())
                        .map(|(_, i)| SlideStmt::ListItem(Box::new(i)));

                    let enum_item = identifier_or_number
                        .clone()
                        .then(just("."))
                        .padded()
                        .then(item_content)
                        .map(|((num, _), i)| SlideStmt::EnumItem(num, Box::new(i)));

                    let with_terminator = choice((
                        list_item,
                        enum_item,
                        string.clone().map(SlideStmt::String),
                        insert,
                        let_assignment
                            .clone()
                            .map(|(name, expr)| SlideStmt::Let(name, expr)),
                    ));

                    let stmt = choice((
                        block,
                        column,
                        with_terminator
                            .then_ignore(
                                filter(|i: &char| char::is_whitespace(*i) && *i != '\n')
                                    .repeated()
                                    .or_not(),
                            )
                            .then_ignore(statement_terminator.clone().repeated().exactly(repeat))
                            .padded(),
                    ))
                    .padded();

                    let marked = identifier
                        .clone()
                        .padded()
                        .then(just(':'))
                        .padded()
                        .then(stmt.clone())
                        .map(|(_, i)| i);

                    choice((marked, stmt))
                })
            }
            .dy()
        };

        let main_seq = slidestmt(true)
            .padded_by(comment.clone().repeated())
            .padded()
            .repeated()
            .dy();

        let final_stmt = slidestmt(false)
            .padded_by(comment.clone().repeated())
            .padded()
            .or_not()
            .dy();

        main_seq
            .then(final_stmt)
            .map(|(mut stmts, stmt)| {
                if let Some(i) = stmt {
                    stmts.push(i)
                }
                stmts
            })
            .dy()
    });

    let function_stmt = choice((let_assignment
        .clone()
        .map(|(name, expr)| FunctionStatement::Let(name, expr)),))
    .padded_by(comment.clone().repeated())
    .then_ignore(
        filter(|i: &char| char::is_whitespace(*i) && *i != '\n')
            .repeated()
            .or_not(),
    );

    let function_stmt_seq = function_stmt
        .separated_by(statement_terminator.clone())
        .then(expression.clone().or_not())
        .dy();

    let function_body = function_stmt_seq
        .clone()
        .padded()
        .padded_by(comment.clone().repeated())
        .delimited_by(just('{'), just('}'))
        .padded()
        .map(|(stmts, ret_expr)| FunctionBody { stmts, ret_expr })
        .dy();

    let param = identifier
        .clone()
        .padded()
        .then_ignore(op(':'))
        .padded()
        .then(identifier.clone())
        .padded()
        .dy();
    let paramlist = param
        .separated_by(op(',').padded())
        .delimited_by(just('('), just(')'));
    let return_type = just("->").padded().ignore_then(identifier.clone());

    let signature = paramlist
        .padded()
        .then(return_type.or_not().padded())
        .map(|(params, ret)| FunctionSignature { params, ret })
        .dy();

    let function_common_part = signature.then(function_body).dy();

    function_expr.define(
        text::keyword("fn")
            .padded()
            .ignore_then(function_common_part.clone())
            .map(|(signature, body)| Box::new(UnnamedFunction { body, signature })),
    );

    let function_stmt = text::keyword("fn")
        .padded()
        .ignore_then(identifier.clone())
        .padded()
        .then(function_common_part)
        .map(|(name, (signature, body))| NamedFunction {
            name,
            body,
            signature,
        })
        .dy();

    let slidebody = stmtseq
        .clone()
        .padded()
        .padded_by(comment.clone().repeated())
        .delimited_by(just('{'), just('}'))
        .dy();

    let themebody = assignment_seq
        .padded()
        .padded_by(comment.clone().repeated())
        .delimited_by(just('{'), just('}'))
        .dy();

    let slide = empty()
        .then_ignore(text::keyword("slide").padded())
        .then(name.clone())
        .then(slidebody.clone())
        .map(|((_, title), body)| Slide { title, body })
        .dy();

    let template = empty()
        .then_ignore(text::keyword("template").padded())
        .then(identifier.clone())
        .then(slidebody)
        .map(|((_, name), body)| Template { name, body })
        .dy();

    let theme = text::keyword("theme")
        .padded()
        .then(identifier.clone())
        .padded()
        .then(themebody)
        .map(|((_, name), _)| Theme { name })
        .dy();

    let field = identifier
        .clone()
        .padded()
        .then_ignore(just(':'))
        .padded()
        .then(identifier.clone())
        .padded()
        .then(just('=').padded().ignore_then(expression).or_not())
        .map(|((name, ty), expr)| Field {
            name,
            default: expr,
            field_type: ty,
        })
        .dy();

    let structbody = field
        .padded()
        .padded_by(comment.clone().repeated())
        .separated_by(just(','))
        .allow_trailing()
        .padded()
        .padded_by(comment.clone().repeated())
        .delimited_by(just('{'), just('}'))
        .dy();

    let named_struct_body = identifier
        .clone()
        .padded()
        .then(structbody)
        .map(|(name, fields)| TypeDef::Struct { name, fields })
        .dy();

    let enumdef = text::keyword("enum")
        .padded()
        .then(identifier.clone())
        .padded()
        .then_ignore(just('='))
        .padded()
        .then(
            named_struct_body
                .clone()
                .padded()
                .padded_by(comment.clone().repeated())
                .separated_by(just('|')),
        )
        .padded()
        .padded_by(comment.clone().repeated())
        .then_ignore(statement_terminator.clone())
        .map(|((_, name), variants)| TypeDef::Enum { name, variants })
        .dy();

    let structdef = text::keyword("struct")
        .padded()
        .ignore_then(named_struct_body)
        .dy();

    let typedef = choice((structdef, enumdef)).dy();

    let title = text::keyword("title").padded().ignore_then(name).dy();

    let toplevel = choice::<_, Simple<char>>((
        slide.map(TopLevel::Slide),
        theme.map(TopLevel::Theme),
        template.map(TopLevel::Template),
        typedef.map(TopLevel::TypeDef),
        let_assignment
            .clone()
            .then_ignore(statement_terminator)
            .map(|(name, expr)| TopLevel::Let(name, expr)),
        title.map(TopLevel::Title),
        function_stmt.map(TopLevel::Function),
    ))
    .padded_by(comment.clone().repeated())
    .padded()
    .dy();

    toplevel
        .repeated()
        .map(|toplevels| Program {
            title: SlideString::Simple("".to_string()),
            statements: toplevels,
        })
        .then_ignore(end())
        .dy()
}

pub fn parse(source: &str) -> Result<Program, Vec<Simple<char>>> {
    parser().parse(source)
}

pub fn format_errors(errs: &[Simple<char>], src: &str) -> String {
    let mut res = Cursor::new(Vec::new());

    errs.iter()
        .map(|e| e.clone().map(|c| c.to_string()))
        .for_each(|e| {
            let report = Report::build(ReportKind::Error, (), e.span().start);

            let report = match e.reason() {
                chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
                    .with_message(format!(
                        "Unclosed delimiter {}",
                        delimiter.fg(Color::Yellow)
                    ))
                    .with_label(
                        Label::new(span.clone())
                            .with_message(format!(
                                "Unclosed delimiter {}",
                                delimiter.fg(Color::Yellow)
                            ))
                            .with_color(Color::Yellow),
                    )
                    .with_label(
                        Label::new(e.span())
                            .with_message(format!(
                                "Must be closed before this {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    ),
                chumsky::error::SimpleReason::Unexpected => report
                    .with_message(format!(
                        "{}, expected {}",
                        if e.found().is_some() {
                            "Unexpected token in input"
                        } else {
                            "Unexpected end of input"
                        },
                        if e.expected().len() == 0 {
                            "something else".to_string()
                        } else {
                            e.expected()
                                .map(|expected| match expected {
                                    Some(expected) => expected.to_string(),
                                    None => "end of input".to_string(),
                                })
                                .collect::<Vec<_>>()
                                .join(", ")
                        }
                    ))
                    .with_label(
                        Label::new(e.span())
                            .with_message(format!(
                                "Unexpected token {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    ),
                chumsky::error::SimpleReason::Custom(msg) => report.with_message(msg).with_label(
                    Label::new(e.span())
                        .with_message(format!("{}", msg.fg(Color::Red)))
                        .with_color(Color::Red),
                ),
            };

            report.finish().write(Source::from(src), &mut res).unwrap();
            res.write_all(&[b'\n']).unwrap();
            res.write_all(&[b'\n']).unwrap();
        });

    String::from_utf8(res.into_inner()).unwrap()
}

#[cfg(test)]
mod tests {
    use crate::parser::{format_errors, parse};

    macro_rules! parser_test {
        ($name: ident: $input: literal) => {
            #[test]
            fn $name() {
                let res = parse($input);
                assert!(res.is_ok(), "{}", format_errors(&res.unwrap_err(), $input));
                println!("{:?}", res.unwrap())
            }
        };
        ($name: ident: $input: literal,) => {
            parser_test!($name: $input);
        };
        ($name: ident: $input: literal, $($rest: tt)*) => {
            parser_test!($name: $input);
            parser_test!($($rest)*);
        };
    }
    parser_test!(
        simple_slide: "slide test {}",
        slide_with_content: r#"slide test { "test"; }"#,
        slide_with_block: r#"slide test { { "test"; } "test"; }"#,
        slide_with_block_reversed: r#"slide test { "test"; { "test"; } }"#,
        newline_terminated: r#"
slide test {
    "test"
    {
        "test"
    }
}"#,
        last_stmt_not_terminated: r#"slide test { "test"  ; { "test" } "test" }"#,
        column: r#"slide test { column {"left"  } column {  "right"} }"#,
        list: r#"slide test { - test; - test1 ; - test2}"#,
        enumerate: r#"slide test { 1. test ; 2. test1; n. test2}"#,
        only_comment: "slide test { // test \n }",
    );
}
