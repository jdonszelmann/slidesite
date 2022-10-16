use std::io::{Cursor, Write};
use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::error::Simple;
use chumsky::{Parser, text};
use chumsky::prelude::{just, take_until, filter, choice, end, empty, recursive, Recursive};
use chumsky::text::TextParser;

#[derive(Debug, Clone)]
pub enum IdentifierOrNumber {
    Identifier(String),
    Number(String),
}

#[derive(Debug, Clone)]
pub enum Atom {
    Identifier(String),
    Number(String),
    String(Box<SlideString>),
    Tuple(Vec<Expression>),
    Struct(String, Vec<(String, Expression)>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Atom(Atom),
    Neg(Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum StringCharacter {
    Char(char),
    Expr(Expression),
}

#[derive(Debug, Clone)]
pub enum SlideString {
    Complex(Vec<StringCharacter>),
    Simple(String),
}

#[derive(Debug, Clone)]
pub enum SlideStmt {
    String(SlideString),
    Block(Vec<SlideStmt>),
    Column(Vec<SlideStmt>),
    ListItem(Box<SlideStmt>),
    EnumItem(IdentifierOrNumber, Box<SlideStmt>),
    Marked(String, Box<SlideStmt>),
    Insert(String),
    Let(String, Expression),
}

#[derive(Debug)]
pub struct Slide {
    pub title: SlideString,
    pub body: Vec<SlideStmt>,
}

#[derive(Debug)]
pub struct Template {
    pub name: String,
    pub body: Vec<SlideStmt>,
}

#[derive(Debug)]
pub struct Field {
    pub name: String,
    pub default: Option<Expression>,
    pub field_type: String,
}

#[derive(Debug)]
pub enum TypeDef {
    Enum {
        name: String,
        variants: Vec<TypeDef>,
    },
    Struct {
        name: String,
        fields: Vec<Field>,
    },
}

#[derive(Debug)]
pub struct Theme {
    pub name: String,
}

#[derive(Debug)]
pub enum TopLevel {
    Slide(Slide),
    Theme(Theme),
    Template(Template),
    TypeDef(TypeDef),
    Let(String, Expression),
    Title(SlideString),
}

#[derive(Debug)]
pub struct Program {
    pub title: SlideString,
    pub statements: Vec<TopLevel>,
}

fn char_to_string(c: char) -> StringCharacter {
    StringCharacter::Char(c)
}

fn parser() -> impl Parser<char, Program, Error=Simple<char>> {
    let comment = just("//").then(take_until(just('\n'))).padded();
    let op = |c| just(c).padded();

    let bare_name = filter(|c: &char| !['"', '\'', '\n', '\r', '{', '}', ';'].contains(c))
        .repeated()
        .map(|i| SlideString::Simple(String::from_iter(i).trim().to_string()));

    let number = text::int(10);
    let identifier = text::ident();

    let identifier_or_number = choice((
        number.map(IdentifierOrNumber::Number),
        identifier.map(IdentifierOrNumber::Identifier)
    ));

    let mut expression = Recursive::<_, _, Simple<char>>::declare();

    let statement_terminator = choice((
        just(';').to(()),
        just('\n').to(()),
        end(),
    ));

    let assignment = identifier
        .padded()
        .then_ignore(op('='))
        .then(expression.clone())
        ;

    let assignment_comma_seq = assignment
        .clone()
        .padded()
        .padded_by(comment.repeated())
        .separated_by(op(','))
        .allow_trailing()
        .padded();

    let assignment_seq = assignment
        .clone()
        .padded()
        .padded_by(comment.repeated())
        .separated_by(statement_terminator.clone())
        .allow_trailing()
        .padded();

    let let_assignment = text::keyword("let").ignore_then(assignment.clone());

    let interpolation = expression
        .clone()
        .padded()
        .delimited_by(
            just('{'), just('}'),
        );

    let escape = choice((
        just("\\\"").to(StringCharacter::Char('\"')),
        just("\\\'").to(StringCharacter::Char('\'')),
        just("\\\\").to(StringCharacter::Char('\\')),
        just("\\{").to(StringCharacter::Char('{')),
        just("\\}").to(StringCharacter::Char('}')),
        interpolation.map(StringCharacter::Expr)
    ));

    let single_quote_string_char = choice((escape.clone(), filter(|c| *c != '\'').map(char_to_string)));
    let double_quote_string_char = choice((escape, filter(|c| *c != '\"').map(char_to_string)));

    let single_quote_string = just('\'')
        .ignore_then(single_quote_string_char.repeated())
        .then_ignore(just('\''))
        .collect::<Vec<StringCharacter>>()
        .map(SlideString::Complex);

    let double_quote_string = just('"')
        .ignore_then(double_quote_string_char.repeated())
        .then_ignore(just('"'))
        .collect::<Vec<StringCharacter>>()
        .map(SlideString::Complex);

    let string = choice((single_quote_string, double_quote_string));

    let long_tuple = expression
        .clone()
        .separated_by(op(','))
        .at_least(2)
        .allow_trailing()
        .padded()
        .delimited_by(just('('), just(')'))
        .map(Atom::Tuple);

    let unit = just('(').padded().then(just(')')).padded().to(Atom::Tuple(vec![]));
    let one_tuple = op('(')
        .ignore_then(expression.clone())
        .then_ignore(op(','))
        .then_ignore(op(')'))
        .map(|i| Atom::Tuple(vec![i]));

    let tuple = choice((
        unit,
        one_tuple,
        long_tuple,
    ));

    let struct_instantiation =
        identifier
        .padded()
        .then(
            assignment_comma_seq
                .clone()
                .padded()
                .padded_by(comment.repeated())
                .delimited_by(
                    just('{'), just('}'),
                )
        ).map(|(name, assignments)| {
        Atom::Struct(name, assignments)
    });

    let atom = choice((
        struct_instantiation,
        number.map(Atom::Number),
        identifier.map(Atom::Identifier),
        string.clone().map(|i| Atom::String(Box::new(i))),
        tuple,
    ));

    let atom_expr = choice((
        atom.map(Expression::Atom),
        expression.clone().padded().delimited_by(just('('), just(')')).padded()
    ));

    let unary = op('-')
        .repeated()
        .then(atom_expr)
        .foldr(|_op, rhs| Expression::Neg(Box::new(rhs)));

    let product = unary.clone()
        .then(op('*').to(Expression::Mul as fn(_, _) -> _)
            .or(op('/').to(Expression::Div as fn(_, _) -> _))
            .then(unary)
            .repeated())
        .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

    let sum = product.clone()
        .then(op('+').to(Expression::Add as fn(_, _) -> _)
            .or(op('-').to(Expression::Sub as fn(_, _) -> _))
            .then(product)
            .repeated())
        .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

    expression.define(
        sum.padded()
    );

    let name = choice((
        string.clone(),
        bare_name,
    )).padded();

    let stmtseq = recursive(|stmtseq| {
        let slidestmt = |do_delimited: bool| {
            let seq = stmtseq.clone();

            let repeat = if do_delimited {
                1
            } else {
                0
            };

            let bare_block = seq
                .padded()
                .padded_by(comment.repeated())
                .delimited_by(
                    just('{'), just('}'),
                );

            let column = text::keyword("column").padded()
                .then(bare_block.clone())
                .map(|(_, block)| SlideStmt::Column(block));

            let block = bare_block.map(SlideStmt::Block);

            recursive(|_stmt| {
                let item_content = choice((
                    block.clone(),
                    string.clone().map(SlideStmt::String),
                    bare_name.map(SlideStmt::String)
                ));

                let insert = text::keyword("insert")
                    .padded()
                    .then(identifier)
                    .map(|(_, i)| SlideStmt::Insert(i));

                let list_item = choice((just("-"), just("*"))).padded().then(item_content.clone())
                    .map(|(_, i)| SlideStmt::ListItem(Box::new(i)));

                let enum_item = identifier_or_number.then(just(".")).padded().then(item_content)
                    .map(|((num, _), i)| SlideStmt::EnumItem(num, Box::new(i)));

                let with_terminator = choice((
                    list_item,
                    enum_item,
                    string.clone().map(SlideStmt::String),
                    insert,
                    let_assignment.clone().map(|(name, expr)| SlideStmt::Let(name, expr)),
                ));

                let stmt = choice((
                    block,
                    column,
                    with_terminator
                        .then_ignore(filter(|i: &char| char::is_whitespace(*i) && *i != '\n').repeated().or_not())
                        .then_ignore(statement_terminator.clone().repeated().exactly(repeat)).padded(),
                )).padded();

                let marked = identifier
                    .padded()
                    .then(just(':'))
                    .padded()
                    .then(stmt.clone())
                    .map(|(_, i)| i);

                choice((
                    marked,
                    stmt
                ))
            })
        };

        let main_seq = slidestmt(true)
            .padded_by(comment.repeated())
            .padded()
            .repeated();

        let final_stmt = slidestmt(false)
            .padded_by(comment.repeated())
            .padded()
            .or_not();

        main_seq.then(final_stmt)
            .map(|(mut stmts, stmt)| {
                if let Some(i) = stmt {
                    stmts.push(i)
                }
                stmts
            })
    });

    let slidebody = stmtseq
        .clone()
        .padded()
        .padded_by(comment.repeated())
        .delimited_by(
            just('{'), just('}'),
        );

    let themebody = assignment_seq
        .padded()
        .padded_by(comment.repeated())
        .delimited_by(
            just('{'), just('}'),
        );

    let slide = empty()
        .then_ignore(text::keyword("slide").padded())
        .then(name.clone())
        .then(slidebody.clone())
        .map(|((_, title), body)| Slide {
            title,
            body,
        });

    let template = empty()
        .then_ignore(text::keyword("template").padded())
        .then(identifier.clone())
        .then(slidebody)
        .map(|((_, name), body)| Template {
            name,
            body,
        });

    let theme = text::keyword("theme").padded()
        .then(identifier.clone())
        .padded()
        .then(themebody)

        .map(|((_, name), _)| Theme {
            name,
        });

    let field = identifier.clone()
        .padded()
        .then_ignore(just(':'))
        .padded()
        .then(identifier.clone())
        .padded()
        .then(
            just('=')
                .padded()
                .ignore_then(expression)
                .or_not()
        )
        .map(|((name, ty), expr)| Field {
            name,
            default: expr,
            field_type: ty,
        });

    let structbody = field
        .padded()
        .padded_by(comment.repeated())
        .separated_by(just(','))
        .allow_trailing()
        .padded()
        .padded_by(comment.repeated())
        .delimited_by(
            just('{'), just('}'),
        );

    let named_struct_body = identifier.clone()
        .padded()
        .then(structbody)
        .map(|(name, fields)| TypeDef::Struct {
            name,
            fields,
        });

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
                .padded_by(comment.repeated())
                .separated_by(just('|'))
        )
        .padded()
        .padded_by(comment.repeated())
        .then_ignore(statement_terminator.clone())
        .map(|((_, name), variants)| TypeDef::Enum {
            name,
            variants,
        });

    let structdef = text::keyword("struct")
        .padded()
        .ignore_then(named_struct_body);

    let typedef = choice((structdef, enumdef));

    let title = text::keyword("title")
        .padded()
        .ignore_then(name);

    let toplevel = choice::<_, Simple<char>>((
        slide.map(TopLevel::Slide),
        theme.map(TopLevel::Theme),
        template.map(TopLevel::Template),
        typedef.map(TopLevel::TypeDef),
        let_assignment
            .then_ignore(statement_terminator)
            .map(|(name, expr)| TopLevel::Let(name, expr)),
        title.map(TopLevel::Title),
    ))
        .padded_by(comment.repeated())
        .padded();

    toplevel.repeated()
        .map(|toplevels| {
            Program {
                title: SlideString::Simple("".to_string()),
                statements: toplevels,
            }
        }).then_ignore(end())
}

pub fn parse(source: &str) -> Result<Program, Vec<Simple<char>>> {
    parser().parse(source)
}

pub fn format_errors(errs: &Vec<Simple<char>>, src: &str) -> String {
    let mut res = Cursor::new(Vec::new());

    errs.into_iter()
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
            res.write(&[b'\n']).unwrap();
            res.write(&[b'\n']).unwrap();
        });

    String::from_utf8(res.into_inner()).unwrap()
}

#[cfg(test)]
mod tests {
    use crate::parser::{parse, format_errors};

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

