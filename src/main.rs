mod parser {
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "src/main.pest"]
    pub struct Parser;
}

use parser::Rule;
use pest::{
    iterators::Pairs,
    // pratt_parser::{Assoc::*, Op, PrattParser},
    Parser,
};
use std::fs::read_to_string;

/*
enum Atom {
    Int,
    Float,
    String,
}

enum Value {
    Atom(Atom),
    List(Box<Value>),
    Dict(Box<Value>),
}

fn oduraja(pairs: Pairs<Rule>, pratt: &PrattParser<Rule>) {
    pratt
        .map_primary(|primary| match primary.as_rule() {
            Rule::int => Value::Atom(primary.as_str().parse::<i32>().unwrap()),
            Rule::expression => Value::Atom(oduraja(primary.into_inner(), pratt)),
            _ => unreachable!(),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::neg => -rhs,
            _ => unreachable!(),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::add => lhs + rhs,
            Rule::sub => lhs - rhs,
            Rule::mul => lhs * rhs,
            Rule::div => lhs / rhs,
            _ => unreachable!(),
        })
        .parse(pairs)
}
*/

fn oduraja(tree: &mut Pairs<Rule>) {
    dbg!(&tree);
    let _pairs = tree
        .next()
        .unwrap()
        .into_inner() // inner of program
        .next()
        .unwrap()
        .into_inner(); // inner of expr
                       /*
                       let _pratt = PrattParser::new()
                           .op(Op::infix(Rule::add, Left) | Op::infix(Rule::sub, Left))
                           .op(Op::infix(Rule::mul, Left) | Op::infix(Rule::div, Left))
                           .op(Op::prefix(Rule::neg));
                       */
}
fn main() {
    let source = read_to_string("examples/01-basics.oduraja").unwrap();

    match parser::Parser::parse(Rule::oduraja, &source) {
        Ok(mut tree) => {
            oduraja(&mut tree);
        }
        Err(err) => {
            println!("Failed parsing input: {:}", err);
        }
    };
}
