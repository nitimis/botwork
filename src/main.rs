mod parser {
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "src/main.pest"]
    pub struct Parser;
}

use parser::Rule;
use pest::{
    iterators::Pair,
    // pratt_parser::{Assoc::*, Op, PrattParser},
    Parser,
};
use std::fs::read_to_string;

fn oduraja(pair: Pair<Rule>) {
    match pair.as_rule() {
        Rule::EOI => todo!(),
        Rule::WHITESPACE => todo!(),
        Rule::NEWLINE => todo!(),
        Rule::oduraja => todo!(),
        Rule::reserved => todo!(),
        Rule::part => todo!(),
        Rule::statements => todo!(),
        Rule::stmt_invoke => todo!(),
        Rule::stmt_define => todo!(),
        Rule::stmt_assign => todo!(),
        Rule::param_invoke => todo!(),
        Rule::param_define => todo!(),
        Rule::COMMENT => todo!(),
        Rule::comment_block => todo!(),
        Rule::comment_line => todo!(),
        Rule::expression => todo!(),
        Rule::infix => todo!(),
        Rule::expression_inner => todo!(),
        Rule::braced_expression => todo!(),
        Rule::unary => todo!(),
        Rule::dot_path => todo!(),
        Rule::literal => todo!(),
        Rule::array => todo!(),
        Rule::ident => todo!(),
        Rule::map => todo!(),
        Rule::map_pair => todo!(),
        Rule::keyword => todo!(),
        Rule::integer => todo!(),
        Rule::float => todo!(),
        Rule::string => todo!(),
        Rule::string_content => todo!(),
        Rule::string_delimiter => todo!(),
        Rule::string_escape => todo!(),
        Rule::exponent => todo!(),
        Rule::multiply => todo!(),
        Rule::divide => todo!(),
        Rule::modulus => todo!(),
        Rule::plus => todo!(),
        Rule::minus => todo!(),
        Rule::less_than => todo!(),
        Rule::less_than_or_equal => todo!(),
        Rule::greater_than => todo!(),
        Rule::greater_than_or_equal => todo!(),
        Rule::not_equal => todo!(),
        Rule::equal => todo!(),
        Rule::logical_and => todo!(),
        Rule::logical_or => todo!(),
        Rule::logical_not => todo!(),
        Rule::binary_operator => todo!(),
        Rule::unary_operator => todo!(),
        Rule::boolean => todo!(),
        Rule::boolean_true => todo!(),
        Rule::boolean_false => todo!(),
    };
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
        Ok(tree) => {
            dbg!(&tree);
            for pair in tree {
                oduraja(pair);
            }
        }
        Err(err) => {
            println!("Failed parsing input: {:}", err);
        }
    };
}
