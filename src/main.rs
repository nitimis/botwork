use botwork::core::{
    eval::{botwork, Context},
    grammar::{BWParser, Rule},
};
use pest::Parser;
use std::fs::read_to_string;

fn main() {
    let source = read_to_string("examples/02-syntaxes.botwork").unwrap();
    match BWParser::parse(Rule::botwork, &source) {
        Ok(tree) => {
            let mut context = Context::default();
            context.init_statements();
            let mut results = Vec::new();
            for pair in tree {
                match botwork(pair, &mut context) {
                    Ok(ok) => results.push(ok),
                    Err(err) => {
                        dbg!(err);
                        break;
                    }
                };
            }
            // dbg!(results);
        }
        Err(err) => {
            println!("Failed parsing input: {:}", err);
        }
    };
}
