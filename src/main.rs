mod parser {
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "src/main.pest"]
    pub struct Parser;
}

use lazy_static::lazy_static;
use parser::Rule;
use pest::{
    iterators::{Pair, Pairs},
    pratt_parser::{Assoc, Op, PrattParser},
    Parser,
};
use std::{collections::HashMap, fs::read_to_string};

use thiserror::Error;

lazy_static! {
    static ref PRATT: PrattParser<Rule> = PrattParser::new()
        .op(Op::infix(Rule::plus, Assoc::Left) | Op::infix(Rule::minus, Assoc::Left))
        .op(Op::infix(Rule::multiply, Assoc::Left) | Op::infix(Rule::divide, Assoc::Left))
        .op(Op::prefix(Rule::unary));
}

#[derive(Error, Debug)]
pub enum OduRajaError {
    #[error("Variable not defined")]
    VariableNotDefined(String),
    #[error("Statement not defined")]
    StatementNotDefined(String),
    #[error("Parameter missing error")]
    ParameterMissingError(String),
    #[error("Parsing integer error")]
    ParsingIntegerError(String),
}

#[derive(Clone, Debug)]
enum Literal {
    None,
    Int(i32),
    Float(f32),
    Bool(bool),
    String(String),
    List(Vec<Literal>),
    Dict(HashMap<String, Literal>),
}

#[derive(Clone, Default)]
struct Context {
    variables: HashMap<String, Literal>,
    statements: HashMap<String, Callback>,
}

struct OduRajaOK {
    literal: Literal,
}

impl Context {
    fn get_variable(&self, name: &String) -> Option<&Literal> {
        self.variables.get(name)
    }

    fn contains_variable(&self, name: &String) -> bool {
        self.variables.contains_key(name)
    }

    fn contains_statement(&self, name: &String) -> bool {
        self.statements.contains_key(name)
    }

    fn get_statement(&self, name: &String) -> Option<&Callback> {
        self.statements.get(name)
    }

    fn init_statement(&mut self) {
        self.statements.insert("log|param|".into(), log_param);
    }
}

type Callback = fn(Pair<Rule>, &mut Context) -> Result<OduRajaOK, OduRajaError>;

fn parse_expr(pairs: Pairs<Rule>, globals: &mut Context) -> Result<OduRajaOK, OduRajaError> {
    PRATT
        .map_primary(|primary| oduraja(primary, globals))
        /*
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::neg => -rhs,
            _ => unreachable!(),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::add => lhs + rhs,
            Rule::sub => lhs - rhs,
            Rule::mul => lhs * rhs,
            Rule::div => lhs / rhs,
            Rule::pow => (1..rhs + 1).map(|_| lhs).product(),
            _ => unreachable!(),
        })
            */
        .parse(pairs)
}

fn hashify(text: &str) -> String {
    text.replace(' ', "").to_lowercase()
}

fn get_stmt_hash(pair: &Pair<Rule>) -> String {
    let mut hash = String::new();
    for pair in pair.clone().into_inner() {
        match pair.as_rule() {
            Rule::part => hash.push_str(&hashify(pair.as_str())),
            Rule::param_invoke | Rule::param_define => hash.push_str("|param|"),
            _ => unreachable!(),
        }
    }
    hash
}

fn log_param(pair: Pair<Rule>, globals: &mut Context) -> Result<OduRajaOK, OduRajaError> {
    let param_pair = pair
        .into_inner()
        .filter(|pair| pair.as_rule() == Rule::param_invoke)
        .last()
        .ok_or(OduRajaError::ParameterMissingError(
            "`Log {param}` requires atleast 1 parameter".into(),
        ))?;
    let ok = oduraja(param_pair, globals)?;
    dbg!(&ok.literal);
    Ok(ok)
}

fn no_op(_pair: Pair<Rule>, _globals: &mut Context) -> Result<OduRajaOK, OduRajaError> {
    Ok(OduRajaOK {
        literal: Literal::None,
    })
}

fn stmt_invoke(pair: Pair<Rule>, globals: &mut Context) -> Result<OduRajaOK, OduRajaError> {
    let hash = get_stmt_hash(&pair);
    if !globals.contains_statement(&hash) {
        return Err(OduRajaError::StatementNotDefined(pair.as_str().into()));
    }
    let statement = globals.get_statement(&hash).unwrap();
    statement(pair, globals)
}

fn param_invoke(pair: Pair<Rule>, globals: &mut Context) -> Result<OduRajaOK, OduRajaError> {
    parse_expr(pair.into_inner(), globals)
}

fn integer(pair: Pair<Rule>, _globals: &mut Context) -> Result<OduRajaOK, OduRajaError> {
    match pair.as_str().parse() {
        Ok(integer) => Ok(OduRajaOK {
            literal: Literal::Int(integer),
        }),
        Err(err) => Err(OduRajaError::ParsingIntegerError(err.to_string())),
    }
}

fn oduraja(pair: Pair<Rule>, globals: &mut Context) -> Result<OduRajaOK, OduRajaError> {
    match pair.as_rule() {
        Rule::EOI => todo!(),
        Rule::WHITESPACE => todo!(),
        Rule::oduraja => todo!(),
        Rule::reserved => todo!(),
        Rule::part => no_op(pair, globals),
        Rule::statements => todo!(),
        Rule::stmt_invoke => stmt_invoke(pair, globals),
        Rule::stmt_define => todo!(),
        Rule::stmt_assign => todo!(),
        Rule::param_invoke => param_invoke(pair, globals),
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
        Rule::integer => integer(pair, globals),
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
        Rule::IF => todo!(),
        Rule::ELSE => todo!(),
        Rule::FOR => todo!(),
        Rule::BREAK => todo!(),
        Rule::RETURN => todo!(),
        Rule::CONTINUE => todo!(),
        Rule::WHILE => todo!(),
        Rule::TRY => todo!(),
        Rule::CATCH => todo!(),
        Rule::stmt_if => todo!(),
        Rule::stmt_else => todo!(),
        Rule::reserved_parts => todo!(),
        Rule::IN => todo!(),
        Rule::stmt_for => todo!(),
        Rule::stmt_while => todo!(),
        Rule::stmt_try => todo!(),
        Rule::stmt_catch => todo!(),
        Rule::stmt_break => todo!(),
        Rule::stmt_continue => todo!(),
        Rule::stmt_return => todo!(),
        Rule::primary => todo!(),
    }
}

fn main() {
    let source = read_to_string("examples/01-basics.oduraja").unwrap();
    match parser::Parser::parse(Rule::oduraja, &source) {
        Ok(tree) => {
            let mut context = Context::default();
            context.init_statement();
            for pair in tree {
                match oduraja(pair, &mut context) {
                    Ok(_) => {}
                    Err(err) => {
                        dbg!(err);
                        panic!()
                    }
                };
            }
        }
        Err(err) => {
            println!("Failed parsing input: {:}", err);
        }
    };
}
