mod parser {
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "src/main.pest"]
    pub struct Parser;
}

use parser::Rule;
use pest::{iterators::Pair, pratt_parser::PrattParser, Parser};
use std::{cell::RefCell, collections::HashMap, fs::read_to_string, rc::Rc};

use thiserror::Error;

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;
        PrattParser::new()
            .op(Op::infix(plus, Left) | Op::infix(minus, Left) | Op::infix(logical_or, Left) )
            .op(Op::infix(multiply, Left) | Op::infix(divide, Left) | Op::infix(modulus, Left) |Op::infix(logical_and, Left) )
            .op(Op::prefix(minus) | Op::prefix(logical_not))
    };
}

/// Ode Raja Err
#[derive(Error, Debug)]
pub enum ORErr {
    #[error("Variable not defined")]
    VariableNotDefined(String),
    #[error("Statement not defined")]
    StatementNotDefined(String),
    #[error("Parameter missing error")]
    ParameterMissingError(String),
    #[error("Parsing integer error")]
    ParsingIntegerError(String),
    #[error("Nested error")]
    NestedError(String),
    #[error("Operation performed on incompatible types")]
    OperationIncompatibleError(String),
    #[error("Operator not found")]
    OperatorNotFoundError(String),
    #[error("Operaands not found")]
    OperandsNotFound(String),
}

#[derive(Clone, Debug)]
enum Literal {
    None,
    Int(i32),
    Float(f32),
    Bool(bool),
    String(String),
    Array(Vec<Literal>),
    Map(HashMap<String, Literal>),
    //Op(Rule),
}

#[derive(Clone, Default)]
struct Context {
    // variables: HashMap<String, Literal>,
    statements: HashMap<String, Callback>,
}

impl Context {
    /*
        fn get_variable(&self, name: &String) -> Option<&Literal> {
            self.variables.get(name)
        }

        fn contains_variable(&self, name: &String) -> bool {
            self.variables.contains_key(name)
        }
    */
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

type ORResult<O, E = ORErr> = Result<O, E>;
type LiteralResult = ORResult<Literal>;
type Callback = fn(Pair<Rule>, &mut Context) -> LiteralResult;

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

fn log_param(pair: Pair<Rule>, globals: &mut Context) -> LiteralResult {
    let param_pair = pair
        .into_inner()
        .filter(|pair| pair.as_rule() == Rule::param_invoke)
        .last()
        .ok_or(ORErr::ParameterMissingError(
            "`Log {param}` requires atleast 1 parameter".into(),
        ))?;
    let ok = oduraja(param_pair, globals)?;
    // TODO Implement Display for token
    Ok(dbg!(ok))
}

fn no_op(_pair: Pair<Rule>, _globals: &mut Context) -> LiteralResult {
    Ok(Literal::None)
}

fn stmt_invoke(pair: Pair<Rule>, globals: &mut Context) -> LiteralResult {
    let hash = get_stmt_hash(&pair);
    if !globals.contains_statement(&hash) {
        return Err(ORErr::StatementNotDefined(pair.as_str().into()));
    }
    let statement = globals.get_statement(&hash).unwrap();
    statement(pair, globals)
}

trait Operate {
    fn operate_unary(&self, rhs: Literal) -> LiteralResult;
    fn operate_binary(&self, lhs: Literal, rhs: Literal) -> LiteralResult;
}

impl Operate for Rule {
    fn operate_binary(&self, lhs: Literal, rhs: Literal) -> LiteralResult {
        let err = format!("{:?} {:?} {:?}", lhs, self, rhs);
        let err = Err(ORErr::OperationIncompatibleError(err));
        match self {
            // Arithmatic Operations
            Rule::multiply => todo!(),
            Rule::divide => todo!(),
            Rule::modulus => todo!(),
            Rule::plus => match (lhs, rhs) {
                (Literal::Int(a), Literal::Int(b)) => Ok(Literal::Int(a + b)),
                (Literal::Float(a), Literal::Int(b)) => Ok(Literal::Float(a + b as f32)),
                (Literal::Int(a), Literal::Float(b)) => Ok(Literal::Float(a as f32 + b)),
                (Literal::Float(a), Literal::Float(b)) => Ok(Literal::Float(a + b)),
                _ => err,
            },
            Rule::minus => todo!(),

            // Binary Operations
            Rule::less_than => todo!(),
            Rule::less_than_or_equal => todo!(),
            Rule::greater_than => todo!(),
            Rule::greater_than_or_equal => todo!(),
            Rule::not_equal => todo!(),
            Rule::equal => todo!(),
            Rule::logical_and => todo!(),
            Rule::logical_or => todo!(),
            _ => err,
        }
    }

    fn operate_unary(&self, rhs: Literal) -> LiteralResult {
        let err = format!("{:?} {:?}", self, rhs);
        let err = Err(ORErr::OperationIncompatibleError(err));
        match self {
            Rule::minus => match rhs {
                Literal::Int(a) => Ok(Literal::Int(-a)),
                Literal::Float(a) => Ok(Literal::Float(-a)),
                _ => err,
            },
            Rule::logical_not => match rhs {
                Literal::Bool(a) => Ok(Literal::Bool(!a)),
                _ => err,
            },
            _ => err,
        }
    }
}

fn pratt_parse(pair: Pair<Rule>, globals: &mut Context) -> LiteralResult {
    let globals = Rc::new(RefCell::new(globals));
    let result = PRATT_PARSER
        .map_primary(|primary| oduraja(primary, &mut globals.borrow_mut()))
        .map_infix(|lhs, op, rhs| op.as_rule().operate_binary(lhs?, rhs?))
        .map_prefix(|op, rhs| op.as_rule().operate_unary(rhs?))
        .parse(pair.into_inner());
    result
}

fn integer(pair: Pair<Rule>, _globals: &mut Context) -> LiteralResult {
    match pair.as_str().parse() {
        Ok(integer) => Ok(Literal::Int(integer)),
        Err(err) => Err(ORErr::ParsingIntegerError(err.to_string())),
    }
}

fn float(pair: Pair<Rule>, _globals: &mut Context) -> LiteralResult {
    match pair.as_str().parse() {
        Ok(float) => Ok(Literal::Float(float)),
        Err(err) => Err(ORErr::ParsingIntegerError(err.to_string())),
    }
}

fn boolean_true(_pair: Pair<Rule>, _globals: &mut Context) -> LiteralResult {
    Ok(Literal::Bool(true))
}

fn boolean_false(_pair: Pair<Rule>, _globals: &mut Context) -> LiteralResult {
    Ok(Literal::Bool(false))
}

/*
fn token_op(pair: Pair<Rule>, _globals: &mut Context) -> TokenResult {
    Ok(Literal::Op(pair.as_rule()))
}*/

fn string(pair: Pair<Rule>, _globals: &mut Context) -> LiteralResult {
    Ok(Literal::String(pair.as_str().into()))
}

fn array(pair: Pair<Rule>, globals: &mut Context) -> LiteralResult {
    let mut tokens = vec![];
    for inner_pair in pair.into_inner() {
        let token = oduraja(inner_pair, globals)?;
        tokens.push(token);
    }
    Ok(Literal::Array(tokens))
}

fn map(pair: Pair<Rule>, globals: &mut Context) -> LiteralResult {
    let mut map = HashMap::new();
    for inner_pair in pair.into_inner() {
        let mut kv = inner_pair.into_inner();
        if let Literal::String(keyword) =
            oduraja(kv.next().expect("Getting keyword from map_pair"), globals)?
        {
            let token = oduraja(kv.next().expect("Getting value from map_pair"), globals)?;
            map.insert(keyword, token);
        } else {
            unreachable!("Keyword in map MUST always be a String identifier token")
        }
    }
    Ok(Literal::Map(map))
}

fn oduraja(pair: Pair<Rule>, globals: &mut Context) -> LiteralResult {
    let op = match pair.as_rule() {
        Rule::EOI => no_op,
        Rule::WHITESPACE => todo!(),
        Rule::oduraja => todo!(),
        Rule::reserved => todo!(),
        Rule::part => todo!(),
        Rule::statements => todo!(),
        Rule::stmt_invoke => stmt_invoke,
        Rule::stmt_define => todo!(),
        Rule::stmt_assign => todo!(),
        Rule::param_invoke => pratt_parse,
        Rule::param_define => todo!(),
        Rule::COMMENT => todo!(),
        Rule::comment_block => todo!(),
        Rule::comment_line => todo!(),
        Rule::expression => todo!(),
        Rule::infix => todo!(),
        Rule::expression_inner => todo!(),
        Rule::unary => pratt_parse,
        Rule::dot_path => todo!(),
        Rule::literal => todo!(),
        Rule::array => array,
        Rule::ident => todo!(),
        Rule::map => map,
        Rule::map_pair => todo!(),
        Rule::keyword => string,
        Rule::integer => integer,
        Rule::float => float,
        Rule::string => string,
        Rule::string_content => todo!(),
        Rule::string_delimiter => todo!(),
        Rule::string_escape => todo!(),
        Rule::braced_expression => todo!(),
        Rule::exponent => todo!(),
        Rule::multiply => todo!(),
        Rule::divide => todo!(),
        Rule::modulus => todo!(),
        Rule::plus => todo!(),
        Rule::minus => no_op,
        Rule::less_than => todo!(),
        Rule::less_than_or_equal => todo!(),
        Rule::greater_than => todo!(),
        Rule::greater_than_or_equal => todo!(),
        Rule::not_equal => todo!(),
        Rule::equal => todo!(),
        Rule::logical_and => todo!(),
        Rule::logical_or => todo!(),
        Rule::logical_not => no_op,
        Rule::binary_operator => todo!(),
        Rule::unary_operator => todo!(),
        Rule::boolean => todo!(),
        Rule::boolean_true => boolean_true,
        Rule::boolean_false => boolean_false,
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
        Rule::seperator => todo!(),
    };
    op(pair, globals)
}

fn main() {
    let source = read_to_string("examples/01-expressions.oduraja").unwrap();
    match parser::Parser::parse(Rule::oduraja, &source) {
        Ok(tree) => {
            let mut context = Context::default();
            context.init_statement();
            let mut results = Vec::new();
            for pair in tree {
                match oduraja(pair, &mut context) {
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
