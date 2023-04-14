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
            .op(Op::infix(plus, Left) | Op::infix(minus, Left))
            .op(Op::infix(multiply, Left) | Op::infix(divide, Left) | Op::infix(modulus, Left))
            .op(Op::prefix(unary))
    };
}

/// Odu Raja Ok
#[derive(Debug)]
struct OROk {
    token: Token,
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
}

#[derive(Clone, Debug)]
enum Token {
    None,
    Int(i32),
    /*
    Float(f32),
    Bool(bool),
    String(String),
    List(Vec<Literal>),
    Dict(HashMap<String, Literal>),
    */
    Op(Rule),
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

type ORResult = Result<OROk, ORErr>;
type Callback = fn(Pair<Rule>, &mut Context) -> ORResult;

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

fn log_param(pair: Pair<Rule>, globals: &mut Context) -> ORResult {
    let param_pair = pair
        .into_inner()
        .filter(|pair| pair.as_rule() == Rule::param_invoke)
        .last()
        .ok_or(ORErr::ParameterMissingError(
            "`Log {param}` requires atleast 1 parameter".into(),
        ))?;
    let ok = oduraja(param_pair, globals)?;
    Ok(dbg!(ok))
}

fn no_op(_pair: Pair<Rule>, _globals: &mut Context) -> ORResult {
    Ok(OROk { token: Token::None })
}

fn stmt_invoke(pair: Pair<Rule>, globals: &mut Context) -> ORResult {
    let hash = get_stmt_hash(&pair);
    if !globals.contains_statement(&hash) {
        return Err(ORErr::StatementNotDefined(pair.as_str().into()));
    }
    let statement = globals.get_statement(&hash).unwrap();
    statement(pair, globals)
}

trait Operate {
    fn operate_unary(&self, rhs: ORResult) -> ORResult;
    fn operate_binary(&self, lhs: ORResult, rhs: ORResult) -> ORResult;
}

impl Operate for ORResult {
    fn operate_binary(&self, lhs: ORResult, rhs: ORResult) -> ORResult {
        match (lhs, self, rhs) {
            (
                Ok(OROk { token: lhs }),
                Ok(OROk {
                    token: Token::Op(op),
                }),
                Ok(OROk { token: rhs }),
            ) => match op {
                Rule::multiply => todo!(),
                Rule::divide => todo!(),
                Rule::modulus => todo!(),
                Rule::plus => match (lhs, rhs) {
                    (Token::Int(a), Token::Int(b)) => Ok(OROk {
                        token: Token::Int(a + b),
                    }),
                    rest => Err(ORErr::OperationIncompatibleError(format!("{:?}", rest))),
                },
                Rule::minus => todo!(),
                Rule::less_than => todo!(),
                Rule::less_than_or_equal => todo!(),
                Rule::greater_than => todo!(),
                Rule::greater_than_or_equal => todo!(),
                Rule::not_equal => todo!(),
                Rule::equal => todo!(),
                Rule::logical_and => todo!(),
                Rule::logical_or => todo!(),
                _ => unreachable!("Unknown Operator {:?}", op),
            },
            //TODO: Stack Trace is lost. I am stupid. Need help preserving it :P
            _ => Err(ORErr::NestedError("Error with Expression".into())),
        }
        // Ok(OROk { token: Token::None })
    }

    fn operate_unary(&self, rhs: ORResult) -> ORResult {
        match (self, rhs) {
            (
                Ok(OROk {
                    token: Token::Op(op),
                }),
                Ok(OROk { token: _rhs }),
            ) => match op {
                Rule::minus => todo!(),
                Rule::logical_not => todo!(),
                _ => unreachable!("Unknown Operator {:?}", op),
            },
            //TODO: Stack Trace is lost. I am stupid. Need help preserving it :P
            _ => Err(ORErr::NestedError("Error with Expression".into())),
        }
    }
}

fn param_invoke(pair: Pair<Rule>, globals: &mut Context) -> ORResult {
    let globals = Rc::new(RefCell::new(globals));
    let result = PRATT_PARSER
        .map_primary(|primary| oduraja(primary, &mut globals.borrow_mut()))
        .map_infix(|lhs, op, rhs| {
            let op = oduraja(op, &mut globals.borrow_mut());
            op.operate_binary(lhs, rhs)
        })
        .map_prefix(|op, rhs| {
            let op = oduraja(op, &mut globals.borrow_mut());
            op.operate_unary(rhs)
        })
        .parse(pair.into_inner());
    result
}

fn integer(pair: Pair<Rule>, _globals: &mut Context) -> ORResult {
    match pair.as_str().parse() {
        Ok(integer) => Ok(OROk {
            token: Token::Int(integer),
        }),
        Err(err) => Err(ORErr::ParsingIntegerError(err.to_string())),
    }
}

fn token_op(pair: Pair<Rule>, _globals: &mut Context) -> ORResult {
    Ok(OROk {
        token: Token::Op(pair.as_rule()),
    })
}

fn oduraja(pair: Pair<Rule>, globals: &mut Context) -> ORResult {
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
        Rule::param_invoke => param_invoke,
        Rule::param_define => todo!(),
        Rule::COMMENT => todo!(),
        Rule::comment_block => todo!(),
        Rule::comment_line => todo!(),
        Rule::expression => todo!(),
        Rule::infix => todo!(),
        Rule::expression_inner => todo!(),
        Rule::unary => todo!(),
        Rule::dot_path => todo!(),
        Rule::literal => todo!(),
        Rule::array => todo!(),
        Rule::ident => todo!(),
        Rule::map => todo!(),
        Rule::map_pair => todo!(),
        Rule::keyword => todo!(),
        Rule::integer => integer,
        Rule::float => todo!(),
        Rule::string => todo!(),
        Rule::string_content => todo!(),
        Rule::string_delimiter => todo!(),
        Rule::string_escape => todo!(),
        Rule::braced_expression => todo!(),
        Rule::exponent => token_op,
        Rule::multiply => token_op,
        Rule::divide => token_op,
        Rule::modulus => token_op,
        Rule::plus => token_op,
        Rule::minus => token_op,
        Rule::less_than => token_op,
        Rule::less_than_or_equal => token_op,
        Rule::greater_than => token_op,
        Rule::greater_than_or_equal => token_op,
        Rule::not_equal => token_op,
        Rule::equal => token_op,
        Rule::logical_and => token_op,
        Rule::logical_or => token_op,
        Rule::logical_not => token_op,
        Rule::binary_operator => token_op,
        Rule::unary_operator => token_op,
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
                    Ok(ok) => results.push(ok.token),
                    Err(err) => {
                        dbg!(err);
                        panic!()
                    }
                };
            }
            dbg!(results);
        }
        Err(err) => {
            println!("Failed parsing input: {:}", err);
        }
    };
}
