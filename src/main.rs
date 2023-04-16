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
            .op(
            Op::infix(less_than, Left)
            | Op::infix(less_than_or_equal, Left)
            | Op::infix(greater_than, Left)
            | Op::infix(greater_than_or_equal, Left)
            | Op::infix(not_equal, Left)
            | Op::infix(equal, Left)
            | Op::infix(exponent, Left)
        )
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
    ParsingError(String),
    #[error("Parsing integer error")]
    ParsingIntegerError(String),
    #[error("Nested error")]
    NestedError(String),
    #[error("Operation performed on incompatible types")]
    OperationIncompatibleError(String),
    #[error("Operator not found")]
    OperatorNotFoundError(String),
    #[error("Operands not found")]
    OperandsNotFound(String),
}

#[derive(Clone, Debug, Default)]
enum Literal {
    #[default]
    None,
    Int(i32),
    Float(f32),
    Bool(bool),
    String(String),
    Array(Vec<Literal>),
    Map(HashMap<String, Literal>),
}

#[derive(Clone, Debug, Default, PartialEq)]
enum InteruptKind {
    #[default]
    None,
    Continue,
    Break,
    Return,
}

/// Metadata used to indicate program interuption.
/// Like BREAK, CONTINUE, RETURN
#[derive(Clone, Debug, Default)]
struct Interupt {
    kind: InteruptKind,
    literal: Literal,
}

#[derive(Clone, Default)]
enum StmtType<'a> {
    #[default]
    None,
    Native(Callback),
    Custom(Pair<'a, Rule>),
}

#[derive(Clone, Default)]
struct Context<'a> {
    variables: HashMap<String, Literal>,
    statements: HashMap<String, StmtType<'a>>,
    interupt: Interupt,
}

impl Context<'_> {
    fn get_variable(&self, name: &String) -> LiteralResult {
        match self.variables.get(name) {
            Some(value) => Ok(value.to_owned()),
            None => Err(ORErr::VariableNotDefined(format!(
                "Varaible not defined: {}",
                name
            ))),
        }
    }

    /*
    fn contains_variable(&self, name: &String) -> bool {
        self.variables.contains_key(name)
    }
    */

    fn set_variable(&mut self, name: String, literal: Literal) -> Option<Literal> {
        self.variables.insert(name, literal)
    }

    fn contains_statement(&self, name: &String) -> bool {
        self.statements.contains_key(name)
    }

    fn get_statement(&self, name: &String) -> Option<&StmtType<'_>> {
        self.statements.get(name)
    }

    fn init_statements(&mut self) {
        self.statements
            .insert("log|param|".into(), StmtType::Native(log_param));
    }

    fn push_statement(&mut self, stmt_header: Pair<Rule>, stmt_block: Pair<Rule>) {
        let stmt_hash = get_stmt_hash(&stmt_header);
        self.statements
            .insert(stmt_hash, StmtType::Custom(stmt_block));
    }

    fn push_interupt(&mut self, interupt: Interupt) {
        if InteruptKind::None == self.interupt.kind {
            self.interupt = interupt
        } else {
            unreachable!("Tried pusing interupt, while an other one os already in progress!")
        }
    }

    fn push_interupt_break(&mut self) {
        self.push_interupt(Interupt {
            kind: InteruptKind::Break,
            literal: Literal::None,
        })
    }

    fn push_interupt_continue(&mut self) {
        self.push_interupt(Interupt {
            kind: InteruptKind::Continue,
            literal: Literal::None,
        })
    }

    fn push_interupt_return(&mut self, literal: Literal) {
        self.push_interupt(Interupt {
            kind: InteruptKind::Return,
            literal,
        })
    }

    fn has_loop_interupt(&self) -> bool {
        match self.interupt.kind {
            InteruptKind::Continue | InteruptKind::Break => true,
            InteruptKind::None | InteruptKind::Return => false,
        }
    }

    fn pop_interupt(&mut self) -> Interupt {
        let interupt = self.interupt.clone();
        self.interupt.literal = Literal::None; // Just in case
        self.interupt.kind = InteruptKind::None;
        interupt
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
    let statement = globals
        .get_statement(&hash)
        .ok_or(ORErr::VariableNotDefined(pair.as_str().into()))?;
    match statement {
        StmtType::None => unreachable!("None is just a default!"),
        StmtType::Native(statement) => statement(pair, globals),
        StmtType::Custom(statement) => {
            //TODO: Zip parameter names from header & insert it onto globals
            oduraja(statement, globals)
        }
    }
}

trait Operate {
    fn operate_unary(&self, rhs: Literal) -> LiteralResult;
    fn operate_binary(&self, lhs: Literal, rhs: Literal) -> LiteralResult;
}

impl Operate for Rule {
    fn operate_binary(&self, lhs: Literal, rhs: Literal) -> LiteralResult {
        let err = format!("{:?} {:?} {:?}", lhs, self, rhs);
        let err = Err(ORErr::OperationIncompatibleError(err));
        use Literal::*;
        use Rule::*;
        match self {
            // Arithmatic Operations
            multiply => match (lhs, rhs) {
                (Int(a), Int(b)) => Ok(Int(a * b)),
                (Float(a), Int(b)) => Ok(Float(a * b as f32)),
                (Int(a), Float(b)) => Ok(Float(a as f32 * b)),
                (Float(a), Float(b)) => Ok(Float(a * b)),
                _ => err,
            },
            divide => match (lhs, rhs) {
                (Int(a), Int(b)) => Ok(Float(a as f32 / b as f32)),
                (Float(a), Int(b)) => Ok(Float(a / b as f32)),
                (Int(a), Float(b)) => Ok(Float(a as f32 / b)),
                (Float(a), Float(b)) => Ok(Float(a / b)),
                _ => err,
            },
            modulus => match (lhs, rhs) {
                (Int(a), Int(b)) => Ok(Int(a % b)),
                (Float(a), Int(b)) => Ok(Float(a % b as f32)),
                (Int(a), Float(b)) => Ok(Float(a as f32 % b)),
                (Float(a), Float(b)) => Ok(Float(a % b)),
                _ => err,
            },
            plus => match (lhs, rhs) {
                (Int(a), Int(b)) => Ok(Int(a + b)),
                (Float(a), Int(b)) => Ok(Float(a + b as f32)),
                (Int(a), Float(b)) => Ok(Float(a as f32 + b)),
                (Float(a), Float(b)) => Ok(Float(a + b)),
                (String(a), String(b)) => Ok(String(format!("{}{}", a, b))),
                (Array(a), Array(b)) => {
                    Ok(Array(a.iter().cloned().chain(b.iter().cloned()).collect()))
                }
                _ => err,
            },
            minus => match (lhs, rhs) {
                (Int(a), Int(b)) => Ok(Int(a - b)),
                (Float(a), Int(b)) => Ok(Float(a - b as f32)),
                (Int(a), Float(b)) => Ok(Float(a as f32 - b)),
                (Float(a), Float(b)) => Ok(Float(a - b)),
                _ => err,
            },

            // Binary Operations
            less_than => match (lhs, rhs) {
                (Int(a), Int(b)) => Ok(Bool(a < b)),
                (Float(a), Int(b)) => Ok(Bool(a < b as f32)),
                (Int(a), Float(b)) => Ok(Bool((a as f32) < b)),
                (Float(a), Float(b)) => Ok(Bool(a < b)),
                _ => err,
            },
            less_than_or_equal => match (lhs, rhs) {
                (Int(a), Int(b)) => Ok(Bool(a <= b)),
                (Float(a), Int(b)) => Ok(Bool(a <= b as f32)),
                (Int(a), Float(b)) => Ok(Bool((a as f32) <= b)),
                (Float(a), Float(b)) => Ok(Bool(a <= b)),
                _ => err,
            },
            greater_than => match (lhs, rhs) {
                (Int(a), Int(b)) => Ok(Bool(a > b)),
                (Float(a), Int(b)) => Ok(Bool(a > b as f32)),
                (Int(a), Float(b)) => Ok(Bool((a as f32) > b)),
                (Float(a), Float(b)) => Ok(Bool(a > b)),
                _ => err,
            },
            greater_than_or_equal => match (lhs, rhs) {
                (Int(a), Int(b)) => Ok(Bool(a >= b)),
                (Float(a), Int(b)) => Ok(Bool(a >= b as f32)),
                (Int(a), Float(b)) => Ok(Bool((a as f32) >= b)),
                (Float(a), Float(b)) => Ok(Bool(a >= b)),
                _ => err,
            },
            not_equal => match (lhs, rhs) {
                (Int(a), Int(b)) => Ok(Bool(a != b)),
                (Float(a), Int(b)) => Ok(Bool(a != b as f32)),
                (Int(a), Float(b)) => Ok(Bool((a as f32) != b)),
                (Float(a), Float(b)) => Ok(Bool(a != b)),
                (Bool(a), Bool(b)) => Ok(Bool(a != b)),
                (String(a), String(b)) => Ok(Bool(a != b)),
                _ => err,
            },
            equal => match (lhs, rhs) {
                (Int(a), Int(b)) => Ok(Bool(a == b)),
                (Float(a), Int(b)) => Ok(Bool(a == b as f32)),
                (Int(a), Float(b)) => Ok(Bool((a as f32) == b)),
                (Float(a), Float(b)) => Ok(Bool(a == b)),
                (Bool(a), Bool(b)) => Ok(Bool(a == b)),
                (String(a), String(b)) => Ok(Bool(a == b)),
                _ => err,
            },

            exponent => match (lhs, rhs) {
                (Int(a), Int(b)) => Ok(Int(a.pow(b as u32))),
                (Float(a), Int(b)) => Ok(Float(a.powf(b as f32))),
                _ => err,
            },
            logical_and => match (lhs, rhs) {
                (Bool(a), Bool(b)) => Ok(Bool(a && b)),
                _ => err,
            },
            logical_or => match (lhs, rhs) {
                (Bool(a), Bool(b)) => Ok(Bool(a || b)),
                _ => err,
            },
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
        let keyword = kv
            .next()
            .ok_or(ORErr::ParsingError("Getting keyword from map_pair".into()))?;
        let value = kv
            .next()
            .ok_or(ORErr::ParsingError("Getting keyword from map_pair".into()))?;
        if kv.next().is_some() {
            unreachable!("Map statment still has unused pairs!");
        }
        if let Literal::String(keyword) = oduraja(keyword, globals)? {
            let value = oduraja(value, globals)?;
            map.insert(keyword, value);
        } else {
            return Err(ORErr::ParsingError(
                "Keyword in map MUST always be a String identifier token".into(),
            ));
        }
    }
    Ok(Literal::Map(map))
}

fn stmt_assign(pair: Pair<Rule>, globals: &mut Context) -> LiteralResult {
    let mut inner = pair.into_inner();
    let ident = inner
        .next()
        .ok_or(ORErr::ParsingError("Getting ident failed".into()))?
        .as_str()
        .to_string();
    let value = inner
        .next()
        .ok_or(ORErr::ParsingError("Getting value failed".into()))?;
    if inner.next().is_some() {
        unreachable!("Assignment statment still has unused pairs!");
    }
    let value = oduraja(value, globals)?;
    globals.set_variable(ident, value.clone());
    Ok(value)
}

fn ident(pair: Pair<Rule>, globals: &mut Context) -> LiteralResult {
    let ident = pair.as_str().to_string();
    globals.get_variable(&ident)
}

fn stmt_if(pair: Pair<Rule>, globals: &mut Context) -> LiteralResult {
    let mut inner = pair.clone().into_inner();
    let condition = inner
        .next()
        .ok_or(ORErr::ParsingError("Getting condition failed".into()))?;
    let block = inner
        .next()
        .ok_or(ORErr::ParsingError("Getting true block failed".into()))?;
    let condition = oduraja(condition, globals)?;
    if let Literal::Bool(is_true) = condition {
        if is_true {
            oduraja(block, globals)
        } else {
            match inner.next() {
                Some(block) => {
                    let result = oduraja(block, globals);
                    if inner.next().is_some() {
                        unreachable!("If statment still has unused pairs!");
                    }
                    result
                }
                None => no_op(pair, globals),
            }
        }
    } else {
        Err(ORErr::OperationIncompatibleError(
            "The conditional expression in `If` should always evaluvate to boolean value".into(),
        ))
    }
}

fn stmt_block(pair: Pair<Rule>, globals: &mut Context) -> LiteralResult {
    let mut results = vec![];
    for block in pair.into_inner() {
        if Rule::stmt_break == block.as_rule() {
            globals.push_interupt_break();
            return Ok(Literal::Array(results));
        } else if Rule::stmt_continue == block.as_rule() {
            globals.push_interupt_continue();
            return Ok(Literal::Array(results));
        } else if globals.has_loop_interupt() {
            return Ok(Literal::Array(results));
        } else {
            let result = oduraja(block, globals)?;
            results.push(result);
        }
    }
    Ok(Literal::Array(results))
}

fn stmt_for(pair: Pair<Rule>, globals: &mut Context) -> LiteralResult {
    let mut inner = pair.into_inner();
    let ident = inner
        .next()
        .ok_or(ORErr::ParsingError("Getting ident failed".into()))?;
    let array = inner
        .next()
        .ok_or(ORErr::ParsingError("Getting array failed".into()))?;
    let block = inner
        .next()
        .ok_or(ORErr::ParsingError("Getting block failed".into()))?;
    if inner.next().is_some() {
        unreachable!("For statment still has unused pairs!");
    }
    if let Literal::Array(array) = oduraja(array.clone(), globals)? {
        let mut results = vec![];
        for i in array {
            globals.set_variable(ident.as_str().to_string(), i);
            let result = oduraja(block.clone(), globals)?;
            results.push(result);
            let interupt = globals.pop_interupt();
            match interupt.kind {
                InteruptKind::None => (),
                InteruptKind::Continue => continue,
                InteruptKind::Break => return Ok(Literal::Array(results)),
                InteruptKind::Return => {
                    // push it back because it needs to be habdled by stmt_block
                    globals.push_interupt(interupt);
                    break;
                }
            }
        }
        Ok(Literal::Array(results))
    } else {
        Err(ORErr::OperationIncompatibleError(format!(
            "FOR loop requires array to iterate over. But found: {}",
            array
        )))
    }
}

fn stmt_while(pair: Pair<Rule>, globals: &mut Context) -> LiteralResult {
    let mut inner = pair.into_inner();
    let expr = inner
        .next()
        .ok_or(ORErr::ParsingError("Getting ident failed".into()))?;
    let block = inner
        .next()
        .ok_or(ORErr::ParsingError("Getting block failed".into()))?;
    if inner.next().is_some() {
        unreachable!("While statment still has unused pairs!");
    }
    let mut results = vec![];
    loop {
        if let Literal::Bool(should_loop) = oduraja(expr.clone(), globals)? {
            if !should_loop {
                return Ok(Literal::Array(results));
            }
            let result = oduraja(block.clone(), globals)?;
            results.push(result);
            let interupt = globals.pop_interupt();
            match interupt.kind {
                InteruptKind::None => (),
                InteruptKind::Continue => continue,
                InteruptKind::Break => return Ok(Literal::Array(results)),
                InteruptKind::Return => {
                    // push it back because it needs to be habdled by stmt_block
                    globals.push_interupt(interupt);
                    return Ok(Literal::Array(results));
                }
            }
            return Ok(Literal::Array(results));
        } else {
            return Err(ORErr::OperationIncompatibleError(
                "While loop requires expressions to return boolean".into(),
            ));
        }
    }
}

fn stmt_try(pair: Pair<Rule>, globals: &mut Context) -> LiteralResult {
    let mut inner = pair.into_inner();
    let try_block = inner
        .next()
        .ok_or(ORErr::ParsingError("Getting try block failed".into()))?;
    let catch_block = inner
        .next()
        .ok_or(ORErr::ParsingError("Getting catch block failed".into()))?;
    if inner.next().is_some() {
        unreachable!("While statment still has unused pairs!");
    }
    let try_result = oduraja(try_block, globals);
    if try_result.is_ok() {
        try_result
    } else {
        oduraja(catch_block, globals)
    }
}

fn stmt_define(pair: Pair<Rule>, globals: &mut Context) -> LiteralResult {
    let mut inner = pair.into_inner();
    let stmt_header = inner.next().ok_or(ORErr::ParsingError(
        "Getting statement header failed".into(),
    ))?;
    let stmt_block = inner
        .next()
        .ok_or(ORErr::ParsingError("Getting statement block failed".into()))?;
    if inner.next().is_some() {
        unreachable!("Statement definition still has unused pairs!");
    }
    globals.push_statement(stmt_header, stmt_block);
    Ok(Literal::None)
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
        Rule::stmt_define => stmt_define,
        Rule::stmt_assign => stmt_assign,
        Rule::param_invoke => pratt_parse,
        Rule::param_define => todo!(),
        Rule::COMMENT => todo!(),
        Rule::comment_block => todo!(),
        Rule::comment_line => todo!(),
        Rule::expression => pratt_parse,
        Rule::infix => todo!(),
        Rule::expression_inner => todo!(),
        Rule::unary => pratt_parse,
        Rule::dot_path => todo!(),
        Rule::literal => todo!(),
        Rule::array => array,
        Rule::ident => ident,
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
        Rule::stmt_if => stmt_if,
        Rule::stmt_else => stmt_block,
        Rule::reserved_parts => todo!(),
        Rule::IN => todo!(),
        Rule::stmt_for => stmt_for,
        Rule::stmt_while => stmt_while,
        Rule::stmt_try => stmt_try,
        Rule::stmt_catch => stmt_block,
        Rule::stmt_break => todo!(),
        Rule::stmt_continue => todo!(),
        Rule::stmt_return => todo!(),
        Rule::primary => todo!(),
        Rule::seperator => no_op,
        Rule::stmt_block => stmt_block,
        Rule::stmt_header => todo!(),
    };
    op(pair, globals)
}

fn main() {
    let source = read_to_string("examples/02-syntaxes.oduraja").unwrap();
    match parser::Parser::parse(Rule::oduraja, &source) {
        Ok(tree) => {
            let mut context = Context::default();
            context.init_statements();
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
