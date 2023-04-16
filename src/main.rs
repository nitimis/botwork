use botwork::core::{
    eval::{botwork, Context},
    grammar::{BWParser, Rule},
};
use clap::Parser as Clap;
use pest::Parser;
use std::{fs::read_to_string, path::PathBuf};

/// Simple program to greet a person
#[derive(Clap, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Name of the botwork file to run
    #[arg(short, long)]
    file: PathBuf,
}

fn main() -> Result<(), std::io::Error> {
    let args = Args::parse();
    let source = read_to_string(args.file)?;
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
    Ok(())
}
