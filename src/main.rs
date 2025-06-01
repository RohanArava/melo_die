pub mod lexer;
pub mod errors;
pub mod parser;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        panic!("Not enough arguments. Please provide file name as first argument.");
    }
    
    let source = std::fs::read_to_string(&args[1]).expect("Failed to read file");
    let mut lexer = lexer::lexer::Lexer::new(&source);
    let tokens = match lexer.tokenize() {
        Ok(tokens) => tokens,
        Err(err) => panic!("Lexer Failed: {:?}", err),
    };
    
    let mut parser = parser::parser::Parser::new(&tokens);
    let program = parser.parse().unwrap();
    println!("{:#?}", program);
}
