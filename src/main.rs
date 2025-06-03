use clap::{ ArgAction, Parser };

pub mod lexer;
pub mod errors;
pub mod parser;
pub mod codegen;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Source file to compile
    #[arg()]
    source: String,

    /// Output file name
    #[arg(short, long, default_value_t = String::from(""))]
    output: String,

    /// Produce LLVM IR (.ll file)
    #[arg(long = "ll", action = ArgAction::SetTrue)]
    produce_ll: bool,

    /// Produce assembly (.s file)
    #[arg(short = 'S', action = ArgAction::SetTrue)]
    to_assembly: bool,

    /// Compile to object file only (.o file)
    #[arg(short = 'c', action = ArgAction::SetTrue)]
    compile_only: bool,
}

fn main() {
    let args = Args::parse();

    let source = std::fs::read_to_string(&args.source).expect("Failed to read file");
    let mut lexer = lexer::lexer::Lexer::new(&source);
    let tokens = match lexer.tokenize() {
        Ok(tokens) => tokens,
        Err(err) => panic!("Lexer Failed: {:?}", err),
    };

    let mut parser = parser::parser::Parser::new(&tokens);
    let program = parser.parse().unwrap();

    let mut analyzer = parser::semantic::SemanticAnalyzer::new();
    match analyzer.analyze(&program) {
        Ok(()) => println!("Program is semantically valid!"),
        Err(errors) => {
            for error in errors {
                println!("Error: {:?}", error);
            }
            return;
        }
    }

    let input_path = std::path::Path::new(&args.source);
    let output_name = if args.output.is_empty() {
        input_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("output")
    } else {
        std::path::Path
            ::new(&args.output)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("output")
    };

    let output_extension = if args.output.is_empty() {
        None
    } else {
        match std::path::Path::new(&args.output).extension() {
            Some(ext) => ext.to_str(),
            None => None
        }
    };

    if args.compile_only {
        match
            codegen::compile::compile_to_object(
                &program,
                output_name,
                output_extension,
                args.produce_ll
            )
        {
            Ok(()) => println!("Compilation successful!"),
            Err(err) => println!("Compilation failed: {}", err),
        }
    } else if args.to_assembly {
        match
            codegen::compile::compile_to_assembly(
                &program,
                output_name,
                output_extension,
                args.produce_ll
            )
        {
            Ok(()) => println!("Compilation successful!"),
            Err(err) => println!("Compilation failed: {}", err),
        }
    } else {
        match
            codegen::compile::compile_to_executable(
                &program,
                output_name,
                output_extension,
                args.produce_ll
            )
        {
            Ok(()) => println!("Compilation successful!"),
            Err(err) => println!("Compilation failed: {}", err),
        }
    }
}
