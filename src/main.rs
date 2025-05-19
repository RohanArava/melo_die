fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2{
        panic!("Not enough arguments. Please provide file name as first argument.");
    }
    let source = std::fs::read_to_string(&args[1]).expect("Failed to read file");

    println!("{:#?}", source);
}
