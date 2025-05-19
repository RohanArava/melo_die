use super::lexer::Lexer;
use super::token::{ TokenType };

fn token_types_from_source(source: &str) -> Vec<TokenType> {
    let mut lexer = Lexer::new(source);
    let result = lexer.tokenize();
    assert!(result.is_ok());
    let tokens = result.unwrap();

    tokens
        .into_iter()
        .map(|t| t.token_type)
        .collect()
}

#[test]
fn test_basic_operators() {
    let input = "+ - * / % = == != < <= > >= && || !";
    let expected = vec![
        TokenType::Plus,
        TokenType::Minus,
        TokenType::Star,
        TokenType::Slash,
        TokenType::Percent,
        TokenType::Equal,
        TokenType::EqualEqual,
        TokenType::BangEqual,
        TokenType::Less,
        TokenType::LessEqual,
        TokenType::Greater,
        TokenType::GreaterEqual,
        TokenType::AndAnd,
        TokenType::OrOr,
        TokenType::Bang,
        TokenType::Eof
    ];
    assert_eq!(token_types_from_source(input), expected);
}

#[test]
fn test_keywords_and_identifiers() {
    let input = "let function if else while for return foo _bar var123";
    let expected = vec![
        TokenType::Let,
        TokenType::Function,
        TokenType::If,
        TokenType::Else,
        TokenType::While,
        TokenType::For,
        TokenType::Return,
        TokenType::Identifier("foo".to_string()),
        TokenType::Identifier("_bar".to_string()),
        TokenType::Identifier("var123".to_string()),
        TokenType::Eof
    ];
    assert_eq!(token_types_from_source(input), expected);
}

#[test]
fn test_integers() {
    let input = "0 1 42 99999";
    let expected = vec![
        TokenType::Number(0),
        TokenType::Number(1),
        TokenType::Number(42),
        TokenType::Number(99999),
        TokenType::Eof
    ];
    assert_eq!(token_types_from_source(input), expected);
}

#[test]
fn test_grouping_and_punctuation() {
    let input = "( ) { } , ;";
    let expected = vec![
        TokenType::LeftParen,
        TokenType::RightParen,
        TokenType::LeftBrace,
        TokenType::RightBrace,
        TokenType::Comma,
        TokenType::Semicolon,
        TokenType::Eof
    ];
    assert_eq!(token_types_from_source(input), expected);
}

#[test]
fn test_ternary_operator() {
    let input = "a ? b : c";
    let expected = vec![
        TokenType::Identifier("a".to_string()),
        TokenType::Question,
        TokenType::Identifier("b".to_string()),
        TokenType::Colon,
        TokenType::Identifier("c".to_string()),
        TokenType::Eof
    ];
    assert_eq!(token_types_from_source(input), expected);
}

#[test]
fn test_assignment_and_declaration() {
    let input = "let x = 10; x = x + 1;";
    let expected = vec![
        TokenType::Let,
        TokenType::Identifier("x".to_string()),
        TokenType::Equal,
        TokenType::Number(10),
        TokenType::Semicolon,
        TokenType::Identifier("x".to_string()),
        TokenType::Equal,
        TokenType::Identifier("x".to_string()),
        TokenType::Plus,
        TokenType::Number(1),
        TokenType::Semicolon,
        TokenType::Eof
    ];
    assert_eq!(token_types_from_source(input), expected);
}

#[test]
fn test_function_definition() {
    let input = "function add(a, b) { return a + b; }";
    let expected = vec![
        TokenType::Function,
        TokenType::Identifier("add".to_string()),
        TokenType::LeftParen,
        TokenType::Identifier("a".to_string()),
        TokenType::Comma,
        TokenType::Identifier("b".to_string()),
        TokenType::RightParen,
        TokenType::LeftBrace,
        TokenType::Return,
        TokenType::Identifier("a".to_string()),
        TokenType::Plus,
        TokenType::Identifier("b".to_string()),
        TokenType::Semicolon,
        TokenType::RightBrace,
        TokenType::Eof
    ];
    assert_eq!(token_types_from_source(input), expected);
}

#[test]
fn test_if_else_loop() {
    let input =
        r#"
        if x > 0 {
            return x;
        } else {
            let y = 0;
            while y < 10 {
                y = y + 1;
            }
            return y;
        }
    "#;

    let expected = vec![
        TokenType::If,
        TokenType::Identifier("x".to_string()),
        TokenType::Greater,
        TokenType::Number(0),
        TokenType::LeftBrace,
        TokenType::Return,
        TokenType::Identifier("x".to_string()),
        TokenType::Semicolon,
        TokenType::RightBrace,
        TokenType::Else,
        TokenType::LeftBrace,
        TokenType::Let,
        TokenType::Identifier("y".to_string()),
        TokenType::Equal,
        TokenType::Number(0),
        TokenType::Semicolon,
        TokenType::While,
        TokenType::Identifier("y".to_string()),
        TokenType::Less,
        TokenType::Number(10),
        TokenType::LeftBrace,
        TokenType::Identifier("y".to_string()),
        TokenType::Equal,
        TokenType::Identifier("y".to_string()),
        TokenType::Plus,
        TokenType::Number(1),
        TokenType::Semicolon,
        TokenType::RightBrace,
        TokenType::Return,
        TokenType::Identifier("y".to_string()),
        TokenType::Semicolon,
        TokenType::RightBrace,
        TokenType::Eof
    ];

    assert_eq!(token_types_from_source(input), expected);
}

#[test]
fn test_unexpected_character() {
    let source = "@";
    let mut lexer = Lexer::new(source);
    let result = lexer.tokenize();
    assert!(result.is_err());

    let err = result.unwrap_err();
    assert_eq!(err.message, "Unexpected character '@'");
    assert_eq!(err.line, 1);
}
