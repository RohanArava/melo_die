#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    // Single-character symbols
    LeftParen,      // (
    RightParen,     // )
    LeftBrace,      // {
    RightBrace,     // }
    Semicolon,      // ;
    Comma,          // =
    Equal,          // =
    Plus,           // +
    Minus,          // -
    Star,           // *
    Slash,          // /
    Percent,        // %

    // Comparison operators
    EqualEqual,     // ==
    BangEqual,      // !=
    Less,           // <
    LessEqual,      // <=
    Greater,        // >
    GreaterEqual,   // >=

    // Logical operators
    AndAnd,         // &&
    OrOr,           // ||

    // Unary
    Bang,           // !

    // Ternary
    Question,       // ?
    Colon,          // :

    // Literals
    Identifier(String),
    Number(i64),

    // Keywords
    Let,
    Function,
    If,
    Else,
    While,
    For,
    Return,

    // End of file
    Eof,
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: i32,
    pub column: i32,
}


impl Token{
    pub fn new(token_type: TokenType, lexeme: String, line: i32, column: i32)-> Token{
        Token{token_type, lexeme, line, column}
    }
}