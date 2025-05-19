use crate::lexer::token::{ Token, TokenType }; // Adjust path as needed
use crate::errors::lex_errors::LexError;

pub struct Lexer {
    source: Vec<char>,
    start: usize,
    current: usize,
    line: i32,
    column: i32,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            start: 0,
            current: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();

        while !self.is_at_end() {
            self.start = self.current;
            match self.scan_token() {
                Ok(Some(token)) => tokens.push(token),
                Ok(None) => (),
                Err(err) => return Err(err),
            }
        }
        // Add EOF token
        tokens.push(Token::new(TokenType::Eof, "".into(), self.line, self.column));
        Ok(tokens)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> char {
        let ch = self.source[self.current];
        self.current += 1;
        self.column += 1;
        ch
    }

    fn peek(&self) -> char {
        if self.is_at_end() { '\0' } else { self.source[self.current] }
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.source[self.current] != expected {
            return false;
        }
        self.current += 1;
        self.column += 1;
        true
    }

    fn scan_token(&mut self) -> Result<Option<Token>, LexError> {
        let ch = self.advance();
        match ch {
            // Whitespace and newlines
            ' ' | '\r' | '\t' => Ok(None),
            '\n' => {
                self.line += 1;
                self.column = 1;
                Ok(None)
            }

            // Single and double character tokens
            '(' => Ok(Some(self.make_token(TokenType::LeftParen))),
            ')' => Ok(Some(self.make_token(TokenType::RightParen))),
            '{' => Ok(Some(self.make_token(TokenType::LeftBrace))),
            '}' => Ok(Some(self.make_token(TokenType::RightBrace))),
            ',' => Ok(Some(self.make_token(TokenType::Comma))),
            ';' => Ok(Some(self.make_token(TokenType::Semicolon))),
            '+' => Ok(Some(self.make_token(TokenType::Plus))),
            '-' => Ok(Some(self.make_token(TokenType::Minus))),
            '*' => Ok(Some(self.make_token(TokenType::Star))),
            '/' => Ok(Some(self.make_token(TokenType::Slash))),
            '%' => Ok(Some(self.make_token(TokenType::Percent))),
            '!' => {
                let token_type = if self.match_char('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                Ok(Some(self.make_token(token_type)))
            }
            '=' => {
                let token_type = if self.match_char('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                Ok(Some(self.make_token(token_type)))
            }
            '<' => {
                let token_type = if self.match_char('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                Ok(Some(self.make_token(token_type)))
            }
            '>' => {
                let token_type = if self.match_char('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                Ok(Some(self.make_token(token_type)))
            }
            '&' => {
                if self.match_char('&') {
                    Ok(Some(self.make_token(TokenType::AndAnd)))
                } else {
                    return Err(LexError::new("Unexpected character '&'", self.line, self.column));
                }
            }
            '|' => {
                if self.match_char('|') {
                    Ok(Some(self.make_token(TokenType::OrOr)))
                } else {
                    return Err(LexError::new("Unexpected character '|'", self.line, self.column));
                }
            }
            '?' => Ok(Some(self.make_token(TokenType::Question))),
            ':' => Ok(Some(self.make_token(TokenType::Colon))),

            ch if ch.is_ascii_digit() => Ok(Some(self.number())),
            ch if is_identifier_start(ch) => Ok(Some(self.identifier())),

            _ => Err(LexError::new(format!("Unexpected character '{}'", ch),self.line,self.column))
        }
    }

    fn number(&mut self) -> Token {
        while self.peek().is_ascii_digit() {
            self.advance();
        }
        let value = self.source[self.start..self.current].iter().collect::<String>();
        Token::new(TokenType::Number(value.parse().unwrap()), value, self.line, self.column)
    }

    fn identifier(&mut self) -> Token {
        while is_identifier_part(self.peek()) {
            self.advance();
        }

        let value = self.source[self.start..self.current].iter().collect::<String>();
        let token_type = match value.as_str() {
            "let" => TokenType::Let,
            "function" => TokenType::Function,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "while" => TokenType::While,
            "for" => TokenType::For,
            "return" => TokenType::Return,
            _ => TokenType::Identifier(value.clone()),
        };

        Token::new(token_type, value, self.line, self.column)
    }

    fn make_token(&self, token_type: TokenType) -> Token {
        let lexeme = self.source[self.start..self.current].iter().collect();
        Token::new(token_type, lexeme, self.line, self.column)
    }
}

fn is_identifier_start(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

fn is_identifier_part(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}
