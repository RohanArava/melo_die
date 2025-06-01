use crate::lexer::token::{ Token, TokenType };
use crate::parser::ast::{
    Constant,
    Expression,
    Operator,
    Statement,
    CodeBlock,
    FunctionDeclaration,
    Program,
};
use crate::errors::parse_errors::ParseError;

#[derive(Copy, Clone, PartialEq, PartialOrd, Debug)]
pub enum Precedence {
    Lowest = 0,
    Ternary,
    LogicalOr,
    LogicalAnd,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
}

pub fn get_precedence_from_token(token_type: &TokenType) -> Precedence {
    match token_type {
        TokenType::OrOr => Precedence::LogicalOr,
        TokenType::AndAnd => Precedence::LogicalAnd,
        TokenType::EqualEqual | TokenType::BangEqual => Precedence::Equality,
        TokenType::Less | TokenType::LessEqual | TokenType::Greater | TokenType::GreaterEqual =>
            Precedence::Comparison,
        TokenType::Plus | TokenType::Minus => Precedence::Term,
        TokenType::Star | TokenType::Slash | TokenType::Percent => Precedence::Factor,
        TokenType::Question => Precedence::Ternary,
        _ => Precedence::Lowest,
    }
}

pub fn token_to_unary_operator(token_type: &TokenType) -> Option<Operator> {
    match token_type {
        TokenType::Bang => Some(Operator::Not),
        TokenType::Minus => Some(Operator::Negate),
        _ => None,
    }
}

pub fn token_to_binary_operator(token_type: &TokenType) -> Option<Operator> {
    match token_type {
        TokenType::Plus => Some(Operator::Add),
        TokenType::Minus => Some(Operator::Sub),
        TokenType::Star => Some(Operator::Mul),
        TokenType::Slash => Some(Operator::Div),
        TokenType::Percent => Some(Operator::Mod),

        TokenType::EqualEqual => Some(Operator::Equal),
        TokenType::BangEqual => Some(Operator::NotEqual),
        TokenType::Less => Some(Operator::LessThan),
        TokenType::LessEqual => Some(Operator::LessThanEqual),
        TokenType::Greater => Some(Operator::GreaterThan),
        TokenType::GreaterEqual => Some(Operator::GreaterThanEqual),

        TokenType::AndAnd => Some(Operator::And),
        TokenType::OrOr => Some(Operator::Or),

        _ => None,
    }
}

pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Parser {
            tokens,
            current: 0,
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len() - 1
    }

    fn advance(&mut self) -> Option<&Token> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.tokens.get(self.current - 1)
    }

    fn expect(&mut self, expected: TokenType) -> Result<&Token, ParseError> {
        match self.peek() {
            Some(token) if token.token_type == expected => Ok(self.advance().unwrap()),
            Some(token) =>
                Err(
                    ParseError::new(
                        format!("Expected {:?} but found {:?}", expected, token.token_type),
                        token.line,
                        token.column
                    )
                ),
            None =>
                Err(
                    ParseError::new(
                        format!("Expected {:?} but found end of input", expected),
                        self.tokens.last().map_or(0, |t| t.line),
                        self.tokens.last().map_or(0, |t| t.column)
                    )
                ),
        }
    }

    fn check(&self, token_type: TokenType) -> bool {
        self.peek().map_or(false, |t| t.token_type == token_type)
    }

    fn match_token(&mut self, token_type: TokenType) -> bool {
        if self.check(token_type) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub fn parse(&mut self) -> Result<Program, ParseError> {
        let mut functions = Vec::new();

        while !self.is_at_end() {
            if self.check(TokenType::Function) {
                functions.push(self.parse_function_declaration()?);
            } else {
                println!("{:#?}", self.peek());
                return Err(
                    ParseError::new(
                        "Expected function declaration at top level".to_string(),
                        self.peek().unwrap().line,
                        self.peek().unwrap().column
                    )
                );
            }
        }

        Ok(Program { functions })
    }

    fn parse_function_declaration(&mut self) -> Result<FunctionDeclaration, ParseError> {
        self.expect(TokenType::Function)?;

        let name = match self.advance() {
            Some(token) =>
                match &token.token_type {
                    TokenType::Identifier(name) => name.clone(),
                    _ => {
                        return Err(
                            ParseError::new(
                                "Expected function name".to_string(),
                                token.line,
                                token.column
                            )
                        );
                    }
                }
            None => {
                return Err(ParseError::new("Expected function name".to_string(), 0, 0));
            }
        };

        self.expect(TokenType::LeftParen)?;

        let mut arguments = Vec::new();
        if !self.check(TokenType::RightParen) {
            loop {
                match self.advance() {
                    Some(token) =>
                        match &token.token_type {
                            TokenType::Identifier(param_name) => {
                                arguments.push(param_name.clone());
                            }
                            _ => {
                                return Err(
                                    ParseError::new(
                                        "Expected parameter name".to_string(),
                                        token.line,
                                        token.column
                                    )
                                );
                            }
                        }
                    None => {
                        return Err(ParseError::new("Expected parameter name".to_string(), 0, 0));
                    }
                }

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }

        self.expect(TokenType::RightParen)?;

        let returns_value = match self.advance() {
            Some(token) =>
                match &token.token_type {
                    TokenType::Int => true,
                    TokenType::Void => false,
                    _ => {
                        return Err(
                            ParseError::new(
                                "Expected return type".to_string(),
                                token.line,
                                token.column
                            )
                        );
                    }
                }
            None => {
                return Err(ParseError::new("Expected return type".to_string(), 0, 0));
            }
        };

        if self.match_token(TokenType::LeftBrace) {
            let body = self.parse_code_block()?;

            return Ok(FunctionDeclaration {
                name,
                arguments,
                body: Some(body),
                returns_value,
            });
        } else {
            return Ok(FunctionDeclaration {
                name,
                arguments,
                body: None,
                returns_value,
            });
        }
    }

    fn parse_code_block(&mut self) -> Result<CodeBlock, ParseError> {
        let mut statements = Vec::new();

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.parse_statement()?);
        }

        self.expect(TokenType::RightBrace)?;
        Ok(CodeBlock(statements))
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.peek() {
            Some(token) =>
                match &token.token_type {
                    TokenType::Let => self.parse_variable_declaration(),
                    TokenType::If => self.parse_if_statement(),
                    TokenType::While => self.parse_while_statement(),
                    TokenType::For => self.parse_for_statement(),
                    TokenType::Return => self.parse_return_statement(),
                    TokenType::Identifier(_) => {
                        // When token is an identifier, it ca either be an assignment or an expression
                        // So we save the checkpoint, try parsing as if it was an assignment,
                        // If not we restore to checkpoint and continue parsing as expression
                        let checkpoint = self.current;
                        if let Ok(name) = self.parse_identifier() {
                            if self.match_token(TokenType::Equal) {
                                let expression = self.parse_expression(Precedence::Lowest)?;
                                return Ok(Statement::VariableAssignment { name, expression });
                            }
                        }

                        self.current = checkpoint;
                        let expression = self.parse_expression(Precedence::Lowest)?;
                        Ok(Statement::Expression { expression })
                    }
                    _ => {
                        let expression = self.parse_expression(Precedence::Lowest)?;
                        Ok(Statement::Expression { expression })
                    }
                }
            None => Err(ParseError::new("Expected statement".to_string(), 0, 0)),
        }
    }

    fn parse_identifier(&mut self) -> Result<String, ParseError> {
        match self.advance() {
            Some(token) =>
                match &token.token_type {
                    TokenType::Identifier(name) => Ok(name.clone()),
                    _ =>
                        Err(
                            ParseError::new(
                                "Expected identifier".to_string(),
                                token.line,
                                token.column
                            )
                        ),
                }
            None => Err(ParseError::new("Expected identifier".to_string(), 0, 0)),
        }
    }

    fn parse_variable_declaration(&mut self) -> Result<Statement, ParseError> {
        self.expect(TokenType::Let)?;
        let name = self.parse_identifier()?;

        let expression = if self.match_token(TokenType::Equal) {
            Some(self.parse_expression(Precedence::Lowest)?)
        } else {
            None
        };

        Ok(Statement::VariableDeclaration { name, expression })
    }

    fn parse_if_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect(TokenType::If)?;
        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect(TokenType::LeftBrace)?;
        let then_block = self.parse_code_block()?;

        let else_block = if self.match_token(TokenType::Else) {
            self.expect(TokenType::LeftBrace)?;
            Some(self.parse_code_block()?)
        } else {
            None
        };

        Ok(Statement::IfStatement {
            condition,
            then_block,
            else_block,
        })
    }

    fn parse_while_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect(TokenType::While)?;
        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect(TokenType::LeftBrace)?;
        let body = self.parse_code_block()?;

        Ok(Statement::WhileStatement { condition, body })
    }

    fn parse_for_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect(TokenType::For)?;

        let initializer = self.parse_statement()?;
        self.expect(TokenType::Semicolon)?;

        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect(TokenType::Semicolon)?;

        let increment = match self.parse_statement()? {
            Statement::VariableAssignment { name, expression } =>
                Statement::VariableAssignment { name, expression },
            _ => {
                return Err(
                    ParseError::new(
                        "Expected assignment in for loop increment".to_string(),
                        self.peek().map_or(0, |t| t.line),
                        self.peek().map_or(0, |t| t.column)
                    )
                );
            }
        };

        self.expect(TokenType::LeftBrace)?;
        let body_statements = self.parse_code_block()?.0;
        let body = CodeBlock(body_statements);

        Ok(Statement::ForStatement {
            initializer: Box::new(initializer),
            condition,
            increment: Box::new(increment),
            body,
        })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect(TokenType::Return)?;

        let value = if self.check(TokenType::RightBrace) || self.is_at_end() {
            return Err(
                ParseError::new(
                    "Expected return value".to_string(),
                    self.peek().unwrap().line,
                    self.peek().unwrap().column
                )
            );
        } else if self.check(TokenType::Void) {
            self.advance();
            None
        } else {
            Some(self.parse_expression(Precedence::Lowest)?)
        };

        Ok(Statement::Return { value })
    }

    pub fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        return match self.advance() {
            Some(current_token) => {
                let token = current_token.clone();
                match token.token_type {
                    TokenType::Number(value) => Ok(Expression::Constant(Constant::Integer(value))),
                    TokenType::Identifier(name) => {
                        if self.match_token(TokenType::LeftParen) {
                            let mut args = vec![];
                            if !self.check(TokenType::RightParen) {
                                loop {
                                    if self.check(TokenType::RightParen) {
                                        return Err(
                                            ParseError::new(
                                                "Expected expression after comma, but found `)`".to_string(),
                                                self.peek().unwrap().line,
                                                self.peek().unwrap().column
                                            )
                                        );
                                    }

                                    args.push(self.parse_expression(Precedence::Lowest)?);

                                    if !self.match_token(TokenType::Comma) {
                                        break;
                                    }
                                }
                            }
                            self.expect(TokenType::RightParen)?;
                            Ok(Expression::FunctionCall {
                                function_name: name,
                                arguments: args,
                            })
                        } else {
                            Ok(Expression::Variable(name))
                        }
                    }
                    TokenType::Minus | TokenType::Bang => {
                        let op = token_to_unary_operator(&token.token_type).ok_or(
                            ParseError::new(
                                format!("Expected a '-' or '!' but found {:?}", token.token_type),
                                token.line,
                                token.column
                            )
                        )?;
                        let right = self.parse_expression(Precedence::Unary)?;
                        Ok(Expression::UnaryExpression {
                            operator: op,
                            expression: Box::new(right),
                        })
                    }
                    TokenType::LeftParen => {
                        let expr = self.parse_expression(Precedence::Lowest)?;
                        self.expect(TokenType::RightParen)?;
                        Ok(expr)
                    }
                    _ =>
                        Err(
                            ParseError::new(
                                format!("Unexpected token '{:?}' found", token.lexeme),
                                token.line,
                                token.column
                            )
                        ),
                }
            }
            None =>
                Err(
                    ParseError::new(
                        "Expected a token but found end of input",
                        self.tokens.last().map_or(0, |t| t.line),
                        self.tokens.last().map_or(0, |t| t.column)
                    )
                ),
        };
    }

    pub fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        let mut left = self.parse_primary()?;

        while !self.is_at_end() {
            let current_token = match self.peek() {
                Some(token) => token,
                None => {
                    break;
                }
            };

            let token_precedence = get_precedence_from_token(&current_token.token_type);

            if precedence >= token_precedence {
                break;
            }

            if current_token.token_type == TokenType::Question {
                self.advance();
                let true_expr = self.parse_expression(Precedence::Lowest)?;
                self.expect(TokenType::Colon)?;
                let false_expr = self.parse_expression(Precedence::Ternary)?;

                left = Expression::TernaryExpression {
                    condition: Box::new(left),
                    true_expression: Box::new(true_expr),
                    false_expression: Box::new(false_expr),
                };
            } else if let Some(operator) = token_to_binary_operator(&current_token.token_type) {
                self.advance();
                let right = self.parse_expression(token_precedence)?;

                left = Expression::BinaryExpression {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }

        Ok(left)
    }
}
