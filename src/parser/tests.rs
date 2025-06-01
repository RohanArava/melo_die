// Generated tests

#[cfg(test)]
mod parser_tests {
    use crate::lexer::token::{ Token, TokenType };
    use crate::parser::ast::{
        Constant,
        Expression,
        Operator,
        Statement,
        Program,
    };
    use crate::parser::parser::Parser;
    use crate::errors::parse_errors::ParseError;

    // Helper function to create tokens for testing
    fn create_tokens(token_types: Vec<TokenType>) -> Vec<Token> {
        token_types
            .into_iter()
            .enumerate()
            .map(|(i, token_type)| {
                Token {
                    token_type,
                    lexeme: format!("token_{}", i),
                    line: 1,
                    column: i as i32,
                }
            })
            .collect()
    }

    fn parse_from_tokens(tokens: Vec<Token>) -> Result<Program, ParseError> {
        let mut parser = Parser::new(&tokens);
        parser.parse()
    }

    #[test]
    fn test_empty_function() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("test".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.functions.len(), 1);
        assert_eq!(program.functions[0].name, "test");
        assert_eq!(program.functions[0].arguments.len(), 0);
        assert_eq!(program.functions[0].body.as_ref().expect("Function body not found").0.len(), 0);
    }

    #[test]
    fn test_function_with_parameters() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("add".to_string()),
                TokenType::LeftParen,
                TokenType::Identifier("a".to_string()),
                TokenType::Comma,
                TokenType::Identifier("b".to_string()),
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.functions[0].arguments, vec!["a", "b"]);
    }

    #[test]
    fn test_variable_declaration() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("test".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::Let,
                TokenType::Identifier("x".to_string()),
                TokenType::Equal,
                TokenType::Number(42),
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_ok());
        let program = result.unwrap();
        let body = match &program.functions[0].body {
            Some(body) => body,
            None => panic!("Function body not found")
        };
        assert_eq!(body.0.len(), 1);

        match &body.0[0] {
            Statement::VariableDeclaration { name, expression } => {
                assert_eq!(name, "x");
                assert!(expression.is_some());
            }
            _ => panic!("Expected variable declaration"),
        }
    }

    #[test]
    fn test_variable_assignment() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("test".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::Identifier("x".to_string()),
                TokenType::Equal,
                TokenType::Number(10),
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_ok());
        let program = result.unwrap();

        match &program.functions[0].body.as_ref().expect("Function body not found").0[0] {
            Statement::VariableAssignment { name, expression: _ } => {
                assert_eq!(name, "x");
            }
            _ => panic!("Expected variable assignment"),
        }
    }

    #[test]
    fn test_return_statement() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("test".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::Return,
                TokenType::Number(5),
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_ok());
        let program = result.unwrap();

        match &program.functions[0].body.as_ref().expect("Function body not found").0[0] {
            Statement::Return { value } => {
                assert!(value.is_some());
            }
            _ => panic!("Expected return statement"),
        }

        assert!(program.functions[0].returns_value);
    }

    #[test]
    fn test_if_statement() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("test".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::If,
                TokenType::Number(1),
                TokenType::LeftBrace,
                TokenType::Return,
                TokenType::Number(10),
                TokenType::RightBrace,
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_ok());
        let program = result.unwrap();

        match &program.functions[0].body.as_ref().expect("Function body not found").0[0] {
            Statement::IfStatement { condition: _, then_block, else_block } => {
                assert_eq!(then_block.0.len(), 1);
                assert!(else_block.is_none());
            }
            _ => panic!("Expected if statement"),
        }
    }

    #[test]
    fn test_if_else_statement() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("test".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::If,
                TokenType::Number(1),
                TokenType::LeftBrace,
                TokenType::Return,
                TokenType::Number(10),
                TokenType::RightBrace,
                TokenType::Else,
                TokenType::LeftBrace,
                TokenType::Return,
                TokenType::Number(20),
                TokenType::RightBrace,
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_ok());
        let program = result.unwrap();

        match &program.functions[0].body.as_ref().expect("Function body not found").0[0] {
            Statement::IfStatement { condition: _, then_block, else_block } => {
                assert_eq!(then_block.0.len(), 1);
                assert!(else_block.is_some());
                assert_eq!(else_block.as_ref().unwrap().0.len(), 1);
            }
            _ => panic!("Expected if-else statement"),
        }
    }

    #[test]
    fn test_while_statement() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("test".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::While,
                TokenType::Number(1),
                TokenType::LeftBrace,
                TokenType::Return,
                TokenType::Number(5),
                TokenType::RightBrace,
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_ok());
        let program = result.unwrap();

        match &program.functions[0].body.as_ref().expect("Function body not found").0[0] {
            Statement::WhileStatement { condition: _, body } => {
                assert_eq!(body.0.len(), 1);
            }
            _ => panic!("Expected while statement"),
        }
    }

    #[test]
    fn test_for_statement() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("test".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::For,
                TokenType::Let,
                TokenType::Identifier("i".to_string()),
                TokenType::Equal,
                TokenType::Number(0),
                TokenType::Semicolon,
                TokenType::Identifier("i".to_string()),
                TokenType::Less,
                TokenType::Number(10),
                TokenType::Semicolon,
                TokenType::Identifier("i".to_string()),
                TokenType::Equal,
                TokenType::Identifier("i".to_string()),
                TokenType::Plus,
                TokenType::Number(1),
                TokenType::LeftBrace,
                TokenType::RightBrace,
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_ok());
        let program = result.unwrap();

        match &program.functions[0].body.as_ref().expect("Function body not found").0[0] {
            Statement::ForStatement { initializer: _, condition: _, increment: _, body } => {
                assert_eq!(body.0.len(), 0);
            }
            _ => panic!("Expected for statement"),
        }
    }

    #[test]
    fn test_binary_expressions() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("test".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::Return,
                TokenType::Number(1),
                TokenType::Plus,
                TokenType::Number(2),
                TokenType::Star,
                TokenType::Number(3),
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_ok());
        let program = result.unwrap();

        match &program.functions[0].body.as_ref().expect("Function body not found").0[0] {
            Statement::Return { value: Some(expr) } => {
                match expr {
                    Expression::BinaryExpression { left: _, operator, right: _ } => {
                        assert_eq!(*operator, Operator::Add);
                    }
                    _ => panic!("Expected binary expression"),
                }
            }
            _ => panic!("Expected return with expression"),
        }
    }

    #[test]
    fn test_unary_expressions() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("test".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::Return,
                TokenType::Minus,
                TokenType::Number(5),
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_ok());
        let program = result.unwrap();

        match &program.functions[0].body.as_ref().expect("Function body not found").0[0] {
            Statement::Return { value: Some(expr) } => {
                match expr {
                    Expression::UnaryExpression { operator, expression: _ } => {
                        assert_eq!(*operator, Operator::Negate);
                    }
                    _ => panic!("Expected unary expression"),
                }
            }
            _ => panic!("Expected return with expression"),
        }
    }

    #[test]
    fn test_ternary_expression() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("test".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::Return,
                TokenType::Number(1),
                TokenType::Question,
                TokenType::Number(10),
                TokenType::Colon,
                TokenType::Number(20),
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_ok());
        let program = result.unwrap();

        match &program.functions[0].body.as_ref().expect("Function body not found").0[0] {
            Statement::Return { value: Some(expr) } => {
                match expr {
                    Expression::TernaryExpression {
                        condition: _,
                        true_expression: _,
                        false_expression: _,
                    } => {
                        // Success
                    }
                    _ => panic!("Expected ternary expression"),
                }
            }
            _ => panic!("Expected return with expression"),
        }
    }

    #[test]
    fn test_function_call() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("test".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::Return,
                TokenType::Identifier("foo".to_string()),
                TokenType::LeftParen,
                TokenType::Number(1),
                TokenType::Comma,
                TokenType::Number(2),
                TokenType::RightParen,
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_ok());
        let program = result.unwrap();

        match &program.functions[0].body.as_ref().expect("Function body not found").0[0] {
            Statement::Return { value: Some(expr) } => {
                match expr {
                    Expression::FunctionCall { function_name, arguments } => {
                        assert_eq!(function_name, "foo");
                        assert_eq!(arguments.len(), 2);
                    }
                    _ => panic!("Expected function call"),
                }
            }
            _ => panic!("Expected return with expression"),
        }
    }

    #[test]
    fn test_parenthesized_expression() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("test".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::Return,
                TokenType::LeftParen,
                TokenType::Number(1),
                TokenType::Plus,
                TokenType::Number(2),
                TokenType::RightParen,
                TokenType::Star,
                TokenType::Number(3),
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_ok());
    }

    #[test]
    fn test_multiple_functions() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("first".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::RightBrace,
                TokenType::Function,
                TokenType::Identifier("second".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.functions.len(), 2);
        assert_eq!(program.functions[0].name, "first");
        assert_eq!(program.functions[1].name, "second");
    }

    #[test]
    fn test_operator_precedence() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("test".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::Return,
                TokenType::Number(1),
                TokenType::Plus,
                TokenType::Number(2),
                TokenType::Star,
                TokenType::Number(3),
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_ok());
        let program = result.unwrap();

        // Should parse as 1 + (2 * 3), not (1 + 2) * 3
        match &program.functions[0].body.as_ref().expect("Function body not found").0[0] {
            Statement::Return { value: Some(expr) } => {
                match expr {
                    Expression::BinaryExpression { left, operator, right } => {
                        assert_eq!(*operator, Operator::Add);
                        match left.as_ref() {
                            Expression::Constant(Constant::Integer(1)) => {}
                            _ => panic!("Left should be constant 1"),
                        }
                        match right.as_ref() {
                            Expression::BinaryExpression { operator: Operator::Mul, .. } => {}
                            _ => panic!("Right should be multiplication"),
                        }
                    }
                    _ => panic!("Expected binary expression"),
                }
            }
            _ => panic!("Expected return statement"),
        }
    }

    #[test]
    fn test_logical_operators() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("test".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::Return,
                TokenType::Number(1),
                TokenType::AndAnd,
                TokenType::Number(2),
                TokenType::OrOr,
                TokenType::Number(3),
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_ok());
    }

    #[test]
    fn test_comparison_operators() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("test".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::Return,
                TokenType::Number(1),
                TokenType::Less,
                TokenType::Number(2),
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_ok());
    }

    // Error handling tests
    #[test]
    fn test_missing_function_name() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_err());
    }

    #[test]
    fn test_missing_closing_brace() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("test".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::Return,
                TokenType::Number(5)
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_err());
    }

    #[test]
    fn test_invalid_expression() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("test".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::Return,
                TokenType::Plus, // Invalid: operator without operand
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_err());
    }

    #[test]
    fn test_missing_semicolon_in_for_loop() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("test".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::For,
                TokenType::Let,
                TokenType::Identifier("i".to_string()),
                TokenType::Equal,
                TokenType::Number(0),
                // Missing semicolon here
                TokenType::Identifier("i".to_string()),
                TokenType::Less,
                TokenType::Number(10),
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_err());
    }

    #[test]
    fn test_empty_function_call_args() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("test".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::Return,
                TokenType::Identifier("foo".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_ok());
        let program = result.unwrap();

        match &program.functions[0].body.as_ref().expect("Function body not found").0[0] {
            Statement::Return { value: Some(expr) } => {
                match expr {
                    Expression::FunctionCall { function_name, arguments } => {
                        assert_eq!(function_name, "foo");
                        assert_eq!(arguments.len(), 0);
                    }
                    _ => panic!("Expected function call"),
                }
            }
            _ => panic!("Expected return with expression"),
        }
    }

    #[test]
    fn test_nested_expressions() {
        let tokens = create_tokens(
            vec![
                TokenType::Function,
                TokenType::Identifier("test".to_string()),
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::Return,
                TokenType::Identifier("foo".to_string()),
                TokenType::LeftParen,
                TokenType::Number(1),
                TokenType::Plus,
                TokenType::Number(2),
                TokenType::RightParen,
                TokenType::Star,
                TokenType::Number(3),
                TokenType::RightBrace
            ]
        );

        let result = parse_from_tokens(tokens);
        assert!(result.is_ok());
    }
}
