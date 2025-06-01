use crate::parser::semantic::Type;
use crate::parser::ast::Operator;

#[derive(Debug, Clone)]
pub enum SemanticError {
    UndefinedVariable(String),
    UndefinedFunction(String),
    VariableAlreadyDefined(String),
    FunctionAlreadyDefined(String),
    TypeMismatch { expected: Type, found: Type },
    InvalidUnaryOperation { operator: Operator, operand_type: Type },
    InvalidBinaryOperation { operator: Operator, left_type: Type, right_type: Type },
    WrongArgumentCount { function: String, expected: usize, found: usize },
    ReturnTypeMismatch { expected: Type, found: Type },
    ReturnInVoidFunction,
    MissingReturn(String),
    VoidFunctionInExpression(String),
}