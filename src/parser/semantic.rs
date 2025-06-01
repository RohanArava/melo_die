use std::collections::HashMap;
use crate::errors::semantic_errors::SemanticError;
use crate::parser::ast::{ Constant, Expression, Operator, Statement, CodeBlock, Program };

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Integer,
    Void,
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub name: String,
    pub parameters: Vec<String>,
    pub return_type: Type,
    pub is_defined: bool,
}

#[derive(Debug, Clone)]
pub struct Scope {
    variables: HashMap<String, Type>,
    parent: Option<Box<Scope>>,
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            variables: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_child(parent: Scope) -> Self {
        Scope {
            variables: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn define_variable(&mut self, name: String, var_type: Type) -> Result<(), SemanticError> {
        if self.variables.contains_key(&name) {
            return Err(SemanticError::VariableAlreadyDefined(name));
        }
        self.variables.insert(name, var_type);
        Ok(())
    }

    pub fn get_variable(&self, name: &str) -> Option<&Type> {
        if let Some(var_type) = self.variables.get(name) {
            Some(var_type)
        } else if let Some(parent) = &self.parent {
            parent.get_variable(name)
        } else {
            None
        }
    }

    pub fn assign_variable(&self, name: &str) -> Result<&Type, SemanticError> {
        self.get_variable(name).ok_or_else(|| SemanticError::UndefinedVariable(name.to_string()))
    }
}

pub struct SemanticAnalyzer {
    functions: HashMap<String, FunctionInfo>,
    current_function: Option<FunctionInfo>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer {
            functions: HashMap::new(),
            current_function: None,
        }
    }

    pub fn analyze(&mut self, program: &Program) -> Result<(), Vec<SemanticError>> {
        let mut errors = Vec::new();

        for function in &program.functions {
            let return_type = if function.returns_value {
                Type::Integer
            } else {
                Type::Void
            };

            let func_info = FunctionInfo {
                name: function.name.clone(),
                parameters: function.arguments.clone(),
                return_type,
                is_defined: match function.body {
                    Some(_) => true,
                    None => false,
                },
            };

            if self.functions.contains_key(&function.name) {
                errors.push(SemanticError::FunctionAlreadyDefined(function.name.clone()));
            } else {
                self.functions.insert(function.name.clone(), func_info);
            }
        }

        for function in &program.functions {
            if let Some(func_info) = self.functions.get(&function.name).cloned() {
                if !func_info.is_defined {
                    continue;
                }
                self.current_function = Some(func_info.clone());

                let mut scope = Scope::new();

                for param in &function.arguments {
                    if let Err(err) = scope.define_variable(param.clone(), Type::Integer) {
                        errors.push(err);
                    }
                }

                match &function.body {
                    Some(body) => {
                        if
                            let Err(mut func_errors) = self.analyze_code_block(
                                body,
                                &mut scope
                            )
                        {
                            errors.append(&mut func_errors);
                        }

                        if func_info.return_type != Type::Void {
                            if !self.has_return_statement(body) {
                                errors.push(SemanticError::MissingReturn(function.name.clone()));
                            }
                        }
                    },
                    None => {}
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn analyze_code_block(
        &mut self,
        block: &CodeBlock,
        scope: &mut Scope
    ) -> Result<(), Vec<SemanticError>> {
        let mut errors = Vec::new();

        for statement in &block.0 {
            if let Err(mut stmt_errors) = self.analyze_statement(statement, scope) {
                errors.append(&mut stmt_errors);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn analyze_statement(
        &mut self,
        statement: &Statement,
        scope: &mut Scope
    ) -> Result<(), Vec<SemanticError>> {
        let mut errors = Vec::new();

        match statement {
            Statement::Expression { expression } => {
                // Expression statements can include void function calls
                if let Err(err) = self.analyze_expression(expression, scope, true) {
                    errors.push(err);
                }
            }

            Statement::VariableDeclaration { name, expression } => {
                let var_type = if let Some(expr) = expression {
                    match self.analyze_expression(expr, scope, false) {
                        Ok(expr_type) => expr_type,
                        Err(err) => {
                            errors.push(err);
                            Type::Integer
                        }
                    }
                } else {
                    Type::Integer
                };

                if let Err(err) = scope.define_variable(name.clone(), var_type) {
                    errors.push(err);
                }
            }

            Statement::VariableAssignment { name, expression } => {
                match scope.assign_variable(name) {
                    Ok(var_type) => {
                        match self.analyze_expression(expression, scope, false) {
                            Ok(expr_type) => {
                                if *var_type != expr_type {
                                    errors.push(SemanticError::TypeMismatch {
                                        expected: var_type.clone(),
                                        found: expr_type,
                                    });
                                }
                            }
                            Err(err) => errors.push(err),
                        }
                    }
                    Err(err) => errors.push(err),
                }
            }

            Statement::IfStatement { condition, then_block, else_block } => {
                match self.analyze_expression(condition, scope, false) {
                    Ok(Type::Integer) => {}
                    Ok(Type::Void) => {
                        errors.push(
                            SemanticError::VoidFunctionInExpression("if condition".to_string())
                        );
                    }
                    Err(err) => errors.push(err),
                }

                let mut then_scope = Scope::new_child(scope.clone());
                if let Err(mut block_errors) = self.analyze_code_block(then_block, &mut then_scope) {
                    errors.append(&mut block_errors);
                }

                if let Some(else_block) = else_block {
                    let mut else_scope = Scope::new_child(scope.clone());
                    if
                        let Err(mut block_errors) = self.analyze_code_block(
                            else_block,
                            &mut else_scope
                        )
                    {
                        errors.append(&mut block_errors);
                    }
                }
            }

            Statement::WhileStatement { condition, body } => {
                match self.analyze_expression(condition, scope, false) {
                    Ok(Type::Integer) => {} 
                    Ok(Type::Void) => {
                        errors.push(
                            SemanticError::VoidFunctionInExpression("while condition".to_string())
                        );
                    }
                    Err(err) => errors.push(err),
                }

                let mut body_scope = Scope::new_child(scope.clone());
                if let Err(mut block_errors) = self.analyze_code_block(body, &mut body_scope) {
                    errors.append(&mut block_errors);
                }
            }

            Statement::ForStatement { initializer, condition, increment, body } => {
                let mut for_scope = Scope::new_child(scope.clone());

                if let Err(mut init_errors) = self.analyze_statement(initializer, &mut for_scope) {
                    errors.append(&mut init_errors);
                }

                match self.analyze_expression(condition, &for_scope, false) {
                    Ok(Type::Integer) => {}
                    Ok(Type::Void) => {
                        errors.push(
                            SemanticError::VoidFunctionInExpression("for condition".to_string())
                        );
                    }
                    Err(err) => errors.push(err),
                }

                if let Err(mut inc_errors) = self.analyze_statement(increment, &mut for_scope) {
                    errors.append(&mut inc_errors);
                }

                if let Err(mut body_errors) = self.analyze_code_block(body, &mut for_scope) {
                    errors.append(&mut body_errors);
                }
            }

            Statement::Return { value } => {
                if let Some(current_func) = &self.current_function {
                    match (value, &current_func.return_type) {
                        (Some(_), Type::Void) => {
                            errors.push(SemanticError::ReturnInVoidFunction);
                        }
                        (Some(expr), expected_type) => {
                            match self.analyze_expression(expr, scope, false) {
                                Ok(expr_type) => {
                                    if expr_type != *expected_type {
                                        errors.push(SemanticError::ReturnTypeMismatch {
                                            expected: expected_type.clone(),
                                            found: expr_type,
                                        });
                                    }
                                }
                                Err(err) => errors.push(err),
                            }
                        }
                        (None, Type::Void) => {}
                        (None, expected_type) => {
                            errors.push(SemanticError::ReturnTypeMismatch {
                                expected: expected_type.clone(),
                                found: Type::Void,
                            });
                        }
                    }
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn analyze_expression(
        &self,
        expression: &Expression,
        scope: &Scope,
        allow_no_return: bool
    ) -> Result<Type, SemanticError> {
        match expression {
            Expression::Constant(Constant::Integer(_)) => Ok(Type::Integer),

            Expression::Variable(name) => {
                scope
                    .get_variable(name)
                    .cloned()
                    .ok_or_else(|| SemanticError::UndefinedVariable(name.clone()))
            }

            Expression::FunctionCall { function_name, arguments } => {
                let func_info = self.functions
                    .get(function_name)
                    .ok_or_else(|| SemanticError::UndefinedFunction(function_name.clone()))?;

                if arguments.len() != func_info.parameters.len() {
                    return Err(SemanticError::WrongArgumentCount {
                        function: function_name.clone(),
                        expected: func_info.parameters.len(),
                        found: arguments.len(),
                    });
                }

                for arg in arguments {
                    let arg_type = self.analyze_expression(arg, scope, false)?;
                    if arg_type == Type::Void {
                        return Err(
                            SemanticError::VoidFunctionInExpression(
                                format!("argument to function {}", function_name)
                            )
                        );
                    }
                }

                if !allow_no_return && func_info.return_type == Type::Void {
                    return Err(SemanticError::VoidFunctionInExpression(function_name.clone()));
                }

                Ok(func_info.return_type.clone())
            }

            Expression::BinaryExpression { left, operator, right } => {
                let left_type = self.analyze_expression(left, scope, false)?;
                let right_type = self.analyze_expression(right, scope, false)?;

                match operator {
                    | Operator::Add
                    | Operator::Sub
                    | Operator::Mul
                    | Operator::Div
                    | Operator::Mod => {
                        if left_type == Type::Integer && right_type == Type::Integer {
                            Ok(Type::Integer)
                        } else {
                            Err(SemanticError::InvalidBinaryOperation {
                                operator: operator.clone(),
                                left_type,
                                right_type,
                            })
                        }
                    }

                    | Operator::Equal
                    | Operator::NotEqual
                    | Operator::LessThan
                    | Operator::GreaterThan
                    | Operator::LessThanEqual
                    | Operator::GreaterThanEqual => {
                        if left_type == Type::Integer && right_type == Type::Integer {
                            Ok(Type::Integer)
                        } else {
                            Err(SemanticError::InvalidBinaryOperation {
                                operator: operator.clone(),
                                left_type,
                                right_type,
                            })
                        }
                    }

                    Operator::And | Operator::Or => {
                        if left_type == Type::Integer && right_type == Type::Integer {
                            Ok(Type::Integer)
                        } else {
                            Err(SemanticError::InvalidBinaryOperation {
                                operator: operator.clone(),
                                left_type,
                                right_type,
                            })
                        }
                    }

                    _ =>
                        Err(SemanticError::InvalidBinaryOperation {
                            operator: operator.clone(),
                            left_type,
                            right_type,
                        }),
                }
            }

            Expression::UnaryExpression { operator, expression } => {
                let expr_type = self.analyze_expression(expression, scope, false)?;

                match operator {
                    Operator::Negate => {
                        if expr_type == Type::Integer {
                            Ok(Type::Integer)
                        } else {
                            Err(SemanticError::InvalidUnaryOperation {
                                operator: operator.clone(),
                                operand_type: expr_type,
                            })
                        }
                    }

                    Operator::Not => {
                        if expr_type == Type::Integer {
                            Ok(Type::Integer)
                        } else {
                            Err(SemanticError::InvalidUnaryOperation {
                                operator: operator.clone(),
                                operand_type: expr_type,
                            })
                        }
                    }

                    _ =>
                        Err(SemanticError::InvalidUnaryOperation {
                            operator: operator.clone(),
                            operand_type: expr_type,
                        }),
                }
            }

            Expression::TernaryExpression { condition, true_expression, false_expression } => {

                let condition_type = self.analyze_expression(condition, scope, false)?;
                if condition_type == Type::Void {
                    return Err(
                        SemanticError::VoidFunctionInExpression("ternary condition".to_string())
                    );
                }

                let true_type = self.analyze_expression(true_expression, scope, false)?;
                let false_type = self.analyze_expression(false_expression, scope, false)?;

                if true_type == Type::Void {
                    return Err(
                        SemanticError::VoidFunctionInExpression("ternary true branch".to_string())
                    );
                }
                if false_type == Type::Void {
                    return Err(
                        SemanticError::VoidFunctionInExpression("ternary false branch".to_string())
                    );
                }

                if true_type == false_type {
                    Ok(true_type)
                } else {
                    Err(SemanticError::TypeMismatch {
                        expected: true_type,
                        found: false_type,
                    })
                }
            }
        }
    }

    fn has_return_statement(&self, block: &CodeBlock) -> bool {
        for statement in &block.0 {
            match statement {
                Statement::Return { .. } => {
                    return true;
                }
                Statement::IfStatement { then_block, else_block, .. } => {
                    if let Some(else_block) = else_block {
                        if
                            self.has_return_statement(then_block) &&
                            self.has_return_statement(else_block)
                        {
                            return true;
                        }
                    }
                }
                _ => {}
            }
        }
        false
    }
}
