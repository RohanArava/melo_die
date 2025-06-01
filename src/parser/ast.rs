#[derive(Debug, Clone)]
pub enum Constant {
    Integer(i64),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Not,
    Negate,
    And,
    Or,
    Equal,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    NotEqual
}

#[derive(Debug, Clone)]
pub enum Expression {
    Constant(Constant),
    Variable(String),
    FunctionCall { function_name: String, arguments: Vec<Expression> },
    BinaryExpression { left: Box<Expression>, operator: Operator, right: Box<Expression> },
    UnaryExpression { operator: Operator, expression: Box<Expression> },
    TernaryExpression {
        condition: Box<Expression>,
        true_expression: Box<Expression>,
        false_expression: Box<Expression>,
    },
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub name: String,
    pub arguments: Vec<String>,
    pub body: Option<CodeBlock>,
    pub returns_value: bool
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expression { expression: Expression },
    VariableDeclaration { name: String, expression: Option<Expression> },
    VariableAssignment { name: String, expression: Expression },
    IfStatement {
        condition: Expression,
        then_block: CodeBlock,
        else_block: Option<CodeBlock>,
    },
    WhileStatement {
        condition: Expression,
        body: CodeBlock,
    },
    ForStatement {
        initializer: Box<Statement>,
        condition: Expression,
        increment: Box<Statement>,
        body: CodeBlock
    },
    Return {
        value: Option<Expression>,
    },
}

#[derive(Debug, Clone)]
pub struct CodeBlock(pub Vec<Statement>);

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<FunctionDeclaration>,
}