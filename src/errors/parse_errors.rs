#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub message: String,
    pub line: i32,
    pub column: i32,
}

impl ParseError {
    pub fn new(message: impl Into<String>, line: i32, column: i32) -> Self {
        Self {
            message: message.into(),
            line,
            column,
        }
    }
}
