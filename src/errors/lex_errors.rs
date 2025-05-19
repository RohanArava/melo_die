#[derive(Debug, Clone, PartialEq)]
pub struct LexError {
    pub message: String,
    pub line: i32,
    pub column: i32,
}

impl LexError {
    pub fn new(message: impl Into<String>, line: i32, column: i32) -> Self {
        Self {
            message: message.into(),
            line,
            column,
        }
    }
}
