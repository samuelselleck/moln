use crate::lexer::{Token, TokenIterator, TokenKind};
use crate::utils::MultiPeek;

/// A wrapper around the token iterator that ensures proper span tracking and
/// adds helper methods. All token consumption MUST go through this struct's
/// methods to maintain accurate span information.
pub struct TokenStream<'src> {
    tokens: MultiPeek<TokenIterator<'src>>,
    last_consumed: Option<Token>,
}

impl<'src> TokenStream<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            tokens: MultiPeek::new(TokenIterator::new(source)),
            last_consumed: None,
        }
    }

    /// Peek at the next token without consuming it
    pub fn peek(&mut self) -> &Token {
        self.tokens.peek().expect("should be infinite")
    }

    /// Peek at the nth token ahead (0 = next token)
    pub fn peek_nth(&mut self, n: usize) -> TokenKind {
        self.tokens
            .peek_nth(n)
            .map(|t| t.kind)
            .expect("should be infinite")
    }

    /// Consume and return the next token
    pub fn next(&mut self) -> Token {
        let token = self.tokens.next().expect("should be infinite");
        self.last_consumed = Some(token);
        token
    }

    /// Consume the next token only if it matches the predicate
    pub fn next_if(&mut self, f: impl FnOnce(TokenKind) -> bool) -> Option<Token> {
        let token = self.tokens.next_if(|t| f(t.kind))?;
        self.last_consumed = Some(token);
        Some(token)
    }

    /// Get the start position for a new span (position of next token)
    pub fn start_span(&mut self) -> usize {
        self.peek().span.start
    }

    /// Get the end position for a span (end of last consumed token)
    pub fn end_span(&self) -> usize {
        self.last_consumed.map(|t| t.span.end).unwrap_or(0)
    }
}
