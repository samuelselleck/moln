use ast::{Ast, RootStatement, Spanned};

use crate::error::{CResult, CompilerError};
use crate::lexer::{Token, TokenKind};
use crate::token_stream::TokenStream;

pub mod ast;
mod errors;
pub mod expression;
pub mod value;

/// Parses a vadermoln source file into an AST.
pub struct Parser<'src> {
    tokens: TokenStream<'src>,
    context_stack: Vec<&'static str>,
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            tokens: TokenStream::new(source),
            context_stack: Vec::new(),
        }
    }

    pub fn vadermoln(&mut self) -> CResult<Ast> {
        let mut elems = vec![];
        while !self.is_at_eof() {
            let elem = match self.peek_token_kind() {
                TokenKind::Fn => self.function()?.map(RootStatement::Function),
                TokenKind::Type => self
                    .type_definition()?
                    .map(|(s, t)| RootStatement::Type(s, t)),
                _ => {
                    return Err(CompilerError::unexpected_token(
                        self.next_token(),
                        "expected top level type",
                    ));
                }
            };
            elems.push(elem);
        }
        Ok(Ast { elems })
    }

    pub fn is_at_eof(&mut self) -> bool {
        self.tokens.peek().kind == TokenKind::EOF
    }

    fn peek_token_kind(&mut self) -> TokenKind {
        self.tokens.peek().kind
    }

    fn peek_nth_token(&mut self, i: usize) -> TokenKind {
        self.tokens.peek_nth(i)
    }

    fn next_token_if(&mut self, f: impl FnOnce(TokenKind) -> bool) -> Option<Token> {
        self.tokens.next_if(f)
    }

    fn next_token(&mut self) -> Token {
        self.tokens.next()
    }

    /// Convenience method to track spans of a parsed section.
    pub fn spanned<T, F>(&mut self, f: F) -> CResult<Spanned<T>>
    where
        F: FnOnce(&mut Self) -> CResult<T>,
    {
        let start = self.tokens.start_span();
        let result = f(self);
        let end = self.tokens.end_span().max(start + 1);
        Ok(Spanned {
            node: result?,
            span: (start..end).into(),
        })
    }

    fn push_context(&mut self, context: &'static str) {
        self.context_stack.push(context);
        // println!(
        //     "{}entered: {:?}",
        //     " ".repeat(self.context_stack.len()),
        //     self.context_stack.last()
        // );
    }

    fn pop_context(&mut self) {
        // println!(
        //     "{}exited:  {:?}",
        //     " ".repeat(self.context_stack.len()),
        //     self.context_stack.pop()
        // );
        self.context_stack.pop();
    }

    // fn source_of(&self, span: Span) -> &str {
    //     &self.tokens.inner().src[span.as_range()]
    // }
}
