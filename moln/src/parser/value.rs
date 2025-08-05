use moln_macros::token_context;

use crate::{
    error::CResult,
    lexer::{Token, TokenKind},
    parser::ast::{Block, Function, Map, Statement, VariableDeclaration, VariableDefinition},
};

use super::{
    CompilerError, Parser,
    ast::{Spanned, Type, UNIT_TYPE},
};

impl<'src> Parser<'src> {
    // this function is used to peek forward far enough to be sure that the next
    // ast node is a map. This is needed to dissambiguate from a block { .. }.
    // This function is by far the place with the largest amount of lookahead
    pub fn is_map_next(&mut self) -> bool {
        let mut lookahead = 0;
        // expect a open curly bracket
        if self.peek_nth_token(lookahead) != TokenKind::OpenCurlBrack {
            return false;
        }
        lookahead += 1;
        // skip an unknown ammount of legal comment blocks.
        // this isn't great since it's technically an arbitrary ammount
        // of lookahead, but  should very very seldom be more than 3 or 4 or so
        while self.peek_nth_token(lookahead) == TokenKind::Comment {
            lookahead += 1;
        }
        // expect an identifier
        if !matches!(self.peek_nth_token(lookahead), TokenKind::Identifier(_)) {
            return false;
        }
        lookahead += 1;
        // then a colon
        if self.peek_nth_token(lookahead) != TokenKind::Colon {
            return false;
        }
        // now we are sure, this must be a map
        return true;
    }

    pub fn block(&mut self) -> Result<Spanned<Block>, CompilerError> {
        self.spanned(|p| {
            p.expect(TokenKind::OpenCurlBrack)?;
            let mut elements = vec![];
            let mut semicolon = true;
            while !(p.peek_token_kind() == TokenKind::CloseCurlBrack) {
                if !semicolon {
                    return Err(CompilerError::new(
                        "expression must end with semicolon \
                    when not at end of block",
                    )
                    .annotation(
                        p.next_token().span,
                        "expected semicolon or block ending here",
                    ));
                }
                let elem = match p.peek_token_kind() {
                    TokenKind::Let => {
                        p.next_token();
                        let definition = p.variable_definition()?;
                        let init = p
                            .next_token_if(|v| v == TokenKind::Assign)
                            .is_some()
                            .then(|| p.expression())
                            .transpose()?
                            .map(Into::into);
                        Statement::Declaration(VariableDeclaration { definition, init })
                    }
                    _ => Statement::Expression(p.expression()?),
                };
                elements.push(elem);
                semicolon = p.next_token_if(|v| v == TokenKind::SemiColon).is_some();
            }
            p.expect(TokenKind::CloseCurlBrack)?;
            Ok(Block {
                statements: elements,
                unit_return: semicolon,
            })
        })
    }

    #[token_context("Map ({foo: .. bar: ..})")]
    pub fn map(&mut self) -> CResult<Spanned<Map>> {
        self.spanned(|p| {
            p.expect(TokenKind::OpenCurlBrack)?;
            let mut entries = vec![];
            loop {
                entries.push(match p.next_token() {
                    Token {
                        kind: TokenKind::Identifier(ident_symbol),
                        ..
                    } => {
                        p.expect(TokenKind::Colon)?;
                        let value = p.expression()?;
                        //skip comma between fields
                        p.next_token_if(|t| t == TokenKind::Comma);
                        (ident_symbol, value)
                    }
                    Token {
                        kind: TokenKind::CloseCurlBrack,
                        ..
                    } => {
                        break;
                    }
                    _ => {
                        return Err(CompilerError::unexpected_token(
                            p.next_token(),
                            "expected identifier or closing bracket",
                        ));
                    }
                });
            }
            Ok(Map(entries))
        })
    }

    #[token_context("Sequence ([foo, 5px], or (foo, 5px))")]
    pub fn sequence_of_enclosed_in<T>(
        &mut self,
        f: impl Fn(&mut Parser) -> Result<T, CompilerError>,
        open: TokenKind,
        close: TokenKind,
    ) -> Result<Vec<T>, CompilerError> {
        self.expect(open)?;
        if self.next_token_if(|t| t == close).is_some() {
            return Ok(vec![]);
        }

        let mut entries = vec![];
        loop {
            entries.push(f(self)?);
            if self.next_token_if(|t| t == close).is_some() {
                break;
            }
            self.expect(TokenKind::Comma)?;
        }
        Ok(entries)
    }

    pub fn variable_definition(&mut self) -> Result<Spanned<VariableDefinition>, CompilerError> {
        self.spanned(|p| {
            let ident_symbol = p.spanned(|p| match p.next_token() {
                Token {
                    kind: TokenKind::Identifier(symbol_id),
                    ..
                } => Ok(symbol_id),
                t => return Err(CompilerError::unexpected_token(t, "expected identifier")),
            })?;
            // TODO support function/array/tuple etc. types here.
            let type_symbol = p
                .next_token_if(|t| t == TokenKind::Colon)
                .is_some()
                .then(|| {
                    p.spanned(|p| {
                        Ok(match p.next_token() {
                            Token {
                                kind: TokenKind::Identifier(symbol_id),
                                ..
                            } => symbol_id,
                            t => return Err(CompilerError::unexpected_token(t, "expected type")),
                        })
                    })
                })
                .transpose()?;
            Ok(VariableDefinition {
                variable: ident_symbol,
                known_type: type_symbol.map(|v| v.map(Type::Named)),
            })
        })
    }

    pub fn function(&mut self) -> Result<Function, CompilerError> {
        self.expect(TokenKind::Fn)?;
        let ident_symbol = self.spanned(|p| match p.next_token() {
            Token {
                kind: TokenKind::Identifier(symbol_id),
                ..
            } => Ok(symbol_id),
            t => return Err(CompilerError::unexpected_token(t, "expected identifier")),
        })?;
        let arguments = self.spanned(|p| {
            p.sequence_of_enclosed_in(
                |p| p.variable_definition(),
                TokenKind::OpenParenth,
                TokenKind::CloseParenth,
            )
        })?;
        let return_type = self.spanned(|p| {
            Ok((p.peek_token_kind() != TokenKind::OpenCurlBrack)
                .then(|| {
                    p.expect(TokenKind::ThinArrow)?;

                    let symbol_id = match p.next_token() {
                        Token {
                            kind: TokenKind::Identifier(symbol_id),
                            ..
                        } => symbol_id,
                        t => return Err(CompilerError::unexpected_token(t, "expected identifier")),
                    };
                    // TODO allow other type parameters than named types
                    Ok(Type::Named(symbol_id))
                })
                .transpose()?
                // zero-sized Type span is unit type
                .unwrap_or_else(|| UNIT_TYPE))
        })?;

        let body = self.block()?;
        Ok(Function {
            name: ident_symbol,
            parameters: arguments,
            body,
            return_type,
        })
    }
}
