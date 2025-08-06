use moln_macros::token_context;

use crate::{
    error::CResult,
    lexer::{Token, TokenKind},
    parser::ast::{Block, Function, Statement, VariableDeclaration, VariableDefinition, Variant},
    symbol_interning::SymbolId,
};

use super::{
    CompilerError, Parser,
    ast::{FunctionParam, Spanned, Type, VariantDefinition},
};

impl<'src> Parser<'src> {
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
    pub fn variant(&mut self) -> CResult<Spanned<Variant>> {
        self.spanned(|p| {
            p.expect(TokenKind::Tick)?;
            let name = matches!(p.peek_token_kind(), TokenKind::Identifier(_))
                .then(|| p.identifier())
                .transpose()?
                .unwrap_or_else(|| SymbolId::from_str(""));
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
            Ok(Variant(name, entries))
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
            let ident_symbol = p.spanned(|p| p.identifier())?;
            let type_symbol = p
                .next_token_if(|t| t == TokenKind::Colon)
                .is_some()
                .then(|| p.spanned(|p| p.r#type()))
                .transpose()?;
            Ok(VariableDefinition {
                variable: ident_symbol,
                known_type: type_symbol,
            })
        })
    }

    pub fn identifier(&mut self) -> CResult<SymbolId> {
        match self.next_token() {
            Token {
                kind: TokenKind::Identifier(symbol_id),
                ..
            } => Ok(symbol_id),
            t => return Err(CompilerError::unexpected_token(t, "expected identifier")),
        }
    }

    pub fn function(&mut self) -> Result<Function, CompilerError> {
        self.expect(TokenKind::Fn)?;
        let ident_symbol = self.spanned(|p| p.identifier())?;
        let parameters = self.spanned(|p| {
            p.sequence_of_enclosed_in(
                |p| {
                    let parameter = p.spanned(|p| p.identifier())?;
                    p.expect(TokenKind::Colon)?;
                    let parameter_type = p.spanned(|p| p.r#type())?;
                    Ok(FunctionParam {
                        identifier: parameter,
                        r#type: parameter_type,
                    })
                },
                TokenKind::OpenParenth,
                TokenKind::CloseParenth,
            )
        })?;
        let return_type = self.spanned(|p| {
            Ok((p.peek_token_kind() != TokenKind::OpenCurlBrack)
                .then(|| {
                    p.expect(TokenKind::ThinArrow)?;
                    p.r#type()
                })
                .transpose()?
                // zero-sized Type span is unit type
                .unwrap_or_else(|| Type::Unit))
        })?;

        let body = self.block()?;
        Ok(Function {
            name: ident_symbol,
            parameters,
            body,
            return_type,
        })
    }

    pub fn r#type(&mut self) -> CResult<Type> {
        match self.peek_token_kind() {
            TokenKind::Identifier(symbol_id) => {
                self.next_token();
                Ok(Type::Named(symbol_id))
            }
            TokenKind::Fn => {
                self.next_token();
                let parameters = self.sequence_of_enclosed_in(
                    |p| p.r#type(),
                    TokenKind::OpenParenth,
                    TokenKind::CloseParenth,
                )?;
                let return_type = (self.peek_token_kind() == TokenKind::ThinArrow)
                    .then(|| {
                        self.next_token();
                        self.r#type()
                    })
                    .transpose()?
                    .unwrap_or_else(|| Type::Unit);
                Ok(Type::Function {
                    parameters,
                    return_type: Box::new(return_type),
                })
            }
            TokenKind::OpenSquareBrack => {
                self.next_token();
                let interior_type = self.r#type()?;
                self.expect(TokenKind::SemiColon)?;
                let len = self.next_token();
                match len.kind {
                    TokenKind::Integer(value) => {
                        self.expect(TokenKind::CloseSquareBrack)?;
                        Ok(Type::Array(Box::new(interior_type), value as usize))
                    }
                    _ => {
                        return Err(CompilerError::unexpected_token(
                            len,
                            "array length must be an integer",
                        ));
                    }
                }
            }
            TokenKind::Tick => {
                let mut variants = vec![];
                loop {
                    self.expect(TokenKind::Tick)?;
                    let name = matches!(self.peek_token_kind(), TokenKind::Identifier(_))
                        .then(|| self.identifier())
                        .transpose()?
                        .unwrap_or_else(|| SymbolId::from_str(""));
                    let fields = self.sequence_of_enclosed_in(
                        |p| {
                            let field = p.identifier()?;
                            p.expect(TokenKind::Colon)?;
                            let field_type = p.r#type()?;
                            Ok((field, field_type))
                        },
                        TokenKind::OpenCurlBrack,
                        TokenKind::CloseCurlBrack,
                    )?;
                    variants.push(VariantDefinition(name, fields));
                    if self.next_token_if(|t| t == TokenKind::VertLine).is_none() {
                        break;
                    }
                }
                Ok(Type::SumType(variants))
            }
            _ => {
                return Err(CompilerError::unexpected_token(
                    self.next_token(),
                    "expected type",
                ));
            }
        }
    }
}
