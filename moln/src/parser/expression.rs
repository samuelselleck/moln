use crate::{
    lexer::{Token, TokenKind},
    parser::ast::{BinaryOp, Call, Conditional, Op, UnaryPostfixOp, UnaryPrefixOp},
    symbol_interning::SymbolId,
};

use super::{
    CompilerError, Parser,
    ast::{Expression, ExpressionKind, Spanned},
};

impl<'src> Parser<'src> {
    pub fn expression(&mut self) -> Result<Expression, CompilerError> {
        self.expression_with_min_bp(0)
    }

    pub fn expr_val(&mut self) -> Result<Expression, CompilerError> {
        Ok(match self.peek_token_kind() {
            TokenKind::Integer(i) => Expression {
                span: self.next_token().span,
                node: ExpressionKind::Int(i),
            },
            TokenKind::Float(f) => Expression {
                span: self.next_token().span,
                node: ExpressionKind::Float(f),
            },
            TokenKind::Identifier(s) => Expression {
                span: self.next_token().span,
                node: ExpressionKind::Variable(s),
            },
            TokenKind::String(s) => Expression {
                span: self.next_token().span,
                node: ExpressionKind::String(s),
            },
            TokenKind::True => Expression {
                span: self.next_token().span,
                node: ExpressionKind::Boolean(true),
            },
            TokenKind::False => Expression {
                span: self.next_token().span,
                node: ExpressionKind::Boolean(false),
            },
            TokenKind::Type => self
                .type_definition()?
                .map(|(s, t)| ExpressionKind::TypeDefinition(s, t)),
            TokenKind::Tick => self.variant()?.map(ExpressionKind::Variant),
            TokenKind::OpenCurlBrack => self.block()?.map(ExpressionKind::Block),
            TokenKind::If => {
                self.spanned(|p| {
                    //test
                    p.next_token(); // consume 'if' token
                    let condition = Box::new(p.expression()?);
                    let pass = p.block()?;
                    let fail = p
                        .next_token_if(|t| t == TokenKind::Else)
                        .is_some()
                        .then(|| p.block())
                        .transpose()?;
                    Ok(ExpressionKind::Conditional(Conditional {
                        condition,
                        pass,
                        fail,
                    }))
                })?
            }
            TokenKind::Fn => self.function()?.map(ExpressionKind::FunctionDefinition),
            TokenKind::OpenSquareBrack => self.spanned(|p| {
                p.sequence_of_enclosed_in(
                    |p| p.expression(),
                    TokenKind::OpenSquareBrack,
                    TokenKind::CloseSquareBrack,
                )
                .map(ExpressionKind::List)
            })?,
            TokenKind::OpenParenth => self.spanned(|p| {
                p.next_token();
                Ok(
                    if p.next_token_if(|t| t == TokenKind::CloseParenth).is_some() {
                        ExpressionKind::Unit
                    } else {
                        let expr = p.expression()?;
                        p.expect(TokenKind::CloseParenth)?;
                        ExpressionKind::Parenthesis(Box::new(expr))
                    },
                )
            })?,
            TokenKind::Not => {
                let ((), rbp) = prefix_binding_power(&UnaryPrefixOp::Not);
                self.spanned(|p| {
                    Ok(ExpressionKind::UnaryPrefix {
                        op: Spanned {
                            span: p.expect(TokenKind::Not)?.span,
                            node: UnaryPrefixOp::Not,
                        },
                        val: Box::new(p.expression_with_min_bp(rbp)?),
                    })
                })?
            }
            TokenKind::Minus => {
                //prefix minus
                let ((), rbp) = prefix_binding_power(&UnaryPrefixOp::Neg);
                self.spanned(|p| {
                    Ok(ExpressionKind::UnaryPrefix {
                        op: Spanned {
                            span: p.expect(TokenKind::Minus)?.span,
                            node: UnaryPrefixOp::Neg,
                        },
                        val: Box::new(p.expression_with_min_bp(rbp)?),
                    })
                })?
            }
            _ => {
                return Err(CompilerError::unexpected_token(
                    self.next_token(),
                    "expected identifier, integer, float, block, string,
                    array, parentheses, '-' or '!'",
                ));
            }
        })
    }

    fn expression_with_min_bp(&mut self, min_bp: u8) -> Result<Expression, CompilerError> {
        let start = self.tokens.start_span();
        let mut value = self.expr_val()?;

        loop {
            let op = match self.peek_token_kind() {
                // postfix call (TODO these are a bit ugly - values populated later. try to fix)
                TokenKind::OpenParenth => {
                    Op::Postfix(UnaryPostfixOp::Call(Call { arguments: vec![] }))
                }
                TokenKind::Period => {
                    Op::Postfix(UnaryPostfixOp::FieldAccess(SymbolId::from_str("")))
                }

                // binary operators
                TokenKind::Assign => Op::Binary(BinaryOp::Assign),
                TokenKind::Plus => Op::Binary(BinaryOp::Add),
                TokenKind::Minus => Op::Binary(BinaryOp::Sub),
                TokenKind::Asterisk => Op::Binary(BinaryOp::Mult),
                TokenKind::Range => Op::Binary(BinaryOp::Range),
                TokenKind::Eq => Op::Binary(BinaryOp::Eq),
                TokenKind::LessOrEq => Op::Binary(BinaryOp::LessOrEq),
                TokenKind::MoreOrEq => Op::Binary(BinaryOp::MoreOrEq),
                TokenKind::NotEq => Op::Binary(BinaryOp::NotEq),
                TokenKind::Or => Op::Binary(BinaryOp::Or),
                TokenKind::And => Op::Binary(BinaryOp::And),
                TokenKind::CloseAngBrack => Op::Binary(BinaryOp::LargerThan),
                TokenKind::OpenAngBrack => Op::Binary(BinaryOp::SmallerThan),
                TokenKind::Exp => Op::Binary(BinaryOp::Exp),
                TokenKind::Mod => Op::Binary(BinaryOp::Mod),
                TokenKind::Slash => Op::Binary(BinaryOp::Div),
                _ => {
                    break;
                }
            };

            match op {
                Op::Binary(op) => {
                    let (lbp, rbp) = bin_binding_powers(&op);
                    if lbp < min_bp {
                        break;
                    }
                    let op = Spanned {
                        span: self.next_token().span,
                        node: op,
                    };
                    let rhs = self.expression_with_min_bp(rbp)?;

                    let end = self.tokens.end_span();
                    value = Expression {
                        span: (start..end).into(),
                        node: ExpressionKind::Binary {
                            left: Box::new(value),
                            op,
                            right: Box::new(rhs),
                        },
                    };
                }
                Op::Postfix(mut op) => {
                    let (lbp, ()) = postfix_binding_power(&op);
                    if lbp < min_bp {
                        break;
                    }
                    let op_spanned = self.spanned(|p| {
                        match &mut op {
                            UnaryPostfixOp::Call(call) => {
                                call.arguments.extend(p.sequence_of_enclosed_in(
                                    |p| p.expression(),
                                    TokenKind::OpenParenth,
                                    TokenKind::CloseParenth,
                                )?)
                            }
                            UnaryPostfixOp::FieldAccess(identifiers) => {
                                p.expect(TokenKind::Period)?;
                                *identifiers = p.identifier()?;
                            }
                        }
                        Ok(op)
                    })?;
                    let end = self.tokens.end_span();
                    value = Expression {
                        span: (start..end).into(),
                        node: ExpressionKind::UnaryPostfix {
                            val: Box::new(value),
                            op: op_spanned,
                        },
                    };
                }
            }
        }
        Ok(value)
    }
}

fn bin_binding_powers(op: &BinaryOp) -> (u8, u8) {
    match op {
        BinaryOp::Assign => (0, 1),
        BinaryOp::Or | BinaryOp::And => (2, 3),
        BinaryOp::NotEq
        | BinaryOp::LessOrEq
        | BinaryOp::MoreOrEq
        | BinaryOp::LargerThan
        | BinaryOp::SmallerThan
        | BinaryOp::Eq => (4, 5),
        BinaryOp::Add | BinaryOp::Sub => (6, 7),
        BinaryOp::Mult | BinaryOp::Div => (8, 9),
        BinaryOp::Mod => (10, 11),
        BinaryOp::Exp => (14, 15),
        BinaryOp::Range => (16, 17),
    }
}

fn prefix_binding_power(op: &UnaryPrefixOp) -> ((), u8) {
    match op {
        UnaryPrefixOp::Neg | UnaryPrefixOp::Not => ((), 18),
    }
}

fn postfix_binding_power(op: &UnaryPostfixOp) -> (u8, ()) {
    match op {
        UnaryPostfixOp::Call(_) => (20, ()),
        UnaryPostfixOp::FieldAccess(_) => (20, ()),
    }
}
