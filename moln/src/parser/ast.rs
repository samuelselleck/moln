use std::{fmt::Display, ops::Deref};

use crate::{lexer::Span, symbol_interning::SymbolId};

#[derive(Debug)]
pub struct Ast {
    pub elems: Vec<Spanned<RootStatement>>,
}

#[derive(Debug)]
pub enum RootStatement {
    Function(Function),
    Type(SymbolId, Type),
}

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn map<R>(self, f: impl FnOnce(T) -> R) -> Spanned<R> {
        Spanned {
            node: f(self.node),
            span: self.span,
        }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

pub type Expression = Spanned<ExpressionKind>;

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    // --- Literals & identifiers ---
    Variable(SymbolId),
    Float(f64),
    Int(i64),
    String(SymbolId),
    Boolean(bool),
    // the unit value
    Unit,

    // --- Complex types ---
    FunctionDefinition(Function),
    TypeDefinition(SymbolId, Type),
    Variant(Variant),
    List(Vec<Expression>),
    Parenthesis(Box<Expression>),

    // --- Block --------
    Block(Block),

    // control flow
    Conditional(Conditional),

    // --- Operators ---
    UnaryPrefix {
        op: Spanned<UnaryPrefixOp>,
        val: Box<Expression>,
    },
    UnaryPostfix {
        val: Box<Expression>,
        op: Spanned<UnaryPostfixOp>,
    },
    Binary {
        left: Box<Expression>,
        op: Spanned<BinaryOp>,
        right: Box<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Named(SymbolId),
    Function {
        // TODO add and use spans
        parameters: Vec<Type>,
        return_type: Box<Type>,
    },
    Array(Box<Type>, usize),
    SumType(Vec<VariantDefinition>),
    Unit,
}

impl Type {
    pub fn f64() -> Self {
        Self::Named(SymbolId::from_str("f64"))
    }
    pub fn i64() -> Self {
        Self::Named(SymbolId::from_str("i64"))
    }
    pub fn bool() -> Self {
        Self::Named(SymbolId::from_str("bool"))
    }
    pub fn str() -> Self {
        Self::Named(SymbolId::from_str("str"))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariantDefinition(pub SymbolId, pub Vec<(SymbolId, Type)>);

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Named(symbol_id) => {
                write!(f, "{}", symbol_id.as_str())
            }
            Type::Function {
                parameters,
                return_type,
            } => {
                write!(f, "fn(")?;
                if let Some((last, most)) = parameters.split_last() {
                    for p in most {
                        write!(f, "{}, ", p)?;
                    }
                    write!(f, "{}", last)?;
                }
                write!(f, ") -> {}", return_type)
            }
            Type::Array(t, len) => {
                write!(f, "[{};{}]", t, len)
            }
            Type::SumType(variants) => {
                for variant in variants {
                    let VariantDefinition(name, fields) = variant;
                    write!(f, "'{}{{", name)?;
                    if let Some(((s, t), most)) = fields.split_last() {
                        for (s, t) in most {
                            write!(f, "{}: {}, ", s, t)?;
                        }
                        write!(f, "{}: {}", s, t)?;
                    }
                    write!(f, "}}")?;
                    if variants.last() != Some(variant) {
                        write!(f, " | ")?
                    }
                }
                Ok(())
            }
            Type::Unit => write!(f, "()"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct VariableDefinition {
    pub variable: Spanned<SymbolId>,
    pub known_type: Option<Spanned<Type>>,
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub definition: Spanned<VariableDefinition>,
    pub init: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Spanned<SymbolId>,
    pub parameters: Spanned<Vec<FunctionParam>>,
    pub return_type: Spanned<Type>,
    pub body: Spanned<Block>,
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub identifier: Spanned<SymbolId>,
    pub r#type: Spanned<Type>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub unit_return: bool,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Declaration(VariableDeclaration),
}

#[derive(Debug, Clone)]
pub struct Conditional {
    pub condition: Box<Expression>,
    pub pass: Spanned<Block>,
    pub fail: Option<Spanned<Block>>,
}

#[derive(Debug, Clone)]
pub struct Variant(pub SymbolId, pub Vec<(SymbolId, Expression)>);

#[derive(Debug, Clone)]
pub struct Call {
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub enum Op {
    Binary(BinaryOp),
    Postfix(UnaryPostfixOp),
    // prefix can be handled directly
}

#[derive(Debug, Clone)]
pub enum UnaryPostfixOp {
    // function call
    Call(Call),
    FieldAccess(SymbolId),
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryPrefixOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Assign,      // =
    Add,         // +
    Sub,         // -
    Mult,        // *
    Div,         // /
    Mod,         // %%
    Range,       // ..
    Eq,          // ==
    NotEq,       // !=
    LessOrEq,    // <=
    MoreOrEq,    // >=
    LargerThan,  // >
    SmallerThan, //<
    Or,          // ||
    And,         // &&
    Exp,         // ^
}
