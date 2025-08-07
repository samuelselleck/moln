use std::fmt::Display;

use crate::symbol_interning::SymbolId;

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum TokenKind {
    // brackets
    OpenAngBrack,
    CloseAngBrack,
    OpenCurlBrack,
    CloseCurlBrack,
    CloseParenth,
    OpenParenth,
    OpenSquareBrack,
    CloseSquareBrack,

    // single character operators
    Slash,
    Assign,
    Plus,
    Minus,
    Asterisk,
    AtSymbol,
    Mod,
    Period,
    Colon,
    Comma,
    Hashtag,
    Not,
    Exp,
    VertLine,
    Ampersand,

    // multi character operators
    PathSep,
    Range,
    LessOrEq,
    MoreOrEq,
    Eq,
    NotEq,
    Or,
    And,
    ThinArrow,

    // variables
    Identifier(SymbolId),

    // keywords
    For,
    In,
    If,
    Else,
    Fn,
    // TODO While,

    // values
    Integer(i64),
    Float(f64),
    String(SymbolId),
    True,
    False,

    // special
    Comment,
    SemiColon,
    Let,
    Tick,
    Type,

    #[default]
    Unknown,
    EOF,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TokenKind::OpenAngBrack => "\"<\"",
            TokenKind::CloseAngBrack => "\">\"",
            TokenKind::OpenCurlBrack => "\"{\"",
            TokenKind::CloseCurlBrack => "\"}\"",
            TokenKind::CloseParenth => "\")\"",
            TokenKind::OpenParenth => "\"(\"",
            TokenKind::Slash => "\"/\"",
            TokenKind::Assign => "\"=\"",
            TokenKind::Plus => "\"+\"",
            TokenKind::Minus => "\"-\"",
            TokenKind::Asterisk => "\"*\"",
            TokenKind::AtSymbol => "\"@\"",
            TokenKind::Identifier(s) => s.as_str(),
            TokenKind::For => "for",
            TokenKind::In => "in",
            TokenKind::If => "if",
            TokenKind::Mod => "\"%\"",
            TokenKind::Integer(i) => &i.to_string(),
            TokenKind::Float(f) => &f.to_string(),
            TokenKind::Period => "\".\"",
            TokenKind::Colon => "\":\"",
            TokenKind::EOF => "end of line",
            TokenKind::Unknown => "<UNKNOWN>",
            TokenKind::Comment => "comment",
            TokenKind::Comma => "\",\"",
            TokenKind::String(s) => s.as_str(),
            TokenKind::Hashtag => "\"#\"",
            TokenKind::Range => "\"..\"",
            TokenKind::OpenSquareBrack => "\"[\"",
            TokenKind::CloseSquareBrack => "\"]\"",
            TokenKind::Eq => "\"==\"",
            TokenKind::LessOrEq => "\"<=\"",
            TokenKind::MoreOrEq => "\">=\"",
            TokenKind::Not => "\"!\"",
            TokenKind::NotEq => "\"!=\"",
            TokenKind::VertLine => "\"|\"",
            TokenKind::Or => "\"||\"",
            TokenKind::Ampersand => "\"&\"",
            TokenKind::And => "\"&&\"",
            TokenKind::Exp => "\"^\"",
            TokenKind::PathSep => "\"::\"",
            TokenKind::SemiColon => "\";\"",
            TokenKind::Else => "else",
            TokenKind::Fn => "fn",
            TokenKind::ThinArrow => "\"->\"",
            TokenKind::Let => "let",
            TokenKind::True => "true",
            TokenKind::False => "false",
            TokenKind::Type => "type",
            TokenKind::Tick => "'",
        };
        write!(f, "{}", s)
    }
}
