use std::str::CharIndices;
mod span;
mod token;
pub use span::Span;
pub use token::TokenKind;

use crate::{symbol_interning::SymbolId, utils::MultiPeek};

#[derive(Debug, Clone, Copy, Default)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

/// Iterator over tokens in the given file
/// NOTE: This iterator is infinite, returning TokenKind::EOF
/// forever when hitting end of file.
pub struct TokenIterator<'src> {
    itr: MultiPeek<CharIndices<'src>>,
    pub src: &'src str,
}

impl<'src> Iterator for TokenIterator<'src> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        //trim all white spaces
        while let Some(_) = self.itr.next_if(|(_, c)| c.is_whitespace()) {}

        let Some((_, char)) = self.itr.peek().cloned() else {
            let eof_start = self.src.chars().count();
            return Some(Token {
                span: Span {
                    start: eof_start,
                    end: eof_start + 1,
                },
                kind: TokenKind::EOF,
            });
        };

        Some(match char {
            c if c.is_alphabetic() => self.ident_like(),
            v if v.is_ascii_digit() => self.number(),
            '(' => self.single(TokenKind::OpenParenth),
            ')' => self.single(TokenKind::CloseParenth),
            '[' => self.single(TokenKind::OpenSquareBrack),
            ']' => self.single(TokenKind::CloseSquareBrack),
            '{' => self.single(TokenKind::OpenCurlBrack),
            '}' => self.single(TokenKind::CloseCurlBrack),
            '*' => self.single(TokenKind::Asterisk),
            '+' => self.single(TokenKind::Plus),
            '-' => self.one_or_two_if_with(TokenKind::Minus, '>', TokenKind::ThinArrow),
            '@' => self.single(TokenKind::AtSymbol),
            ':' => self.one_or_two_if_with(TokenKind::Colon, ':', TokenKind::PathSep),
            ';' => self.single(TokenKind::SemiColon),
            ',' => self.single(TokenKind::Comma),
            '#' => self.single(TokenKind::Hashtag),
            '^' => self.single(TokenKind::Exp),
            '/' => self.comment_or_slash(),
            '"' => self.string(),
            '.' => self.one_or_two_if_with(TokenKind::Period, '.', TokenKind::Range),
            '%' => self.single(TokenKind::Mod),
            '<' => self.one_or_two_if_with(TokenKind::OpenAngBrack, '=', TokenKind::LessOrEq),
            '>' => self.one_or_two_if_with(TokenKind::CloseAngBrack, '=', TokenKind::MoreOrEq),
            '=' => self.one_or_two_if_with(TokenKind::Assign, '=', TokenKind::Eq),
            '!' => self.one_or_two_if_with(TokenKind::Not, '=', TokenKind::NotEq),
            '|' => self.one_or_two_if_with(TokenKind::VertLine, '|', TokenKind::Or),
            '&' => self.one_or_two_if_with(TokenKind::Ampersand, '&', TokenKind::And),
            c => {
                eprintln!("unknown token: {:?}", c);
                self.single(TokenKind::Unknown)
            }
        })
    }
}

impl<'src> TokenIterator<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            itr: MultiPeek::new(source.char_indices()),
            src: source,
        }
    }

    fn single(&mut self, token_type: TokenKind) -> Token {
        let Some((start, c)) = self.itr.next() else {
            unreachable!("already peeked to match on this function")
        };
        let end = start + c.len_utf8();
        return Token {
            span: Span { start, end },
            kind: token_type,
        };
    }

    fn ident_like(&mut self) -> Token {
        let Some((start, _)) = self.itr.next() else {
            unreachable!("already peeked to match on this function")
        };
        let mut end = start;

        while let Some((i, _)) = self
            .itr
            .next_if(|(_, c)| c.is_ascii_alphabetic() || c.is_ascii_digit() || c == &'_')
        {
            end = i;
        }
        end += 1;
        let src = &self.src[start..end];

        // Trim "self." and "this." in the entire file,
        // instead returning the identifier directly afterwards
        if matches!(src, "self" | "this") {
            if self.itr.next_if(|&(_, c)| c == '.').is_some() {
                // read the next ident instead
                return self.ident_like();
            }
        }

        // Keywords
        let token_type = match src {
            "for" => TokenKind::For,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "in" => TokenKind::In,
            "fn" => TokenKind::Fn,
            "let" => TokenKind::Let,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            ident => TokenKind::Identifier(SymbolId::from_str(ident)),
        };
        return Token {
            span: Span { start, end },
            kind: token_type,
        };
    }

    fn number(&mut self) -> Token {
        let Some((start, _)) = self.itr.next() else {
            unreachable!("already peeked to match on this function")
        };

        let mut end = start;
        while let Some((i, _)) = self.itr.next_if(|(_, c)| c.is_ascii_digit()) {
            end = i;
        }
        let is_float_next = self.itr.peek().is_some_and(|&(_, c)| c == '.')
            && self
                .itr
                .peek_nth(1)
                .is_some_and(|&(_, c)| c.is_ascii_digit());

        if is_float_next {
            // skip '.'
            self.itr.next();
            while let Some((i, _)) = self.itr.next_if(|(_, c)| c.is_ascii_digit()) {
                end = i;
            }
        }
        end += 1;
        Token {
            span: Span { start, end },
            kind: if is_float_next {
                TokenKind::Float(
                    self.src[start..end]
                        .parse()
                        .expect("valid because of above validation"),
                )
            } else {
                TokenKind::Integer(
                    self.src[start..end]
                        .parse()
                        .expect("valid because of above validation"),
                )
            },
        }
    }

    fn comment_or_slash(&mut self) -> Token {
        let Some((start, _)) = self.itr.next() else {
            unreachable!("already peeked to match on this function")
        };

        let mut end = start;
        let token_type = match self.itr.peek() {
            Some(&(_, '/')) => {
                while let Some((i, _)) = self.itr.next_if(|&(_, c)| c != '\n') {
                    end = i;
                }
                TokenKind::Comment
            }
            _ => TokenKind::Slash,
        };
        end += 1;

        Token {
            span: Span { start, end },
            kind: token_type,
        }
    }

    fn one_or_two_if_with(&mut self, if_one: TokenKind, if_with: char, if_two: TokenKind) -> Token {
        let Some((start, character)) = self.itr.next() else {
            unreachable!("already peeked to match on this function")
        };
        let (token_type, end) = if let Some((i, _)) = self.itr.next_if(|&(_, c)| c == if_with) {
            (if_two, i)
        } else {
            (if_one, start + character.len_utf8())
        };
        Token {
            span: Span { start, end },
            kind: token_type,
        }
    }

    fn string(&mut self) -> Token {
        let Some((start, _)) = self.itr.next() else {
            unreachable!("already peeked to match on this function")
        };

        let mut end = start;
        let mut esc = false;
        loop {
            let Some((i, c)) = self.itr.next() else { break };
            end = i;
            match c {
                '\\' => {
                    esc = !esc;
                    continue;
                }
                '"' => {
                    if !esc {
                        break;
                    }
                }
                _ => (),
            };
            esc = false;
        }
        end += 1;

        Token {
            span: Span { start, end },
            kind: TokenKind::String(SymbolId::from_str(&self.src[start..end])),
        }
    }
}
