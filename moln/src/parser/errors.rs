use crate::error::CompilerError;
use crate::lexer::TokenKind;
use crate::{Parser, lexer::Token};

impl<'src> Parser<'src> {
    pub fn expect(&mut self, token: TokenKind) -> Result<Token, CompilerError> {
        let next = self.next_token();
        if next.kind == token {
            Ok(next)
        } else {
            Err(CompilerError::new("unexpected character(s)")
                .annotation(next.span, format!("expected {}", token)))
        }
    }

    pub fn expect_sequence<const N: usize>(
        &mut self,
        expected_types: [TokenKind; N],
    ) -> Result<[Token; N], CompilerError> {
        let mut values = [Token::default(); N];
        for i in 0..N {
            values[i] = self.expect(expected_types[i])?;
        }
        Ok(values)
    }
}
