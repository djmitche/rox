use crate::src::Src;
use crate::token::{Token, TokenType, Tokens};
use crate::util::str_offset_in;
use anyhow::Result;

struct Scanner<'p> {
    program: &'p str,
    offset: usize,
    tokens: Tokens,
}

impl<'p> Scanner<'p> {
    fn new(program: &'p str) -> Self {
        Scanner {
            program,
            offset: 0,
            tokens: Vec::new(),
        }
    }

    /// Peek at the next character.
    fn peek(&self) -> Option<char> {
        self.program[self.offset..].chars().next()
    }

    /// Peek at the character at the given offset from the current point.
    fn peek_at(&self, offset: usize) -> Option<char> {
        self.program[self.offset + offset..].chars().next()
    }

    /// Advance the given offset.
    fn advance(&mut self, len: usize) {
        self.offset += len;
    }

    /// Advance to the next newline
    fn advance_over_line(&mut self) {
        if let Some(len) = self.program[self.offset..].find('\n') {
            self.offset += len + 1;
        } else {
            self.offset = self.program.len();
        }
    }

    /// If the input is equal to the given keyword, push the corresponding token and advance over
    /// it.
    fn match_keyword(&mut self, ty: TokenType, kw: &str) -> bool {
        if self.program[self.offset..].starts_with(kw) {
            let next_char = self.program[self.offset + kw.len()..]
                .chars()
                .next()
                .unwrap_or(' ');
            if !next_char.is_alphanumeric() {
                self.push_token(ty, kw.len());
                return true;
            }
        }
        false
    }

    fn match_number(&mut self) -> Result<()> {
        // TODO: this should probably consider things like `123abc` invalid, but that is now
        // treated as Number(123) Identifier(abc).
        let start_offset = self.offset;
        let mut seen_dot = false;
        let mut last_was_dot = false;
        self.advance(1);
        while let Some(c) = self.peek() {
            match c {
                '0'..='9' => {
                    last_was_dot = false;
                    self.advance(1);
                }
                '.' => {
                    if seen_dot {
                        anyhow::bail!("Number with multiple dots");
                    }
                    seen_dot = true;
                    last_was_dot = true;
                    self.advance(1);
                }
                _ => break,
            }
        }
        if last_was_dot {
            anyhow::bail!("Number with a trailing dot");
        }
        let end_offset = self.offset;
        self.tokens.push(Token {
            ty: TokenType::Number,
            src: Src {
                offset: start_offset,
                len: end_offset - start_offset,
            },
        });
        Ok(())
    }

    fn match_string(&mut self) -> Result<()> {
        let start_offset = self.offset;
        let mut terminated = false;
        self.advance(1);
        while let Some(c) = self.peek() {
            self.advance(1);
            if c == '"' {
                terminated = true;
                break;
            }
        }
        if !terminated {
            anyhow::bail!("Unterminated string");
        }
        let end_offset = self.offset;
        self.tokens.push(Token {
            ty: TokenType::String,
            src: Src {
                offset: start_offset,
                len: end_offset - start_offset,
            },
        });
        Ok(())
    }

    fn match_identifier(&mut self) -> Result<()> {
        let start_offset = self.offset;
        self.advance(1);
        while let Some(c) = self.peek() {
            if !c.is_alphanumeric() && c != '_' {
                break;
            }
            self.advance(1);
        }
        let end_offset = self.offset;
        self.tokens.push(Token {
            ty: TokenType::Identifier,
            src: Src {
                offset: start_offset,
                len: end_offset - start_offset,
            },
        });
        Ok(())
    }

    fn push_token(&mut self, ty: TokenType, len: usize) {
        self.tokens.push(Token {
            ty,
            src: Src {
                offset: self.offset,
                len,
            },
        });
        self.offset += len;
    }

    fn scan(mut self) -> Result<Tokens> {
        use TokenType::*;
        let mut i = 100; // XXX
        while let Some(c) = self.peek() {
            i -= 1;
            if i == 0 {
                panic!();
            }
            // Note that some of the guard expressions here (`if ..`) modify `self` in their
            // success condition.
            match dbg!(c) {
                ' ' | '\t' | '\n' | '\r' => self.advance(1),
                '(' => self.push_token(LeftParen, 1),
                ')' => self.push_token(RightParen, 1),
                '{' => self.push_token(LeftBrace, 1),
                '}' => self.push_token(RightBrace, 1),
                ',' => self.push_token(Comma, 1),
                '.' => self.push_token(Dot, 1),
                '-' => self.push_token(Minus, 1),
                '+' => self.push_token(Plus, 1),
                ';' => self.push_token(Semicolon, 1),
                '/' if self.peek_at(1) != Some('/') => self.push_token(Slash, 1),
                '/' => self.advance_over_line(),
                '*' => self.push_token(Star, 1),
                '!' if self.peek_at(1) == Some('=') => self.push_token(BangEqual, 2),
                '!' => self.push_token(Bang, 1),
                '=' if self.peek_at(1) == Some('=') => self.push_token(EqualEqual, 2),
                '=' => self.push_token(Equal, 1),
                '>' if self.peek_at(1) == Some('=') => self.push_token(GreaterEqual, 2),
                '>' => self.push_token(Greater, 1),
                '<' if self.peek_at(1) == Some('=') => self.push_token(LessEqual, 2),
                '<' => self.push_token(Less, 1),
                'a' if self.match_keyword(And, "and") => {}
                'c' if self.match_keyword(Class, "class") => {}
                'e' if self.match_keyword(Else, "else") => {}
                'f' if self.match_keyword(False, "false") => {}
                'f' if self.match_keyword(Fun, "fun") => {}
                'f' if self.match_keyword(For, "for") => {}
                'i' if self.match_keyword(If, "if") => {}
                'n' if self.match_keyword(Nil, "nil") => {}
                'o' if self.match_keyword(Or, "or") => {}
                'p' if self.match_keyword(Print, "print") => {}
                'r' if self.match_keyword(Return, "return") => {}
                's' if self.match_keyword(Super, "super") => {}
                't' if self.match_keyword(This, "this") => {}
                't' if self.match_keyword(True, "true") => {}
                'v' if self.match_keyword(Var, "var") => {}
                'w' if self.match_keyword(While, "while") => {}
                '"' => self.match_string()?,
                '0'..='9' => self.match_number()?,
                'a'..='z' | 'A'..='Z' | '_' => self.match_identifier()?,
                _ => anyhow::bail!("Unexpected character: {c}"),
            }
        }
        return Ok(self.tokens);
    }
}

/// Scan the given input into a sequence of tokens.
pub fn scan(program: &str) -> Result<Tokens> {
    let scanner = Scanner::new(program);
    scanner.scan()
}

#[cfg(test)]
mod test {
    use super::*;
    use TokenType::*;

    /// Convert Tokens into tuples of type and string slice. This allows easier assertions in
    /// tests, instead of comparing against `Token` values, which include offsets in the program.
    ///
    /// The cost is that assertions don't completely cover those offsets. For example, in `a + a`,
    /// the first and last tokens are represented as `(Identifier, "a")`, and the assertion would
    /// pass even if both are slices of the first occurrence of `a` in the program.
    fn scan_result<'p>(program: &'p str) -> Vec<(TokenType, &'p str)> {
        let tokens = scan(dbg!(program)).unwrap();
        tokens
            .into_iter()
            .map(|token| (token.ty, token.src_str(program)))
            .collect()
    }

    #[test]
    fn parens_and_whitespace() {
        let exp = vec![(LeftParen, "("), (RightParen, ")")];
        assert_eq!(scan_result("()"), exp);
        assert_eq!(scan_result("( )"), exp);
        assert_eq!(scan_result(" ()"), exp);
        assert_eq!(scan_result("() "), exp);
        assert_eq!(scan_result(" ( )"), exp);
        assert_eq!(scan_result(" () "), exp);
        assert_eq!(scan_result(" ( ) "), exp);
    }

    #[test]
    fn punctuation() {
        let exp = vec![
            (LeftParen, "("),
            (RightParen, ")"),
            (LeftBrace, "{"),
            (RightBrace, "}"),
            (Comma, ","),
            (Dot, "."),
            (Minus, "-"),
            (Plus, "+"),
            (Slash, "/"),
            (Semicolon, ";"),
            (Star, "*"),
            (Bang, "!"),
            (BangEqual, "!="),
            (EqualEqual, "=="),
            (Equal, "="),
            (Greater, ">"),
            (GreaterEqual, ">="),
            (Less, "<"),
            (LessEqual, "<="),
        ];
        assert_eq!(scan_result("(){},.-+/;*!!====>>=<<="), exp);
        assert_eq!(
            scan_result("( ) { } , . - + / ; * ! != == = > >= < <="),
            exp
        );
    }

    #[test]
    fn multi_equal() {
        // This is a choice, but we choose to parse multiple `=` characters preferentially as `==`
        // with an odd number ending with `=`.
        assert_eq!(scan_result("==="), vec![(EqualEqual, "=="), (Equal, "=")]);
        assert_eq!(
            scan_result("===="),
            vec![(EqualEqual, "=="), (EqualEqual, "==")]
        );
        assert_eq!(
            scan_result("====="),
            vec![(EqualEqual, "=="), (EqualEqual, "=="), (Equal, "=")]
        );
    }

    #[test]
    fn keywords_and_whitespace() {
        let exp = vec![(And, "and")];
        assert_eq!(scan_result("and"), exp);
        assert_eq!(scan_result(" and"), exp);
        assert_eq!(scan_result("and "), exp);
        assert_eq!(scan_result(" and "), exp);
    }

    #[test]
    fn keywords() {
        let exp = vec![
            (And, "and"),
            (Class, "class"),
            (Else, "else"),
            (False, "false"),
            (Fun, "fun"),
            (For, "for"),
            (If, "if"),
            (Nil, "nil"),
            (Or, "or"),
            (Print, "print"),
            (Return, "return"),
            (Super, "super"),
            (This, "this"),
            (True, "true"),
            (Var, "var"),
            (While, "while"),
        ];
        assert_eq!(
            scan_result(
                "and class else false fun for if nil or print return super this true var while"
            ),
            exp
        );
    }

    #[test]
    fn keywords_mushed_together() {
        assert_eq!(scan_result("andand"), vec![(Identifier, "andand")]);
        assert_eq!(scan_result("varif"), vec![(Identifier, "varif")]);
        assert_eq!(scan_result("and and"), vec![(And, "and"), (And, "and")]);
    }

    #[test]
    fn keywords_and_punctuation() {
        let exp = vec![(LeftParen, "("), (And, "and"), (RightBrace, "}")];
        assert_eq!(scan_result("(and}"), exp);
        assert_eq!(scan_result("( and }"), exp);
    }

    #[test]
    fn numbers() {
        assert_eq!(scan_result("1.2"), vec![(Number, "1.2")]);
        assert_eq!(scan_result("12.34"), vec![(Number, "12.34")]);
        assert_eq!(scan_result("1"), vec![(Number, "1")]);
    }

    #[test]
    fn comments() {
        assert_eq!(scan_result("if // 123"), vec![(If, "if")]);
        assert_eq!(
            scan_result("if // 123\nelse"),
            vec![(If, "if"), (Else, "else")]
        );
    }

    #[test]
    fn invalid_numbers() {
        assert!(scan("1.").is_err());
        assert!(scan("12.").is_err());
        assert!(scan("1.2.3").is_err());
        // Maybe these should be an error, but let's just tokenize them as we find them.
        assert_eq!(scan_result(".1"), vec![(Dot, "."), (Number, "1")]);
        assert_eq!(scan_result(".12"), vec![(Dot, "."), (Number, "12")]);
    }

    #[test]
    fn identifiers() {
        for ident in ["foo", "_foo", "foo_", "foo_bar", "foo123", "_123", "__"] {
            assert_eq!(scan_result(ident), vec![(Identifier, ident)]);
        }
    }

    #[test]
    fn strings() {
        assert_eq!(scan_result(r#""foo""#), vec![(String, r#""foo""#)]);
        assert_eq!(scan_result(r#""""#), vec![(String, r#""""#)]);
        assert_eq!(scan_result(r#""123""#), vec![(String, r#""123""#)]);
        assert_eq!(scan_result(r#""if""#), vec![(String, r#""if""#)]);
        assert_eq!(scan_result(r#""foo bar""#), vec![(String, r#""foo bar""#)]);
        assert_eq!(scan_result("\"foo\nbar\""), vec![(String, "\"foo\nbar\"")]);
    }
}
