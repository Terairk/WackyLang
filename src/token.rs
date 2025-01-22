use crate::shared::SharedString;
use crate::spanned::Spanned;
use crate::CharExt;
use chumsky::{prelude::*, text, text::TextParser};
use std::fmt;

pub trait Lexeme {
    fn lexeme(&self) -> SharedString;
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Symbol(Symbol),
    Keyword(Keyword),
    Ident(SharedString),
    IntLiter(SharedString),
    CharLiter(SharedString),
    StrLiter(SharedString),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.lexeme())
    }
}

impl Lexeme for Token {
    fn lexeme(&self) -> SharedString {
        match self {
            Token::Symbol(t) => t.lexeme(),
            Token::Keyword(t) => t.lexeme(),
            Token::Ident(s) => s.clone(),
            Token::IntLiter(s) => s.clone(),
            Token::CharLiter(s) => s.clone(),
            Token::StrLiter(s) => s.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Symbol {
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Lt,
    Gt,
    Semicolon,
    Comma,
    Star,
    Percent,
    Plus,
    Minus,
    ForwardSlash,
    Bang,
    Equals,
    BangEquals,
    EqualsEquals,
    Lte,
    Gte,
    And,
    Or,
}

impl Lexeme for Symbol {
    fn lexeme(&self) -> SharedString {
        SharedString::from(match self {
            Symbol::OpenParen => "(",
            Symbol::CloseParen => ")",
            Symbol::OpenBracket => "[",
            Symbol::CloseBracket => "]",
            Symbol::Lt => "<",
            Symbol::Gt => ">",
            Symbol::Semicolon => ";",
            Symbol::Comma => ",",
            Symbol::Star => "*",
            Symbol::Percent => "%",
            Symbol::Plus => "+",
            Symbol::Minus => "-",
            Symbol::ForwardSlash => "/",
            Symbol::Bang => "!",
            Symbol::Equals => "=",
            Symbol::BangEquals => "!=",
            Symbol::EqualsEquals => "==",
            Symbol::Lte => "<=",
            Symbol::Gte => ">=",
            Symbol::And => "&&",
            Symbol::Or => "||",
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Keyword {
    Begin,
    End,
    Is,
    Skip,
    Read,
    Free,
    Return,
    Exit,
    Print,
    Println,
    If,
    Then,
    Else,
    Fi,
    While,
    Do,
    Done,
    Newpair,
    Pair,
    Fst,
    Snd,
    Call,
    Int,
    Bool,
    Char,
    String,
    Len,
    Ord,
    Chr,
    Null,
    True,
    False,
}

impl Lexeme for Keyword {
    fn lexeme(&self) -> SharedString {
        SharedString::from(match self {
            Keyword::Begin => "begin",
            Keyword::End => "end",
            Keyword::Is => "is",
            Keyword::Skip => "skip",
            Keyword::Read => "read",
            Keyword::Free => "free",
            Keyword::Return => "return",
            Keyword::Exit => "exit",
            Keyword::Print => "print",
            Keyword::Println => "println",
            Keyword::If => "if",
            Keyword::Then => "then",
            Keyword::Else => "else",
            Keyword::Fi => "fi",
            Keyword::While => "while",
            Keyword::Do => "do",
            Keyword::Done => "done",
            Keyword::Newpair => "newpair",
            Keyword::Pair => "pair",
            Keyword::Fst => "fst",
            Keyword::Snd => "snd",
            Keyword::Call => "call",
            Keyword::Int => "int",
            Keyword::Bool => "bool",
            Keyword::Char => "char",
            Keyword::String => "string",
            Keyword::Len => "len",
            Keyword::Ord => "ord",
            Keyword::Chr => "chr",
            Keyword::Null => "null",
            Keyword::True => "true",
            Keyword::False => "false",
        })
    }
}

pub fn lexer() -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
    let open_angle = just('<');
    let close_angle = just('>');
    let plus = just('+');
    let minus = just('-');
    let bang = just('!');
    let equals = just('=');
    let ampersand = just('&');
    let pipe = just('|');

    // parser for those symbol-tokens whos ambiguity only arises with other
    // symbol tokens, e.g. `>=` is included while `-` is not because of negative
    // integer literal ambiguity
    let symbol = choice((
        // atomic symbol tokens, i.e. without ambiguity
        just('(').to(Symbol::OpenParen),
        just(')').to(Symbol::CloseParen),
        just('[').to(Symbol::OpenBracket),
        just(']').to(Symbol::CloseBracket),
        just(';').to(Symbol::Semicolon),
        just(',').to(Symbol::Comma),
        just('*').to(Symbol::Star),
        just('%').to(Symbol::Percent),
        just('/').to(Symbol::ForwardSlash),
        ampersand.then(ampersand).to(Symbol::And),
        pipe.then(pipe).to(Symbol::Or),
        // parsers for symbols modified by equals: `<`, `<=`, `>`, `>=`, `!`, `!=`, `=`, `==`
        open_angle
            .then(equals)
            .to(Symbol::Lte)
            .or(open_angle.to(Symbol::Lt)),
        close_angle
            .then(equals)
            .to(Symbol::Gte)
            .or(close_angle.to(Symbol::Gt)),
        bang.then(equals)
            .to(Symbol::BangEquals)
            .or(bang.to(Symbol::Bang)),
        equals
            .then(equals)
            .to(Symbol::EqualsEquals)
            .or(equals.to(Symbol::Equals)),
    ))
    .map(Token::Symbol);

    // parser for keywords and identifiers
    // WACC identifiers are C-style, so we can use the default `text::ident` parser
    let ident = text::ident().map(|s: String| match s.as_str() {
        "begin" => Token::Keyword(Keyword::Begin),
        "end" => Token::Keyword(Keyword::End),
        "is" => Token::Keyword(Keyword::Is),
        "skip" => Token::Keyword(Keyword::Skip),
        "read" => Token::Keyword(Keyword::Read),
        "free" => Token::Keyword(Keyword::Free),
        "return" => Token::Keyword(Keyword::Return),
        "exit" => Token::Keyword(Keyword::Exit),
        "print" => Token::Keyword(Keyword::Print),
        "println" => Token::Keyword(Keyword::Println),
        "if" => Token::Keyword(Keyword::If),
        "then" => Token::Keyword(Keyword::Then),
        "else" => Token::Keyword(Keyword::Else),
        "fi" => Token::Keyword(Keyword::Fi),
        "while" => Token::Keyword(Keyword::While),
        "do" => Token::Keyword(Keyword::Do),
        "done" => Token::Keyword(Keyword::Done),
        "newpair" => Token::Keyword(Keyword::Newpair),
        "pair" => Token::Keyword(Keyword::Pair),
        "fst" => Token::Keyword(Keyword::Fst),
        "snd" => Token::Keyword(Keyword::Snd),
        "call" => Token::Keyword(Keyword::Call),
        "int" => Token::Keyword(Keyword::Int),
        "bool" => Token::Keyword(Keyword::Bool),
        "char" => Token::Keyword(Keyword::Char),
        "string" => Token::Keyword(Keyword::String),
        "len" => Token::Keyword(Keyword::Len),
        "ord" => Token::Keyword(Keyword::Ord),
        "chr" => Token::Keyword(Keyword::Chr),
        "null" => Token::Keyword(Keyword::Null),
        "true" => Token::Keyword(Keyword::True),
        "false" => Token::Keyword(Keyword::False),
        _ => Token::Ident(SharedString::from(s)),
    });

    // A parser for integer literals
    let int_liter = {
        let digits = one_of("0123456789")
            .repeated()
            .at_least(1)
            .collect::<String>();
        let sign = plus
            .or(minus)
            .map(char::to_string_owned)
            .or_not()
            .map(Option::unwrap_or_default);
        sign.then(digits)
            .map(|(sign, digits)| Token::IntLiter(SharedString::from(sign.to_string() + &*digits)))
            .labelled("<int-liter>")
    };

    // A character parser
    let normal_char = filter(char::normal_wacc_char).map(char::to_string_owned);
    let escaped_char = just('\\')
        .chain(filter(char::escaped_wacc_char))
        .collect::<String>();
    let character = normal_char.or(escaped_char).labelled("<character>");

    // character delimiter and literal parser
    let char_liter_delim = just('\'');
    let char_liter = character
        .delimited_by(char_liter_delim, char_liter_delim)
        .map(SharedString::from)
        .map(Token::CharLiter)
        .labelled("<char-liter>");

    // string delimiter and literal parser
    let str_liter_delim = just('"');
    let str_liter = character
        .repeated()
        .delimited_by(str_liter_delim, str_liter_delim)
        .collect::<String>()
        .map(SharedString::from)
        .map(Token::StrLiter)
        .labelled("<str-liter>");

    // a single token can be one of: symbol, int literal, char literal, string literal,
    // and finally keyword/identifier
    let token = symbol
        .or(
            // disambiguation parser for integer literals, `+` and `-` symbols
            int_liter.or(choice([
                plus.to(Token::Symbol(Symbol::Plus)),
                minus.to(Token::Symbol(Symbol::Minus)),
            ])),
        )
        .or(char_liter)
        .or(str_liter)
        .or(ident)
        // If we encounter an error, skip and attempt to lex the next character as a token instead
        .recover_with(skip_then_retry_until([]));

    // the comments are only single-line, started with a hashtag
    let comment = just("#").then_ignore(take_until(just('\n'))).padded();

    // TODO: figure out error recovery with delimiters ' and " ????

    // the final token parser should ignore whitespace and comments, preserving the span of each token
    token
        .map_with_span(|t, s| Spanned::new(t, s))
        .padded()
        .padded_by(comment.repeated())
        .repeated()
}
