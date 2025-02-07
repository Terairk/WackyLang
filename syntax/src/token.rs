#![allow(clippy::arbitrary_source_item_ordering)]

use crate::ext::ParserExt as _;
use crate::{alias, ast, ext::CharExt as _, private};
use chumsky::combinator::MapWith;
use chumsky::extra::ParserExtra;
use chumsky::input::MapExtra;
use chumsky::{error::Rich, input::StrInput, prelude::*, text};
use extend::ext;
use internment::ArcIntern;
use std::fmt;
use std::fmt::Debug;
use std::hash::Hash;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Delim {
    Bracket = 0,
    Paren = 1,
}

impl Delim {
    /// The current number of variants.
    const NUM_VARIANTS: usize = 2;
    /// The current variants.
    const VARIANTS: [Self; Self::NUM_VARIANTS] = [Self::Paren, Self::Bracket];
    /// Placeholder element, useful for allocating arrays.
    const PLACEHOLDER: Self = Self::Paren;

    /// A copy of the current variants.
    #[must_use]
    #[inline]
    pub const fn variants() -> [Self; Self::NUM_VARIANTS] {
        Self::VARIANTS
    }

    /// A reference to the current variants.
    #[must_use]
    #[inline]
    pub const fn variants_ref() -> &'static [Self; Self::NUM_VARIANTS] {
        &Self::VARIANTS
    }

    #[allow(
        clippy::indexing_slicing,
        clippy::as_conversions,
        clippy::arithmetic_side_effects
    )]
    #[inline]
    #[must_use]
    pub const fn variants_except(self) -> [Self; Self::NUM_VARIANTS - 1] {
        // allocate output array
        let mut others: [Self; Self::NUM_VARIANTS - 1] = [Self::PLACEHOLDER];

        // for loops not allowed in const-rust
        let mut i = 0;
        let mut j = 0;
        while i < Self::NUM_VARIANTS {
            let delim = Self::VARIANTS[i];

            // `Eq` trait calls not allowed in const-rust,
            // have to cast to u8-repr and compare that instead
            if self as u8 != delim as u8 {
                others[j] = delim;
                j += 1;
            }
            i += 1;
        }

        others
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum InvalidCharReason {
    NonAsciiChar,
    InvalidEscape,
    MoreThanOneChar,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    // literals
    Ident(ast::Ident),
    IntLiter(i32),
    CharLiter(char),
    StrLiter(ArcIntern<str>),

    // delimiter symbols
    Open(Delim),
    Close(Delim),

    // other symbols
    Lte,
    Lt,
    Gte,
    Gt,
    BangEquals,
    Bang,
    EqualsEquals,
    Equals,
    Plus,
    Minus,
    Star,
    Percent,
    ForwardSlash,
    And,
    Or,
    Semicolon,
    Comma,

    // keywords
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

impl fmt::Display for Token {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Ident(ref s) => write!(f, "{s}"),
            Self::IntLiter(ref i) => write!(f, "{i}"),
            Self::CharLiter(ref c) => write!(f, "{c}"), // TODO: unescape the character literal, i.e. newline -> '\c'
            Self::StrLiter(ref s) => write!(f, "{s}"), // TODO: unescape the string literal, as with char literal
            Self::Open(ref d) => match *d {
                Delim::Paren => write!(f, "("),
                Delim::Bracket => write!(f, "["),
            },
            Self::Close(ref d) => match *d {
                Delim::Paren => write!(f, ")"),
                Delim::Bracket => write!(f, "]"),
            },
            Self::Lte => write!(f, "<="),
            Self::Lt => write!(f, "<"),
            Self::Gte => write!(f, ">="),
            Self::Gt => write!(f, ">"),
            Self::BangEquals => write!(f, "!="),
            Self::Bang => write!(f, "!"),
            Self::EqualsEquals => write!(f, "=="),
            Self::Equals => write!(f, "="),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Percent => write!(f, "%"),
            Self::ForwardSlash => write!(f, "/"),
            Self::And => write!(f, "&&"),
            Self::Or => write!(f, "||"),
            Self::Semicolon => write!(f, ";"),
            Self::Comma => write!(f, ","),
            Self::Begin => write!(f, "begin"),
            Self::End => write!(f, "end"),
            Self::Is => write!(f, "is"),
            Self::Skip => write!(f, "skip"),
            Self::Read => write!(f, "read"),
            Self::Free => write!(f, "free"),
            Self::Return => write!(f, "return"),
            Self::Exit => write!(f, "exit"),
            Self::Print => write!(f, "print"),
            Self::Println => write!(f, "println"),
            Self::If => write!(f, "if"),
            Self::Then => write!(f, "then"),
            Self::Else => write!(f, "else"),
            Self::Fi => write!(f, "fi"),
            Self::While => write!(f, "while"),
            Self::Do => write!(f, "do"),
            Self::Done => write!(f, "done"),
            Self::Newpair => write!(f, "newpair"),
            Self::Pair => write!(f, "pair"),
            Self::Fst => write!(f, "fst"),
            Self::Snd => write!(f, "snd"),
            Self::Call => write!(f, "call"),
            Self::Int => write!(f, "int"),
            Self::Bool => write!(f, "bool"),
            Self::Char => write!(f, "char"),
            Self::String => write!(f, "string"),
            Self::Len => write!(f, "len"),
            Self::Ord => write!(f, "ord"),
            Self::Chr => write!(f, "chr"),
            Self::Null => write!(f, "null"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
        }
    }
}

/// An intermediary token-type that encodes the corrections made to the grammar, to accommodate for
/// the mistakes made by the longest-match lexing algorithm
enum LongestMatchCorrectionToken<'src, I: Input<'src>> {
    Token(Token),
    NoWhitespacePlusMinusIntLiter(Vec<(Token, I::Span)>),
}

#[allow(
    clippy::missing_panics_doc,
    clippy::too_many_lines,
    clippy::single_call_fn
)]
#[inline]
fn unflattened_token_lexer<'src, I>(
) -> impl alias::Parser<'src, I, Vec<(LongestMatchCorrectionToken<'src, I>, I::Span)>>
where
    I: StrInput<'src, Token = char, Slice = &'src str>,
{
    // TODO: add labels where appropriate
    // TODO: think more about error handling: in particular the char/string delimiters

    // TODO: eventually replace with custom error type with variants and so on,
    // TODO: so it is easier to create type-safe custom errors that can be reported later on

    // WACC identifiers are C-style, so we can use the default `text::ident` parser
    let ident = text::ident()
        .pipe((
            ast::Ident::from_str,
            Token::Ident,
            LongestMatchCorrectionToken::Token,
        ))
        .labelled("<ident>");

    // copy the Regex pattern found in the WACC spec verbatim
    let int_liter = regex("[\\+-]?[0-9]+")
        .try_map(|s: &str, span| {
            s.parse::<i32>().map_err(|_| {
                Rich::custom(
                    span,
                    format!("The integer literal '{s}' does not fit within 32 bytes"),
                )
            })
        })
        .map(Token::IntLiter)
        .labelled("<int-liter>");

    // character parser
    let well_formed_character = choice((
        any().filter(char::normal_wacc_char),
        just('\\')
            .ignore_then(any())
            .filter(char::escaped_wacc_char)
            .map(|c| {
                #[allow(clippy::unwrap_used)]
                c.lookup_escaped_wacc_char().unwrap()
            }),
    ))
    .labelled("<character>");

    // character literal parser
    let char_delim = just('\'');
    let char_liter = char_delim
    .ignore_then(
        choice((
            well_formed_character.map(
                |c| (c, false)
            ),
            any().and_is(char_delim.not()).map(
                |c| (c, true)
            ),
        ))
        .repeated()
        .collect::<Vec<(char, bool)>>()
        .then_ignore(char_delim),
    )
    .try_map_with(|content, e| {
        let ct_len = content.len();
        if ct_len > 2 {
            Err(Rich::custom(e.span(), "Char literals should only have one character"))
        } else if ct_len == 0 {
            Err(Rich::custom(e.span(), "Empty character literal"))
        } else {
            if let Some(&(c, ill)) = content.get(0) {
                if ill {
                    if c == '\\' {
                        Err(Rich::custom(e.span(), "Invalid control character"))
                    } else {
                        Err(Rich::custom(e.span(), "Non-ASCII character"))
                    }
                } else {
                    if ct_len == 2 {
                        Err(Rich::custom(e.span(), "Char literals should only have one character"))
                    } else {
                        Ok(Token::CharLiter(c))
                    }
                }
            } else {
                unreachable!();
            }
        }
    })
    .recover_with(via_parser(
        char_delim.ignore_then(
            choice((
                just('\\').ignore_then(any()),
                any().and_is(char_delim.not()),
            ))
            .repeated()
            .collect::<String>()
            .then_ignore(char_delim)
            .map_with(|_, _| {
                Token::CharLiter('6')
            })
        ),
    ))
    .map(LongestMatchCorrectionToken::Token)
    .labelled("<char-liter>");

    // string literal parser
    let str_delim = just('"');
    let str_liter = str_delim
        .ignore_then(
        choice((
            well_formed_character.map(
                |c| (c, false)
            ),
            any().and_is(str_delim.not()).map(
                |c| (c, true)
                ),
            ))
        .repeated()
        .collect::<Vec<(char, bool)>>()
        .then_ignore(str_delim)
        .try_map_with(|chars, e| {
            let mut string_builder = String::with_capacity(chars.len());

            for &(c, ill_formed) in &chars {
                if ill_formed {
                    if c == '\\' {
                        return Err(Rich::custom(e.span(), "Invalid control character"));
                    } else if c == '\n' {
                        return Err(Rich::custom(e.span(), "No new line"));
                    } else {
                        return Err(Rich::custom(e.span(), "Non-ASCII character"));
                    }
                }

                string_builder.push(c);
            }

            return Ok(ArcIntern::from(string_builder));
        })
        )
        .recover_with(via_parser(
            str_delim.ignore_then(
                choice((
                    just('\\').ignore_then(any()),
                    any().and_is(str_delim.not()),
                ))
                .repeated()
                .collect::<String>()
                .then_ignore(str_delim)
                .map_with(|lit, _| {
                    ArcIntern::from(lit)
                })
            ),
        ))
        .pipe((
            Token::StrLiter,
        ))
        .map(LongestMatchCorrectionToken::Token)
        .labelled("<str-liter>");

    let delim_symbols = choice((
        just('(').to(Token::Open(Delim::Paren)),
        just(')').to(Token::Close(Delim::Paren)),
        just('[').to(Token::Open(Delim::Bracket)),
        just(']').to(Token::Close(Delim::Bracket)),
    ))
    .map(LongestMatchCorrectionToken::Token);

    // all other symbols parser
    let plus = just('+').to(Token::Plus);
    let minus = just('-').to(Token::Minus);
    let other_symbols = choice((
        // if some symbols are ambiguous, make sure to place the more 'long' ones ahead
        just("<=").to(Token::Lte),
        just('<').to(Token::Lt),
        just(">=").to(Token::Gte),
        just('>').to(Token::Gt),
        just("!=").to(Token::BangEquals),
        just('!').to(Token::Bang),
        just("==").to(Token::EqualsEquals),
        just('=').to(Token::Equals),
        // symbols without ambiguity
        plus.clone(),
        minus.clone(),
        just('*').to(Token::Star),
        just('%').to(Token::Percent),
        just('/').to(Token::ForwardSlash),
        just("&&").to(Token::And),
        just("||").to(Token::Or),
        just(';').to(Token::Semicolon),
        just(',').to(Token::Comma),
    ))
    .map(LongestMatchCorrectionToken::Token);

    let keywords = choice([
        text::keyword("begin").to(Token::Begin),
        text::keyword("end").to(Token::End),
        text::keyword("is").to(Token::Is),
        text::keyword("skip").to(Token::Skip),
        text::keyword("read").to(Token::Read),
        text::keyword("free").to(Token::Free),
        text::keyword("return").to(Token::Return),
        text::keyword("exit").to(Token::Exit),
        text::keyword("print").to(Token::Print),
        text::keyword("println").to(Token::Println),
        text::keyword("if").to(Token::If),
        text::keyword("then").to(Token::Then),
        text::keyword("else").to(Token::Else),
        text::keyword("fi").to(Token::Fi),
        text::keyword("while").to(Token::While),
        text::keyword("do").to(Token::Do),
        text::keyword("done").to(Token::Done),
        text::keyword("newpair").to(Token::Newpair),
        text::keyword("pair").to(Token::Pair),
        text::keyword("fst").to(Token::Fst),
        text::keyword("snd").to(Token::Snd),
        text::keyword("call").to(Token::Call),
        text::keyword("int").to(Token::Int),
        text::keyword("bool").to(Token::Bool),
        text::keyword("char").to(Token::Char),
        text::keyword("string").to(Token::String),
        text::keyword("len").to(Token::Len),
        text::keyword("ord").to(Token::Ord),
        text::keyword("chr").to(Token::Chr),
        text::keyword("null").to(Token::Null),
        text::keyword("true").to(Token::True),
        text::keyword("false").to(Token::False),
    ])
    .map(LongestMatchCorrectionToken::Token);

    // if integer literals are separated by plus/minus with no whitespace
    // then this should be parsed as a separate disambiguation token
    let spanned_int_liter = int_liter.clone().span_tuple();
    let plus_or_minus = plus.or(minus).span_tuple();
    let no_whitespace_plus_minus_int_liter = group((
        spanned_int_liter.clone(),
        plus_or_minus.clone(),
        spanned_int_liter.clone(),
    ))
    .map(|(lhs, op, rhs)| vec![lhs, op, rhs])
    .foldl(
        group((plus_or_minus, spanned_int_liter)).repeated(),
        |mut lhs, (op, rhs)| {
            lhs.push(op);
            lhs.push(rhs);
            lhs
        },
    )
    .map(LongestMatchCorrectionToken::NoWhitespacePlusMinusIntLiter);

    let token = choice((
        // parse the exceptions to the "longest match" algorithm first
        no_whitespace_plus_minus_int_liter,
        // parse literals before other symbols, as they have precedence over any other occurrences
        // of symbols e.g. the literal +14343 should take precedence over the plus symbol '+'
        int_liter.map(LongestMatchCorrectionToken::Token),
        char_liter,
        str_liter,
        // parse symbols
        delim_symbols,
        other_symbols,
        // first parse keywords, only then parse identifiers as a fallback
        keywords,
        ident,
    ));

    // <comment>  ::=  ‘#’ (any-character-except-EOL)* (⟨EOL⟩ | ⟨EOF⟩)
    let eol = just('\n').ignored();
    let comment = group((
        just('#').ignored(),
        any().and_is(eol.not()).repeated(),
        choice((eol, end())),
    ))
    .labelled("<comment>");

    // tokens are padded by comments and whitespace
    token
        .map_with(|t, e| (t, e.span()))
        .padded_by(comment.padded().repeated())
        .padded()
        // If we encounter an error, skip and attempt to lex the next character as a token instead
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
        // We must consume the entire source at the end
        .then_ignore(end())
}

#[must_use]
#[inline]
pub fn lexer<'src, I>() -> impl alias::Parser<'src, I, Vec<(Token, I::Span)>>
where
    I: StrInput<'src, Token = char, Slice = &'src str>,
    I::Span: Clone,
{
    let lexer = unflattened_token_lexer::<'src, I>();

    lexer.map(|tokens| {
        // create new flattened vector
        let mut vec = Vec::<(Token, I::Span)>::with_capacity(tokens.len());

        // iterate through the tokens and either add them directly to the new
        // vector, or flatten them first and then append them
        for (t, s) in tokens {
            match t {
                LongestMatchCorrectionToken::Token(t) => {
                    vec.push((t, s));
                }
                LongestMatchCorrectionToken::NoWhitespacePlusMinusIntLiter(mut t) => {
                    vec.append(&mut t);
                }
            }
        }

        // we are done flattening, return the result
        vec
    })
}

/// An extension trait for [Parser] local to this file, for convenient dot-syntax utility methods
#[ext(name = LocalParserExt, supertraits = Sized + private::Sealed)]
impl<'src, I, O, E, T> T
where
    I: Input<'src>,
    E: ParserExtra<'src, I>,
    T: Parser<'src, I, O, E>,
{
    /// Convenience method to wrap items in span-tuple type.
    #[allow(clippy::type_complexity)]
    #[inline]
    fn span_tuple(self) -> MapWith<Self, O, fn(O, &mut MapExtra<'src, '_, I, E>) -> (O, I::Span)> {
        self.map_with(|t, e| (t, e.span()))
    }
}
