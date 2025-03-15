use crate::alias::InternStr;
use crate::multi_item::{MultiItem, MultiItemVec};
use crate::parsing::alias::{LexerExtra, LexerOutput};
use crate::parsing::ext::{CharExt, ParserExt};
use crate::parsing::token::{Delim, Token};
use crate::parsing::{alias, build_syntactic_report};
use crate::source::{SourcedSpan, StrSourceId};
use crate::{private, StreamType};
use chumsky::combinator::{MapWith, ToSlice};
use chumsky::error::Rich;
use chumsky::extra::ParserExtra;
use chumsky::input::{Input, MapExtra, StrInput};
use chumsky::prelude::{any, choice, end, group, just, regex, skip_then_retry_until, via_parser};
use chumsky::IterParser;
use chumsky::{text, Parser};
use extend::ext;
use std::io;
use std::io::Write;
use thiserror::Error;

// constants associated with int-literal parsing
const BINARY_RADIX: u32 = 2;
const OCTAL_RADIX: u32 = 8;
const DECIMAL_RADIX: u32 = 10;
const HEXADECIMAL_RADIX: u32 = 16;
const INT_LITERAL_PREFIXES: [&str; 6] = ["0b", "0B", "0o", "0O", "0x", "0X"];

#[allow(clippy::items_after_statements)]
fn int_liter<'src, I, E>() -> impl alias::Parser<'src, I, Token, E>
where
    I: StrInput<'src, Token = char, Slice = &'src str>,
    E: ParserExtra<'src, I, Error = Rich<'src, I::Token, I::Span>>,
{
    // binary literals are prefixed with either `0b` or `0B`
    let binary = regex(r"[+-]?(?:0b|0B)[01]+")
        .parse_i32(BINARY_RADIX)
        .labelled("<binary-int-liter>")
        .as_context();

    // octal literals are prefixed with either `0o` or `0O`
    let octal = regex(r"[+-]?(?:0o|0O)[0-7]+")
        .parse_i32(OCTAL_RADIX)
        .labelled("<octal-int-liter>")
        .as_context();

    // copy the Regex pattern found in the WACC spec verbatim for decimal literals
    let decimal = regex(r"[+-]?[0-9]+")
        .parse_i32(DECIMAL_RADIX)
        .labelled("<decimal-int-liter>")
        .as_context();

    // hexadecimal literals are prefixed with either `0x` or `0X`
    let hexadecimal = regex(r"[+-]?(?:0x|0X)[0-9a-fA-F]+")
        .parse_i32(HEXADECIMAL_RADIX)
        .labelled("<hexadecimal-int-liter>")
        .as_context();

    // An integer literal is one of these: note decimal does last due to no prefix
    choice((hexadecimal, octal, binary, decimal))
        // once we have parsed an integer-literal string, we can unwrap any errors
        // in order to resume parser-control-flow to the usual
        .try_map(|result, _| result)
        .map(Token::IntLiter)
        .labelled("<int-liter>")
        .as_context()
}

#[allow(
    clippy::missing_panics_doc,
    clippy::too_many_lines,
    clippy::single_call_fn
)]
#[inline]
pub fn lexer<'src, I, E>() -> impl alias::Parser<'src, I, Vec<(Token, I::Span)>, E>
where
    I: StrInput<'src, Token = char, Slice = &'src str>,
    E: ParserExtra<'src, I, Error = Rich<'src, I::Token, I::Span>>,
{
    // TODO: add labels where appropriate
    // TODO: think more about error handling: in particular the char/string delimiters

    // TODO: eventually replace with custom error type with variants and so on,
    // TODO: so it is easier to create type-safe custom errors that can be reported later on

    // <comment>  ::=  ‘#’ (any-character-except-EOL)* (⟨EOL⟩ | ⟨EOF⟩)
    let eol = just('\n').ignored();
    let comment = group((
        just('#').ignored(),
        any().and_is(eol.not()).repeated(),
        choice((eol, end())),
    ))
    .labelled("<comment>")
    .as_context();
    let comments = comment.padded().repeated();

    // WACC identifiers are C-style, so we can use the default `text::ident` parser
    let ident = text::ident()
        .pipe((InternStr::from, Token::Ident))
        .span_tuple()
        .labelled("<ident>")
        .as_context();

    // All integer literals
    let int_liter = int_liter().span_tuple();

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
    .labelled("<character>")
    .as_context();

    // character literal parser
    let char_delim = just('\'');
    let char_liter = char_delim
        .ignore_then(
            choice((
                well_formed_character.map(|c| (c, false)),
                any().and_is(char_delim.not()).map(|c| (c, true)),
            ))
            .repeated()
            .collect::<Vec<(char, bool)>>()
            .then_ignore(char_delim),
        )
        .try_map_with(|content, e| {
            let ct_len = content.len();
            if ct_len > 2 {
                Err(Rich::custom(
                    e.span(),
                    "Char literals should only have one character",
                ))
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
                            Err(Rich::custom(
                                e.span(),
                                "Char literals should only have one character",
                            ))
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
                .map_with(|_, _| Token::CharLiter('6')),
            ),
        ))
        .span_tuple()
        .labelled("<char-liter>")
        .as_context();

    // string literal parser
    let str_delim = just('"');
    let str_liter = str_delim
        .ignore_then(
            choice((
                well_formed_character.map(|c| (c, false)),
                any().and_is(str_delim.not()).map(|c| (c, true)),
            ))
            .repeated()
            .collect::<Vec<(char, bool)>>()
            .then_ignore(str_delim)
            .try_map_with(|chars, e| {
                let mut string_builder = String::with_capacity(chars.len());

                for &(c, ill_formed) in &chars {
                    if ill_formed {
                        return Err(if c == '\\' {
                            Rich::custom(e.span(), "Invalid control character")
                        } else if c == '\n' {
                            Rich::custom(e.span(), "No new line")
                        } else {
                            Rich::custom(e.span(), "Non-ASCII character")
                        });
                    }

                    string_builder.push(c);
                }

                Ok(InternStr::from(string_builder.as_str()))
            }),
        )
        .recover_with(via_parser(
            str_delim.ignore_then(
                choice((just('\\').ignore_then(any()), any().and_is(str_delim.not())))
                    .repeated()
                    .collect::<String>()
                    .then_ignore(str_delim)
                    .map_with(|lit, _| InternStr::from(lit.as_str())),
            ),
        ))
        .map(Token::StrLiter)
        .span_tuple()
        .labelled("<str-liter>")
        .as_context();

    let delim_symbols = choice((
        just('(').to(Token::Open(Delim::Paren)),
        just(')').to(Token::Close(Delim::Paren)),
        just('[').to(Token::Open(Delim::Bracket)),
        just(']').to(Token::Close(Delim::Bracket)),
    ))
    .span_tuple();

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
        just("&&").to(Token::AmpersandAmpersand),
        just("||").to(Token::PipePipe),
        just(':').to(Token::Colon),
        just(';').to(Token::Semicolon),
        just(',').to(Token::Comma),
    ))
    .span_tuple();

    let keywords = choice([
        text::keyword("begin").to(Token::Begin),
        text::keyword("end").to(Token::End),
        text::keyword("tailrec").to(Token::Tailrec),
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
        text::keyword("elif").to(Token::Elif),
        text::keyword("else").to(Token::Else),
        text::keyword("fi").to(Token::Fi),
        text::keyword("while").to(Token::While),
        text::keyword("do").to(Token::Do),
        text::keyword("done").to(Token::Done),
        text::keyword("loop").to(Token::Loop),
        text::keyword("break").to(Token::Break),
        text::keyword("nextloop").to(Token::Nextloop),
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
    .span_tuple();

    // create single-item token parser
    let single_item_token = choice((
        // parse literals before other symbols, as they have precedence over any other occurrences
        // of symbols e.g. the literal +14343 should take precedence over the plus symbol '+'
        int_liter.clone(),
        char_liter,
        str_liter,
        // parse symbols
        delim_symbols,
        other_symbols,
        // first parse keywords, only then parse identifiers as a fallback
        keywords,
        ident.clone(),
    ))
    .map(MultiItem::Item);

    // if integer literals (or identifiers) are separated by plus/minus with no whitespace
    // then this should be parsed as a separate disambiguation token
    let int_liter_or_ident = int_liter.clone().or(ident.clone());
    let plus_or_minus = plus.or(minus).span_tuple();
    let no_whitespace_plus_minus_int_liter = group((
        int_liter_or_ident.clone(),
        comments.ignored(), // any amount of whitespace/comments should be ignored at this position
        plus_or_minus.clone(),
        int_liter_or_ident.clone(),
    ))
    .map(|(lhs, (), op, rhs)| vec![lhs, op, rhs])
    .foldl(
        group((plus_or_minus, int_liter_or_ident)).repeated(),
        |mut lhs, (op, rhs)| {
            lhs.push(op);
            lhs.push(rhs);
            lhs
        },
    )
    .map(MultiItem::multi);

    let token = choice((
        // parse the exceptions to the "longest match" algorithm first
        no_whitespace_plus_minus_int_liter,
        // fallback to single item token
        single_item_token,
    ));

    // tokens are padded by comments and whitespace
    token
        .padded_by(comments)
        .padded()
        // If we encounter an error, skip and attempt to lex the next character as a token instead
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect::<MultiItemVec<_>>()
        .map(MultiItemVec::into_inner)
        // We must consume the entire source at the end
        .then_ignore(end())
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

    /// Parses a string slice to [`i32`] (or an error variant upon failure) given some [`radix`]
    /// with [`i32::from_str_radix`] function. The error is not emmitted to the parsing-system
    /// in order to not trigger unnecessary control-flow alterations
    #[allow(clippy::type_complexity)]
    fn parse_i32(
        self,
        radix: u32,
    ) -> MapWith<
        ToSlice<T, O>,
        &'src str,
        impl for<'b> Fn(&'src str, &mut MapExtra<'src, 'b, I, E>) -> Result<i32, E::Error> + Clone,
    >
    where
        I: StrInput<'src, Token = char, Slice = &'src str>,
        E: ParserExtra<'src, I, Error = Rich<'src, I::Token, I::Span>>,
    {
        self.to_slice()
            .map_with(move |s: &'src str, e: &mut MapExtra<_, _>| {
                // check for plus or minus
                let mut s_trimmed = s.trim_start_matches(['+', '-']);
                let minus_sign = match (s == s_trimmed, s.chars().next()) {
                    (true, _) | (_, Some('+')) => None,
                    (_, Some('-')) => Some('-'),
                    _ => unreachable!("This should never happen"),
                };

                // Trim any prefixes (should ideally only be one) of them
                for prefix in INT_LITERAL_PREFIXES {
                    let s_trimmed_new = s_trimmed.trim_start_matches(prefix);

                    // if prefix trimmed successfully, break
                    if s_trimmed_new != s_trimmed {
                        s_trimmed = s_trimmed_new;
                        break;
                    }
                }

                // Create final string-to-be-parsed together with any minus sign. Parse into `i32`
                // literal, or map any errors as appropriate.
                let s_to_parse =
                    minus_sign.map_or_else(|| s_trimmed.to_owned(), |c| format!("{c}{s_trimmed}"));
                i32::from_str_radix(&s_to_parse, radix).map_err(|_| {
                    Rich::custom(
                        e.span(),
                        format!("The integer literal '{s}' does not fit within 32 bytes"),
                    )
                })
            })
    }
}

#[derive(Debug, Error)]
pub enum LexingPhaseError {
    #[error("Encountered lexing error while parsing, error report written to provided output")]
    LexingErrorWritten,
    #[error(transparent)]
    IoError(#[from] io::Error),
}

/// # Errors
/// TODO: add errors docs
///
#[allow(
    clippy::missing_panics_doc,
    clippy::expect_used,
    clippy::needless_pass_by_value
)]
#[inline]
pub fn lexing_phase<S: AsRef<str>, W: Write + Clone>(
    source: S,
    source_id: StrSourceId,
    lexing_error_code: i32,
    stream_type: StreamType,
    output_stream: W,
) -> Result<Vec<(Token, SourcedSpan)>, LexingPhaseError> {
    let source = source.as_ref();

    // so the pattern is, make sure everything is WAAYYY to generic :) and supply
    // concrete implementations later via turbofish syntax :)))
    let (tokens, lexing_errs): LexerOutput = Parser::parse(
        &lexer::<_, LexerExtra>(),
        source.with_context((source_id, ())),
    )
    .into_output_errors();

    // Done to appease the borrow checker while displaying errors
    if !lexing_errs.is_empty() {
        for e in &lexing_errs {
            build_syntactic_report(
                e,
                source,
                lexing_error_code,
                stream_type,
                output_stream.clone(),
            )?;
        }
        return Err(LexingPhaseError::LexingErrorWritten);
    }

    Ok(tokens.expect("If lexing errors are not empty, tokens should be Valid"))
}
