use crate::source::SourcedSpan;
use crate::StreamType;
use ariadne::{CharSet, Label, Report, Source};
use chumsky::error::Rich;
use std::io::Write;
use std::{fmt, io};

pub mod ast;
pub mod ast_frame;
pub mod lexer;
pub mod parser;
pub mod token;

/// Namespace for all the type/trait aliases used by this module.
pub(super) mod alias {
    use crate::parsing::ast;
    use crate::parsing::token::Token;
    use crate::source::SourcedSpan;
    use chumsky::extra;
    use chumsky::extra::ParserExtra;
    use chumsky::input::WithContext;
    use chumsky::prelude::{Input, Rich};

    /// Trait alias for generic [`chumsky::Parser`] implementations used by the various parsers here
    pub trait Parser<'src, I, O, E> = chumsky::Parser<'src, I, O, E> + Clone
    where
        I: Input<'src>,
        I::Token: PartialEq,
        E: ParserExtra<'src, I, Error = Rich<'src, I::Token, I::Span>>;

    pub trait StatelessParser<'src, I, O> = Parser<'src, I, O, extra::Full<Rich<'src, I::Token, I::Span>, (), ()>>
    where
        I: Input<'src>,
        I::Token: PartialEq;

    pub type ErrorExtra<'a, E> = extra::Full<E, (), ()>;
    pub type ParseOutput<'a, O, E> = (Option<O>, Vec<E>);

    pub type InputError<'a, I> = Rich<'a, <I as Input<'a>>::Token, <I as Input<'a>>::Span>;
    pub type InputExtra<'a, I> = ErrorExtra<'a, InputError<'a, I>>;
    pub type InputParseOutput<'a, I, O> = ParseOutput<'a, O, InputError<'a, I>>;

    pub type LexerInput<'a> = WithContext<SourcedSpan, &'a str>;
    pub type LexerExtra<'a> = InputExtra<'a, LexerInput<'a>>;
    pub type LexerOutput<'a> = InputParseOutput<'a, LexerInput<'a>, Vec<(Token, SourcedSpan)>>;

    pub type ProgramError<'a> = Rich<'a, Token, SourcedSpan>;
    pub type ProgramExtra<'a> = ErrorExtra<'a, ProgramError<'a>>;
    pub type ProgramOutput<'a> = ParseOutput<'a, ast::Program, ProgramError<'a>>;
}

/// Namespace for module-wide extension traits/methods
pub(super) mod ext {
    use crate::private;
    use chumsky::combinator::{Map, MapWith, OrNot, Then};
    use chumsky::{
        combinator::TryMapWith, error::LabelError as _, extra::ParserExtra, input::MapExtra,
        prelude::Input, DefaultExpected, Parser,
    };
    use extend::ext;
    use fn_pipe::FnPipe;

    /// Trait for holding all the [`char`] extension methods.
    #[ext(pub, name = CharExt, supertraits = private::Sealed)]
    impl char {
        /// One of `0`, `b`, `t`, `n`, `f`, `r`, `"`, `'` or `\`.
        fn escaped_wacc_char(&self) -> bool {
            matches!(*self, '0' | 'b' | 't' | 'n' | 'f' | 'r' | '"' | '\'' | '\\')
        }

        /// Looks up the character that a WACC-escaped character represents:
        ///
        /// | WACC-escaped character | ASCII Value | Description     |
        /// |------------------------|-------------|-----------------|
        /// | `0`                    | `0x00`      | null terminator |
        /// | `b`                    | `0x08`      | backspace       |
        /// | `t`                    | `0x09`      | tab             |
        /// | `n`                    | `0x0a`      | new line        |
        /// | `f`                    | `0x0c`      | form feed       |
        /// | `r`                    | `0x0d`      | carriage return |
        /// | `"`                    | `0x22`      | double quote    |
        /// | `'`                    | `0x27`      | single quote    |
        /// | `\`                    | `0x5c`      | backslash       |
        ///
        /// Source: WACC-language spec, Table 2.
        #[allow(clippy::as_conversions)]
        fn lookup_escaped_wacc_char(&self) -> Option<char> {
            match *self {
                // Table-2 of the specification
                '0' => Some(0x00 as char),
                'b' => Some(0x08 as char),
                't' => Some(0x09 as char),
                'n' => Some(0x0a as char),
                'f' => Some(0x0c as char),
                'r' => Some(0x0d as char),
                '"' => Some(0x22 as char),
                '\'' => Some(0x27 as char),
                '\\' => Some(0x5c as char),
                _ => None,
            }
        }

        /// Any graphic ASCII character except `\`, `'` and `"`;
        /// character `g` is graphic if `g >= ' '` where `' '` has ASCII-code of `0x20`.
        fn normal_wacc_char(&self) -> bool {
            match *self {
                // according to the WACC specification, graphic ASCII characters `g` are those
                // such that `g >= ' '`, therefore NON-graphic characters range from the
                // null-terminator `'\x00'` and upto-but-excluding space `'\x20'`
                '\x00'..'\x20'

                // the WACC specification also explicitly excludes these characters
                | '\\' | '\'' | '"' => false,

                // everything else in the ASCII-character range is fine, i.e. from `'\x20'` upto
                // `'\x7F'` inclusive
                '\x20'..='\x7F' => true,

                // all other characters are non-ASCII and should not be accepted
                _ => false,
            }
        }

        /// Just like [`char::to_owned`] but takes ownership of the character.
        fn to_owned(self) -> String {
            self.to_string()
        }
    }

    /// Trait for holding all the [`Parser`] extension methods.
    #[ext(pub, name = ParserExt, supertraits = Sized + private::Sealed)]
    impl<'a, I, O, E, T> T
    where
        I: Input<'a>,
        E: ParserExtra<'a, I>,
        T: Parser<'a, I, O, E>,
    {
        /// Currently [`chumsky`]'s [`chumsky::pratt::Infix`] parser does not support non-associative
        /// infix operations, so this is a replacement implementation until that feature is added
        #[allow(clippy::type_complexity)]
        fn infix_non_assoc<A, F, Op>(
            self,
            op_parser: A,
            f: F,
        ) -> MapWith<
            Then<T, OrNot<Then<A, T, Op, O, E>>, O, Option<(Op, O)>, E>,
            (O, Option<(Op, O)>),
            impl Fn((O, Option<(Op, O)>), &mut MapExtra<'a, '_, I, E>) -> O + Clone,
        >
        where
            Self: Clone,
            A: Parser<'a, I, Op, E>,
            F: Fn(O, Op, O, &mut MapExtra<'a, '_, I, E>) -> O + Clone,
        {
            self.clone().then(op_parser.then(self).or_not()).map_with(
                move |(lhs, maybe_rhs), extra| match maybe_rhs {
                    None => lhs,
                    Some((op, rhs)) => f(lhs, op, rhs, extra),
                },
            )
        }

        /// Like [`Parser::map`] but instead of mapping by calling a function, it runs a pipe
        /// of functions instead. See [`fn_pipe`].
        fn pipe<U, F>(self, f: F) -> Map<Self, O, impl Fn(O) -> U + Clone>
        where
            F: FnPipe(O) -> U + Clone,
        {
            self.map(move |x| f.run((x,)))
        }

        /// Like [`Parser::map_with`] but instead of mapping by calling a function, it runs a pipe
        /// of functions instead. See [`fn_pipe`].
        fn pipe_with<U, F>(
            self,
            f: F,
        ) -> MapWith<Self, O, impl Fn(O, &mut MapExtra<'a, '_, I, E>) -> U + Clone>
        where
            F: FnPipe(O, &mut MapExtra<'a, '_, I, E>) -> U + Clone,
        {
            self.map_with(move |x, extra| f.run((x, extra)))
        }

        /// Like [`Parser::select`] but applies to the output of a [`Parser`]; its internally
        /// implemented using the [`Parser::try_map_with`] combinator method.
        #[allow(clippy::type_complexity)]
        fn select_output<U, F>(
            self,
            f: F,
        ) -> TryMapWith<
            Self,
            O,
            impl Fn(O, &mut MapExtra<'a, '_, I, E>) -> Result<U, E::Error> + Clone,
        >
        where
            F: Fn(O, &mut MapExtra<'a, '_, I, E>) -> Option<U> + Clone,
        {
            self.try_map_with(move |x, extra| {
                f(x, extra).ok_or(E::Error::expected_found(
                    Some(DefaultExpected::SomethingElse),
                    None,
                    extra.span(),
                ))
            })
        }
    }
}

/// # Errors
/// TODO: add errors docs
#[allow(clippy::unwrap_used)]
#[inline]
pub fn build_syntactic_report<T, S: AsRef<str>, W: Write>(
    error: &Rich<T, SourcedSpan>,
    source: S,
    error_code: i32,
    stream_type: StreamType,
    output_stream: W,
) -> io::Result<()>
where
    T: fmt::Display,
{
    let config = ariadne::Config::default().with_char_set(CharSet::Ascii);
    let report = Report::build(ariadne::ReportKind::Error, error.span().clone())
        .with_config(config)
        .with_message("Syntax error".to_owned())
        .with_code(error_code)
        .with_label(Label::new(error.span().clone()).with_message(error.reason().to_owned()))
        .with_labels(error.contexts().map(|(label, span)| {
            Label::new(span.clone()).with_message(format!("while parsing this {label}"))
        }))
        .finish();
    let cache = (error.span().source_id().clone(), Source::from(source));
    match stream_type {
        StreamType::Stdout => report.write_for_stdout(cache, output_stream),
        StreamType::Stderr => report.write(cache, output_stream),
    }
}
