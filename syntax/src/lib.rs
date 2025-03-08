//! TODO: crate documentation
//!
//! this is here as a placeholder documentation
//!
//!

// enable Rust-unstable features for convenience
#![feature(trait_alias)]
#![feature(stmt_expr_attributes)]
#![feature(unboxed_closures)]

use crate::error::{semantic_error_to_reason, SemanticError};
use crate::source::SourcedSpan;
use ariadne::{CharSet, Label, Report, Source};
use chumsky::error::Rich;
use std::fmt;
use std::ops::{Deref, DerefMut};

/// Namespace for all the type/trait aliases used by this crate.
pub(crate) mod alias {
    use chumsky::extra;
    use chumsky::extra::ParserExtra;
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
}

pub mod ast;
pub mod error;
pub mod fold_program;
pub mod rename;
pub mod types;

/// Namespace for crate-wide extension traits/methods
pub(crate) mod ext {
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

pub(crate) mod container {
    use chumsky::container::Container;

    #[derive(Clone, PartialEq, Eq, Debug, Hash)]
    pub enum MultiItem<T> {
        Multi(Vec<T>),
        Item(T),
    }

    #[derive(Clone, PartialEq, Eq, Debug, Hash)]
    #[repr(transparent)]
    pub struct ItemVec<T>(Vec<T>);

    impl<T> ItemVec<T> {
        pub fn into_inner(self) -> Vec<T> {
            self.0
        }
    }

    impl<T> Default for ItemVec<T> {
        fn default() -> Self {
            Self(Vec::new())
        }
    }

    impl<T> Container<MultiItem<T>> for ItemVec<T> {
        fn push(&mut self, item: MultiItem<T>) {
            match item {
                MultiItem::Multi(mut items) => self.0.append(&mut items),
                MultiItem::Item(item) => self.0.push(item),
            }
        }
    }
}

pub mod node;
pub mod parser;

pub(crate) mod private {
    // sealed traits support
    pub trait Sealed {}
    impl<T: ?Sized> Sealed for T {}
}

pub mod source;

pub mod token;
pub mod typecheck;

#[allow(clippy::unwrap_used)]
pub fn build_syntactic_report<T>(error: &Rich<T, SourcedSpan>, source: String)
where
    T: fmt::Display,
{
    let config = ariadne::Config::default().with_char_set(CharSet::Ascii);
    Report::build(ariadne::ReportKind::Error, error.span().clone())
        .with_config(config)
        .with_message(format!("Syntax error"))
        .with_code(69)
        .with_label(Label::new(error.span().clone()).with_message(error.reason().to_owned()))
        .with_labels(error.contexts().map(|(label, span)| {
            Label::new(span.clone()).with_message(format!("while parsing this {label}"))
        }))
        .finish()
        .print((error.span().source_id().clone(), Source::from(source)))
        .unwrap();
}

#[allow(clippy::unwrap_used)]
#[inline]
pub fn semantic_report_helper(
    file_path: &String,
    message: &str,
    error: &SemanticError,
    span: &SourcedSpan,
    source: String,
) {
    let config = ariadne::Config::default().with_char_set(CharSet::Ascii);
    Report::build(
        ariadne::ReportKind::Error,
        (file_path, span.clone().as_range()),
    )
    .with_config(config)
    .with_message(message)
    .with_code(420)
    .with_label(
        Label::new((file_path, span.clone().as_range()))
            .with_message(semantic_error_to_reason(error)),
    )
    .finish()
    .print((file_path, Source::from(source)))
    .unwrap();
}

pub fn build_semantic_error_report(file_path: &String, error: &SemanticError, source: String) {
    match error {
        SemanticError::TypeMismatch(span, _, _) => {
            semantic_report_helper(file_path, "Type Error", error, span, source);
        }
        SemanticError::DuplicateIdent(ident) => {
            semantic_report_helper(
                file_path,
                "Duplicate Identifier",
                error,
                &ident.span(),
                source,
            );
        }
        SemanticError::InvalidNumberOfIndexes(span, _, _) => {
            semantic_report_helper(file_path, "Wrong number of indexes", error, &span, source);
        }
        // Handle other error variants similarly
        _ => {
            // Generic error report for other cases
            let span = match error {
                SemanticError::ArityMismatch(node, _, _) => node.span().clone(),
                SemanticError::AssignmentWithBothSidesUnknown(span) => span.clone(),
                SemanticError::TypeMismatch(span, _, _) => span.clone(),
                SemanticError::InvalidFreeType(span, _) => span.clone(),
                SemanticError::MismatchedArgCount(span, _, _) => span.clone(),
                SemanticError::InvalidIndexType(span, _) => span.clone(),
                SemanticError::UndefinedIdent(node) => node.span().clone(),
                SemanticError::ReturnInMain(span) => span.clone(),
                _ => panic!("Unhandled error variant"),
            };
            semantic_report_helper(file_path, "Semantic Error", error, &span, source);
        }
    }
}

#[repr(transparent)]
#[derive(Debug)]
#[allow(dead_code)]
struct DisplayVec<T>(Vec<T>);

impl<T> DisplayVec<T> {
    const DISPLAY_WIDTH: usize = 4;
    const OFFSET_WIDTH: usize = 2;
}

impl<T> Deref for DisplayVec<T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for DisplayVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[allow(clippy::arithmetic_side_effects)]
impl<T: fmt::Display> fmt::Display for DisplayVec<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;

        let mut width = 0;
        for i in self.iter() {
            // control items-per-line width
            if width == 0 {
                writeln!(f)?;
                for _ in 0..Self::OFFSET_WIDTH {
                    write!(f, " ")?;
                }
                width = Self::DISPLAY_WIDTH;
            }
            width -= 1;
            write!(f, "{i}, ")?;
        }

        write!(f, "\n}}")
    }
}
