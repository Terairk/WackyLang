//! TODO: crate documentation
//!
//! this is here as a placeholder documentation

// enable Rust-unstable features for convenience
#![feature(trait_alias)]
#![feature(stmt_expr_attributes)]
#![feature(unboxed_closures)]

/// Namespace for all the type/trait aliases used by this crate.
pub(crate) mod alias {
    use chumsky::extra;
    use chumsky::prelude::{Input, Rich};

    /// Trait alias for generic [`chumsky::Parser`] implementations used by the various parsers here
    pub trait Parser<'src, I, T> = chumsky::Parser<'src, I, T, extra::Full<Rich<'src, I::Token, I::Span>, (), ()>>
        + Clone
    where
        I: Input<'src>,
        I::Token: PartialEq;
}

pub mod ast;

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
                // null-terminator `'\0'` and upto-but-excluding space `' '`
                '\0'..' '

                // the WACC specification also explicitly excludes these characters
                | '\\' | '\'' | '"'  => false,

                // everything else is fine
                _ => true,
            }
        }

        /// Just like [`char::to_string`] but takes ownership of the character.
        fn to_string_owned(self) -> String {
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

pub mod node;

pub mod nonempty;
pub mod parser;

pub(crate) mod private {
    // sealed traits support
    pub trait Sealed {}
    impl<T> Sealed for T {}
}

pub mod source;

pub mod token;
