#![allow(clippy::arbitrary_source_item_ordering)]

use crate::ext::ParserExt as _;
use crate::{
    alias, ast,
    nonempty::NonemptyArray,
    private,
    source::{SourcedNode, SourcedSpan},
    token::Delim,
    token::Token,
    types,
};
use chumsky::pratt::right;
use chumsky::{
    combinator::{DelimitedBy, MapWith},
    extra::ParserExtra,
    input::BorrowInput,
    input::{MapExtra, ValueInput},
    pratt::{infix, left, prefix},
    prelude::*,
    primitive::Just,
    recovery::{RecoverWith, ViaParser},
    select_ref, Parser,
};
use extend::ext;

/// A file-local type alias for better readability of type definitions
type SN<T> = SourcedNode<T>;

#[must_use]
#[inline]
pub fn ident_parser<'src, I>() -> impl alias::Parser<'src, I, ast::Ident>
where
    I: BorrowInput<'src, Token = Token>,
{
    // identifiers can be extracted directly from `Ident` tokens, copying
    // an internal atomic reference to an interned identifier string
    #[allow(clippy::pattern_type_mismatch)]
    (select_ref! { Token::Ident(x) => x.clone() }).labelled("<ident>")
}

#[allow(clippy::pattern_type_mismatch)]
#[must_use]
#[inline]
pub fn liter_parser<'src, I>() -> impl alias::Parser<'src, I, ast::Liter>
where
    I: BorrowInput<'src, Token = Token>,
{
    // some literals can be extracted from their corresponding tokens
    let int_liter =
        select_ref! { Token::IntLiter(x) => ast::Liter::IntLiter(*x) }.labelled("<int-liter>");
    let char_liter =
        select_ref! { Token::CharLiter(x) => ast::Liter::CharLiter(*x) }.labelled("<char-liter>");
    let str_liter = select_ref! { Token::StrLiter(x) => ast::Liter::StrLiter(x.clone()) }
        .labelled("<str-liter>");

    // some literals can be created from keywords
    let bool_liter = choice((
        just(Token::True).to(ast::Liter::BoolLiter(true)),
        just(Token::False).to(ast::Liter::BoolLiter(false)),
    ))
    .labelled("<bool-liter>");
    let pair_liter = just(Token::Null)
        .to(ast::Liter::PairLiter)
        .labelled("<pair-liter>");

    // final literal parser is one of these five
    choice((int_liter, char_liter, str_liter, bool_liter, pair_liter))
}

#[inline]
pub fn array_elem_parser<'src, I, Ident, Expr>(
    ident: Ident,
    expr: Expr,
) -> impl alias::Parser<'src, I, ast::ArrayElem>
where
    I: BorrowInput<'src, Token = Token, Span = SourcedSpan>,
    Ident: alias::Parser<'src, I, ast::Ident>,
    Expr: alias::Parser<'src, I, ast::Expr>,
{
    let array_elem_indices = expr
        .delim_by(Delim::Bracket)
        .sn()
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .pipe((NonemptyArray::try_from_boxed_slice, Result::unwrap));
    let array_elem = group((ident.sn(), array_elem_indices))
        .map_group(ast::ArrayElem::new)
        .labelled("<array-elem>");

    array_elem
}

#[must_use]
#[inline]
pub fn expr_parser<'src, I>() -> impl alias::Parser<'src, I, ast::Expr>
where
    I: BorrowInput<'src, Token = Token, Span = SourcedSpan> + ValueInput<'src>,
{
    recursive(|expr| {
        let ident = ident_parser();
        let liter = liter_parser();
        let array_elem = array_elem_parser(ident.clone(), expr.clone());

        // parse parenthesized expressions
        let paren_expr = expr.delim_by(Delim::Paren).sn();

        // 'Atoms' are expressions that contain no ambiguity
        let atom = choice((
            liter.map(ast::Expr::Liter),
            // array elements begin with identifiers, so
            // give them precedence over identifiers
            array_elem.map(ast::Expr::ArrayElem),
            ident.map(ast::Expr::Ident),
            paren_expr.map(ast::Expr::Paren),
        ));

        // Perform simplistic error recovery on Atom expressions
        let atom = atom
            // Attempt to recover anything that looks like a parenthesised expression but contains errors
            .recover_with_delim(Delim::Paren, ast::Expr::Error)
            // Attempt to recover anything that looks like an array-element but contains errors
            .recover_with_delim(Delim::Bracket, ast::Expr::Error);

        // unary and binary operator parsers
        let unary_oper = choice((
            just(Token::Bang).to(ast::UnaryOper::Not),
            just(Token::Minus).to(ast::UnaryOper::Minus),
            just(Token::Len).to(ast::UnaryOper::Len),
            just(Token::Ord).to(ast::UnaryOper::Ord),
            just(Token::Chr).to(ast::UnaryOper::Chr),
        ))
        .sn()
        .labelled("<unary-oper>");
        let product_oper = choice((
            just(Token::Star).to(ast::BinaryOper::Mul),
            just(Token::ForwardSlash).to(ast::BinaryOper::Div),
            just(Token::Percent).to(ast::BinaryOper::Mod),
        ))
        .sn()
        .labelled("<binary-oper>");
        let sum_oper = choice((
            just(Token::Plus).to(ast::BinaryOper::Add),
            just(Token::Minus).to(ast::BinaryOper::Sub),
        ))
        .sn()
        .labelled("<binary-oper>");
        let arith_cmp_oper = choice((
            just(Token::Lte).to(ast::BinaryOper::Lte),
            just(Token::Lt).to(ast::BinaryOper::Lt),
            just(Token::Gte).to(ast::BinaryOper::Gte),
            just(Token::Gt).to(ast::BinaryOper::Gt),
        ))
        .sn()
        .labelled("<binary-oper>");
        let eq_cmp_oper = choice((
            just(Token::EqualsEquals).to(ast::BinaryOper::Eq),
            just(Token::BangEquals).to(ast::BinaryOper::Neq),
        ))
        .sn()
        .labelled("<binary-oper>");
        let land_oper = just(Token::And)
            .to(ast::BinaryOper::And)
            .sn()
            .labelled("<binary-oper>");
        let lor_oper = just(Token::Or)
            .to(ast::BinaryOper::Or)
            .sn()
            .labelled("<binary-oper>");

        // procedure to turn patterns into binary expressions
        let binary_create = |lhs, op, rhs, extra: &mut MapExtra<'src, '_, I, _>| {
            SN::new(ast::Expr::Binary(lhs, op, rhs), extra.span())
        };

        // a PRATT parser for prefix and left-infix operator expressions
        #[allow(clippy::shadow_unrelated)]
        let atom = atom.sn().pratt((
            // We want unary operations to happen before any binary ones, so their precedence
            // is set to be the highest. But amongst themselves the precedence is the same.
            prefix(3, unary_oper, |op, rhs, extra| {
                SN::new(ast::Expr::Unary(op, rhs), extra.span())
            }),
            // Product ops (multiply, divide, and mod) have equal precedence, and the highest
            // binary operator precedence overall
            infix(left(2), product_oper, binary_create),
            // Sum ops (add and subtract) have equal precedence, just below the precedence
            // of product ops
            infix(left(1), sum_oper, binary_create),
        ));

        // Arithmetic comparisons (<, <=, >, >=) have equal precedence, just below the
        // precedence of sum ops
        let atom = atom.infix_non_assoc(arith_cmp_oper, binary_create);
        // Equality comparisons (== and !=) have equal precedence, just below the
        // precedence of arithmetic comparisons
        let atom = atom.infix_non_assoc(eq_cmp_oper, binary_create);

        // a PRATT parser for right-infix operator expressions
        #[allow(clippy::shadow_unrelated)]
        let expr = atom.pratt((
            // logical AND (&&) has more precedence than logical OR (||), where AND has
            // precedence just below that of equality comparisons
            infix(right(2), land_oper, binary_create),
            infix(right(1), lor_oper, binary_create),
        ));

        // as of now, no other type of expression exists
        expr.map(SN::into_inner) // TODO: this removes span from output. If this is not desirable, undo this line
    })
}

#[must_use]
#[inline]
pub fn type_parser<'src, I>() -> impl alias::Parser<'src, I, types::Type>
where
    I: BorrowInput<'src, Token = Token, Span = SourcedSpan> + ValueInput<'src>,
{
    recursive(|r#type| {
        // base types have no recursion
        let base_type = choice((
            just(Token::Int).to(types::BaseType::Int),
            just(Token::Bool).to(types::BaseType::Bool),
            just(Token::Char).to(types::BaseType::Char),
            just(Token::String).to(types::BaseType::String),
        ))
        .sn()
        .labelled("<base-type>");

        // array types are left-recursive, and `chumsky` is a PEG parser meaning it does not
        // handle left-recursion by default (i.e. it will recurse infinitely) so we need to use
        // memoization in order to prevent this and allow correct left-recursive grammar parsing
        let array_type = r#type
            .foldl_with(
                // here we have to try and match zero-or-more occurrences of `[]`; it's the only
                // way to get the parser to consume the array-related brackets properly;
                // setting a minimum with `at_least(1)` makes it not work for some reason..?
                group((
                    just(Token::Open(Delim::Bracket)),
                    just(Token::Close(Delim::Bracket)),
                ))
                .ignored()
                .repeated(),
                |ty, (), extra| {
                    types::Type::ArrayType(SN::new(types::ArrayType::new(ty), extra.span()))
                },
            )
            // as explained above, in order to get the parser to consume tokens correctly, we have
            // to allow the possibility that at this point, this isn't even an ArrayType, so we are
            // filtering only for array types at this point
            .select_output(|ty, _| match ty {
                types::Type::ArrayType(ty) => Some(ty),
                _ => None,
            })
            .memoized()
            .labelled("<array-type>");

        // pair-element type parser;
        // an array type and all other types look the same until the very last, so we should
        // give precedence to array types to make sure they are not incorrectly missed
        let pair_elem_type = choice((
            array_type.clone().map(types::PairElemType::ArrayType),
            base_type.clone().map(types::PairElemType::BaseType),
            just(Token::Pair).to_span().map(types::PairElemType::Pair),
        ))
        .labelled("<pair-elem-type>");
        let pair_type = just(Token::Pair)
            .ignore_then(
                group((
                    pair_elem_type.clone().then_ignore(just(Token::Comma)),
                    pair_elem_type,
                ))
                .map_group(types::Type::PairType)
                .delim_by(Delim::Paren)
                // Attempt to recover anything that looks like a (parenthesised) pair type but contains errors
                .recover_with_delim(Delim::Paren, types::Type::Error),
            )
            .labelled("<pair-type>");

        // a type is either a base type, array type, or pair type;
        // an array type and all other types look the same until the very last, so we should
        // give precedence to array types to make sure they are not incorrectly missed
        #[allow(clippy::shadow_unrelated)]
        let r#type = choice((
            array_type.map(types::Type::ArrayType),
            base_type.map(types::Type::BaseType),
            pair_type,
        ))
        .labelled("<type>");

        r#type
    })
}

#[inline]
pub fn stat_parser<'src, I, P>(stat_chain: P) -> impl alias::Parser<'src, I, ast::Stat>
where
    I: BorrowInput<'src, Token = Token, Span = SourcedSpan> + ValueInput<'src>,
    P: alias::Parser<'src, I, ast::StatBlock>,
{
    let ident = ident_parser();
    let expr = expr_parser();
    let array_elem = array_elem_parser(ident.clone(), expr.clone());

    // wrap parsers with SN<_>
    let stat_chain = stat_chain.sn();
    let ident = ident.sn();
    let expr = expr.sn();
    let r#type = type_parser().sn();

    // a sequence of expressions separated by commas
    let expr_sequence = expr
        .clone()
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .map(Vec::into_boxed_slice);

    // array literal parser
    let array_liter = expr_sequence
        .clone()
        .delim_by(Delim::Bracket)
        .map(ast::RValue::ArrayLiter);

    // newpair parser
    let newpair = just(Token::Newpair).ignore_then(
        group((expr.clone().then_ignore(just(Token::Comma)), expr.clone()))
            .delim_by(Delim::Paren)
            .map_group(ast::RValue::NewPair),
    );

    // declare the LValue parser
    let mut lvalue = Recursive::declare();

    // pair-elem parser
    let pair_elem = choice((
        just(Token::Fst).ignore_then(lvalue.clone().sn().map(ast::PairElem::Fst)),
        just(Token::Snd).ignore_then(lvalue.clone().sn().map(ast::PairElem::Snd)),
    ));

    // function call parser
    let function_call = just(Token::Call).ignore_then(
        group((ident.clone(), expr_sequence.delim_by(Delim::Paren))).map_group(ast::RValue::call),
    );

    // define the lvalue parser: array-elem contains identifier within it, so it should be
    // given parsing precedence to disambiguate properly
    lvalue.define(choice((
        array_elem.map(ast::LValue::ArrayElem),
        pair_elem.clone().sn().map(ast::LValue::PairElem),
        ident.clone().map(ast::LValue::Ident),
    )));

    // rvalue parser
    let rvalue = choice((
        array_liter,
        newpair,
        pair_elem.map(ast::RValue::PairElem),
        function_call,
        // TODO: the expression parser turns errors into error nodes, which makes it
        //       seem like the parser succeeded even if it failed. This messes with
        //       backtracking control flow, so until we figure out a way to "propagate"
        //       the erroneous state of the parser, expressions will have to be parsed last
        expr.clone().map(ast::RValue::Expr),
    ));

    // variable definition parser
    let variable_definition = group((
        r#type,
        ident.then_ignore(just(Token::Equals)),
        rvalue.clone(),
    ))
    .map_group(ast::Stat::var_definition);

    // assignment parser
    let assignment = group((lvalue.clone().then_ignore(just(Token::Equals)), rvalue))
        .map_group(ast::Stat::assignment);

    // if-then-else parser
    let if_then_else = just(Token::If).ignore_then(
        group((
            expr.clone().then_ignore(just(Token::Then)),
            stat_chain.clone().then_ignore(just(Token::Else)),
            stat_chain.clone().then_ignore(just(Token::Fi)),
        ))
        .map_group(ast::Stat::if_then_else),
    );

    // while-loop parser
    let while_do = just(Token::While).ignore_then(
        group((
            expr.clone().then_ignore(just(Token::Do)),
            stat_chain.clone().then_ignore(just(Token::Done)),
        ))
        .map_group(ast::Stat::while_do),
    );

    // begin-end scope
    let scoped = just(Token::Begin)
        .ignore_then(stat_chain.clone().then_ignore(just(Token::End)))
        .map(ast::Stat::Scoped);

    // statement parser
    // TODO: figure out labels and error recovery
    let stat = choice((
        just(Token::Skip).to(ast::Stat::Skip),
        variable_definition,
        assignment,
        just(Token::Read).ignore_then(lvalue).map(ast::Stat::Read),
        just(Token::Free)
            .ignore_then(expr.clone())
            .map(ast::Stat::Free),
        just(Token::Return)
            .ignore_then(expr.clone())
            .map(ast::Stat::Return),
        just(Token::Exit)
            .ignore_then(expr.clone())
            .map(ast::Stat::Exit),
        just(Token::Print)
            .ignore_then(expr.clone())
            .map(ast::Stat::Print),
        just(Token::Println)
            .ignore_then(expr.clone())
            .map(ast::Stat::Println),
        if_then_else,
        while_do,
        scoped,
    ));

    #[allow(clippy::let_and_return)] // because this is likely to be changed/extended in the future
    stat
}

#[inline]
pub fn program_parser<'src, I>() -> impl alias::Parser<'src, I, ast::Program>
where
    I: BorrowInput<'src, Token = Token, Span = SourcedSpan> + ValueInput<'src>,
{
    let r#type = type_parser::<I>().sn();
    let ident = ident_parser::<I>().sn();

    // statement chain parser
    let stat_chain = recursive(|stat_chain| {
        let stat = stat_parser(stat_chain).sn();

        // parse statement chains
        #[allow(clippy::shadow_unrelated)]
        let stat_chain = stat
            .separated_by(just(Token::Semicolon))
            .at_least(1)
            .collect::<Vec<_>>()
            .pipe((ast::StatBlock::try_new, Result::unwrap));

        #[allow(clippy::let_and_return)]
        // because this is likely to be changed/extended in the future
        stat_chain
    })
    .sn();

    // func params parser
    let func_params = group((r#type.clone(), ident.clone()))
        .map_group(ast::FuncParam::new)
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .map(Vec::into_boxed_slice)
        .delim_by(Delim::Paren);

    // function parser
    let func = group((
        r#type,
        ident,
        func_params.then_ignore(just(Token::Is)),
        stat_chain.clone().then_ignore(just(Token::End)),
    ))
    .map_group(ast::Func::new)
    .validate(|func, #[allow(unused)] extra, #[allow(unused)] emitter| {
        if !func.body.is_return_block() {
            // TODO: come up with custom error types for better more typesafe error reporting
            emitter.emit(Rich::custom(
                extra.span(),
                format!(
                    "The body of function {} is not a returning block",
                    func.name
                ),
            ))
        }

        // return func to continue validation of other functions
        func
    });

    // program parser
    // TODO: figure out error recovery and proper labeling
    let program = just(Token::Begin)
        .ignore_then(
            group((
                func.repeated()
                    .collect::<Vec<_>>()
                    .map(Vec::into_boxed_slice),
                stat_chain,
            ))
            .map_group(ast::Program::new),
        )
        .then_ignore(just(Token::End));

    #[allow(clippy::let_and_return)] // because this is likely to be changed/extended in the future
    program
}

/// An extension trait for [Parser] local to this file, for convenient dot-syntax utility methods
#[ext(name = LocalParserExt, supertraits = Sized + private::Sealed)]
impl<'src, I, O, E, T> T
where
    I: Input<'src>,
    E: ParserExtra<'src, I>,
    T: Parser<'src, I, O, E>,
{
    /// Convenience method to wrap items in [SN] type.
    #[allow(clippy::type_complexity)]
    #[inline]
    fn sn(self) -> MapWith<Self, O, fn(O, &mut MapExtra<'src, '_, I, E>) -> SN<O>>
    where
        I: Input<'src, Token = Token, Span = SourcedSpan>,
    {
        self.map_with(|t, e| SN::new(t, e.span()))
    }

    /// Convenience method to delimit a parser pattern by a [Delim].
    #[allow(clippy::type_complexity)]
    #[inline]
    fn delim_by(
        self,
        delim: Delim,
    ) -> DelimitedBy<Self, Just<Token, I, E>, Just<Token, I, E>, Token, Token>
    where
        I: Input<'src, Token = Token>,
    {
        self.delimited_by(just(Token::Open(delim)), just(Token::Close(delim)))
    }

    /// Convenience method to attempt to recover error recover by searching for the end
    /// of a [`Delim`] delimiter, while respecting any nested delimiter structure.
    #[inline]
    fn recover_with_delim<F>(
        self,
        delim: Delim,
        fallback: F,
    ) -> RecoverWith<Self, ViaParser<impl Parser<'src, I, O, E> + Clone>>
    where
        I: ValueInput<'src, Token = Token>,
        F: Fn(I::Span) -> O + Clone,
    {
        self.recover_with(via_parser(nested_delimiters(
            Token::Open(delim),
            Token::Close(delim),
            Delim::variants_except(delim).map(|d| (Token::Open(d), Token::Close(d))),
            fallback,
        )))
    }
}
