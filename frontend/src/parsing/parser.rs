#![allow(clippy::arbitrary_source_item_ordering)]

use crate::parsing::alias::{ProgramExtra, ProgramOutput};
use crate::parsing::ast::{ConditionalExprFragment, ConditionalStatFragment};
use crate::parsing::ext::ParserExt;
use crate::parsing::token::{Delim, Token};
use crate::parsing::{alias, ast, build_syntactic_report};
use crate::source::{SourcedBoxedNode, SourcedNode, SourcedSpan, StrSourceId};
use crate::{private, StreamType};
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
    select_ref,
    Parser,
};
use extend::ext;
use std::io;
use std::io::Write;
use thiserror::Error;
use util::nonempty::NonemptyArray;

/// A file-local type alias for better readability of type definitions
type SN<T> = SourcedNode<T>;
type SBN<T> = SourcedBoxedNode<T>;

#[must_use]
#[inline]
pub fn ident_parser<'src, I, E>() -> impl alias::Parser<'src, I, ast::Ident, E>
where
    I: BorrowInput<'src, Token = Token>,
    E: ParserExtra<'src, I, Error = Rich<'src, I::Token, I::Span>>,
{
    // identifiers can be extracted directly from `Ident` tokens, copying
    // an internal atomic reference to an interned identifier string
    #[allow(clippy::pattern_type_mismatch)]
    (select_ref! { Token::Ident(x) => x.clone().into() })
        .labelled("<ident>")
        .as_context()
}

#[allow(clippy::pattern_type_mismatch)]
#[must_use]
#[inline]
pub fn liter_parser<'src, I, E>() -> impl alias::Parser<'src, I, ast::Liter, E>
where
    I: BorrowInput<'src, Token = Token>,
    E: ParserExtra<'src, I, Error = Rich<'src, I::Token, I::Span>>,
{
    // some literals can be extracted from their corresponding tokens
    let int_liter = select_ref! { Token::IntLiter(x) => ast::Liter::IntLiter(*x) }
        .labelled("<int-liter>")
        .as_context();
    let char_liter = select_ref! { Token::CharLiter(x) => ast::Liter::CharLiter(*x) }
        .labelled("<char-liter>")
        .as_context();
    let str_liter = select_ref! { Token::StrLiter(x) => ast::Liter::StrLiter(x.clone()) }
        .labelled("<str-liter>")
        .as_context();

    // some literals can be created from keywords
    let bool_liter = choice((
        just(Token::True).to(ast::Liter::BoolLiter(true)),
        just(Token::False).to(ast::Liter::BoolLiter(false)),
    ))
    .labelled("<bool-liter>")
    .as_context();
    let pair_liter = just(Token::Null)
        .to(ast::Liter::PairLiter)
        .labelled("<pair-liter>")
        .as_context();

    // final literal parser is one of these five
    choice((int_liter, char_liter, str_liter, bool_liter, pair_liter))
}

#[inline]
pub fn array_elem_parser<'src, I, E, Ident, Expr>(
    ident: Ident,
    expr: Expr,
) -> impl alias::Parser<'src, I, ast::ArrayElem, E>
where
    I: BorrowInput<'src, Token = Token, Span = SourcedSpan>,
    E: ParserExtra<'src, I, Error = Rich<'src, I::Token, I::Span>>,
    Ident: alias::Parser<'src, I, ast::Ident, E>,
    Expr: alias::Parser<'src, I, ast::Expr, E>,
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
        .labelled("<array-elem>")
        .as_context();

    array_elem
}

#[allow(clippy::too_many_lines)]
#[must_use]
#[inline]
pub fn expr_parser<'src, I, E>() -> impl alias::Parser<'src, I, ast::Expr, E>
where
    I: BorrowInput<'src, Token = Token, Span = SourcedSpan> + ValueInput<'src>,
    E: ParserExtra<'src, I, Error = Rich<'src, I::Token, I::Span>>,
{
    recursive(|expr| {
        let ident = ident_parser();
        let liter = liter_parser();
        let array_elem = array_elem_parser(ident.clone(), expr.clone()).sbn();

        // parse parenthesized expressions
        let paren_expr = expr.clone().delim_by(Delim::Paren).sbn();

        // parse if-(elif)-else expressions
        let if_header = group((
            just(Token::If).ignore_then(expr.clone()).sbn(),
            just(Token::Then).ignore_then(expr.clone()).sbn(),
        ))
        .map_group(ConditionalExprFragment::new);
        let elifs = group((
            just(Token::Elif).ignore_then(expr.clone()).sbn(),
            just(Token::Then).ignore_then(expr.clone()).sbn(),
        ))
        .map_group(ConditionalExprFragment::new)
        .repeated()
        .collect::<Vec<_>>();
        let if_elif_else = group((
            if_header,
            elifs,
            just(Token::Else)
                .ignore_then(expr.clone().sbn())
                .then_ignore(just(Token::Fi)),
        ))
        .map_group(ast::Expr::if_elif_else);

        // 'Atoms' are expressions that contain no ambiguity
        let atom = choice((
            liter.sn().map(ast::Expr::Liter),
            // array elements begin with identifiers, so
            // give them precedence over identifiers
            array_elem.map(ast::Expr::ArrayElem),
            // Bootleg approach to get SN<Ident> from Ident parser
            ident.sn().map(ast::Expr::Ident),
            paren_expr.map(ast::Expr::Paren),
            // if-elif-else expression should be parsed after all others
            if_elif_else,
        ));

        // Perform simplistic error recovery on Atom expressions
        let atom = atom
            // Attempt to recover anything that looks like a parenthesised expression but contains errors
            .recover_with_delim(Delim::Paren, ast::Expr::Error)
            // Attempt to recover anything that looks like an array-element but contains errors
            .recover_with_delim(Delim::Bracket, ast::Expr::Error);

        // unary and binary operator parsers
        let unary_oper = choice((
            just(Token::Bang).to(ast::UnaryOper::LNot),
            just(Token::Minus).to(ast::UnaryOper::Minus),
            just(Token::Len).to(ast::UnaryOper::Len),
            just(Token::Ord).to(ast::UnaryOper::Ord),
            just(Token::Chr).to(ast::UnaryOper::Chr),
        ))
        .sn()
        .labelled("<unary-oper>")
        .as_context();
        let product_oper = choice((
            just(Token::Star).to(ast::BinaryOper::Mul),
            just(Token::ForwardSlash).to(ast::BinaryOper::Div),
            just(Token::Percent).to(ast::BinaryOper::Mod),
        ))
        .sn()
        .labelled("<binary-oper>")
        .as_context();
        let sum_oper = choice((
            just(Token::Plus).to(ast::BinaryOper::Add),
            just(Token::Minus).to(ast::BinaryOper::Sub),
        ))
        .sn()
        .labelled("<binary-oper>")
        .as_context();
        let arith_cmp_oper = choice((
            just(Token::Lte).to(ast::BinaryOper::Lte),
            just(Token::Lt).to(ast::BinaryOper::Lt),
            just(Token::Gte).to(ast::BinaryOper::Gte),
            just(Token::Gt).to(ast::BinaryOper::Gt),
        ))
        .sn()
        .labelled("<binary-oper>")
        .as_context();
        let eq_cmp_oper = choice((
            just(Token::EqualsEquals).to(ast::BinaryOper::Eq),
            just(Token::BangEquals).to(ast::BinaryOper::Neq),
        ))
        .sn()
        .labelled("<binary-oper>")
        .as_context();
        let land_oper = just(Token::AmpersandAmpersand)
            .to(ast::BinaryOper::LAnd)
            .sn()
            .labelled("<binary-oper>")
            .as_context();
        let lor_oper = just(Token::PipePipe)
            .to(ast::BinaryOper::LOr)
            .sn()
            .labelled("<binary-oper>")
            .as_context();

        // procedure to turn patterns into binary expressions
        let binary_create = |lhs, op, rhs, extra: &mut MapExtra<'src, '_, I, _>| {
            SBN::from_unboxed(ast::Expr::Binary(lhs, op, rhs), extra.span())
        };

        // a PRATT parser for prefix and left-infix operator expressions
        #[allow(clippy::shadow_unrelated)]
        let atom = atom.sbn().pratt((
            // We want unary operations to happen before any binary ones, so their precedence
            // is set to be the highest. But amongst themselves the precedence is the same.
            prefix(3, unary_oper, |op, rhs, extra| {
                SBN::from_unboxed(ast::Expr::Unary(op, rhs), extra.span())
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
            // logical AND (&&) has more precedence than logical OR (||),
            // where logical AND (&&) has precedence just below that of equality comparisons
            infix(right(2), land_oper, binary_create),
            infix(right(1), lor_oper, binary_create),
        ));

        // as of now, no other type of expression exists
        expr.map(SBN::into_inner_unboxed)
    })
}

#[must_use]
#[inline]
pub fn type_parser<'src, I, E>() -> impl alias::Parser<'src, I, ast::Type, E>
where
    I: BorrowInput<'src, Token = Token, Span = SourcedSpan> + ValueInput<'src>,
    E: ParserExtra<'src, I, Error = Rich<'src, I::Token, I::Span>>,
{
    recursive(|r#type| {
        // base types have no recursion
        let base_type = choice((
            just(Token::Int).to(ast::BaseType::Int),
            just(Token::Bool).to(ast::BaseType::Bool),
            just(Token::Char).to(ast::BaseType::Char),
            just(Token::String).to(ast::BaseType::String),
        ))
        .sn()
        .labelled("<base-type>")
        .as_context();

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
                    ast::Type::ArrayType(SBN::from_unboxed(ast::ArrayType::new(ty), extra.span()))
                },
            )
            // as explained above, in order to get the parser to consume tokens correctly, we have
            // to allow the possibility that at this point, this isn't even an ArrayType, so we are
            // filtering only for array types at this point
            .select_output(|ty, _| match ty {
                ast::Type::ArrayType(ty) => Some(ty),
                _ => None,
            })
            .memoized()
            .labelled("<array-type>")
            .as_context();

        // pair-element type parser;
        // an array type and all other types look the same until the very last, so we should
        // give precedence to array types to make sure they are not incorrectly missed
        let pair_elem_type = choice((
            array_type.clone().map(ast::PairElemType::ArrayType),
            base_type.clone().map(ast::PairElemType::BaseType),
            just(Token::Pair).to_span().map(ast::PairElemType::Pair),
        ))
        .labelled("<pair-elem-type>")
        .as_context();
        let pair_type = just(Token::Pair)
            .ignore_then(
                group((
                    pair_elem_type.clone().then_ignore(just(Token::Comma)),
                    pair_elem_type,
                ))
                .map_group(ast::Type::PairType)
                .delim_by(Delim::Paren)
                // Attempt to recover anything that looks like a (parenthesised) pair type but contains errors
                .recover_with_delim(Delim::Paren, ast::Type::Error),
            )
            .labelled("<pair-type>")
            .as_context();

        // a type is either a base type, array type, or pair type;
        // an array type and all other types look the same until the very last, so we should
        // give precedence to array types to make sure they are not incorrectly missed
        #[allow(clippy::shadow_unrelated)]
        let r#type = choice((
            array_type.map(ast::Type::ArrayType),
            base_type.map(ast::Type::BaseType),
            pair_type,
        ))
        .labelled("<type>")
        .as_context();

        r#type
    })
}

#[allow(clippy::too_many_lines)]
#[inline]
pub fn stat_parser<'src, I, E, P>(stat_chain: P) -> impl alias::Parser<'src, I, ast::Stat, E>
where
    I: BorrowInput<'src, Token = Token, Span = SourcedSpan> + ValueInput<'src>,
    E: ParserExtra<'src, I, Error = Rich<'src, I::Token, I::Span>>,
    P: alias::Parser<'src, I, ast::StatBlock, E>,
{
    let ident = ident_parser();
    let expr = expr_parser();
    let array_elem = array_elem_parser(ident.clone(), expr.clone());

    // wrap parsers with SN<_>
    let ident = ident.sn();
    let expr = expr.sn();
    let array_elem = array_elem.sn();
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
        .map(ast::RValue::ArrayLiter)
        .labelled("<array-liter>")
        .as_context();

    // newpair parser
    let newpair = just(Token::Newpair).ignore_then(
        group((expr.clone().then_ignore(just(Token::Comma)), expr.clone()))
            .delim_by(Delim::Paren)
            .map_group(ast::RValue::NewPair),
    );

    // declare the LValue parser
    let mut lvalue = Recursive::declare();

    // pair-elem parser
    let pair_elem_selector = choice((
        just(Token::Fst).to(ast::PairElemSelector::Fst),
        just(Token::Snd).to(ast::PairElemSelector::Snd),
    ));
    let pair_elem = pair_elem_selector
        .then(lvalue.clone().sbn())
        .map_group(ast::PairElem)
        .sn()
        .labelled("<pair-elem>")
        .as_context();

    // function call parser
    let function_call = just(Token::Call).ignore_then(
        group((ident.clone(), expr_sequence.delim_by(Delim::Paren))).map_group(ast::RValue::call),
    );

    // define the lvalue parser: array-elem contains identifier within it, so it should be
    // given parsing precedence to disambiguate properly
    lvalue.define(choice((
        array_elem.map(ast::LValue::ArrayElem),
        pair_elem.clone().map(ast::LValue::PairElem),
        ident.clone().map(ast::LValue::Ident),
    )));

    let lvalue = lvalue.sn().labelled("<lvalue>").as_context();

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
    ))
    .sn()
    .labelled("<rvalue>")
    .as_context();

    // variable definition parser
    let variable_definition = group((
        r#type,
        ident.clone().then_ignore(just(Token::Equals)),
        rvalue.clone(),
    ))
    .map_group(ast::Stat::var_definition);

    // assignment parser
    let assignment = group((lvalue.clone().then_ignore(just(Token::Equals)), rvalue))
        .map_group(ast::Stat::assignment);

    // "if"-header parser: "if" goes at the start of both if-only and if-else statements
    let if_header = group((
        just(Token::If).ignore_then(expr.clone()),
        just(Token::Then).ignore_then(stat_chain.clone()),
    ))
    .map_group(ConditionalStatFragment::new);

    // "elif"s parser: "elif"s (optionally) go after the if-only statements, and inbetween if-else statements
    let elifs = group((
        just(Token::Elif).ignore_then(expr.clone()),
        just(Token::Then).ignore_then(stat_chain.clone()),
    ))
    .map_group(ConditionalStatFragment::new)
    .repeated()
    .collect::<Vec<_>>();

    // if-(elif)-only parser
    let if_elif_only = group((if_header.clone(), elifs.clone()))
        .then_ignore(just(Token::Fi))
        .map_group(ast::Stat::if_elif_only);

    // if-(elif)-else parser
    let if_elif_else = group((
        if_header,
        elifs,
        just(Token::Else)
            .ignore_then(stat_chain.clone())
            .then_ignore(just(Token::Fi)),
    ))
    .map_group(ast::Stat::if_elif_else);

    // explicit (and also optional) loop-label parser
    let loop_label = ident.clone().then_ignore(just(Token::Colon)).or_not();

    // while-do loop parser
    let while_do = group((
        loop_label.clone().then_ignore(just(Token::While)),
        expr.clone().then_ignore(just(Token::Do)),
        stat_chain.clone().then_ignore(just(Token::Done)),
    ))
    .map_group(ast::Stat::while_do);

    // do-while loop parser
    let do_while = group((
        loop_label.clone().then_ignore(just(Token::Do)),
        stat_chain.clone().then_ignore(just(Token::While)),
        expr.clone().then_ignore(just(Token::Done)),
    ))
    .map_group(ast::Stat::do_while);

    // loop-do parser
    let loop_do = group((
        loop_label
            .then_ignore(just(Token::Loop))
            .then_ignore(just(Token::Do)),
        stat_chain.clone().then_ignore(just(Token::Done)),
    ))
    .map_group(ast::Stat::loop_do);

    // begin-end scope
    let scoped = just(Token::Begin)
        .ignore_then(stat_chain.clone().then_ignore(just(Token::End)))
        .map(ast::Stat::Scoped);

    // break/nextloop statements
    let break_stat = just(Token::Break)
        .ignore_then(ident.clone().or_not())
        .map(ast::Stat::Break);
    let nextloop_stat = just(Token::Nextloop)
        .ignore_then(ident.or_not())
        .map(ast::Stat::NextLoop);

    // statement parser
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
        if_elif_only,
        if_elif_else,
        while_do,
        do_while,
        loop_do,
        break_stat,
        nextloop_stat,
        scoped,
    ))
    .labelled("<stmt>")
    .as_context();

    #[allow(clippy::let_and_return)] // because this is likely to be changed/extended in the future
    stat
}

#[inline]
pub fn program_parser<'src, I, E>() -> impl alias::Parser<'src, I, ast::Program, E>
where
    I: BorrowInput<'src, Token = Token, Span = SourcedSpan> + ValueInput<'src>,
    E: ParserExtra<'src, I, Error = Rich<'src, I::Token, I::Span>>,
{
    let r#type = type_parser::<I, E>().sn();
    let ident = ident_parser::<I, E>().sn();

    // statement chain parser
    let stat_chain = recursive(|stat_chain| {
        let stat = stat_parser(stat_chain).sn();

        // parse statement chains
        #[allow(clippy::shadow_unrelated)]
        let stat_chain = stat
            .clone()
            .map(|f| vec![f])
            .foldl(
                just(Token::Semicolon).ignore_then(stat).repeated(),
                |mut vec, s| {
                    vec.push(s);
                    vec
                },
            )
            .pipe((ast::StatBlock::try_new, Result::unwrap));

        #[allow(clippy::let_and_return)]
        // because this is likely to be changed/extended in the future
        stat_chain
    });

    // func params parser
    let func_params = group((r#type.clone(), ident.clone()))
        .map_group(ast::FuncParam::new)
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .map(Vec::into_boxed_slice)
        .labelled("<param-list>")
        .as_context();

    // function parser
    let func = group((
        just(Token::Tailrec).or_not().map(|o| o.is_some()),
        r#type,
        ident,
        func_params
            .delim_by(Delim::Paren)
            .then_ignore(just(Token::Is)),
        stat_chain.clone().then_ignore(just(Token::End)),
    ))
    .map_group(ast::Func::new)
    .validate(|func, #[allow(unused)] extra, #[allow(unused)] emitter| {
        if !func.body.is_return_block() {
            emitter.emit(Rich::custom(
                extra.span(),
                format!(
                    "The body of function {} is not a returning block",
                    func.name.inner()
                ),
            ));
        }

        // return func to continue validation of other functions
        func
    })
    .labelled("<func>")
    .as_context();

    // program parser
    let funcs = func
        .repeated()
        .collect::<Vec<_>>()
        .map(Vec::into_boxed_slice);
    let program = funcs
        .then(stat_chain)
        .delimited_by(just(Token::Begin), just(Token::End))
        .map(|(l, r)| ast::Program::new(l, r));

    #[allow(clippy::let_and_return)] // because this is likely to be changed/extended in the future
    program
        .then_ignore(end())
        .labelled("<program>")
        .as_context()
}

#[derive(Debug, Error)]
pub enum ParsingPhaseError {
    #[error("Encountered parsing error, corresponding error report written to provided output")]
    ParsingErrorWritten,
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
pub fn parsing_phase<S: AsRef<str>, W: Write + Clone>(
    source: S,
    source_id: StrSourceId,
    tokens: Vec<(Token, SourcedSpan)>,
    parsing_error_code: i32,
    stream_type: StreamType,
    output_stream: W,
) -> Result<ast::Program, ParsingPhaseError> {
    // attach the span of each token to it before parsing, so it is not forgotten
    let source = source.as_ref();
    let eoi_span = SourcedSpan::new(source_id.clone(), (source.len()..source.len()).into());
    let spanned_tokens = tokens.as_slice().map(eoi_span, |(t, s)| (t, s));

    // parse the tree
    #[allow(clippy::pattern_type_mismatch)]
    let (parsed, parse_errs): ProgramOutput =
        Parser::parse(&program_parser::<_, ProgramExtra>(), spanned_tokens).into_output_errors();

    // return any parsed errors
    if !parse_errs.is_empty() {
        for e in &parse_errs {
            build_syntactic_report(
                e,
                source,
                parsing_error_code,
                stream_type,
                output_stream.clone(),
            )?;
        }
        return Err(ParsingPhaseError::ParsingErrorWritten);
    }

    Ok(parsed.expect("If lexing errors are not empty, tokens should be Valid"))
}

// #[derive(Default)]
// pub struct DebugInspector<'src, I>(PhantomData<&'src I>)
// where
//     I: Input<'src>;
//
// impl<'src, I> Inspector<'src, I> for DebugInspector<'src, I>
// where
//     I: Input<'src>,
//     I::Token: fmt::Debug,
// {
//     type Checkpoint = usize;
//
//     fn on_token(&mut self, token: &I::Token) {
//         println!("DEBUG INSPECTOR: on token: {:#?}", token);
//     }
//
//     fn on_save<'parse>(&self, cursor: &Cursor<'src, 'parse, I>) -> Self::Checkpoint {
//         let location = I::cursor_location(cursor.inner());
//         println!("DEBUG INSPECTOR: on save: {:#?}", location);
//         location
//     }
//
//     fn on_rewind<'parse>(&mut self, marker: &Checkpoint<'src, 'parse, I, Self::Checkpoint>) {
//         let location = I::cursor_location(marker.cursor().inner());
//         let checkpoint = marker.inspector();
//         println!(
//             "DEBUG INSPECTOR: on rewind: from {:#?} to {:#?}",
//             location, checkpoint
//         );
//     }
// }

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

    /// Convenience method to wrap items in [SN] type.
    #[allow(clippy::type_complexity)]
    #[inline]
    fn sbn(self) -> MapWith<Self, O, fn(O, &mut MapExtra<'src, '_, I, E>) -> SBN<O>>
    where
        I: Input<'src, Token = Token, Span = SourcedSpan>,
    {
        self.map_with(|t, e| SBN::from_unboxed(t, e.span()))
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
