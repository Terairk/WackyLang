use crate::ast::{
    ArrayElem, ArrayType, AssignLhs, AssignRhs, BaseType, BinaryOper, Expr, Func, FuncParam, Ident,
    PairElem, PairElemType, Program, Stat, StatChain, Type, UnaryOper,
};
use crate::shared::SharedString;
use crate::spanned::Spanned;
use crate::token::{Keyword, Symbol, Token};
use crate::CharExt;
use chumsky::{
    error::{Error as ChumskyError, Simple},
    prelude::*,
    Error,
};

pub fn expr_parser() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    recursive(|expr: Recursive<'_, Token, Spanned<Expr>, Simple<Token>>| {
        // identifiers can be parsed directly from `Ident` tokens
        let ident = select! { Token::Ident(s) => Ident(s) }.labelled("<ident>");

        // we need to check that our integer literals fit within 32-bits as specified by the WACC-language Spec
        let int_liter = filter_map(move |span, t| match t {
            Token::IntLiter(s) => match s.parse::<i32>() {
                Ok(i) => Ok(Expr::IntLiter(i)),
                Err(_) => Err(Simple::custom(
                    span,
                    format!("The integer literal '{}' does not fit within 32 bytes", s),
                )),
            },
            _ => Err(Simple::expected_input_found(span, None, Some(t))),
        })
        .labelled("<int-liter>");

        // boolean literals can be parsed directly from keywords
        let bool_liter = choice((
            just(Token::Keyword(Keyword::True)).to(Expr::BoolLiter(true)),
            just(Token::Keyword(Keyword::False)).to(Expr::BoolLiter(false)),
        ))
        .labelled("<bool-liter>");

        // characters have to be further processed based on if they are
        // normal or escaped characters
        let char_liter = select! { Token::CharLiter(s) =>
            Expr::CharLiter(if s.len() == 1 {
                // length-one string implies the character is a normal WACC character
                let c: char = s.chars().next().unwrap();
                assert!(c.normal_wacc_char());
                c
            } else {
                // length-two string implies escaped WACC character
                assert_eq!(s.len(), 2);
                let mut char_iter = s.chars();
                assert_eq!(char_iter.next().unwrap(), '\\');
                let c: char = char_iter.next().unwrap();
                // map each escaped-character to what it actually means
                c.lookup_escaped_wacc_char().unwrap()
            })
        }
        .labelled("<char-liter>");

        // strings also have to be further processed, paring each `\`-escaped character appropriately
        let str_liter = select! { Token::StrLiter(s) => {
            let mut s_chars = s.chars();
            let mut processed_str: String = String::new();

            // build up parsed string by walking through character iterator and testing
            // for any `\` characters to see if we need to lookup WACC-escaped characters
            while let Some(c) = s_chars.next() {
                // handle escaped-character, no panics should happen here
                // if the lexing stage contains no logical errors
                if (c == '\\') {
                    processed_str.push(s_chars.next().unwrap().lookup_escaped_wacc_char().unwrap());
                } else {
                    processed_str.push(c);
                }
            }

            Expr::StrLiter(SharedString::from(processed_str))
        } }
        .labelled("<str-liter>");

        // Pair literals can only be `null` and `null` can only mean pair literals
        let pair_liter =
            select! { Token::Keyword(Keyword::Null) => Expr::PairLiter }.labelled("<pair-liter>");

        // base-case expression parser, i.e. the non-recursive cases
        let base_expr = choice((int_liter, bool_liter, char_liter, str_liter, pair_liter));

        // array element parser
        let array_elem_index = expr.clone().delimited_by(
            just(Token::Symbol(Symbol::OpenBracket)),
            just(Token::Symbol(Symbol::CloseBracket)),
        );
        let array_elem = ident
            .then(array_elem_index.clone())
            .then(array_elem_index.repeated())
            .map(|((array_name, first_index), other_indices)| {
                Expr::ArrayElem(ArrayElem {
                    array_name,
                    first_index: Box::new(first_index.inner),
                    other_indices: other_indices.iter().map(|s| s.inner.clone()).collect(),
                })
            })
            .labelled("<array-elem>");

        // parser which resolves array-element vs. identifier ambiguity, namely:
        // an identifier followed by `([<expr>])+` is an array-element, and otherwise
        // its just an identifier
        let ident_and_array_elem = array_elem.or(ident.map(Expr::Ident));

        // Parse parenthesized expressions
        let paren_expr = expr
            .clone()
            .delimited_by(
                just(Token::Symbol(Symbol::OpenParen)),
                just(Token::Symbol(Symbol::CloseParen)),
            )
            .map(|e| Expr::Paren(Box::new(e.inner)));

        // 'Atoms' are expressions that contain no ambiguity
        let atom = base_expr
            .or(ident_and_array_elem)
            .or(paren_expr)
            .map_with_span(|e, s| Spanned::new(e, s))
            // Attempt to recover anything that looks like a parenthesised expression but contains errors
            .recover_with(nested_delimiters(
                Token::Symbol(Symbol::OpenParen),
                Token::Symbol(Symbol::CloseParen),
                [(
                    Token::Symbol(Symbol::OpenBracket),
                    Token::Symbol(Symbol::CloseBracket),
                )],
                |span| Spanned::new(Expr::Error, span),
            ))
            // Attempt to recover anything that looks like an array-element but contains errors
            .recover_with(nested_delimiters(
                Token::Symbol(Symbol::OpenBracket),
                Token::Symbol(Symbol::CloseBracket),
                [(
                    Token::Symbol(Symbol::OpenParen),
                    Token::Symbol(Symbol::CloseParen),
                )],
                |span| Spanned::new(Expr::Error, span),
            ));

        // unary operators all have the same precedence, and higher than binary operators
        let unary_oper = choice((
            just(Token::Symbol(Symbol::Bang)).to(UnaryOper::Not),
            just(Token::Symbol(Symbol::Minus)).to(UnaryOper::Minus),
            just(Token::Keyword(Keyword::Len)).to(UnaryOper::Len),
            just(Token::Keyword(Keyword::Ord)).to(UnaryOper::Ord),
            just(Token::Keyword(Keyword::Chr)).to(UnaryOper::Chr),
        ))
        .labelled("<unary-oper>");
        let unary = unary_oper
            .map_with_span(|o, s| Spanned::new(o, s))
            .repeated()
            .then(atom)
            .foldr(|o, e| {
                let span = o.start()..e.end();
                Spanned::new(Expr::Unary(o.inner, Box::new(e.inner)), span)
            });

        // binary operator parser implementation
        fn binary_op_parser<I, E, B, O>(
            base: B,
            oper: O,
        ) -> impl Parser<I, Spanned<Expr>, Error = E> + Clone
        where
            I: Clone,
            E: Error<I>,
            B: Parser<I, Spanned<Expr>, Error = E> + Clone,
            O: Parser<I, BinaryOper, Error = E> + Clone,
        {
            base.clone()
                .then(oper.then(base).repeated())
                .foldl(|a, (o, b)| {
                    let span = a.start()..b.end();
                    Spanned::new(Expr::Binary(Box::new(a.inner), o, Box::new(b.inner)), span)
                })
        }

        // Product ops (multiply, divide, and mod) have equal precedence
        let product = binary_op_parser(
            unary,
            choice((
                just(Token::Symbol(Symbol::Star)).to(BinaryOper::Mul),
                just(Token::Symbol(Symbol::ForwardSlash)).to(BinaryOper::Div),
                just(Token::Symbol(Symbol::Percent)).to(BinaryOper::Mod),
            ))
            .labelled("<binary-oper>"),
        );

        // Sum ops (add and subtract) have equal precedence
        let sum = binary_op_parser(
            product,
            choice((
                just(Token::Symbol(Symbol::Plus)).to(BinaryOper::Add),
                just(Token::Symbol(Symbol::Minus)).to(BinaryOper::Sub),
            ))
            .labelled("<binary-oper>"),
        );

        // arithmetic comparisons (<, <=, >, >=) have equal precedence
        let arith_cmp = binary_op_parser(
            sum,
            choice((
                just(Token::Symbol(Symbol::Lt)).to(BinaryOper::Lt),
                just(Token::Symbol(Symbol::Lte)).to(BinaryOper::Lte),
                just(Token::Symbol(Symbol::Gt)).to(BinaryOper::Gt),
                just(Token::Symbol(Symbol::Gte)).to(BinaryOper::Gte),
            ))
            .labelled("<binary-oper>"),
        );

        // equality comparisons (== and !=) have equal precedence
        let eq_cmp = binary_op_parser(
            arith_cmp,
            choice((
                just(Token::Symbol(Symbol::EqualsEquals)).to(BinaryOper::Eq),
                just(Token::Symbol(Symbol::BangEquals)).to(BinaryOper::Neq),
            ))
            .labelled("<binary-oper>"),
        );

        // logical AND (&&) has more precedence than logical OR (||)
        let logical_and = binary_op_parser(
            eq_cmp,
            just(Token::Symbol(Symbol::And))
                .to(BinaryOper::And)
                .labelled("<binary-oper>"),
        );
        let logical_or = binary_op_parser(
            logical_and,
            just(Token::Symbol(Symbol::Or))
                .to(BinaryOper::Or)
                .labelled("<binary-oper>"),
        );

        // all expressions have been covered, and every ambiguity resolved
        // so we can return the logical OR parser, which should parse all-else
        logical_or
    })
}

pub fn type_parser() -> impl Parser<Token, Spanned<Type>, Error = Simple<Token>> + Clone {
    // base types have no recursion
    let rrbase_type = choice((
        just(Token::Keyword(Keyword::Int)).to(BaseType::Int),
        just(Token::Keyword(Keyword::Bool)).to(BaseType::Bool),
        just(Token::Keyword(Keyword::Char)).to(BaseType::Char),
        just(Token::Keyword(Keyword::String)).to(BaseType::String),
    ))
    .labelled("<base-type>");

    // array-types are left-recursive thus the parser will loop infinitely if not eliminated;
    // we can eliminate by rewriting production rules from the following:
    //
    // <type>           => <base-type> | <array-type> | <pair-type>
    // <base-type>      => 'int' | 'bool' | 'char' | 'string'
    // <array-type>     => <type> '[' ']'
    // <pair-type>      => 'pair' '(' <pair-elem-type> ',' <pair-elem-type> ')'
    // <pair-elem-type> => <base-type> | <array-type> | 'pair'
    //
    // into the following right-recursive set of production rules:
    //
    // <type>             => <base-type> | <array-type> | <pair-type>
    // <base-type>        => 'int' | 'bool' | 'char' | 'string'
    // <type-excl-array>   => <base-type> | <pair-type>
    // <array-type>       => <type-excl-array> '[' ']' <array-type-prime>
    // <array-type-prime> => '[' ']' <array-type-prime> | ϵ
    // <pair-type>        => 'pair' '(' <pair-elem-type> ',' <pair-elem-type> ')'
    // <pair-elem-type>   => <base-type> | <array-type> | 'pair'
    //
    // where epsilon (ϵ) means empty string/token-stream. In order to conveniently implement
    // these new production rules, new temporary types are be created which capture these rules

    #[derive(Clone, Debug)]
    enum RRType {
        BaseType(BaseType),
        RRArrayType(RRArrayType),
        RRPairType(RRPairType),
    }

    #[derive(Clone, Debug)]
    enum RRTypeExclArray {
        BaseType(BaseType),
        RRPairType(Box<RRPairType>),
    }

    #[derive(Clone, Debug)]
    struct RRArrayType(RRTypeExclArray, RRArrayTypePrime);

    #[derive(Debug, Clone)]
    enum RRArrayTypePrime {
        OpenClosesBracket(Box<RRArrayTypePrime>),
        Epsilon,
    }

    #[derive(Clone, Debug)]
    struct RRPairType(RRPairElemType, RRPairElemType);

    #[derive(Clone, Debug)]
    enum RRPairElemType {
        BaseType(BaseType),
        RRArrayType(RRArrayType),
        Pair,
    }

    // also create converters to turn the right-recursive structures back into normal AST nodes

    fn convert_rrtype(rrtype: RRType) -> Type {
        match rrtype {
            RRType::BaseType(b) => Type::BaseType(b),
            RRType::RRArrayType(a) => Type::ArrayType(convert_rrarray_type(a)),
            RRType::RRPairType(RRPairType(a, b)) => {
                Type::PairType(convert_rrpair_elem_type(a), convert_rrpair_elem_type(b))
            }
        }
    }

    fn convert_rrarray_type(rrarray_type: RRArrayType) -> ArrayType {
        let RRArrayType(element_type, mut prime) = rrarray_type;

        // extract the innermost array type, then build atop it by unwrapping the RRArrayTypePrime
        let mut array_type = ArrayType {
            elem_type: Box::new(match element_type {
                RRTypeExclArray::BaseType(b) => Type::BaseType(b),
                RRTypeExclArray::RRPairType(p) => {
                    Type::PairType(convert_rrpair_elem_type(p.0), convert_rrpair_elem_type(p.1))
                }
            }),
        };

        // iterate and unwrap RRArrayTypePrime until we reach epsilon
        while let RRArrayTypePrime::OpenClosesBracket(boxed_prime) = &prime {
            array_type = ArrayType {
                elem_type: Box::new(Type::ArrayType(array_type)),
            };
            prime = boxed_prime.as_ref().clone()
        }

        array_type
    }

    fn convert_rrpair_elem_type(rrpair_elem_type: RRPairElemType) -> PairElemType {
        match rrpair_elem_type {
            RRPairElemType::BaseType(b) => PairElemType::BaseType(b),
            RRPairElemType::RRArrayType(a) => PairElemType::ArrayType(convert_rrarray_type(a)),
            RRPairElemType::Pair => PairElemType::Pair,
        }
    }

    // create parser for: <array-type-prime> => '[' ']' <array-type-prime> | ϵ, in order to capture this right-recursion
    let rrarray_type_prime = recursive(
        |prime: Recursive<'_, Token, RRArrayTypePrime, Simple<Token>>| {
            just(Token::Symbol(Symbol::OpenBracket))
                .ignore_then(just(Token::Symbol(Symbol::CloseBracket)))
                .ignore_then(prime)
                .map(|p| RRArrayTypePrime::OpenClosesBracket(Box::new(p)))
                .or_not()
                .map(|p| p.unwrap_or(RRArrayTypePrime::Epsilon))
        },
    );

    // pair-type parser factory
    fn rrpair_type_factory<E: Error<Token>, P>(
        rrpair_elem_type: P,
    ) -> impl Parser<Token, RRPairType, Error = E> + Clone
    where
        P: Parser<Token, RRPairElemType, Error = E> + Clone,
    {
        just(Token::Keyword(Keyword::Pair))
            .ignore_then(just(Token::Symbol(Symbol::OpenParen)))
            .ignore_then(
                rrpair_elem_type
                    .clone()
                    .then_ignore(just(Token::Symbol(Symbol::Comma)))
                    .then(rrpair_elem_type)
                    .then_ignore(just(Token::Symbol(Symbol::CloseParen))),
            )
            .map(|(a, b)| RRPairType(a, b))
    }

    // base-type and pair-type variant array-type parsers
    let rrbase_type_variant_array_type = rrbase_type
        .clone()
        .then_ignore(just(Token::Symbol(Symbol::OpenBracket)))
        .then_ignore(just(Token::Symbol(Symbol::CloseBracket)))
        .then(rrarray_type_prime.clone())
        .map(|(b, prime)| RRArrayType(RRTypeExclArray::BaseType(b), prime));
    fn pair_type_variant_array_type_factory<E: Error<Token>, P, A>(
        rrpair_elem_type: P,
        rrarray_type_prime: A,
    ) -> impl Parser<Token, RRArrayType, Error = E> + Clone
    where
        P: Parser<Token, RRPairElemType, Error = E> + Clone,
        A: Parser<Token, RRArrayTypePrime, Error = E> + Clone,
    {
        rrpair_type_factory(rrpair_elem_type)
            .then_ignore(just(Token::Symbol(Symbol::OpenBracket)))
            .then_ignore(just(Token::Symbol(Symbol::CloseBracket)))
            .then(rrarray_type_prime)
            .map(|(p, prime)| RRArrayType(RRTypeExclArray::RRPairType(Box::new(p)), prime))
    }

    // pair-elem-type parser, which tires to re-use as many building blocks as possible
    // without incurring extra recursion
    let rrpair_elem_type = recursive(
        |pair_elem_type: Recursive<'_, Token, RRPairElemType, Simple<Token>>| {
            choice((
                rrbase_type.clone().map(RRPairElemType::BaseType),
                rrbase_type_variant_array_type
                    .clone()
                    .map(RRPairElemType::RRArrayType),
                pair_type_variant_array_type_factory(pair_elem_type, rrarray_type_prime.clone())
                    .map(RRPairElemType::RRArrayType),
                just(Token::Keyword(Keyword::Pair)).to(RRPairElemType::Pair),
            ))
        },
    );

    // trivially create pair-type parser and array-type parser
    let rrpair_type = rrpair_type_factory(rrpair_elem_type.clone());
    let rrarray_type = rrbase_type_variant_array_type.or(pair_type_variant_array_type_factory(
        rrpair_elem_type,
        rrarray_type_prime,
    ));

    // when creating the type parser, give precedence to array-types, so they are
    // parsed first to the maximal extent if they exist, otherwise fall back to base/pair types
    let rrtype = rrarray_type
        .map(RRType::RRArrayType)
        .or(rrbase_type.map(RRType::BaseType))
        .or(rrpair_type.map(RRType::RRPairType));

    // TODO: figure out how and where to place error recovery for the type parser

    // return the final parsed type
    rrtype.map_with_span(|t, s| Spanned::new(convert_rrtype(t), s))
}

pub fn stat_parser_factory<P>(
    stat_chain: P,
) -> impl Parser<Token, Spanned<Stat>, Error = Simple<Token>> + Clone
where
    P: Parser<Token, Spanned<StatChain>, Error = Simple<Token>> + Clone,
{
    let expr = expr_parser();
    let r#type = type_parser();

    // identifiers can be parsed directly from `Ident` tokens
    let ident = select! { Token::Ident(s) => Ident(s) }.labelled("<ident>");

    // skip parser
    let skip = just(Token::Keyword(Keyword::Skip)).to(Stat::Skip);

    let expr_sequence = expr
        .clone()
        .separated_by(just(Token::Symbol(Symbol::Comma)));

    // array literal parser
    let array_liter = expr_sequence
        .clone()
        .delimited_by(
            just(Token::Symbol(Symbol::OpenBracket)),
            just(Token::Symbol(Symbol::CloseBracket)),
        )
        .map(|exprs| AssignRhs::ArrayLiter(exprs.into_iter().map(|e| e.inner).collect()));

    // newpair parser
    let newpair = just(Token::Keyword(Keyword::Newpair))
        .ignore_then(
            expr.clone()
                .then_ignore(just(Token::Symbol(Symbol::Comma)))
                .then(expr.clone())
                .delimited_by(
                    just(Token::Symbol(Symbol::OpenParen)),
                    just(Token::Symbol(Symbol::CloseParen)),
                ),
        )
        .map(|(a, b)| AssignRhs::Newpair(a.inner, b.inner));

    // pair-elem parser
    let pair_elem = choice((
        just(Token::Keyword(Keyword::Fst))
            .ignore_then(expr.clone())
            .map(|e| PairElem::Fst(e.inner)),
        just(Token::Keyword(Keyword::Snd))
            .ignore_then(expr.clone())
            .map(|e| PairElem::Snd(e.inner)),
    ));

    // function call parser
    let function_call = just(Token::Keyword(Keyword::Call))
        .ignore_then(ident)
        .then(expr_sequence.delimited_by(
            just(Token::Symbol(Symbol::OpenParen)),
            just(Token::Symbol(Symbol::CloseParen)),
        ))
        .map(|(id, exprs)| AssignRhs::Call {
            func_name: id,
            args: exprs.into_iter().map(|e| e.inner).collect(),
        });

    // array element parser
    let array_elem_index = expr.clone().delimited_by(
        just(Token::Symbol(Symbol::OpenBracket)),
        just(Token::Symbol(Symbol::CloseBracket)),
    );
    let array_elem = ident
        .then(array_elem_index.clone())
        .then(array_elem_index.repeated())
        .map(|((array_name, first_index), other_indices)| ArrayElem {
            array_name,
            first_index: Box::new(first_index.inner),
            other_indices: other_indices.iter().map(|s| s.inner.clone()).collect(),
        })
        .labelled("<array-elem>");

    // parser which resolves array-element vs. identifier ambiguity, namely:
    // an identifier followed by `([<expr>])+` is an array-element, and otherwise
    // its just an identifier
    let ident_and_array_elem = array_elem
        .map(AssignLhs::ArrayElem)
        .or(ident.map(AssignLhs::Ident));

    // assign-lhs parser
    let assign_lhs = ident_and_array_elem.or(pair_elem.clone().map(AssignLhs::PairElem));

    // assign-rhs parser
    let assign_rhs = choice((
        expr.clone().map(|e| AssignRhs::Expr(e.inner)),
        array_liter,
        newpair,
        pair_elem.map(AssignRhs::PairElem),
        function_call,
    ));

    // variable definition parser
    let variable_definition = r#type
        .then(ident)
        .then_ignore(just(Token::Symbol(Symbol::Equals)))
        .then(assign_rhs.clone())
        .map(|((t, id), rhs)| Stat::VarDefinition {
            var_type: t.inner,
            name: id,
            value: rhs,
        });

    // assignment parser
    let assignment = assign_lhs
        .clone()
        .then_ignore(just(Token::Symbol(Symbol::Equals)))
        .then(assign_rhs)
        .map(|(lhs, rhs)| Stat::Assignment { lhs, rhs });

    // if-then-else parser
    let if_then_else = just(Token::Keyword(Keyword::If))
        .ignore_then(expr.clone())
        .then_ignore(just(Token::Keyword(Keyword::Then)))
        .then(stat_chain.clone())
        .then_ignore(just(Token::Keyword(Keyword::Else)))
        .then(stat_chain.clone())
        .then_ignore(just(Token::Keyword(Keyword::Fi)))
        .map(|((if_cond, then_body), else_body)| Stat::IfThenElse {
            if_cond: if_cond.inner,
            then_body: then_body.inner,
            else_body: else_body.inner,
        });

    // while-loop parser
    let while_do = just(Token::Keyword(Keyword::While))
        .ignore_then(expr.clone())
        .then_ignore(just(Token::Keyword(Keyword::Do)))
        .then(stat_chain.clone())
        .then_ignore(just(Token::Keyword(Keyword::Done)))
        .map(|(while_cond, body)| Stat::WhileDo {
            while_cond: while_cond.inner,
            body: body.inner,
        });

    // begin-end scope
    let scoped = just(Token::Keyword(Keyword::Begin))
        .ignore_then(stat_chain.clone())
        .then_ignore(just(Token::Keyword(Keyword::End)))
        .map(|st| Stat::Scoped(st.inner));

    // statement parser
    let stat = choice((
        skip,
        variable_definition,
        assignment,
        just(Token::Keyword(Keyword::Read))
            .ignore_then(assign_lhs)
            .map(Stat::Read),
        just(Token::Keyword(Keyword::Free))
            .ignore_then(expr.clone())
            .map(|e| Stat::Free(e.inner)),
        just(Token::Keyword(Keyword::Return))
            .ignore_then(expr.clone())
            .map(|e| Stat::Return(e.inner)),
        just(Token::Keyword(Keyword::Exit))
            .ignore_then(expr.clone())
            .map(|e| Stat::Exit(e.inner)),
        just(Token::Keyword(Keyword::Print))
            .ignore_then(expr.clone())
            .map(|e| Stat::Print(e.inner)),
        just(Token::Keyword(Keyword::Println))
            .ignore_then(expr)
            .map(|e| Stat::Println(e.inner)),
        if_then_else,
        while_do,
        scoped,
    ));

    // TODO: figure out labels and error recovery

    stat.map_with_span(|stat, span| Spanned::new(stat, span))
}

pub fn parser() -> impl Parser<Token, Spanned<Program>, Error = Simple<Token>> + Clone {
    let r#type = type_parser();

    // statement chain parser
    let stat_chain = recursive(
        |stat_chain: Recursive<'_, Token, Spanned<StatChain>, Simple<Token>>| {
            let stat = stat_parser_factory(stat_chain);

            // parse statement chains
            let stat_chain = stat
                .separated_by(just(Token::Symbol(Symbol::Semicolon)))
                .at_least(1)
                .map(|stats| {
                    StatChain::try_new(stats.iter().map(|s| s.inner.clone()).collect()).unwrap()
                });

            // map the parsed result to span
            stat_chain.map_with_span(|st, span| Spanned::new(st, span))
        },
    );

    // identifiers can be parsed directly from `Ident` tokens
    let ident = select! { Token::Ident(s) => Ident(s) }.labelled("<ident>");

    // func param parser
    let func_param = r#type.clone().then(ident).map(|(t, id)| FuncParam {
        param_type: t.inner,
        name: id,
    });

    // function parser
    let function = r#type
        .then(ident)
        .then(
            func_param
                .separated_by(just(Token::Symbol(Symbol::Comma)))
                .delimited_by(
                    just(Token::Symbol(Symbol::OpenParen)),
                    just(Token::Symbol(Symbol::CloseParen)),
                ),
        )
        .then_ignore(just(Token::Keyword(Keyword::Is)))
        .then(stat_chain.clone())
        .then_ignore(just(Token::Keyword(Keyword::End)))
        .map(|(((t, id), params), body)| Func {
            return_type: t.inner,
            name: id,
            params: params.into_boxed_slice(),
            body: body.inner,
        });

    // program parser
    let program = just(Token::Keyword(Keyword::Begin))
        .ignore_then(function.repeated())
        .then(stat_chain)
        .then_ignore(just(Token::Keyword(Keyword::End)))
        .map(|(funcs, body)| Program {
            functions: funcs.into_boxed_slice(),
            body: body.inner,
        });

    // TODO: figure out error recovery

    // map programme to spanned
    program.map_with_span(|p, s| Spanned::new(p, s))
}
