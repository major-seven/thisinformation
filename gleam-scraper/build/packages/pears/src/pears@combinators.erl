-module(pears@combinators).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([map/2, map_error/2, labelled/2, to/2, alt/2, any/0, eof/0, satisfying/1, just/1, pair/2, seq/1, left/2, right/2, many0/1, many1/1, lazy/1, one_of/1, none_of/1, between/3, do_choice/2, choice/1, sep_by0/2, sep_by1/2, maybe/1, unwrap/2, recognize/1]).

-spec consume_token(
    pears@input:input(FNB),
    fun((FNB) -> {ok, pears:parsed(FNB, FND)} | {error, pears:parse_error(FNB)})
) -> {ok, pears:parsed(FNB, FND)} | {error, pears:parse_error(FNB)}.
consume_token(In, F) ->
    case pears@input:get(In) of
        none ->
            {error, {unexpected_end_of_input, In, [<<"any token"/utf8>>]}};

        {some, Value} ->
            F(Value)
    end.

-spec map(
    fun((pears@input:input(FNI)) -> {ok, pears:parsed(FNI, FNJ)} |
        {error, pears:parse_error(FNI)}),
    fun((FNJ) -> FNM)
) -> fun((pears@input:input(FNI)) -> {ok, pears:parsed(FNI, FNM)} |
    {error, pears:parse_error(FNI)}).
map(P, Fun) ->
    fun(Input) -> _pipe = P(Input),
        gleam@result:map(
            _pipe,
            fun(Parsed) ->
                {parsed,
                    erlang:element(2, Parsed),
                    Fun(erlang:element(3, Parsed))}
            end
        ) end.

-spec map_error(
    fun((pears@input:input(FNP)) -> {ok, pears:parsed(FNP, FNQ)} |
        {error, pears:parse_error(FNP)}),
    fun((pears:parse_error(FNP)) -> pears:parse_error(FNP))
) -> fun((pears@input:input(FNP)) -> {ok, pears:parsed(FNP, FNQ)} |
    {error, pears:parse_error(FNP)}).
map_error(P, Fun) ->
    fun(Input) -> _pipe = P(Input),
        gleam@result:map_error(_pipe, Fun) end.

-spec labelled(
    fun((pears@input:input(FNX)) -> {ok, pears:parsed(FNX, FNY)} |
        {error, pears:parse_error(FNX)}),
    binary()
) -> fun((pears@input:input(FNX)) -> {ok, pears:parsed(FNX, FNY)} |
    {error, pears:parse_error(FNX)}).
labelled(Parser, Label) ->
    fun(Input) -> _pipe = Parser(Input),
        gleam@result:map_error(_pipe, fun(Err) -> case Err of
                    {unexpected_end_of_input, _, _} ->
                        Err;

                    {unexpected_token, In, _, Error} ->
                        {unexpected_token, In, [Label], Error}
                end end) end.

-spec to(
    fun((pears@input:input(FOD)) -> {ok, pears:parsed(FOD, any())} |
        {error, pears:parse_error(FOD)}),
    FOH
) -> fun((pears@input:input(FOD)) -> {ok, pears:parsed(FOD, FOH)} |
    {error, pears:parse_error(FOD)}).
to(Parser, Value) ->
    map(Parser, fun(_) -> Value end).

-spec alt(
    fun((pears@input:input(FOK)) -> {ok, pears:parsed(FOK, FOL)} |
        {error, pears:parse_error(FOK)}),
    fun((pears@input:input(FOK)) -> {ok, pears:parsed(FOK, FOL)} |
        {error, pears:parse_error(FOK)})
) -> fun((pears@input:input(FOK)) -> {ok, pears:parsed(FOK, FOL)} |
    {error, pears:parse_error(FOK)}).
alt(Parser_1, Parser_2) ->
    fun(Input) -> case Parser_1(Input) of
            {ok, Result} ->
                {ok, Result};

            {error, _} ->
                Parser_2(Input)
        end end.

-spec any() -> fun((pears@input:input(FOS)) -> {ok, pears:parsed(FOS, FOS)} |
    {error, pears:parse_error(FOS)}).
any() ->
    fun(In) ->
        consume_token(
            In,
            fun(Token) -> pears:ok(pears@input:next(In), Token) end
        )
    end.

-spec eof() -> fun((pears@input:input(FOV)) -> {ok, pears:parsed(FOV, nil)} |
    {error, pears:parse_error(FOV)}).
eof() ->
    fun(In) -> case pears@input:get(In) of
            none ->
                pears:ok(In, nil);

            {some, Token} ->
                {error, {unexpected_token, In, [<<"EOF"/utf8>>], Token}}
        end end.

-spec satisfying(fun((FOY) -> boolean())) -> fun((pears@input:input(FOY)) -> {ok,
        pears:parsed(FOY, FOY)} |
    {error, pears:parse_error(FOY)}).
satisfying(F) ->
    fun(In) -> case pears@input:get(In) of
            none ->
                {error,
                    {unexpected_end_of_input,
                        In,
                        [<<"satifying predicate"/utf8>>]}};

            {some, Value} ->
                case F(Value) of
                    true ->
                        pears:ok(pears@input:next(In), Value);

                    false ->
                        {error,
                            {unexpected_token,
                                In,
                                [<<"satisfying predicate"/utf8>>],
                                Value}}
                end
        end end.

-spec just(FPC) -> fun((pears@input:input(FPC)) -> {ok, pears:parsed(FPC, FPC)} |
    {error, pears:parse_error(FPC)}).
just(Item) ->
    fun(In) -> case pears@input:get(In) of
            none ->
                {error,
                    {unexpected_end_of_input, In, [gleam@string:inspect(Item)]}};

            {some, Head} when Head =:= Item ->
                {ok, {parsed, pears@input:next(In), Item}};

            {some, Head@1} ->
                {error,
                    {unexpected_token, In, [gleam@string:inspect(Item)], Head@1}}
        end end.

-spec pair(
    fun((pears@input:input(FPF)) -> {ok, pears:parsed(FPF, FPG)} |
        {error, pears:parse_error(FPF)}),
    fun((pears@input:input(FPF)) -> {ok, pears:parsed(FPF, FPJ)} |
        {error, pears:parse_error(FPF)})
) -> fun((pears@input:input(FPF)) -> {ok, pears:parsed(FPF, {FPG, FPJ})} |
    {error, pears:parse_error(FPF)}).
pair(P1, P2) ->
    fun(In) ->
        gleam@result:'try'(
            P1(In),
            fun(Parsed_1) ->
                gleam@result:'try'(
                    P2(erlang:element(2, Parsed_1)),
                    fun(Parsed_2) ->
                        pears:ok(
                            erlang:element(2, Parsed_2),
                            {erlang:element(3, Parsed_1),
                                erlang:element(3, Parsed_2)}
                        )
                    end
                )
            end
        )
    end.

-spec do_sequence(
    list(fun((pears@input:input(FPW)) -> {ok, pears:parsed(FPW, FPX)} |
        {error, pears:parse_error(FPW)})),
    pears@input:input(FPW),
    list(FPX)
) -> {ok, pears:parsed(FPW, list(FPX))} | {error, pears:parse_error(FPW)}.
do_sequence(Parsers, Input, Acc) ->
    case Parsers of
        [] ->
            pears:ok(Input, gleam@list:reverse(Acc));

        [Parser | Rest] ->
            gleam@result:'try'(
                Parser(Input),
                fun(Parsed) ->
                    do_sequence(
                        Rest,
                        erlang:element(2, Parsed),
                        [erlang:element(3, Parsed) | Acc]
                    )
                end
            )
    end.

-spec seq(
    list(fun((pears@input:input(FPO)) -> {ok, pears:parsed(FPO, FPP)} |
        {error, pears:parse_error(FPO)}))
) -> fun((pears@input:input(FPO)) -> {ok, pears:parsed(FPO, list(FPP))} |
    {error, pears:parse_error(FPO)}).
seq(Parsers) ->
    fun(Input) -> do_sequence(Parsers, Input, []) end.

-spec left(
    fun((pears@input:input(FQG)) -> {ok, pears:parsed(FQG, FQH)} |
        {error, pears:parse_error(FQG)}),
    fun((pears@input:input(FQG)) -> {ok, pears:parsed(FQG, any())} |
        {error, pears:parse_error(FQG)})
) -> fun((pears@input:input(FQG)) -> {ok, pears:parsed(FQG, FQH)} |
    {error, pears:parse_error(FQG)}).
left(P1, P2) ->
    fun(In) ->
        gleam@result:'try'(
            P1(In),
            fun(Parsed_1) ->
                gleam@result:'try'(
                    P2(erlang:element(2, Parsed_1)),
                    fun(Parsed_2) ->
                        pears:ok(
                            erlang:element(2, Parsed_2),
                            erlang:element(3, Parsed_1)
                        )
                    end
                )
            end
        )
    end.

-spec right(
    fun((pears@input:input(FQP)) -> {ok, pears:parsed(FQP, any())} |
        {error, pears:parse_error(FQP)}),
    fun((pears@input:input(FQP)) -> {ok, pears:parsed(FQP, FQT)} |
        {error, pears:parse_error(FQP)})
) -> fun((pears@input:input(FQP)) -> {ok, pears:parsed(FQP, FQT)} |
    {error, pears:parse_error(FQP)}).
right(P1, P2) ->
    fun(In) ->
        gleam@result:'try'(
            P1(In),
            fun(Parsed_1) ->
                gleam@result:'try'(
                    P2(erlang:element(2, Parsed_1)),
                    fun(Parsed_2) ->
                        pears:ok(
                            erlang:element(2, Parsed_2),
                            erlang:element(3, Parsed_2)
                        )
                    end
                )
            end
        )
    end.

-spec many0(
    fun((pears@input:input(FQY)) -> {ok, pears:parsed(FQY, FQZ)} |
        {error, pears:parse_error(FQY)})
) -> fun((pears@input:input(FQY)) -> {ok, pears:parsed(FQY, list(FQZ))} |
    {error, pears:parse_error(FQY)}).
many0(Parser) ->
    fun(In) -> case Parser(In) of
            {ok, Parsed} ->
                gleam@result:'try'(
                    (many0(Parser))(erlang:element(2, Parsed)),
                    fun(Next) ->
                        pears:ok(
                            erlang:element(2, Next),
                            [erlang:element(3, Parsed) |
                                erlang:element(3, Next)]
                        )
                    end
                );

            {error, _} ->
                pears:ok(In, [])
        end end.

-spec many1(
    fun((pears@input:input(FRF)) -> {ok, pears:parsed(FRF, FRG)} |
        {error, pears:parse_error(FRF)})
) -> fun((pears@input:input(FRF)) -> {ok, pears:parsed(FRF, list(FRG))} |
    {error, pears:parse_error(FRF)}).
many1(Parser) ->
    fun(In) ->
        gleam@result:'try'(
            Parser(In),
            fun(Parsed) ->
                gleam@result:'try'(
                    (many0(Parser))(erlang:element(2, Parsed)),
                    fun(Rest) ->
                        pears:ok(
                            erlang:element(2, Rest),
                            [erlang:element(3, Parsed) |
                                erlang:element(3, Rest)]
                        )
                    end
                )
            end
        )
    end.

-spec lazy(
    fun(() -> fun((pears@input:input(FRM)) -> {ok, pears:parsed(FRM, FRN)} |
        {error, pears:parse_error(FRM)}))
) -> fun((pears@input:input(FRM)) -> {ok, pears:parsed(FRM, FRN)} |
    {error, pears:parse_error(FRM)}).
lazy(F) ->
    fun(Input) -> (F())(Input) end.

-spec one_of(list(FRS)) -> fun((pears@input:input(FRS)) -> {ok,
        pears:parsed(FRS, FRS)} |
    {error, pears:parse_error(FRS)}).
one_of(Items) ->
    _pipe = satisfying(fun(C) -> gleam@list:contains(Items, C) end),
    map_error(
        _pipe,
        fun(Err) ->
            Expected = gleam@list:map(Items, fun gleam@string:inspect/1),
            case Err of
                {unexpected_token, In, _, Token} ->
                    {unexpected_token, In, Expected, Token};

                {unexpected_end_of_input, In@1, _} ->
                    {unexpected_end_of_input, In@1, Expected}
            end
        end
    ).

-spec none_of(list(FRW)) -> fun((pears@input:input(FRW)) -> {ok,
        pears:parsed(FRW, FRW)} |
    {error, pears:parse_error(FRW)}).
none_of(Items) ->
    satisfying(fun(C) -> not gleam@list:contains(Items, C) end).

-spec between(
    fun((pears@input:input(FSA)) -> {ok, pears:parsed(FSA, FSB)} |
        {error, pears:parse_error(FSA)}),
    fun((pears@input:input(FSA)) -> {ok, pears:parsed(FSA, any())} |
        {error, pears:parse_error(FSA)}),
    fun((pears@input:input(FSA)) -> {ok, pears:parsed(FSA, any())} |
        {error, pears:parse_error(FSA)})
) -> fun((pears@input:input(FSA)) -> {ok, pears:parsed(FSA, FSB)} |
    {error, pears:parse_error(FSA)}).
between(Parser, Open, Close) ->
    _pipe = Open,
    _pipe@1 = right(_pipe, Parser),
    left(_pipe@1, Close).

-spec do_choice(
    list(fun((pears@input:input(FST)) -> {ok, pears:parsed(FST, FSU)} |
        {error, pears:parse_error(FST)})),
    list(binary())
) -> fun((pears@input:input(FST)) -> {ok, pears:parsed(FST, FSU)} |
    {error, pears:parse_error(FST)}).
do_choice(Parsers, Expected) ->
    fun(In) -> case Parsers of
            [] ->
                case pears@input:get(In) of
                    none ->
                        {error, {unexpected_end_of_input, In, Expected}};

                    {some, Token} ->
                        {error, {unexpected_token, In, Expected, Token}}
                end;

            [Parser | Rest] ->
                case Parser(In) of
                    {ok, Parsed} ->
                        {ok, Parsed};

                    {error, Err} ->
                        New_expected = case Err of
                            {unexpected_token, _, Expected@1, _} ->
                                Expected@1;

                            {unexpected_end_of_input, _, Expected@2} ->
                                Expected@2
                        end,
                        (do_choice(
                            Rest,
                            gleam@list:concat([Expected, New_expected])
                        ))(In)
                end
        end end.

-spec choice(
    list(fun((pears@input:input(FSM)) -> {ok, pears:parsed(FSM, FSN)} |
        {error, pears:parse_error(FSM)}))
) -> fun((pears@input:input(FSM)) -> {ok, pears:parsed(FSM, FSN)} |
    {error, pears:parse_error(FSM)}).
choice(Parsers) ->
    do_choice(Parsers, []).

-spec sep_by0(
    fun((pears@input:input(FTB)) -> {ok, pears:parsed(FTB, FTC)} |
        {error, pears:parse_error(FTB)}),
    fun((pears@input:input(FTB)) -> {ok, pears:parsed(FTB, any())} |
        {error, pears:parse_error(FTB)})
) -> fun((pears@input:input(FTB)) -> {ok, pears:parsed(FTB, list(FTC))} |
    {error, pears:parse_error(FTB)}).
sep_by0(Parser, Separator) ->
    fun(In) -> case Parser(In) of
            {ok, Parsed} ->
                gleam@result:'try'(
                    (many0(right(Separator, Parser)))(erlang:element(2, Parsed)),
                    fun(Rest) ->
                        pears:ok(
                            erlang:element(2, Rest),
                            [erlang:element(3, Parsed) |
                                erlang:element(3, Rest)]
                        )
                    end
                );

            {error, _} ->
                pears:ok(In, [])
        end end.

-spec sep_by1(
    fun((pears@input:input(FTL)) -> {ok, pears:parsed(FTL, FTM)} |
        {error, pears:parse_error(FTL)}),
    fun((pears@input:input(FTL)) -> {ok, pears:parsed(FTL, any())} |
        {error, pears:parse_error(FTL)})
) -> fun((pears@input:input(FTL)) -> {ok, pears:parsed(FTL, list(FTM))} |
    {error, pears:parse_error(FTL)}).
sep_by1(Parser, Separator) ->
    fun(Input) ->
        gleam@result:'try'(
            Parser(Input),
            fun(Parsed) ->
                gleam@result:'try'(
                    (many0(
                        begin
                            _pipe = Separator,
                            right(_pipe, Parser)
                        end
                    ))(erlang:element(2, Parsed)),
                    fun(Rest) ->
                        pears:ok(
                            erlang:element(2, Rest),
                            [erlang:element(3, Parsed) |
                                erlang:element(3, Rest)]
                        )
                    end
                )
            end
        )
    end.

-spec maybe(
    fun((pears@input:input(FTV)) -> {ok, pears:parsed(FTV, FTW)} |
        {error, pears:parse_error(FTV)})
) -> fun((pears@input:input(FTV)) -> {ok,
        pears:parsed(FTV, gleam@option:option(FTW))} |
    {error, pears:parse_error(FTV)}).
maybe(Parser) ->
    fun(In) -> case Parser(In) of
            {ok, Parsed} ->
                pears:ok(
                    erlang:element(2, Parsed),
                    {some, erlang:element(3, Parsed)}
                );

            {error, _} ->
                pears:ok(In, none)
        end end.

-spec unwrap(
    fun((pears@input:input(FUC)) -> {ok,
            pears:parsed(FUC, gleam@option:option(FUD))} |
        {error, pears:parse_error(FUC)}),
    FUD
) -> fun((pears@input:input(FUC)) -> {ok, pears:parsed(FUC, FUD)} |
    {error, pears:parse_error(FUC)}).
unwrap(Parser, Default) ->
    _pipe = Parser,
    map(_pipe, fun(Maybe_value) -> case Maybe_value of
                none ->
                    Default;

                {some, Value} ->
                    Value
            end end).

-spec recognize(
    fun((pears@input:input(FUJ)) -> {ok, pears:parsed(FUJ, any())} |
        {error, pears:parse_error(FUJ)})
) -> fun((pears@input:input(FUJ)) -> {ok, pears:parsed(FUJ, list(FUJ))} |
    {error, pears:parse_error(FUJ)}).
recognize(Parser) ->
    fun(In) ->
        gleam@result:'try'(
            Parser(In),
            fun(Parsed) ->
                Start = erlang:element(3, In),
                Parsed_length = erlang:element(3, erlang:element(2, Parsed)) - Start,
                Consumed = begin
                    _pipe = erlang:element(2, In),
                    _pipe@1 = gleam@list:drop(_pipe, Start),
                    gleam@list:take(_pipe@1, Parsed_length)
                end,
                pears:ok(erlang:element(2, Parsed), Consumed)
            end
        )
    end.
