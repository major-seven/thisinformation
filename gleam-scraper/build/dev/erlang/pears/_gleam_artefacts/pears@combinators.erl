-module(pears@combinators).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([map/2, map_error/2, labelled/2, to/2, alt/2, any/0, eof/0, satisfying/1, just/1, pair/2, seq/1, left/2, right/2, many0/1, many1/1, lazy/1, one_of/1, none_of/1, between/3, do_choice/2, choice/1, sep_by0/2, sep_by1/2, maybe/1, unwrap/2, recognize/1]).

-spec consume_token(
    pears@input:input(ADY),
    fun((ADY) -> {ok, pears:parsed(ADY, AEA)} | {error, pears:parse_error(ADY)})
) -> {ok, pears:parsed(ADY, AEA)} | {error, pears:parse_error(ADY)}.
consume_token(In, F) ->
    case pears@input:get(In) of
        none ->
            {error, {unexpected_end_of_input, In, [<<"any token"/utf8>>]}};

        {some, Value} ->
            F(Value)
    end.

-spec map(
    fun((pears@input:input(AEF)) -> {ok, pears:parsed(AEF, AEG)} |
        {error, pears:parse_error(AEF)}),
    fun((AEG) -> AEJ)
) -> fun((pears@input:input(AEF)) -> {ok, pears:parsed(AEF, AEJ)} |
    {error, pears:parse_error(AEF)}).
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
    fun((pears@input:input(AEM)) -> {ok, pears:parsed(AEM, AEN)} |
        {error, pears:parse_error(AEM)}),
    fun((pears:parse_error(AEM)) -> pears:parse_error(AEM))
) -> fun((pears@input:input(AEM)) -> {ok, pears:parsed(AEM, AEN)} |
    {error, pears:parse_error(AEM)}).
map_error(P, Fun) ->
    fun(Input) -> _pipe = P(Input),
        gleam@result:map_error(_pipe, Fun) end.

-spec labelled(
    fun((pears@input:input(AEU)) -> {ok, pears:parsed(AEU, AEV)} |
        {error, pears:parse_error(AEU)}),
    binary()
) -> fun((pears@input:input(AEU)) -> {ok, pears:parsed(AEU, AEV)} |
    {error, pears:parse_error(AEU)}).
labelled(Parser, Label) ->
    fun(Input) -> _pipe = Parser(Input),
        gleam@result:map_error(_pipe, fun(Err) -> case Err of
                    {unexpected_end_of_input, _, _} ->
                        Err;

                    {unexpected_token, In, _, Error} ->
                        {unexpected_token, In, [Label], Error}
                end end) end.

-spec to(
    fun((pears@input:input(AFA)) -> {ok, pears:parsed(AFA, any())} |
        {error, pears:parse_error(AFA)}),
    AFE
) -> fun((pears@input:input(AFA)) -> {ok, pears:parsed(AFA, AFE)} |
    {error, pears:parse_error(AFA)}).
to(Parser, Value) ->
    map(Parser, fun(_) -> Value end).

-spec alt(
    fun((pears@input:input(AFH)) -> {ok, pears:parsed(AFH, AFI)} |
        {error, pears:parse_error(AFH)}),
    fun((pears@input:input(AFH)) -> {ok, pears:parsed(AFH, AFI)} |
        {error, pears:parse_error(AFH)})
) -> fun((pears@input:input(AFH)) -> {ok, pears:parsed(AFH, AFI)} |
    {error, pears:parse_error(AFH)}).
alt(Parser_1, Parser_2) ->
    fun(Input) -> case Parser_1(Input) of
            {ok, Result} ->
                {ok, Result};

            {error, _} ->
                Parser_2(Input)
        end end.

-spec any() -> fun((pears@input:input(AFP)) -> {ok, pears:parsed(AFP, AFP)} |
    {error, pears:parse_error(AFP)}).
any() ->
    fun(In) ->
        consume_token(
            In,
            fun(Token) -> pears:ok(pears@input:next(In), Token) end
        )
    end.

-spec eof() -> fun((pears@input:input(AFS)) -> {ok, pears:parsed(AFS, nil)} |
    {error, pears:parse_error(AFS)}).
eof() ->
    fun(In) -> case pears@input:get(In) of
            none ->
                pears:ok(In, nil);

            {some, Token} ->
                {error, {unexpected_token, In, [<<"EOF"/utf8>>], Token}}
        end end.

-spec satisfying(fun((AFV) -> boolean())) -> fun((pears@input:input(AFV)) -> {ok,
        pears:parsed(AFV, AFV)} |
    {error, pears:parse_error(AFV)}).
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

-spec just(AFZ) -> fun((pears@input:input(AFZ)) -> {ok, pears:parsed(AFZ, AFZ)} |
    {error, pears:parse_error(AFZ)}).
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
    fun((pears@input:input(AGC)) -> {ok, pears:parsed(AGC, AGD)} |
        {error, pears:parse_error(AGC)}),
    fun((pears@input:input(AGC)) -> {ok, pears:parsed(AGC, AGG)} |
        {error, pears:parse_error(AGC)})
) -> fun((pears@input:input(AGC)) -> {ok, pears:parsed(AGC, {AGD, AGG})} |
    {error, pears:parse_error(AGC)}).
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
    list(fun((pears@input:input(AGT)) -> {ok, pears:parsed(AGT, AGU)} |
        {error, pears:parse_error(AGT)})),
    pears@input:input(AGT),
    list(AGU)
) -> {ok, pears:parsed(AGT, list(AGU))} | {error, pears:parse_error(AGT)}.
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
    list(fun((pears@input:input(AGL)) -> {ok, pears:parsed(AGL, AGM)} |
        {error, pears:parse_error(AGL)}))
) -> fun((pears@input:input(AGL)) -> {ok, pears:parsed(AGL, list(AGM))} |
    {error, pears:parse_error(AGL)}).
seq(Parsers) ->
    fun(Input) -> do_sequence(Parsers, Input, []) end.

-spec left(
    fun((pears@input:input(AHD)) -> {ok, pears:parsed(AHD, AHE)} |
        {error, pears:parse_error(AHD)}),
    fun((pears@input:input(AHD)) -> {ok, pears:parsed(AHD, any())} |
        {error, pears:parse_error(AHD)})
) -> fun((pears@input:input(AHD)) -> {ok, pears:parsed(AHD, AHE)} |
    {error, pears:parse_error(AHD)}).
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
    fun((pears@input:input(AHM)) -> {ok, pears:parsed(AHM, any())} |
        {error, pears:parse_error(AHM)}),
    fun((pears@input:input(AHM)) -> {ok, pears:parsed(AHM, AHQ)} |
        {error, pears:parse_error(AHM)})
) -> fun((pears@input:input(AHM)) -> {ok, pears:parsed(AHM, AHQ)} |
    {error, pears:parse_error(AHM)}).
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
    fun((pears@input:input(AHV)) -> {ok, pears:parsed(AHV, AHW)} |
        {error, pears:parse_error(AHV)})
) -> fun((pears@input:input(AHV)) -> {ok, pears:parsed(AHV, list(AHW))} |
    {error, pears:parse_error(AHV)}).
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
    fun((pears@input:input(AIC)) -> {ok, pears:parsed(AIC, AID)} |
        {error, pears:parse_error(AIC)})
) -> fun((pears@input:input(AIC)) -> {ok, pears:parsed(AIC, list(AID))} |
    {error, pears:parse_error(AIC)}).
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
    fun(() -> fun((pears@input:input(AIJ)) -> {ok, pears:parsed(AIJ, AIK)} |
        {error, pears:parse_error(AIJ)}))
) -> fun((pears@input:input(AIJ)) -> {ok, pears:parsed(AIJ, AIK)} |
    {error, pears:parse_error(AIJ)}).
lazy(F) ->
    fun(Input) -> (F())(Input) end.

-spec one_of(list(AIP)) -> fun((pears@input:input(AIP)) -> {ok,
        pears:parsed(AIP, AIP)} |
    {error, pears:parse_error(AIP)}).
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

-spec none_of(list(AIT)) -> fun((pears@input:input(AIT)) -> {ok,
        pears:parsed(AIT, AIT)} |
    {error, pears:parse_error(AIT)}).
none_of(Items) ->
    satisfying(fun(C) -> not gleam@list:contains(Items, C) end).

-spec between(
    fun((pears@input:input(AIX)) -> {ok, pears:parsed(AIX, AIY)} |
        {error, pears:parse_error(AIX)}),
    fun((pears@input:input(AIX)) -> {ok, pears:parsed(AIX, any())} |
        {error, pears:parse_error(AIX)}),
    fun((pears@input:input(AIX)) -> {ok, pears:parsed(AIX, any())} |
        {error, pears:parse_error(AIX)})
) -> fun((pears@input:input(AIX)) -> {ok, pears:parsed(AIX, AIY)} |
    {error, pears:parse_error(AIX)}).
between(Parser, Open, Close) ->
    _pipe = Open,
    _pipe@1 = right(_pipe, Parser),
    left(_pipe@1, Close).

-spec do_choice(
    list(fun((pears@input:input(AJQ)) -> {ok, pears:parsed(AJQ, AJR)} |
        {error, pears:parse_error(AJQ)})),
    list(binary())
) -> fun((pears@input:input(AJQ)) -> {ok, pears:parsed(AJQ, AJR)} |
    {error, pears:parse_error(AJQ)}).
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
    list(fun((pears@input:input(AJJ)) -> {ok, pears:parsed(AJJ, AJK)} |
        {error, pears:parse_error(AJJ)}))
) -> fun((pears@input:input(AJJ)) -> {ok, pears:parsed(AJJ, AJK)} |
    {error, pears:parse_error(AJJ)}).
choice(Parsers) ->
    do_choice(Parsers, []).

-spec sep_by0(
    fun((pears@input:input(AJY)) -> {ok, pears:parsed(AJY, AJZ)} |
        {error, pears:parse_error(AJY)}),
    fun((pears@input:input(AJY)) -> {ok, pears:parsed(AJY, any())} |
        {error, pears:parse_error(AJY)})
) -> fun((pears@input:input(AJY)) -> {ok, pears:parsed(AJY, list(AJZ))} |
    {error, pears:parse_error(AJY)}).
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
    fun((pears@input:input(AKI)) -> {ok, pears:parsed(AKI, AKJ)} |
        {error, pears:parse_error(AKI)}),
    fun((pears@input:input(AKI)) -> {ok, pears:parsed(AKI, any())} |
        {error, pears:parse_error(AKI)})
) -> fun((pears@input:input(AKI)) -> {ok, pears:parsed(AKI, list(AKJ))} |
    {error, pears:parse_error(AKI)}).
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
    fun((pears@input:input(AKS)) -> {ok, pears:parsed(AKS, AKT)} |
        {error, pears:parse_error(AKS)})
) -> fun((pears@input:input(AKS)) -> {ok,
        pears:parsed(AKS, gleam@option:option(AKT))} |
    {error, pears:parse_error(AKS)}).
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
    fun((pears@input:input(AKZ)) -> {ok,
            pears:parsed(AKZ, gleam@option:option(ALA))} |
        {error, pears:parse_error(AKZ)}),
    ALA
) -> fun((pears@input:input(AKZ)) -> {ok, pears:parsed(AKZ, ALA)} |
    {error, pears:parse_error(AKZ)}).
unwrap(Parser, Default) ->
    _pipe = Parser,
    map(_pipe, fun(Maybe_value) -> case Maybe_value of
                none ->
                    Default;

                {some, Value} ->
                    Value
            end end).

-spec recognize(
    fun((pears@input:input(ALG)) -> {ok, pears:parsed(ALG, any())} |
        {error, pears:parse_error(ALG)})
) -> fun((pears@input:input(ALG)) -> {ok, pears:parsed(ALG, list(ALG))} |
    {error, pears:parse_error(ALG)}).
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
