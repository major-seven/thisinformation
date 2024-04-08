-module(jasper).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([parse_json/1, stringify_json_spaced/2, stringify_json/1, stringify_jsonl/1, parse_jsonl/1, parse_jsonl_all/1, parse_jsonl_valid/1, query_json/2]).
-export_type([parse_error/0, json_value/0, indentation/0, json_query/0, inv_json_query/0, json_query_error/0]).

-type parse_error() :: {unexpected_token, binary()} | unexpected_end_of_input.

-type json_value() :: {object, gleam@dict:dict(binary(), json_value())} |
    {array, list(json_value())} |
    {string, binary()} |
    {number, float()} |
    {boolean, boolean()} |
    null.

-type indentation() :: {spaces, integer()} | tab | {tabs, integer()}.

-type json_query() :: root |
    {key, json_query(), binary()} |
    {key_or, json_query(), binary(), json_value()} |
    {index, json_query(), integer()} |
    {index_or, json_query(), integer(), json_value()} |
    {filter, json_query(), fun((json_value()) -> boolean())} |
    {map, json_query(), fun((json_value()) -> json_value())} |
    {map_keys, json_query(), fun((binary()) -> binary())} |
    {map_values, json_query(), fun((binary(), json_value()) -> json_value())} |
    {filter_map,
        json_query(),
        fun((json_value()) -> {ok, json_value()} | {error, nil})} |
    {for_each, json_query()} |
    {for_each_ok, json_query()}.

-type inv_json_query() :: inv_end |
    {inv_key, binary(), inv_json_query()} |
    {inv_key_or, binary(), json_value(), inv_json_query()} |
    {inv_index, integer(), inv_json_query()} |
    {inv_index_or, integer(), json_value(), inv_json_query()} |
    {inv_filter, fun((json_value()) -> boolean()), inv_json_query()} |
    {inv_map, fun((json_value()) -> json_value()), inv_json_query()} |
    {inv_map_keys, fun((binary()) -> binary()), inv_json_query()} |
    {inv_map_values,
        fun((binary(), json_value()) -> json_value()),
        inv_json_query()} |
    {inv_filter_map,
        fun((json_value()) -> {ok, json_value()} | {error, nil}),
        inv_json_query()} |
    {inv_for_each, inv_json_query()} |
    {inv_for_each_ok, inv_json_query()}.

-type json_query_error() :: {unexpected_type, json_value()} |
    {missing_object_key, json_value(), binary()} |
    {index_out_of_bounds, json_value(), integer()}.

-spec run_parser(
    binary(),
    fun((pears@input:input(binary())) -> {ok, pears:parsed(binary(), GLC)} |
        {error, pears:parse_error(binary())})
) -> {ok, GLC} | {error, parse_error()}.
run_parser(Input, Parser) ->
    case Parser(pears@chars:input(Input)) of
        {ok, {parsed, _, J}} ->
            {ok, J};

        {error, E} ->
            {error, case E of
                    {unexpected_token, _, _, F} ->
                        {unexpected_token, F};

                    {unexpected_end_of_input, _, _} ->
                        unexpected_end_of_input
                end}
    end.

-spec ws0() -> fun((pears@input:input(binary())) -> {ok,
        pears:parsed(binary(), list(binary()))} |
    {error, pears:parse_error(binary())}).
ws0() ->
    _pipe = pears@combinators:one_of(
        [<<" "/utf8>>, <<"\n"/utf8>>, <<"\r"/utf8>>, <<"\t"/utf8>>]
    ),
    pears@combinators:many0(_pipe).

-spec padded(
    fun((pears@input:input(binary())) -> {ok, pears:parsed(binary(), GLL)} |
        {error, pears:parse_error(binary())})
) -> fun((pears@input:input(binary())) -> {ok, pears:parsed(binary(), GLL)} |
    {error, pears:parse_error(binary())}).
padded(P) ->
    pears@combinators:left(P, ws0()).

-spec symbol(binary()) -> fun((pears@input:input(binary())) -> {ok,
        pears:parsed(binary(), binary())} |
    {error, pears:parse_error(binary())}).
symbol(S) ->
    padded(pears@chars:string(S)).

-spec value_parser() -> fun((pears@input:input(binary())) -> {ok,
        pears:parsed(binary(), json_value())} |
    {error, pears:parse_error(binary())}).
value_parser() ->
    Hex_digit = pears@combinators:one_of(
        [<<"0"/utf8>>,
            <<"1"/utf8>>,
            <<"2"/utf8>>,
            <<"3"/utf8>>,
            <<"4"/utf8>>,
            <<"5"/utf8>>,
            <<"6"/utf8>>,
            <<"7"/utf8>>,
            <<"8"/utf8>>,
            <<"9"/utf8>>,
            <<"a"/utf8>>,
            <<"b"/utf8>>,
            <<"c"/utf8>>,
            <<"d"/utf8>>,
            <<"e"/utf8>>,
            <<"f"/utf8>>,
            <<"A"/utf8>>,
            <<"B"/utf8>>,
            <<"C"/utf8>>,
            <<"D"/utf8>>,
            <<"E"/utf8>>,
            <<"F"/utf8>>]
    ),
    Unicode_escape_digits = pears@combinators:recognize(
        pears@combinators:seq([Hex_digit, Hex_digit, Hex_digit, Hex_digit])
    ),
    Escape = begin
        _pipe = pears@combinators:just(<<"\\"/utf8>>),
        pears@combinators:right(
            _pipe,
            pears@combinators:choice(
                [pears@combinators:just(<<"\\"/utf8>>),
                    pears@combinators:just(<<"/"/utf8>>),
                    pears@combinators:just(<<"\""/utf8>>),
                    pears@combinators:to(
                        pears@combinators:just(<<"b"/utf8>>),
                        <<"\x{0008}"/utf8>>
                    ),
                    pears@combinators:to(
                        pears@combinators:just(<<"f"/utf8>>),
                        <<"\x{000C}"/utf8>>
                    ),
                    pears@combinators:to(
                        pears@combinators:just(<<"n"/utf8>>),
                        <<"\n"/utf8>>
                    ),
                    pears@combinators:to(
                        pears@combinators:just(<<"r"/utf8>>),
                        <<"\r"/utf8>>
                    ),
                    pears@combinators:to(
                        pears@combinators:just(<<"t"/utf8>>),
                        <<"\t"/utf8>>
                    ),
                    pears@combinators:map(
                        pears@combinators:right(
                            pears@combinators:just(<<"u"/utf8>>),
                            Unicode_escape_digits
                        ),
                        fun(Value) ->
                            _assert_subject = gleam@int:base_parse(
                                gleam@string:concat(Value),
                                16
                            ),
                            {ok, Number} = case _assert_subject of
                                {ok, _} -> _assert_subject;
                                _assert_fail ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Assertion pattern match failed"/utf8>>,
                                                value => _assert_fail,
                                                module => <<"jasper"/utf8>>,
                                                function => <<"value_parser"/utf8>>,
                                                line => 85})
                            end,
                            _assert_subject@1 = gleam@string:utf_codepoint(
                                Number
                            ),
                            {ok, Codepoint} = case _assert_subject@1 of
                                {ok, _} -> _assert_subject@1;
                                _assert_fail@1 ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Assertion pattern match failed"/utf8>>,
                                                value => _assert_fail@1,
                                                module => <<"jasper"/utf8>>,
                                                function => <<"value_parser"/utf8>>,
                                                line => 86})
                            end,
                            gleam_stdlib:utf_codepoint_list_to_string(
                                [Codepoint]
                            )
                        end
                    )]
            )
        )
    end,
    Str = begin
        _pipe@1 = pears@combinators:none_of([<<"\""/utf8>>, <<"\\"/utf8>>]),
        _pipe@2 = pears@combinators:alt(_pipe@1, Escape),
        _pipe@3 = pears@combinators:many0(_pipe@2),
        _pipe@4 = pears@combinators:map(_pipe@3, fun gleam@string:concat/1),
        pears@combinators:between(
            _pipe@4,
            pears@combinators:just(<<"\""/utf8>>),
            pears@combinators:just(<<"\""/utf8>>)
        )
    end,
    Value@1 = pears@combinators:lazy(fun value_parser/0),
    Num = begin
        _pipe@5 = pears@combinators:maybe(pears@combinators:just(<<"-"/utf8>>)),
        _pipe@7 = pears@combinators:pair(
            _pipe@5,
            begin
                _pipe@6 = pears@combinators:alt(
                    pears@combinators:to(
                        pears@combinators:just(<<"0"/utf8>>),
                        [<<"0"/utf8>>]
                    ),
                    pears@combinators:recognize(
                        pears@combinators:pair(
                            pears@combinators:one_of(
                                [<<"1"/utf8>>,
                                    <<"2"/utf8>>,
                                    <<"3"/utf8>>,
                                    <<"4"/utf8>>,
                                    <<"5"/utf8>>,
                                    <<"6"/utf8>>,
                                    <<"7"/utf8>>,
                                    <<"8"/utf8>>,
                                    <<"9"/utf8>>]
                            ),
                            pears@combinators:many0(pears@chars:digit())
                        )
                    )
                ),
                pears@combinators:map(_pipe@6, fun gleam@string:concat/1)
            end
        ),
        _pipe@10 = pears@combinators:pair(
            _pipe@7,
            pears@combinators:maybe(
                begin
                    _pipe@8 = pears@combinators:just(<<"."/utf8>>),
                    _pipe@9 = pears@combinators:right(
                        _pipe@8,
                        pears@combinators:many1(pears@chars:digit())
                    ),
                    pears@combinators:map(_pipe@9, fun gleam@string:concat/1)
                end
            )
        ),
        _pipe@14 = pears@combinators:pair(
            _pipe@10,
            begin
                _pipe@13 = pears@combinators:recognize(
                    pears@combinators:maybe(
                        begin
                            _pipe@11 = pears@combinators:alt(
                                pears@combinators:just(<<"e"/utf8>>),
                                pears@combinators:just(<<"E"/utf8>>)
                            ),
                            _pipe@12 = pears@combinators:pair(
                                _pipe@11,
                                pears@combinators:maybe(
                                    pears@combinators:one_of(
                                        [<<"+"/utf8>>, <<"-"/utf8>>]
                                    )
                                )
                            ),
                            pears@combinators:pair(
                                _pipe@12,
                                pears@combinators:many1(pears@chars:digit())
                            )
                        end
                    )
                ),
                pears@combinators:map(_pipe@13, fun gleam@string:concat/1)
            end
        ),
        pears@combinators:map(_pipe@14, fun(P) -> case P of
                    {{{Neg, Ns}, Ds}, Ex} ->
                        _pipe@15 = (<<<<<<<<(gleam@option:unwrap(
                                            Neg,
                                            <<""/utf8>>
                                        ))/binary,
                                        Ns/binary>>/binary,
                                    "."/utf8>>/binary,
                                (gleam@option:unwrap(Ds, <<"0"/utf8>>))/binary>>/binary,
                            Ex/binary>>),
                        _pipe@16 = gleam@float:parse(_pipe@15),
                        _pipe@17 = gleam@result:unwrap(_pipe@16, case Neg of
                                {some, _} ->
                                    -1.7976931348623158e308;

                                none ->
                                    1.7976931348623158e308
                            end),
                        {number, _pipe@17}
                end end)
    end,
    Bool = pears@combinators:alt(
        pears@combinators:to(
            pears@chars:string(<<"true"/utf8>>),
            {boolean, true}
        ),
        pears@combinators:to(
            pears@chars:string(<<"false"/utf8>>),
            {boolean, false}
        )
    ),
    Null = pears@combinators:to(pears@chars:string(<<"null"/utf8>>), null),
    Array = begin
        _pipe@18 = pears@combinators:sep_by0(Value@1, symbol(<<","/utf8>>)),
        _pipe@19 = pears@combinators:between(
            _pipe@18,
            symbol(<<"["/utf8>>),
            symbol(<<"]"/utf8>>)
        ),
        pears@combinators:map(_pipe@19, fun(Field@0) -> {array, Field@0} end)
    end,
    Obj = begin
        _pipe@20 = Str,
        _pipe@21 = pears@combinators:left(_pipe@20, symbol(<<":"/utf8>>)),
        _pipe@22 = pears@combinators:pair(_pipe@21, Value@1),
        _pipe@23 = pears@combinators:sep_by0(_pipe@22, symbol(<<","/utf8>>)),
        _pipe@24 = pears@combinators:map(_pipe@23, fun maps:from_list/1),
        _pipe@25 = pears@combinators:between(
            _pipe@24,
            symbol(<<"{"/utf8>>),
            symbol(<<"}"/utf8>>)
        ),
        pears@combinators:map(_pipe@25, fun(Field@0) -> {object, Field@0} end)
    end,
    _pipe@26 = pears@combinators:choice(
        [Num,
            Bool,
            Null,
            pears@combinators:map(Str, fun(Field@0) -> {string, Field@0} end),
            Array,
            Obj]
    ),
    padded(_pipe@26).

-spec json_parser() -> fun((pears@input:input(binary())) -> {ok,
        pears:parsed(binary(), json_value())} |
    {error, pears:parse_error(binary())}).
json_parser() ->
    _pipe = value_parser(),
    pears@combinators:between(_pipe, ws0(), pears@combinators:eof()).

-spec parse_json(binary()) -> {ok, json_value()} | {error, parse_error()}.
parse_json(Value) ->
    run_parser(Value, json_parser()).

-spec stringify_json_spaced_rec(json_value(), binary(), integer()) -> binary().
stringify_json_spaced_rec(Value, Space, Depth) ->
    case Value of
        {object, Obj} ->
            case maps:to_list(Obj) of
                [] ->
                    <<"{}"/utf8>>;

                Ls ->
                    <<<<<<<<<<"{\n"/utf8,
                                        (gleam@string:repeat(Space, Depth + 1))/binary>>/binary,
                                    (begin
                                        _pipe = gleam@list:map(
                                            Ls,
                                            fun(Kv) ->
                                                <<<<<<"\""/utf8,
                                                            (erlang:element(
                                                                1,
                                                                Kv
                                                            ))/binary>>/binary,
                                                        "\": "/utf8>>/binary,
                                                    (stringify_json_spaced_rec(
                                                        erlang:element(2, Kv),
                                                        Space,
                                                        Depth + 1
                                                    ))/binary>>
                                            end
                                        ),
                                        gleam@string:join(
                                            _pipe,
                                            <<",\n"/utf8,
                                                (gleam@string:repeat(
                                                    Space,
                                                    Depth + 1
                                                ))/binary>>
                                        )
                                    end)/binary>>/binary,
                                "\n"/utf8>>/binary,
                            (gleam@string:repeat(Space, Depth))/binary>>/binary,
                        "}"/utf8>>
            end;

        {array, []} ->
            <<"[]"/utf8>>;

        {array, Arr} ->
            <<<<<<<<<<"[\n"/utf8,
                                (gleam@string:repeat(Space, Depth + 1))/binary>>/binary,
                            (begin
                                _pipe@1 = Arr,
                                _pipe@2 = gleam@list:map(
                                    _pipe@1,
                                    fun(_capture) ->
                                        stringify_json_spaced_rec(
                                            _capture,
                                            Space,
                                            Depth + 1
                                        )
                                    end
                                ),
                                gleam@string:join(
                                    _pipe@2,
                                    <<",\n"/utf8,
                                        (gleam@string:repeat(Space, Depth + 1))/binary>>
                                )
                            end)/binary>>/binary,
                        "\n"/utf8>>/binary,
                    (gleam@string:repeat(Space, Depth))/binary>>/binary,
                "]"/utf8>>;

        {string, Str} ->
            <<<<"\""/utf8, Str/binary>>/binary, "\""/utf8>>;

        {number, Flt} ->
            gleam@float:to_string(Flt);

        {boolean, true} ->
            <<"true"/utf8>>;

        {boolean, false} ->
            <<"false"/utf8>>;

        null ->
            <<"null"/utf8>>
    end.

-spec stringify_json_spaced(json_value(), indentation()) -> binary().
stringify_json_spaced(Value, Indentation) ->
    stringify_json_spaced_rec(Value, case Indentation of
            {spaces, N} ->
                gleam@string:repeat(<<" "/utf8>>, N);

            tab ->
                <<"\t"/utf8>>;

            {tabs, N@1} ->
                gleam@string:repeat(<<"\t"/utf8>>, N@1)
        end, 0).

-spec stringify_json(json_value()) -> binary().
stringify_json(Value) ->
    case Value of
        {object, Obj} ->
            <<<<"{"/utf8,
                    (begin
                        _pipe = Obj,
                        _pipe@1 = maps:to_list(_pipe),
                        _pipe@2 = gleam@list:map(
                            _pipe@1,
                            fun(Kv) ->
                                <<<<<<"\""/utf8,
                                            (erlang:element(1, Kv))/binary>>/binary,
                                        "\":"/utf8>>/binary,
                                    (stringify_json(erlang:element(2, Kv)))/binary>>
                            end
                        ),
                        gleam@string:join(_pipe@2, <<","/utf8>>)
                    end)/binary>>/binary,
                "}"/utf8>>;

        {array, Arr} ->
            <<<<"["/utf8,
                    (begin
                        _pipe@3 = Arr,
                        _pipe@4 = gleam@list:map(_pipe@3, fun stringify_json/1),
                        gleam@string:join(_pipe@4, <<","/utf8>>)
                    end)/binary>>/binary,
                "]"/utf8>>;

        {string, Str} ->
            <<<<"\""/utf8, Str/binary>>/binary, "\""/utf8>>;

        {number, Flt} ->
            gleam@float:to_string(Flt);

        {boolean, true} ->
            <<"true"/utf8>>;

        {boolean, false} ->
            <<"false"/utf8>>;

        null ->
            <<"null"/utf8>>
    end.

-spec stringify_jsonl(list(json_value())) -> binary().
stringify_jsonl(Values) ->
    _pipe = Values,
    _pipe@1 = gleam@list:map(_pipe, fun stringify_json/1),
    gleam@string:join(_pipe@1, <<"\n"/utf8>>).

-spec split_jsonl(binary()) -> list(binary()).
split_jsonl(Value) ->
    _pipe = case gleam@string:last(Value) of
        {ok, <<"\n"/utf8>>} ->
            gleam@string:drop_right(Value, 1);

        _ ->
            Value
    end,
    gleam@string:split(_pipe, <<"\n"/utf8>>).

-spec parse_jsonl(binary()) -> {ok, list(json_value())} | {error, parse_error()}.
parse_jsonl(Value) ->
    Parse = fun(_capture) -> run_parser(_capture, json_parser()) end,
    gleam@list:try_map(split_jsonl(Value), Parse).

-spec parse_jsonl_all(binary()) -> list({ok, json_value()} |
    {error, parse_error()}).
parse_jsonl_all(Value) ->
    Parse = fun(_capture) -> run_parser(_capture, json_parser()) end,
    gleam@list:map(split_jsonl(Value), Parse).

-spec parse_jsonl_valid(binary()) -> list(json_value()).
parse_jsonl_valid(Value) ->
    _pipe = Value,
    _pipe@1 = parse_jsonl_all(_pipe),
    gleam@result:values(_pipe@1).

-spec invert_query_rec(json_query(), inv_json_query()) -> inv_json_query().
invert_query_rec(Query, State) ->
    case Query of
        root ->
            State;

        {key, Query@1, Key} ->
            invert_query_rec(Query@1, {inv_key, Key, State});

        {key_or, Query@2, Key@1, O} ->
            invert_query_rec(Query@2, {inv_key_or, Key@1, O, State});

        {index, Query@3, Index} ->
            invert_query_rec(Query@3, {inv_index, Index, State});

        {index_or, Query@4, Index@1, Or} ->
            invert_query_rec(Query@4, {inv_index_or, Index@1, Or, State});

        {filter, Query@5, Predicate} ->
            invert_query_rec(Query@5, {inv_filter, Predicate, State});

        {map, Query@6, Mapping} ->
            invert_query_rec(Query@6, {inv_map, Mapping, State});

        {map_keys, Query@7, Mapping@1} ->
            invert_query_rec(Query@7, {inv_map_keys, Mapping@1, State});

        {map_values, Query@8, Mapping@2} ->
            invert_query_rec(Query@8, {inv_map_values, Mapping@2, State});

        {filter_map, Query@9, Mapping@3} ->
            invert_query_rec(Query@9, {inv_filter_map, Mapping@3, State});

        {for_each, Query@10} ->
            invert_query_rec(Query@10, {inv_for_each, State});

        {for_each_ok, Query@11} ->
            invert_query_rec(Query@11, {inv_for_each_ok, State})
    end.

-spec invert_query(json_query()) -> inv_json_query().
invert_query(Query) ->
    invert_query_rec(Query, inv_end).

-spec query_json_rec(json_value(), inv_json_query()) -> {ok, json_value()} |
    {error, json_query_error()}.
query_json_rec(Json, Query) ->
    case Query of
        inv_end ->
            {ok, Json};

        {inv_key, Key, Q} ->
            _pipe@2 = case Json of
                {object, Obj} = J ->
                    _pipe = Obj,
                    _pipe@1 = gleam@dict:get(_pipe, Key),
                    gleam@result:replace_error(
                        _pipe@1,
                        {missing_object_key, J, Key}
                    );

                J@1 ->
                    {error, {unexpected_type, J@1}}
            end,
            _pipe@3 = gleam@result:map(
                _pipe@2,
                fun(_capture) -> query_json_rec(_capture, Q) end
            ),
            gleam@result:flatten(_pipe@3);

        {inv_key_or, Key@1, Or, Q@1} ->
            _pipe@7 = case Json of
                {object, Obj@1} ->
                    _pipe@4 = Obj@1,
                    _pipe@5 = gleam@dict:get(_pipe@4, Key@1),
                    _pipe@6 = gleam@result:unwrap(_pipe@5, Or),
                    {ok, _pipe@6};

                J@2 ->
                    {error, {unexpected_type, J@2}}
            end,
            _pipe@8 = gleam@result:map(
                _pipe@7,
                fun(_capture@1) -> query_json_rec(_capture@1, Q@1) end
            ),
            gleam@result:flatten(_pipe@8);

        {inv_index, Index, Q@2} ->
            _pipe@11 = case Json of
                {array, Arr} = J@3 ->
                    _pipe@9 = Arr,
                    _pipe@10 = gleam@list:at(_pipe@9, Index),
                    gleam@result:replace_error(
                        _pipe@10,
                        {index_out_of_bounds, J@3, Index}
                    );

                J@4 ->
                    {error, {unexpected_type, J@4}}
            end,
            _pipe@12 = gleam@result:map(
                _pipe@11,
                fun(_capture@2) -> query_json_rec(_capture@2, Q@2) end
            ),
            gleam@result:flatten(_pipe@12);

        {inv_index_or, Index@1, Or@1, Q@3} ->
            _pipe@16 = case Json of
                {array, Arr@1} ->
                    _pipe@13 = Arr@1,
                    _pipe@14 = gleam@list:at(_pipe@13, Index@1),
                    _pipe@15 = gleam@result:unwrap(_pipe@14, Or@1),
                    {ok, _pipe@15};

                J@5 ->
                    {error, {unexpected_type, J@5}}
            end,
            _pipe@17 = gleam@result:map(
                _pipe@16,
                fun(_capture@3) -> query_json_rec(_capture@3, Q@3) end
            ),
            gleam@result:flatten(_pipe@17);

        {inv_filter, Predicate, Q@4} ->
            case Json of
                {array, Arr@2} ->
                    _pipe@18 = Arr@2,
                    _pipe@19 = gleam@list:filter(_pipe@18, Predicate),
                    _pipe@20 = {array, _pipe@19},
                    query_json_rec(_pipe@20, Q@4);

                J@6 ->
                    {error, {unexpected_type, J@6}}
            end;

        {inv_map, Mapping, Q@5} ->
            case Json of
                {array, Arr@3} ->
                    _pipe@21 = Arr@3,
                    _pipe@22 = gleam@list:map(_pipe@21, Mapping),
                    _pipe@23 = {array, _pipe@22},
                    query_json_rec(_pipe@23, Q@5);

                J@7 ->
                    {error, {unexpected_type, J@7}}
            end;

        {inv_map_keys, Mapping@1, Q@6} ->
            case Json of
                {object, Obj@2} ->
                    _pipe@24 = Obj@2,
                    _pipe@25 = maps:to_list(_pipe@24),
                    _pipe@26 = gleam@list:map(
                        _pipe@25,
                        fun(Kv) ->
                            {Mapping@1(erlang:element(1, Kv)),
                                erlang:element(2, Kv)}
                        end
                    ),
                    _pipe@27 = maps:from_list(_pipe@26),
                    _pipe@28 = {object, _pipe@27},
                    query_json_rec(_pipe@28, Q@6);

                J@8 ->
                    {error, {unexpected_type, J@8}}
            end;

        {inv_map_values, Mapping@2, Q@7} ->
            case Json of
                {object, Obj@3} ->
                    _pipe@29 = Obj@3,
                    _pipe@30 = gleam@dict:map_values(_pipe@29, Mapping@2),
                    _pipe@31 = {object, _pipe@30},
                    query_json_rec(_pipe@31, Q@7);

                J@9 ->
                    {error, {unexpected_type, J@9}}
            end;

        {inv_filter_map, Mapping@3, Q@8} ->
            case Json of
                {array, Arr@4} ->
                    _pipe@32 = Arr@4,
                    _pipe@33 = gleam@list:filter_map(_pipe@32, Mapping@3),
                    _pipe@34 = {array, _pipe@33},
                    query_json_rec(_pipe@34, Q@8);

                J@10 ->
                    {error, {unexpected_type, J@10}}
            end;

        {inv_for_each, Q@9} ->
            case Json of
                {array, Arr@5} ->
                    _pipe@35 = Arr@5,
                    _pipe@36 = gleam@list:map(
                        _pipe@35,
                        fun(_capture@4) -> query_json_rec(_capture@4, Q@9) end
                    ),
                    _pipe@37 = gleam@result:all(_pipe@36),
                    gleam@result:map(
                        _pipe@37,
                        fun(Field@0) -> {array, Field@0} end
                    );

                J@11 ->
                    {error, {unexpected_type, J@11}}
            end;

        {inv_for_each_ok, Q@10} ->
            case Json of
                {array, Arr@6} ->
                    _pipe@38 = Arr@6,
                    _pipe@39 = gleam@list:map(
                        _pipe@38,
                        fun(_capture@5) -> query_json_rec(_capture@5, Q@10) end
                    ),
                    _pipe@40 = gleam@result:values(_pipe@39),
                    _pipe@41 = {array, _pipe@40},
                    {ok, _pipe@41};

                J@12 ->
                    {error, {unexpected_type, J@12}}
            end
    end.

-spec query_json(json_value(), json_query()) -> {ok, json_value()} |
    {error, json_query_error()}.
query_json(Json, Query) ->
    query_json_rec(Json, invert_query(Query)).
