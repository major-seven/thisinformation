-module(pears@chars).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([char/1, digit/0, number/0, string/1, input/1, whitespace/0, whitespace0/0, whitespace1/0]).

-spec char(binary()) -> fun((pears@input:input(binary())) -> {ok,
        pears:parsed(binary(), binary())} |
    {error, pears:parse_error(binary())}).
char(C) ->
    pears@combinators:just(C).

-spec digit() -> fun((pears@input:input(binary())) -> {ok,
        pears:parsed(binary(), binary())} |
    {error, pears:parse_error(binary())}).
digit() ->
    pears@combinators:one_of(
        [<<"0"/utf8>>,
            <<"1"/utf8>>,
            <<"2"/utf8>>,
            <<"3"/utf8>>,
            <<"4"/utf8>>,
            <<"5"/utf8>>,
            <<"6"/utf8>>,
            <<"7"/utf8>>,
            <<"8"/utf8>>,
            <<"9"/utf8>>]
    ).

-spec number() -> fun((pears@input:input(binary())) -> {ok,
        pears:parsed(binary(), integer())} |
    {error, pears:parse_error(binary())}).
number() ->
    _pipe = pears@combinators:many1(digit()),
    pears@combinators:map(
        _pipe,
        fun(Digits) ->
            gleam@list:fold(
                Digits,
                0,
                fun(Acc, Digit) ->
                    _assert_subject = gleam@int:parse(Digit),
                    {ok, Digit@1} = case _assert_subject of
                        {ok, _} -> _assert_subject;
                        _assert_fail ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Assertion pattern match failed"/utf8>>,
                                        value => _assert_fail,
                                        module => <<"pears/chars"/utf8>>,
                                        function => <<"number"/utf8>>,
                                        line => 65})
                    end,
                    (Acc * 10) + Digit@1
                end
            )
        end
    ).

-spec string(binary()) -> fun((pears@input:input(binary())) -> {ok,
        pears:parsed(binary(), binary())} |
    {error, pears:parse_error(binary())}).
string(Str) ->
    fun(In) ->
        S = gleam@string:to_graphemes(Str),
        Length = gleam@list:length(S),
        Candidate = pears@input:get_n(In, Length),
        case Candidate =:= S of
            true ->
                pears:ok(pears@input:next_n(In, Length), Str);

            false ->
                case Candidate of
                    [] ->
                        {error, {unexpected_end_of_input, In, [Str]}};

                    [Head | _] ->
                        {error, {unexpected_token, In, [Str], Head}}
                end
        end
    end.

-spec input(binary()) -> pears@input:input(binary()).
input(S) ->
    Tokens = gleam@string:to_graphemes(S),
    {input, Tokens, 0}.

-spec whitespace() -> fun((pears@input:input(binary())) -> {ok,
        pears:parsed(binary(), binary())} |
    {error, pears:parse_error(binary())}).
whitespace() ->
    pears@combinators:satisfying(
        fun(C) -> gleam@string:trim(C) =:= <<""/utf8>> end
    ).

-spec whitespace0() -> fun((pears@input:input(binary())) -> {ok,
        pears:parsed(binary(), list(binary()))} |
    {error, pears:parse_error(binary())}).
whitespace0() ->
    pears@combinators:many0(whitespace()).

-spec whitespace1() -> fun((pears@input:input(binary())) -> {ok,
        pears:parsed(binary(), list(binary()))} |
    {error, pears:parse_error(binary())}).
whitespace1() ->
    pears@combinators:many1(whitespace()).
