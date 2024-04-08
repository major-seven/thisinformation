-module(pears).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([parse/2, parse_string/2, ok/2]).
-export_type([parsed/2, parse_error/1]).

-type parsed(FLE, FLF) :: {parsed, pears@input:input(FLE), FLF}.

-type parse_error(FLG) :: {unexpected_end_of_input,
        pears@input:input(FLG),
        list(binary())} |
    {unexpected_token, pears@input:input(FLG), list(binary()), FLG}.

-spec parse(
    list(FLT),
    fun((pears@input:input(FLT)) -> {ok, parsed(FLT, FLV)} |
        {error, parse_error(FLT)})
) -> {ok, parsed(FLT, FLV)} | {error, parse_error(FLT)}.
parse(I, P) ->
    _pipe = {input, I, 0},
    P(_pipe).

-spec parse_string(
    binary(),
    fun((pears@input:input(binary())) -> {ok, parsed(binary(), FMA)} |
        {error, parse_error(binary())})
) -> {ok, parsed(binary(), FMA)} | {error, parse_error(binary())}.
parse_string(I, P) ->
    _pipe = I,
    _pipe@1 = gleam@string:to_graphemes(_pipe),
    parse(_pipe@1, P).

-spec ok(pears@input:input(FMF), FMH) -> {ok, parsed(FMF, FMH)} |
    {error, parse_error(FMF)}.
ok(Input, Value) ->
    {ok, {parsed, Input, Value}}.
