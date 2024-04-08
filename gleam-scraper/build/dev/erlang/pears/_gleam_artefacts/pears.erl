-module(pears).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([parse/2, parse_string/2, ok/2]).
-export_type([parsed/2, parse_error/1]).

-type parsed(ACB, ACC) :: {parsed, pears@input:input(ACB), ACC}.

-type parse_error(ACD) :: {unexpected_end_of_input,
        pears@input:input(ACD),
        list(binary())} |
    {unexpected_token, pears@input:input(ACD), list(binary()), ACD}.

-spec parse(
    list(ACQ),
    fun((pears@input:input(ACQ)) -> {ok, parsed(ACQ, ACS)} |
        {error, parse_error(ACQ)})
) -> {ok, parsed(ACQ, ACS)} | {error, parse_error(ACQ)}.
parse(I, P) ->
    _pipe = {input, I, 0},
    P(_pipe).

-spec parse_string(
    binary(),
    fun((pears@input:input(binary())) -> {ok, parsed(binary(), ACX)} |
        {error, parse_error(binary())})
) -> {ok, parsed(binary(), ACX)} | {error, parse_error(binary())}.
parse_string(I, P) ->
    _pipe = I,
    _pipe@1 = gleam@string:to_graphemes(_pipe),
    parse(_pipe@1, P).

-spec ok(pears@input:input(ADC), ADE) -> {ok, parsed(ADC, ADE)} |
    {error, parse_error(ADC)}.
ok(Input, Value) ->
    {ok, {parsed, Input, Value}}.
