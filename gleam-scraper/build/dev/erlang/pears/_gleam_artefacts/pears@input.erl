-module(pears@input).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([get/1, get_n/2, next/1, next_n/2, at_end/1]).
-export_type([input/1]).

-type input(AAP) :: {input, list(AAP), integer()}.

-spec get(input(AAQ)) -> gleam@option:option(AAQ).
get(Input) ->
    _pipe = erlang:element(2, Input),
    _pipe@1 = gleam@list:at(_pipe, erlang:element(3, Input)),
    gleam@option:from_result(_pipe@1).

-spec get_n(input(AAT), integer()) -> list(AAT).
get_n(Input, N) ->
    _pipe = erlang:element(2, Input),
    _pipe@1 = gleam@list:drop(_pipe, erlang:element(3, Input)),
    gleam@list:take(_pipe@1, N).

-spec next(input(AAW)) -> input(AAW).
next(Input) ->
    erlang:setelement(3, Input, erlang:element(3, Input) + 1).

-spec next_n(input(AAZ), integer()) -> input(AAZ).
next_n(Input, N) ->
    erlang:setelement(3, Input, erlang:element(3, Input) + N).

-spec at_end(input(any())) -> boolean().
at_end(Input) ->
    erlang:element(3, Input) >= gleam@list:length(erlang:element(2, Input)).
