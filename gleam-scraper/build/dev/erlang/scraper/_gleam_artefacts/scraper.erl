-module(scraper).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).

-spec parse_item(binary()) -> {ok, binary()} | {error, nil}.
parse_item(Item) ->
    _pipe = Item,
    _pipe@1 = gleam@string:trim(_pipe),
    _pipe@2 = gleam@string:split(_pipe@1, <<"</title>"/utf8>>),
    _pipe@3 = gleam@list:first(_pipe@2),
    _pipe@6 = gleam@result:map(_pipe@3, fun(X) -> _pipe@4 = X,
            _pipe@5 = gleam@string:split(_pipe@4, <<"<title>"/utf8>>),
            gleam@list:last(_pipe@5) end),
    gleam@result:flatten(_pipe@6).

-spec parse(binary()) -> list(binary()).
parse(Xml) ->
    _pipe = Xml,
    _pipe@1 = gleam@string:split(_pipe, <<"<item>"/utf8>>),
    _pipe@2 = gleam@list:drop(_pipe@1, 1),
    _pipe@3 = gleam@list:map(_pipe@2, fun parse_item/1),
    gleam@result:values(_pipe@3).

-spec get_titles(binary()) -> list(binary()).
get_titles(Url) ->
    _pipe = shellout:command(<<"curl"/utf8>>, [Url], <<"."/utf8>>, []),
    _pipe@1 = gleam@result:unwrap(_pipe, <<""/utf8>>),
    parse(_pipe@1).

-spec get_date() -> binary().
get_date() ->
    Date = begin
        _pipe = shellout:command(<<"date"/utf8>>, [], <<"."/utf8>>, []),
        _pipe@1 = gleam@result:unwrap(_pipe, <<""/utf8>>),
        _pipe@2 = gleam@string:split(_pipe@1, <<" "/utf8>>),
        gleam@list:filter(_pipe@2, fun(X) -> not gleam@string:is_empty(X) end)
    end,
    Year = begin
        _pipe@3 = gleam@list:last(Date),
        gleam@result:unwrap(_pipe@3, <<""/utf8>>)
    end,
    Date@1 = gleam@list:take(Date, 3),
    _pipe@4 = gleam@list:append(Date@1, [Year]),
    _pipe@5 = gleam@string:join(_pipe@4, <<" "/utf8>>),
    gleam@string:drop_right(_pipe@5, 1).

-spec get_article(binary(), binary()) -> {ok, binary()} | {error, nil}.
get_article(Title, Date) ->
    Json = begin
        _pipe = [{<<"model"/utf8>>, {string, <<"reporter"/utf8>>}},
            {<<"prompt"/utf8>>,
                {string,
                    <<<<<<"date: "/utf8, Date/binary>>/binary, " title: "/utf8>>/binary,
                        Title/binary>>}},
            {<<"stream"/utf8>>, {boolean, false}}],
        maps:from_list(_pipe)
    end,
    Json_string = begin
        _pipe@1 = {object, Json},
        jasper:stringify_json(_pipe@1)
    end,
    gleam@io:println(<<"Getting: "/utf8, Json_string/binary>>),
    Res = begin
        _pipe@2 = shellout:command(
            <<"curl"/utf8>>,
            [<<"http://localhost:11434/api/generate"/utf8>>,
                <<"-d"/utf8>>,
                Json_string,
                <<"-s"/utf8>>],
            <<"."/utf8>>,
            []
        ),
        _pipe@3 = gleam@result:unwrap(_pipe@2, <<"NO ARTICLE"/utf8>>),
        _pipe@4 = jasper:parse_json(_pipe@3),
        gleam@result:map(
            _pipe@4,
            fun(X) -> jasper:query_json(X, {key, root, <<"response"/utf8>>}) end
        )
    end,
    case Res of
        {ok, {ok, {string, X@1}}} ->
            No_newlines = begin
                _pipe@5 = X@1,
                _pipe@6 = gleam@string:to_graphemes(_pipe@5),
                _pipe@7 = gleam@list:filter(
                    _pipe@6,
                    fun(X@2) -> X@2 /= <<"\n"/utf8>> end
                ),
                _pipe@8 = gleam@list:map(_pipe@7, fun(X@3) -> case X@3 of
                            <<"\""/utf8>> ->
                                <<"'"/utf8>>;

                            Y ->
                                Y
                        end end),
                gleam@string:join(_pipe@8, <<""/utf8>>)
            end,
            {ok, No_newlines};

        _ ->
            {error, nil}
    end.

-spec get_best_titles(binary()) -> {ok, binary()} | {error, nil}.
get_best_titles(Titles) ->
    Json = begin
        _pipe = [{<<"model"/utf8>>, {string, <<"llama2"/utf8>>}},
            {<<"prompt"/utf8>>,
                {string,
                    <<"You are the editor of a newspaper. Select the 10 best titles from this list: Do not add anything to the titles, just select them. Just give me a numbered list back without commentary."/utf8,
                        Titles/binary>>}},
            {<<"stream"/utf8>>, {boolean, false}}],
        maps:from_list(_pipe)
    end,
    Json_string = begin
        _pipe@1 = {object, Json},
        jasper:stringify_json(_pipe@1)
    end,
    gleam@io:println(<<"Getting best titles"/utf8>>),
    Res = begin
        _pipe@2 = shellout:command(
            <<"curl"/utf8>>,
            [<<"http://localhost:11434/api/generate"/utf8>>,
                <<"-d"/utf8>>,
                Json_string,
                <<"-s"/utf8>>],
            <<"."/utf8>>,
            []
        ),
        _pipe@3 = gleam@result:unwrap(_pipe@2, <<"NO TITLE"/utf8>>),
        _pipe@4 = jasper:parse_json(_pipe@3),
        gleam@result:map(
            _pipe@4,
            fun(X) -> jasper:query_json(X, {key, root, <<"response"/utf8>>}) end
        )
    end,
    case Res of
        {ok, {ok, {string, X@1}}} ->
            {ok, X@1};

        _ ->
            {error, nil}
    end.

-spec get_new_title(binary()) -> {ok, binary()} | {error, nil}.
get_new_title(Text) ->
    Cropped = begin
        _pipe = gleam@string:to_graphemes(Text),
        _pipe@1 = gleam@list:filter(
            _pipe,
            fun(X) -> (X /= <<"\n"/utf8>>) andalso (X /= <<"\""/utf8>>) end
        ),
        gleam@string:join(_pipe@1, <<""/utf8>>)
    end,
    Json = begin
        _pipe@2 = [{<<"model"/utf8>>, {string, <<"llama2"/utf8>>}},
            {<<"prompt"/utf8>>,
                {string,
                    <<"write one title for the following text, do not give me options: "/utf8,
                        Cropped/binary>>}},
            {<<"stream"/utf8>>, {boolean, false}}],
        maps:from_list(_pipe@2)
    end,
    Json_string = begin
        _pipe@3 = {object, Json},
        jasper:stringify_json(_pipe@3)
    end,
    gleam@io:println(<<"Getting title"/utf8>>),
    Res = begin
        _pipe@4 = shellout:command(
            <<"curl"/utf8>>,
            [<<"http://localhost:11434/api/generate"/utf8>>,
                <<"-d"/utf8>>,
                Json_string,
                <<"-s"/utf8>>],
            <<"."/utf8>>,
            []
        ),
        _pipe@5 = gleam@result:unwrap(_pipe@4, <<"NO TITLE"/utf8>>),
        _pipe@6 = jasper:parse_json(_pipe@5),
        gleam@result:map(
            _pipe@6,
            fun(X@1) ->
                jasper:query_json(X@1, {key, root, <<"response"/utf8>>})
            end
        )
    end,
    case Res of
        {ok, {ok, {string, X@2}}} ->
            {ok, X@2};

        _ ->
            {error, nil}
    end.

-spec main() -> list({ok, {ok, binary()} | {error, {integer(), binary()}}} |
    {error, nil}).
main() ->
    Rss = begin
        _pipe = <<"sources.txt"/utf8>>,
        _pipe@1 = simplifile:read(_pipe),
        _pipe@2 = gleam@result:unwrap(_pipe@1, <<""/utf8>>),
        gleam@string:split(_pipe@2, <<"\n"/utf8>>)
    end,
    Titles = begin
        _pipe@3 = Rss,
        _pipe@4 = gleam@list:map(_pipe@3, fun get_titles/1),
        gleam@list:flatten(_pipe@4)
    end,
    Best_choice = begin
        _pipe@5 = Titles,
        _pipe@6 = gleam@string:join(_pipe@5, <<", "/utf8>>),
        _pipe@7 = get_best_titles(_pipe@6),
        _pipe@8 = gleam@result:unwrap(_pipe@7, <<""/utf8>>),
        _pipe@9 = gleam@string:split(_pipe@8, <<"\n"/utf8>>),
        _pipe@10 = gleam@list:drop(_pipe@9, 2),
        gleam@list:map(_pipe@10, fun(X) -> _pipe@11 = X,
                _pipe@12 = gleam@string:split(_pipe@11, <<". "/utf8>>),
                _pipe@13 = gleam@list:last(_pipe@12),
                _pipe@14 = gleam@result:unwrap(_pipe@13, <<"NOTHING"/utf8>>),
                _pipe@15 = gleam@string:to_graphemes(_pipe@14),
                _pipe@16 = gleam@list:filter(
                    _pipe@15,
                    fun(X@1) -> X@1 /= <<"\""/utf8>> end
                ),
                gleam@string:join(_pipe@16, <<""/utf8>>) end)
    end,
    Best_choice@1 = erlang:element(1, gleam@list:split(Best_choice, 10)),
    Date = get_date(),
    _pipe@17 = Best_choice@1,
    gleam@list:map(_pipe@17, fun(X@2) -> _pipe@18 = get_article(X@2, Date),
            gleam@result:map(
                _pipe@18,
                fun(Text) ->
                    Title = begin
                        _pipe@19 = get_new_title(Text),
                        _pipe@20 = gleam@result:unwrap(
                            _pipe@19,
                            <<"error"/utf8>>
                        ),
                        _pipe@21 = gleam@string:to_graphemes(_pipe@20),
                        _pipe@22 = gleam@list:filter(
                            _pipe@21,
                            fun(X@3) ->
                                ((X@3 /= <<"\\"/utf8>>) andalso (X@3 /= <<"\""/utf8>>))
                                andalso (X@3 /= <<"\n"/utf8>>)
                            end
                        ),
                        gleam@string:join(_pipe@22, <<""/utf8>>)
                    end,
                    Json = begin
                        _pipe@23 = [{<<"title"/utf8>>, {string, Title}},
                            {<<"content"/utf8>>, {string, Text}}],
                        maps:from_list(_pipe@23)
                    end,
                    Json_string = begin
                        _pipe@24 = {object, Json},
                        jasper:stringify_json(_pipe@24)
                    end,
                    _pipe@25 = shellout:command(
                        <<"curl"/utf8>>,
                        [<<"http://localhost:3000/api/new-article/123"/utf8>>,
                            <<"-s"/utf8>>,
                            <<"-X"/utf8>>,
                            <<"POST"/utf8>>,
                            <<"-d"/utf8>>,
                            Json_string,
                            <<"-H"/utf8>>,
                            <<"Content-Type: application/json"/utf8>>],
                        <<"."/utf8>>,
                        []
                    ),
                    gleam@io:debug(_pipe@25)
                end
            ) end).
