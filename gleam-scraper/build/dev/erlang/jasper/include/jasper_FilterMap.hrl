-record(filter_map, {
    'query' :: jasper:json_query(),
    mapping :: fun((jasper:json_value()) -> {ok, jasper:json_value()} |
        {error, nil})
}).
