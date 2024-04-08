-record(map_keys, {
    'query' :: jasper:json_query(),
    mapping :: fun((binary()) -> binary())
}).
