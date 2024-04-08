-record(map, {
    'query' :: jasper:json_query(),
    mapping :: fun((jasper:json_value()) -> jasper:json_value())
}).
