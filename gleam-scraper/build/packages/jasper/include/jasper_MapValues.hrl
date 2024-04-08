-record(map_values, {
    'query' :: jasper:json_query(),
    mapping :: fun((binary(), jasper:json_value()) -> jasper:json_value())
}).
