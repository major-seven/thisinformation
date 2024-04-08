-record(filter, {
    'query' :: jasper:json_query(),
    predicate :: fun((jasper:json_value()) -> boolean())
}).
