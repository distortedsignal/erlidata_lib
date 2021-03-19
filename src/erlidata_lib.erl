-module(erlidata_lib).

-export([query/1, make_query_client/1, get_query/1]).
-export([get_entity/1, make_entity_client/1, get_entity_by_id/1]).
-export([get_property/1, get_property_client/1, get_property_by_id/1]).
-export_type([query_return/0]).

-type query_return() ::
    {ok, proplists:proplist()} |
    {error, term()} |
    uri_string:error().

-spec query(uri_string:uri_map() | uri_string:uri_string()) -> query_return().
query(UriVal) -> erlidata_lib_query:query(UriVal).

-spec make_query_client(fun((string() | binary()) -> uri_string:uri_map())) -> fun((string() | binary()) -> query_return()).
make_query_client(UriMapTransformFun) -> erlidata_lib_query:make_query_client(UriMapTransformFun).

-spec get_entity(uri_string:uri_map() | uri_string:uri_string()) -> query_return().
get_entity(UriVal) -> erlidata_lib_item:get_item(UriVal).

-spec make_entity_client(fun((string()) -> uri_string:uri_map())) -> fun((string()) -> query_return()).
make_entity_client(UriMapTransformFun) -> erlidata_lib_item:make_item_client(UriMapTransformFun).

-spec get_entity_by_id(string() | binary()) -> query_return().
get_entity_by_id(Id) -> erlidata_lib_item:get_item_by_id(Id).

-spec get_property(uri_string:uri_map() | uri_string:uri_string()) -> query_return().
get_property(UriVal) -> erlidata_lib_item:get_item(UriVal).

-spec get_property_client(fun((string()) -> uri_string:uri_map())) -> fun((string) -> query_return()).
get_property_client(UriMapTransformFun) -> erlidata_lib_item:make_item_client(UriMapTransformFun).

-spec get_property_by_id(string() | binary()) -> query_return().
get_property_by_id(Id) -> erlidata_lib_item:get_item_by_id(Id).

-spec get_query(string() | binary()) -> query_return().
get_query(Query) -> erlidata_lib_query:get_query(Query).
