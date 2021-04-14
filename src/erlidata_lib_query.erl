-module(erlidata_lib_query).

-export([query/1]).
-export([make_query_client/1]).
-export([get_query/1]).

%% Takes a url-encoded string, GETs it, and returns the outcome.
-spec query(uri_string:uri_map() | uri_string:uri_string()) -> erlidata_lib:query_return().
query(UriVal) ->
    case application:ensure_started(inets) of
        ok -> safe_query(UriVal);
        Error -> Error
    end.

-spec safe_query(uri_string:uri_map() | uri_string:uri_string()) -> erlidata_lib:query_return().
safe_query(UriVal) when is_map(UriVal) ->
    case uri_string:normalize(UriVal) of
        {error, _, _}=Err -> Err;
        NormalizedUri -> safe_query(NormalizedUri)
    end;
safe_query(UriVal) ->
    erlidata_lib_common:handle_response(erlidata_lib_common:make_request(UriVal)).

-spec make_query_client(fun((string() | binary()) -> uri_string:uri_map())) -> fun((string() | binary()) -> erlidata_lib:query_return()).
make_query_client(UriMapTransformFun) ->
    fun(SparqlQuery) -> erlidata_lib_common:client_get(fun query/1, UriMapTransformFun, SparqlQuery) end.

-spec get_query(string() | binary()) -> erlidata_lib:query_return().
get_query(Query) when is_list(Query) ->
    get_query(list_to_binary(Query));
get_query(Query) ->
    erlidata_lib_common:get_query(Query).