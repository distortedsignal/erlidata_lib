-module(erlidata_lib_query).

-export([query/1, query/2]).
-export([make_query_client/1]).

-spec query(string()) -> erlidata_lib:query_return().
query(URL) ->
    case application:ensure_started(inets) of
        ok ->
            Headers = erlidata_lib_common:make_headers(),
            erlidata_lib_common:handle_response(httpc:request(get, {URL, Headers}, [], []));
        Error -> Error
    end.

-spec query(string(), string()) -> erlidata_lib:query_return().
query(RootURL, SparqlQuery) ->
    query(RootURL ++ SparqlQuery).

-spec make_query_client(string()) ->
    fun((string()) -> erlidata_lib:query_return()).
make_query_client(RootURL) ->
    fun(SparqlQuery) -> query(RootURL, SparqlQuery) end.
