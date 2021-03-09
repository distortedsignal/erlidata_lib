-module(erlidata_lib_query).

-export([query/1, query/2]).
-export([make_query_client/1]).

%% Takes a url-encoded string, GETs it, and returns the outcome.
-spec query(string()) -> erlidata_lib:query_return().
query(URL) ->
    case application:ensure_started(inets) of
        ok -> erlidata_lib_common:handle_response(erlidata_lib_common:make_request(URL));
        Error -> Error
    end.

-spec query(string(), string()) -> erlidata_lib:query_return().
query(RootURL, SparqlQuery) ->
    query(RootURL ++ SparqlQuery).

-spec make_query_client(string()) ->
    fun((string()) -> erlidata_lib:query_return()).
make_query_client(RootURL) ->
    fun(SparqlQuery) -> erlidata_lib_common:client_get(fun query/2, RootURL, SparqlQuery) end.
