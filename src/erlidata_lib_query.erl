-module(erlidata_lib_query).

-export([query/1, query/2]).
-export([make_query_client/1]).

-spec query(string()) -> erlidata_lib:query_return().
query(URL) ->
    case application:ensure_started(inets) of
        ok ->
            Headers = make_headers(),
            handle_response(httpc:request(get, {URL, Headers}, [], []));
        Error -> Error
    end.

-spec query(string(), string()) -> erlidata_lib:query_return().
query(RootURL, SparqlQuery) ->
    query(RootURL ++ SparqlQuery).

-spec make_query_client(string()) ->
    fun((string()) -> erlidata_lib:query_return()).
make_query_client(RootURL) ->
    fun(SparqlQuery) -> query(RootURL, SparqlQuery) end.

-spec make_headers() -> [{string(), string()}].
make_headers() ->
    ModuleVersion = case application:get_key(?MODULE, vsn) of
        {ok, Vsn} when is_list(Vsn) -> Vsn;
        _ -> "dev"
    end,
    [{"Accept", "application/json"}, {"User-Agent", "erlidata_lib/" ++ ModuleVersion}].

-spec handle_response({ok, any()} | {error, term()}) -> {ok, proplists:proplist()} | {error, term()}.
handle_response({error, _}=Err) -> Err;
handle_response({ok, {{_,Status,_}, _, Body}}) when Status >= 200 andalso Status < 300 -> 
    parse_body(Body);
handle_response({ok, {{_, Status, _}, _, Body}}) ->
    handle_error(Status, Body);
handle_response({ok, {{_,Status,_}, Body}}) when Status >= 200 andalso Status < 300 -> 
    parse_body(Body);
handle_response({ok, {{_,Status,_}, Body}}) ->
    handle_error(Status, Body);
handle_response({ok, RequestId}) -> {error, "Unexpected response - got ok and request id: " ++ ref_to_list(RequestId)}.

-spec handle_error(integer(), string()) -> {error, string()}.
handle_error(Status, Body) ->
    {error, lists:flatten(io_lib:format("Got non-2xx status: ~p for body \"~s\"", [Status, Body]))}.

-spec parse_body(string()) -> {ok, proplists:proplist()} | {error, term()}.
parse_body(Body) ->
    jiffy:decode(Body).
