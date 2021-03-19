-module(erlidata_lib_common).

-export([make_headers/0, make_request/1, handle_response/1, client_get/3]).
-export([get_by_id/1, get_query/1]).

-define(GET_ENTITY_MAP_DEFAULT(Id),
    uri_string:normalize(#{
        scheme => <<"http">>,
        host => <<"www.wikidata.org">>,
        path => << <<"entity/">>/binary, Id/binary>>
    })
).

-define(GET_QUERY_MAP_DEFAULT(Query),
    uri_string:normalize(#{
        scheme => <<"https">>,
        host => <<"query.wikidata.org">>,
        path => <<"sparql">>,
        query => Query
    })
).

-spec client_get(
        fun((uri_string:uri_map() | uri_string:uri_string()) -> erlidata_lib:query_return()),
        fun((string() | binary()) -> erlidata_lib:query_return()),
        string() | binary()) -> erlidata_lib:query_return().
client_get(GetterFun, UrlConstructionFun, GetVal) ->
    GetterFun(UrlConstructionFun(GetVal)).

-spec make_headers() -> [{string(), string()}].
make_headers() ->
    ModuleVersion = case application:get_key(erlidata_lib, vsn) of
        {ok, Vsn} when is_list(Vsn) -> Vsn;
        _ -> "dev"
    end,
    [{"Accept", "application/json"}, {"User-Agent", "erlidata_lib/" ++ ModuleVersion}].

-spec make_request(binary() | string() | {binary(), httpc:headers()}) -> {ok, term()} | {error, term()}.
make_request(URL) when is_binary(URL) -> make_request({URL, make_headers()});
make_request(URL) when is_list(URL) -> make_request({list_to_binary(URL), make_headers()});
make_request({URL, Headers}) -> httpc:request(get, {URL, Headers}, [], []).

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

-spec get_by_id(string() | binary()) -> erlidata_lib:query_return().
get_by_id(Id) when is_list(Id) -> get_by_id(list_to_binary(Id));
get_by_id(Id) ->
    case ?GET_ENTITY_MAP_DEFAULT(Id) of
        {error, _, _}=Err -> Err;
        ValidUrl -> handle_response(make_request(ValidUrl))
    end.

-spec get_query(binary()) -> erlidata_lib:query_return().
get_query(Query) ->
    case uri_string:compose_query([{<<"query">>, Query}]) of
        {error, _, _}=Err -> Err;
        ValidQuery ->
            case ?GET_QUERY_MAP_DEFAULT(ValidQuery) of
                {error, _, _}=Err -> Err;
                ValidUrl -> handle_response(make_request(ValidUrl))
            end
    end.
