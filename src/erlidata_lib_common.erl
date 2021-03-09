-module(erlidata_lib_common).

-export([make_headers/0, make_request/1, handle_response/1, client_get/3]).

-spec client_get(
        fun(
            (string(), string()) ->
            erlidata_lib:query_return()
        ),
        string(),
        string()) -> erlidata_lib:query_return().
client_get(GetterFun, RootURL, GetVal) ->
    case GetterFun(RootURL, GetVal) of
        {error, invalid_uri} -> GetterFun(RootURL, http_uri:encode(GetVal));
        Other -> Other
    end.

-spec make_headers() -> [{string(), string()}].
make_headers() ->
    ModuleVersion = case application:get_key(erlidata_lib, vsn) of
        {ok, Vsn} when is_list(Vsn) -> Vsn;
        _ -> "dev"
    end,
    [{"Accept", "application/json"}, {"User-Agent", "erlidata_lib/" ++ ModuleVersion}].

-spec make_request(string() | {string(), httpc:headers()}) -> {ok, term()} | {error, term()}.
make_request(URL) when is_list(URL) -> make_request({URL, make_headers()});
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
