-module(erlidata_lib_common).

-export([make_headers/0, handle_response/1]).

-spec make_headers() -> [{string(), string()}].
make_headers() ->
    ModuleVersion = case application:get_key(erlidata_lib, vsn) of
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
