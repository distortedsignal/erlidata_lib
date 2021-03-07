-module(erlidata_lib_entity).

-export([get_entity/1, get_entity/2]).
-export([make_entity_client/1]).

-spec get_entity(string()) -> erlidata_lib:query_return().
get_entity(URL) ->
    case application:ensure_started(inets) of
        ok ->
            Headers = erlidata_lib_common:make_headers(),
            erlidata_lib_common:handle_response(httpc:request(get, {URL, Headers}, [], []));
        Error -> Error
    end.

-spec get_entity(string(), string()) -> erlidata_lib:query_return().
get_entity(RootURL, EntityId) ->
    get_entity(RootURL ++ EntityId).

-spec make_entity_client(string()) ->
    fun((string()) -> erlidata_lib:query_return()).
make_entity_client(RootURL) ->
    fun(EntityId) -> get_entity(RootURL, EntityId) end.
