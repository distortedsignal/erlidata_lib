-module(erlidata_lib_property).

-export([get_property/1, get_property/2]).
-export([make_property_client/1]).

-spec get_property(string()) -> erlidata_lib:query_return().
get_property(URL) ->
    case application:ensure_started(inets) of
        ok ->
            Headers = erlidata_lib_common:make_headers(),
            erlidata_lib_common:handle_response(httpc:request(get, {URL, Headers}, [], []));
        Error -> Error
    end.

-spec get_property(string(), string()) -> erlidata_lib:query_return().
get_property(RootURL, EntityId) ->
    get_property(RootURL ++ EntityId).

-spec make_property_client(string()) ->
    fun((string()) -> erlidata_lib:query_return()).
make_property_client(RootURL) ->
    fun(EntityId) -> get_property(RootURL, EntityId) end.