-module(erlidata_lib_item).

-export([get_item/1]).
-export([make_item_client/1]).
-export([get_item_by_id/1]).

%% Items are things like Properties and Entites
%% These have extremely similar semantics but
%% subtly different uses. 

-spec get_item(uri_string:uri_map() | uri_string:uri_string()) -> erlidata_lib:query_return().
get_item(UriVal) ->
    case application:ensure_started(inets) of
        ok -> safe_get_item(UriVal);
        Error -> Error
    end.

-spec safe_get_item(uri_string:uri_map() | uri_string:uri_string()) -> erlidata_lib:query_return().
safe_get_item(UriVal) when is_map(UriVal) ->
    case uri_string:normalize(UriVal) of
        {error, _, _}=Err -> Err;
        NormalizedUri -> safe_get_item(NormalizedUri)
    end;
safe_get_item(UriVal) ->
    erlidata_lib_common:handle_response(erlidata_lib_common:make_request(UriVal)).

-spec make_item_client(fun((string() | binary()) -> uri_string:uri_map())) -> fun((string() | binary()) -> erlidata_lib:query_return()).
make_item_client(UriMapTransformFun) ->
    fun(EntityId) -> erlidata_lib_common:client_get(fun get_item/1, UriMapTransformFun, EntityId) end.

-spec get_item_by_id(string() | binary()) -> erlidata_lib:query_return().
get_item_by_id(Id) when is_list(Id) ->
    get_item_by_id(list_to_binary(Id));
get_item_by_id(Id) ->
    erlidata_lib_common:get_by_id(Id).