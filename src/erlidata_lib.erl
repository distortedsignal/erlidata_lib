-module(erlidata_lib).

-export([query/1, query/2, make_query_client/1]).
-export([get_entity/1, get_entity/2, make_entity_client/1]).
-export([get_property/1, get_property/2, get_property_client/1]).
-export_type([query_return/0]).

-type query_return() ::
    {ok, proplists:proplist()} |
    {error, term()}.

-spec query(string()) -> query_return().
query(Url) -> erlidata_lib_query:query(Url).

-spec query(string(), string()) -> query_return().
query(RootURL, SparqlQuery) -> erlidata_lib_query:query(RootURL, SparqlQuery).

-spec make_query_client(string()) -> fun((string()) -> query_return()).
make_query_client(RootURL) -> erlidata_lib_query:make_query_client(RootURL).

-spec get_entity(string()) -> query_return().
get_entity(Url) -> erlidata_lib_entity:get_entity(Url).

-spec get_entity(string(), string()) -> query_return().
get_entity(RootURL, EntityId) -> erlidata_lib_entity:get_entity(RootURL, EntityId).

-spec make_entity_client(string()) -> fun((string()) -> query_return()).
make_entity_client(RootURL) -> erlidata_lib_entity:make_entity_client(RootURL).

-spec get_property(string()) -> query_return().
get_property(Url) -> erlidata_lib_property:get_property(Url).

-spec get_property(string(), string()) -> query_return().
get_property(RootUrl, PropertyId) -> erlidata_lib_property:get_property(RootUrl, PropertyId).

-spec get_property_client(string()) -> fun((string) -> query_return()).
get_property_client(RootUrl) -> erlidata_lib_property:make_property_client(RootUrl).
