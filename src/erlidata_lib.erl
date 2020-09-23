-module(erlidata_lib).

-export([query/1, query/2, make_query_client/1]).
-export_type([query_return/0]).

-type query_return() ::
    {ok, proplists:proplist()} |
    {error, string() | term()}.

-spec query(string()) -> query_return().
query(Url) -> erlidata_lib_query:query(Url).

-spec query(string(), string()) -> query_return().
query(RootURL, SparqlQuery) -> erlidata_lib_query:query(RootURL, SparqlQuery).

-spec make_query_client(string()) -> fun((string()) -> query_return()).
make_query_client(RootURL) -> erlidata_lib_query:make_query_client(RootURL).