# erlidata_lib

An OTP Library for consuming Wikidata in a sane fashion

## Usage

### Entities

#### Get an entity by URL
```erl
> erlidata_lib:get_entity("http://www.wikidata.org/entity/Q151345").
```

#### Get an entity specifying a Root URL
```erl
> erlidata_lib:get_entity("http://www.wikidata.org/entity/", "Q151345").
```

#### Make and use an entity client
```erl
> EntityClient = erlidata_lib:make_entity_client("http://www.wikidata.org/entity/").
...
> EntityClient("Q151345").
```

### Properties

#### Get a property by URL
```erl
> erlidata_lib:get_entity("http://www.wikidata.org/entity/P31").
```

#### Get a property specifying a Root URL
```erl
> erlidata_lib:get_entity("http://www.wikidata.org/entity/", "P31").
```

#### Make and use a property client
```erl
> EntityClient = erlidata_lib:make_entity_client("http://www.wikidata.org/entity/").
...
> EntityClient("P31").
```

### Queries

#### Make a query
```erl
> erlidata_lib:query("https://query.wikidata.org/sparql?query=%23Goats%0ASELECT%20%3Fitem%20%3FitemLabel%20WHERE%20%7B%20%3Fitem%20wdt%3AP31%20wd%3AQ2934.%20SERVICE%20wikibase%3Alabel%20%7B%20bd%3AserviceParam%20wikibase%3Alanguage%20%22%5BAUTO_LANGUAGE%5D%2Cen%22.%20%7D%7D").
```

#### Make a query specifying a Root URL
```erl
> erlidata_lib:query("https://query.wikidata.org/", "sparql?query=%23Goats%0ASELECT%20%3Fitem%20%3FitemLabel%20WHERE%20%7B%20%3Fitem%20wdt%3AP31%20wd%3AQ2934.%20SERVICE%20wikibase%3Alabel%20%7B%20bd%3AserviceParam%20wikibase%3Alanguage%20%22%5BAUTO_LANGUAGE%5D%2Cen%22.%20%7D%7D").
```

#### Make and use a query client
```
> QueryClient = erlidata_lib:make_query_client("https://query.wikidata.org/").
....
> QueryClient("sparql?query=%23Goats%0ASELECT%20%3Fitem%20%3FitemLabel%20WHERE%20%7B%20%3Fitem%20wdt%3AP31%20wd%3AQ2934.%20SERVICE%20wikibase%3Alabel%20%7B%20bd%3AserviceParam%20wikibase%3Alanguage%20%22%5BAUTO_LANGUAGE%5D%2Cen%22.%20%7D%7D").
```

## Build
```bash
> rebar3 compile
```

## Load in a shell
```bash
> rebar3 auto
```
