% Copyright 2023 Philip Cristiano
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

-module(erlds).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-export([ensure_table/4]).
-export([
    delete_item/1,
    batch_get_items/1,
    new_id/0,
    id_to_binary/1,
    get_item/1,
    put_item/2,
    put_item/3,
    query_kind_by_ancestor/3,
    query_kind_by_ancestor_and_properties/4,
    query_kind_by_properties/3
]).

-export([
    pl_list_to_obj_maps/1,
    pl_to_obj_map/1
]).

-define(GOTH_TOKEN, 'Elixir.Goth.Token').
-define(SCOPE_URL, <<"https://www.googleapis.com/auth/datastore">>).
-define(DATASTORE_CONNECTION, 'Elixir.GoogleApi.Datastore.V1.Connection').
-define(GOOGLE_API, 'Elixir.GoogleApi.Datastore.V1.Api.Projects').

-spec delete_item(list()) -> ok.
delete_item(Path) ->
    Key = #{path => path_to_ds_path(Path)},
    ?LOG_DEBUG(#{
        message => delete_item,
        key => Key
    }),
    Mutation = #{delete => Key},

    case bizops_ds_lib:mutate([Mutation]) of
        {ok, _} -> ok
    end.

ensure_table(_Name, _Attrs, _KeySchema, _Opts) ->
    ok.

-spec put_item(list({binary(), binary()}), map()) -> ok | {error, already_exists}.
put_item(Path, Values) ->
    ?with_span(<<"lookup">>, #{}, fun(_Ctx) ->
        put_item(Path, Values, #{mutation => upsert})
    end).

-spec put_item(list({binary(), binary()}), map(), map()) -> ok | {error, already_exists}.
put_item(Path, Values, #{mutation := Mut}) ->
    Key = #{path => path_to_ds_path(Path)},
    ?LOG_DEBUG(#{
        message => put_item,
        key => Key
    }),
    Props = map_to_ds_props(Values),
    Entity = #{key => Key, properties => Props},
    Mutation = #{Mut => Entity},

    case bizops_ds_lib:mutate([Mutation]) of
        {ok, _} -> ok;
        {error, #{status := 409}} -> {error, already_exists}
    end.

batch_get_items(Paths) ->
    Keys = lists:map(fun(Path) -> #{path => path_to_ds_path(Path)} end, Paths),
    ?LOG_DEBUG(#{
        message => batch_get_item,
        keys => Keys
    }),
    LookupRequest = #{keys => Keys},

    case bizops_ds_lib:lookup(LookupRequest) of
        {ok, LookupResponse} -> lookup_resp_to_obj(LookupResponse)
    end.

-spec get_item(list()) -> {ok, map()} | not_found.
get_item(Path) ->
    ?LOG_DEBUG(#{
        message => get_item,
        path => Path
    }),

    case batch_get_items([Path]) of
        {ok, [Item]} -> {ok, Item};
        {ok, []} -> not_found
    end.

query_kind_by_ancestor(Kind, AncestorKey, _Limit) ->
    Query =
        <<<<"SELECT * FROM ">>/binary, Kind/binary,
            <<" WHERE __key__ HAS ANCESTOR @org_key\n">>/binary>>,

    Args = #{<<"org_key">> => AncestorKey, limit => 50},
    NamedBindings = args_to_named_bindings(Args),

    {ok, EntityResults} = bizops_ds_lib:query(Query, NamedBindings),
    Objs =
        case EntityResults of
            nil -> [];
            Results -> lists:map(fun entity_result_to_obj/1, Results)
        end,

    {ok, Objs}.

-spec build_equality_conditionals(map()) -> binary().
build_equality_conditionals(Properties) ->
    Equalities = maps:fold(
        fun(K, _V, AccIn) ->
            [[K, <<" = @">>, K] | AccIn]
        end,
        [],
        Properties
    ),

    AndedEqualities = lists:join(<<" AND ">>, Equalities),
    Statement = erlang:iolist_to_binary(AndedEqualities),
    Statement.

query_kind_by_ancestor_and_properties(Kind, AncestorKey, Properties, _Limit) ->
    Select =
        <<<<"SELECT * FROM ">>/binary, Kind/binary,
            <<" WHERE __key__ HAS ANCESTOR @org_key AND ">>/binary>>,
    Conditionals = build_equality_conditionals(Properties),

    Query = <<Select/binary, Conditionals/binary>>,

    Args = maps:merge(#{<<"org_key">> => AncestorKey, limit => 50}, Properties),
    ?LOG_DEBUG(#{
        message => query_kind_by_ancestor_and_properties,
        query => Query,
        args => Args
    }),

    NamedBindings = args_to_named_bindings(Args),

    {ok, EntityResults} = bizops_ds_lib:query(Query, NamedBindings),
    Objs =
        case EntityResults of
            nil -> [];
            Results -> lists:map(fun entity_result_to_obj/1, Results)
        end,

    {ok, Objs}.

query_kind_by_properties(Kind, Properties, Limit) ->
    Select =
        <<<<"SELECT * FROM ">>/binary, Kind/binary, <<" WHERE ">>/binary>>,
    Conditionals = build_equality_conditionals(Properties),

    Query = <<Select/binary, Conditionals/binary>>,

    Args = maps:merge(#{limit => Limit}, Properties),
    ?LOG_DEBUG(#{
        message => query_kind_by_properties,
        query => Query,
        args => Args
    }),

    NamedBindings = args_to_named_bindings(Args),

    {ok, EntityResults} = bizops_ds_lib:query(Query, NamedBindings),
    Objs =
        case EntityResults of
            nil -> [];
            Results -> lists:map(fun entity_result_to_obj/1, Results)
        end,

    {ok, Objs}.

args_to_named_bindings(Args) ->
    maps:map(
        fun(_K, Val) ->
            #{value => value_to_ds_prop(Val)}
        end,
        Args
    ).

-spec new_id() -> binary().
new_id() ->
    erlang:list_to_binary(uuid:to_string(uuid:uuid4())).

id_to_binary(ID) ->
    uuid:to_binary(erlang:binary_to_list(ID)).

pl_list_to_obj_maps(LofPLs) ->
    lists:map(fun pl_to_obj_map/1, LofPLs).

pl_to_obj_map(PL) ->
    AtomKeyedMaps = lists:map(fun pl_key_to_atom/1, PL),
    maps:from_list(AtomKeyedMaps).

pl_key_to_atom({K, V}) when is_binary(K) ->
    {erlang:binary_to_atom(K), V}.

path_to_ds_path(P) ->
    lists:map(fun path_item_to_ds/1, P).

map_to_ds_props(M) ->
    maps:map(fun(_K, V) -> value_to_ds_prop(V) end, M).

value_to_ds_prop({b, V}) when is_binary(V) ->
    EncodedValue = base64:encode(V),
    #{blobValue => EncodedValue, excludeFromIndexes => true};
value_to_ds_prop(V) when is_binary(V) ->
    #{stringValue => V};
value_to_ds_prop({{Y, Mo, D}, {H, Mi, S}}) ->
    #{
        timestampValue => #{
            '__struct__' => 'Elixir.DateTime',
            calendar => 'Elixir.Calendar.ISO',
            day => D,
            hour => H,
            microsecond => {0, 0},
            minute => Mi,
            month => Mo,
            second => S,
            std_offset => 0,
            time_zone => <<"Etc/UTC">>,
            utc_offset => 0,
            year => Y,
            zone_abbr => <<"UTC">>
        }
    };
value_to_ds_prop(V = [{_Kind, _ID} | _T]) ->
    Path = path_to_ds_path(V),
    #{keyValue => #{path => Path}};
value_to_ds_prop(V) when is_integer(V) ->
    #{integerValue => V}.

path_item_to_ds({Kind, Name}) ->
    #{kind => Kind, name => Name}.

lookup_resp_to_obj(#{
    % Struct doesn't seem to work with dialyzer
    %'__struct__' := 'Elixir.GoogleApi.Datastore.V1.Model.LookupResponse',
    found := nil
}) ->
    {ok, []};
lookup_resp_to_obj(#{
    % Struct doesn't seem to work with dialyzer
    %'__struct__' := 'Elixir.GoogleApi.Datastore.V1.Model.LookupResponse',
    found := EntityResults
}) ->
    Objs = lists:map(fun entity_result_to_obj/1, EntityResults),
    {ok, Objs}.

entity_result_to_obj(#{
    '__struct__' := 'Elixir.GoogleApi.Datastore.V1.Model.EntityResult', entity := Entity
}) ->
    entity_to_obj(Entity).

entity_to_obj(#{
    '__struct__' := 'Elixir.GoogleApi.Datastore.V1.Model.Entity',
    properties := Properties
}) ->
    ?LOG_DEBUG(#{
        message => entity_properties_to_values,
        props => Properties
    }),
    ds_props_to_values(Properties).

ds_props_to_values(Properties) ->
    maps:fold(
        fun(KeyBin, Prop, Acc) ->
            Key = erlang:binary_to_atom(KeyBin),
            Value = ds_prop_to_value(Prop),
            Acc#{Key => Value}
        end,
        #{},
        Properties
    ).

ds_prop_to_value(
    #{'__struct__' := 'Elixir.GoogleApi.Datastore.V1.Model.Value', blobValue := BlobValue}
) when not is_atom(BlobValue) ->
    base64:decode(BlobValue);
ds_prop_to_value(
    #{
        '__struct__' := 'Elixir.GoogleApi.Datastore.V1.Model.Value',
        timestampValue := #{
            day := D,
            hour := H,
            minute := Mi,
            month := Mo,
            second := S,
            year := Y
        }
    }
) ->
    {{Y, Mo, D}, {H, Mi, S}};
ds_prop_to_value(
    V = #{
        '__struct__' := 'Elixir.GoogleApi.Datastore.V1.Model.Value'
    }
) ->
    ?LOG_DEBUG(#{
        message => ds_prop_to_value,
        value => V
    }),
    MapWithoutStruct = maps:without(['__struct__', excludeFromIndexes, meaning], V),
    Vals = maps:values(MapWithoutStruct),
    ?LOG_DEBUG(#{
        message => ds_prop_to_value,
        values => Vals
    }),
    {value, FoundValue} = lists:search(fun not_nil/1, Vals),
    FoundValue.

not_nil(nil) ->
    false;
not_nil(_) ->
    true.
