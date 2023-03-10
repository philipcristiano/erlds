-module(erlds_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, erlds).
-define(MOCK_MODS, [erlds_config, erlds_lib]).

put_item_test() ->
    erlds_meck:load(?MOCK_MODS),
    PartitionID = <<"test_partition_id">>,
    ProjectID = <<"test_project_id">>,
    Path = [{<<"kind">>, <<"id">>}],
    Values = #{key => <<"value">>},

    ok = meck:expect(erlds_lib, mutate, ['_'], {ok, undefined}),
    ok = meck:expect(erlds_config, gcp_project_id, [], ProjectID),

    ok = ?MUT:put_item(PartitionID, Path, Values),

    [{mutate, CallArgs}] = erlds_meck:history_calls(erlds_lib),

    ?assertMatch(
        [
            [
                #{
                    upsert :=
                        #{
                            key :=
                                #{
                                    path :=
                                        [#{kind := <<"kind">>, name := <<"id">>}]
                                },
                            properties :=
                                #{key := #{stringValue := <<"value">>}}
                        }
                }
            ]
        ],
        CallArgs
    ),

    erlds_meck:unload(?MOCK_MODS).

get_item_test() ->
    erlds_meck:load(?MOCK_MODS),

    PartitionID = <<"test_partition_id">>,
    ProjectID = <<"test_project_id">>,
    Path = [{<<"kind">>, <<"id">>}],
    % Values = #{key => <<"value">>},
    Return = #{
        '__struct__' => 'Elixir.GoogleApi.Datastore.V1.Model.LookupResponse',
        found => [
            #{
                '__struct__' => 'Elixir.GoogleApi.Datastore.V1.Model.EntityResult',
                entity =>
                    #{
                        '__struct__' => 'Elixir.GoogleApi.Datastore.V1.Model.Entity',
                        properties => #{<<"key">> => string_value(<<"value">>)}
                    }
            }
        ]
    },

    ok = meck:expect(erlds_lib, lookup, ['_'], {ok, Return}),
    ok = meck:expect(erlds_config, gcp_project_id, [], ProjectID),

    {ok, Item} = ?MUT:get_item(PartitionID, Path),

    [{lookup, CallArgs}] = erlds_meck:history_calls(erlds_lib),

    ?assertMatch(
        [
            #{
                keys :=
                    [
                        #{
                            path :=
                                [#{kind := <<"kind">>, name := <<"id">>}]
                        }
                    ]
            }
        ],
        CallArgs
    ),
    ?assertMatch(#{key := <<"value">>}, Item),

    erlds_meck:unload(?MOCK_MODS).

string_value(Val) ->
    #{
        '__struct__' => 'Elixir.GoogleApi.Datastore.V1.Model.Value',
        blobValue => nil,
        stringValue => Val
    }.
