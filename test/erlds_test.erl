-module(erlds_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, erlds).
-define(MOCK_MODS, [erlds_config, erlds_lib]).

put_item_test() ->
    erlds_meck:load(?MOCK_MODS),
    Path = [{<<"kind">>, <<"id">>}],
    Values = #{key => <<"value">>},

    ok = meck:expect(erlds_lib, mutate, ['_'], {ok, undefined}),

    ok = ?MUT:put_item(Path, Values),

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

    {ok, Item} = ?MUT:get_item(Path),

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
