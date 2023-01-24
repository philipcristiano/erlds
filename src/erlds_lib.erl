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

-module(erlds_lib).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-export([
    lookup/1,
    mutate/1,
    query/3
]).

-define(GOTH_TOKEN, 'Elixir.Goth.Token').
-define(SCOPE_URL, <<"https://www.googleapis.com/auth/datastore">>).
-define(DATASTORE_CONNECTION, 'Elixir.GoogleApi.Datastore.V1.Connection').
-define(GOOGLE_API, 'Elixir.GoogleApi.Datastore.V1.Api.Projects').
-define(ATTR, #{attributes => #{<<"peer.service">> => <<"Datastore">>}}).

mutate(Mutations) when is_list(Mutations) ->
    Conn = get_connection(),
    ProjectID = erlds_config:gcp_project_id(),
    {ok, #{transaction := TID}} = ?GOOGLE_API:datastore_projects_begin_transaction(
        Conn, ProjectID, [], []
    ),
    CommitRequest = #{mutations => Mutations, transaction => TID},

    ?with_span(<<"mutate">>, ?ATTR, fun(_Ctx) ->
        ?GOOGLE_API:datastore_projects_commit(Conn, ProjectID, [{body, CommitRequest}], [])
    end).

lookup(LookupRequest) ->
    Conn = get_connection(),
    ProjectID = erlds_config:gcp_project_id(),

    ?with_span(<<"lookup">>, ?ATTR, fun(_Ctx) ->
        ?GOOGLE_API:datastore_projects_lookup(Conn, ProjectID, [{body, LookupRequest}], [])
    end).

query(PartitionID, Query, NamedBindings) ->
    GQLQuery = #{queryString => Query, named_bindings => NamedBindings, allow_literals => true},
    Request = #{gqlQuery => GQLQuery, partitionId => PartitionID},
    ?LOG_DEBUG(#{
        message => q_query,
        named_binding => NamedBindings,
        q => Query
    }),

    Conn = get_connection(),
    ProjectID = erlds_config:gcp_project_id(),

    ?with_span(<<"datastore_projects_run_query">>, ?ATTR, fun(_Ctx) ->
        {ok, #{
            % Struct doesn't seem to work with dialyzer
            %'__struct__' := 'Elixir.GoogleApi.Datastore.V1.Model.RunQueryResponse',
            batch := #{entityResults := EntityResults}
        }} = ?GOOGLE_API:datastore_projects_run_query(Conn, ProjectID, [{body, Request}]),
        {ok, EntityResults}
    end).

get_connection() ->
    ?with_span(<<"get_datastore_connection">>, ?ATTR, fun(_Ctx) ->
        {ok, #{token := Token}} = ?GOTH_TOKEN:for_scope(?SCOPE_URL),
        Conn = ?DATASTORE_CONNECTION:new(Token),
        Conn
    end).
