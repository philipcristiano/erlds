-module(erlds_config).

-export([gcp_project_id/0]).

gcp_project_id() ->
    erlang:list_to_binary(os:getenv("GCP_PROJECT_ID", "UNKNOWN")).
