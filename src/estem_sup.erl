%%%-------------------------------------------------------------------
%%% @author Mikhail Uvarov <arcusfelis@gmail.com>
%%% Mikhail is an author of the bindings
%%%
%%% @author Fernando Benavides <elbrujohalcon@inaka.net>
%%% Fernando is an author of "JInterface Sample Application"
%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
%%% Copyright 2015 Mikhail Uvarov
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%%     Unless required by applicable law or agreed to in writing, software
%%%     distributed under the License is distributed on an "AS IS" BASIS,
%%%     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%%     See the License for the specific language governing permissions and
%%%     limitations under the License.
%%%
%%%-------------------------------------------------------------------
-module(estem_sup).
-author('elbrujohalcon@inaka.net').
-author('arcusfelis@gmail.com').

-behaviour(supervisor).

-export([start_link/0, init/1]).

%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------
%% @doc  Starts a new supervisor
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%-------------------------------------------------------------------
%% SUPERVISOR API
%%-------------------------------------------------------------------
%% @hidden
%%NOTE: If the process dies... it can't be restarted, because all information
%%		stored in the java process may or may not be lost.
-spec init([]) -> {ok, {{one_for_one, 0, 1}, [supervisor:child_spec()]}}.
init([]) ->
  {ok,
    {_SupFlags = {one_for_one, 0, 1},
      [
        {estem_java, {estem_java, start_link, []}, permanent, 2000, worker, [estem_java]}
      ]}}.
