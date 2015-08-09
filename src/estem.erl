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
-module(estem).
-author('elbrujohalcon@inaka.net').
-author('arcusfelis@gmail.com').

-behaviour(application).

-export([start/0, stop/0]).
-export([start/2, stop/1]).

%%-------------------------------------------------------------------
%% ADMIN API
%%-------------------------------------------------------------------
%% @doc Starts the application
-spec start() -> ok | {error, {already_started, ?MODULE}}.
start() -> application:start(?MODULE).

%% @doc Stops the application
-spec stop() -> ok.
stop() -> application:stop(?MODULE).

%%-------------------------------------------------------------------
%% BEHAVIOUR CALLBACKS
%%-------------------------------------------------------------------
%% @private
-spec start(any(), any()) -> {ok, pid()}.
start(_StartType, _StartArgs) -> estem_sup:start_link().

%% @private
-spec stop(any()) -> ok.
stop(_State) -> estem_java:stop().
