%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <elbrujohalcon@inaka.net>
%%% @doc Java Interface.
%%% @end
%%%-------------------------------------------------------------------
-module(estem_java).
-author('elbrujohalcon@inaka.net').

%%NOTE: We let java server run for as long as it needs to run.
%%      Even if we choose a smaller timeout, it will run for a longer time anyway if it needs to.
-define(CALL_TIMEOUT, infinity).
-define(JAVA_SERVER, {estem_server, java_node()}).

-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0]).

-export([pid/0, stem/1]).

-record(state, {java_port :: port(),
                java_node :: atom()}).
-type state() :: #state{}.

%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------
%% @doc  Starts a new monitor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Returns the pid of the java process
-spec pid() -> pid().
pid() -> gen_server:call(?JAVA_SERVER, {pid}, ?CALL_TIMEOUT).

-spec stem(binary()) -> [binary()].
stem(Binary) -> gen_server:call(?JAVA_SERVER, {stem, Binary}, ?CALL_TIMEOUT).

%% @doc Stops the java process
-spec stop() -> ok.
stop() -> gen_server:cast(?JAVA_SERVER, {stop}).

%%-------------------------------------------------------------------
%% GEN_SERVER API
%%-------------------------------------------------------------------
%% @private
-spec init([]) -> {ok, state()}.
init([]) ->
  case os:find_executable("java") of
    [] ->
      throw({stop, java_missing});
    Java ->
      ThisNode = this_node(),
      JavaNode = java_node(),
      {ok, App} = application:get_application(),
      Priv = priv_dir(App),
      Classpath = string:join([otp_lib("/OtpErlang.jar") | filelib:wildcard(Priv ++ "/*.jar")], ":"),
      Port =
        erlang:open_port({spawn_executable, Java},
                         [{line, 1000}, stderr_to_stdout,
                          {args, ["-classpath", Classpath,
                                  "estem.EstemNode",
                                  ThisNode, JavaNode, erlang:get_cookie()]}]),
      wait_for_ready(#state{java_port = Port, java_node = JavaNode})
  end.

wait_for_ready(State = #state{java_port = Port}) ->
  receive
    {Port, {data, {eol, "READY"}}} ->
      _ = lager:notice("Java node started"),
      true = link(pid()),
      true = erlang:monitor_node(State#state.java_node, true),
      {ok, State};
    Info ->
      case handle_info(Info, State) of
        {noreply, NewState} ->
          wait_for_ready(NewState);
        {stop, Reason, _NewState} ->
          {stop, Reason}
      end
  end.

%% @private
-spec handle_call(_Call, _From, state()) -> {reply, undefined, state()}.
handle_call(_Call, _From, State) -> {reply, undefined, State}.

%% @private
-spec handle_info(_Info, state()) -> {stop, nodedown, state()} | {noreply, state()}.
handle_info({nodedown, JavaNode}, State = #state{java_node = JavaNode}) ->
  lager:error("Java node is down!"),
  {stop, nodedown, State};
handle_info({Port, {data, {eol, "SEVERE: " ++ JavaLog}}}, State = #state{java_port = Port}) ->
  _ = lager:error("Java Error:\t~s", [JavaLog]),
  {noreply, State};
handle_info({Port, {data, {eol, "WARNING: " ++ JavaLog}}}, State = #state{java_port = Port}) ->
  _ = lager:warning("Java Warning:\t~s", [JavaLog]),
  {noreply, State};
handle_info({Port, {data, {eol, "INFO: " ++ JavaLog}}}, State = #state{java_port = Port}) ->
  _ = lager:info("Java Info:\t~s", [JavaLog]),
  {noreply, State};
handle_info({Port, {data, {eol, JavaLog}}}, State = #state{java_port = Port}) ->
  _ = lager:debug("Java Log:\t~s", [JavaLog]),
  {noreply, State};
handle_info({Port, {data, {noeol, JavaLog}}}, State = #state{java_port = Port}) ->
  _ = lager:info("Java Log:\t~s...", [JavaLog]),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

%% @private
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.
%% @private
-spec terminate(_, state()) -> true.
terminate(_Reason, State) -> erlang:port_close(State#state.java_port).
%% @private
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%-------------------------------------------------------------------
%% PRIVATE
%%-------------------------------------------------------------------
%% @private
priv_dir(App) ->
  case code:priv_dir(App) of
    {error, bad_name} ->
      lager:info("Couldn't find priv dir for ~p, using ./priv", [App]), "./priv";
    PrivDir ->
      filename:absname(PrivDir)
  end.

%% @private
%% @doc returns the absolute path to the otp erlang JAR
otp_lib(Path) ->
  JPriv = priv_dir(jinterface),
  test_priv_path(Path, file:read_file_info(JPriv ++ Path), JPriv ++ Path).

test_priv_path(_, {ok, _}, Absolute_Path) -> Absolute_Path;
test_priv_path(Path, {error, _}, _) -> filename:absname(code:lib_dir() ++ Path).

this_node() -> atom_to_list(node()).

java_node() ->
  case string:tokens(atom_to_list(node()), "@") of
    [Name, Server] -> list_to_atom(Name ++ "_java@" ++ Server);
    _Node -> throw({bad_node_name, node()})
  end.
