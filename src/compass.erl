%%% @author Ivan Iacono <ivan.iacono@erlang-solutions.com> - Erlang Solutions Ltd
%%% @copyright (C) 2013, Erlang Solutions Ltd
%%% @doc This is the erlang implementation for the hmcxxx compass module.
%%% @end

-module(compass).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([read/0, rotate_degrees/2]).

-define(SERVER, ?MODULE). 

-define(ADDR, 16#21).
-define(I2C_CHANNEL, i2c1).
-define(READ_CMD, 16#41).

-define(DEFECT, 30).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

read() ->
    gen_server:call(?SERVER, {call, read}).

stop() ->
    gen_server:cast(?SERVER, stop).

rotate_degrees(Degrees, Direction) ->
    gen_server:cast(?SERVER, {cast, rotate_degrees, Degrees, Direction, self()}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    i2c:start_link({?I2C_CHANNEL, "/dev/i2c-1"}),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({call, read}, _From, State) ->
    Reply = compass_read(),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({cast, rotate_degrees, Degrees, Direction, From}, State) ->
    Read = compass_read() - 10,
    io:format("Initial Degrees: ~p~n",[Read]),
    Reply = wait_degrees(Degrees, Direction, Read),
    From ! {rotate_degrees, Reply},
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

wait_degrees(Degrees, right, CurrentDegrees) when (CurrentDegrees + Degrees) > 359 ->
    wait_degrees((CurrentDegrees + Degrees) - 359, CurrentDegrees);
wait_degrees(Degrees, right, CurrentDegrees) ->
    wait_degrees(CurrentDegrees + Degrees, CurrentDegrees);
wait_degrees(Degrees, left, CurrentDegrees) when (CurrentDegrees - Degrees) < 0 ->
    wait_degrees(359 + (CurrentDegrees - Degrees), CurrentDegrees);
wait_degrees(Degrees, left, CurrentDegrees) ->
    wait_degrees(CurrentDegrees - Degrees, CurrentDegrees).

wait_degrees(Degrees, CurrentDegrees) when CurrentDegrees >= Degrees - 1, CurrentDegrees =< Degrees + 1 ->
    io:format("Stop Degrees: ~p~n",[Degrees]),
    io:format("Current Degrees: ~p~n",[compass_read()]),
    true;
wait_degrees(Degrees, _CurrentDegrees) ->
    wait_degrees(Degrees, compass_read()).

compass_read() ->
    i2c:write(?I2C_CHANNEL, ?ADDR, {?READ_CMD}),
    timer:sleep(10),
    {Byte1, Byte2} = i2c:read(?I2C_CHANNEL, ?ADDR, 2),
    ((Byte1 bsl 8) + Byte2) div 10.
