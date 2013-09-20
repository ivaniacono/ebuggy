-module(sharp).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, read/0, detect_obstacle/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-define(SPICHANNEL, spi1).
-define(SPIMODE, 0).
-define(SPIBPW, 8).
-define(SPISPEED, 9600).
-define(SPIDELAY, 10).


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

stop() ->
    gen_server:cast(?SERVER, stop).

read() ->
    gen_server:call(?SERVER, {call, read}).

detect_obstacle() ->
    gen_server:cast(?SERVER, {cast, detect_obstacle, self()}).

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
    spi:start_link({?SPICHANNEL, "/dev/spidev0.0"}),   
    spi:config(?SPICHANNEL, ?SPIMODE, ?SPIBPW, ?SPISPEED, ?SPIDELAY),
    {ok, []}.

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
    Reply = read_distance(),
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
handle_cast({cast, detect_obstacle, From}, State) ->
    Reply = wait_obstacle(read_distance()),
    From ! {detect_obstacle, Reply},
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

read_distance() ->
    {_, Val1, Val2} = spi:transfer(?SPICHANNEL, {1, 2 bsl 6, 0}, 3),
    Value = ((Val1 band 31) bsl 6) + (Val2 bsr 2),
    Volts = Value * (3.3 / 1024),
    case Volts < 0.2 of
	true ->
	    {error, out_of_range};
	false ->
	    41.543 * math:pow(Volts + 0.30221, -1.5281) 
    end.

wait_obstacle({error, out_of_range}) ->
    timer:sleep(10),
    wait_obstacle(read_distance());
wait_obstacle(Distance) when Distance =< 6.5 ->
    true;
wait_obstacle(_Distance) ->
    timer:sleep(10),
    wait_obstacle(read_distance()).
