-module(sharp).

-behaviour(gen_server).

%% API
-export([start_link/0,
         stop/0,
         read/0,
         alarm_obstacle/0,
         ignore_obstacle/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-define(SPICHANNEL, spi1).
-define(SPIMODE, 0).
-define(SPIBPW, 8).
-define(SPISPEED, 9600).
-define(SPIDELAY, 10).

%% All that are interested in knowing if sharp has detected anything
%% will be on the listener list. When sharp detects something a
%% message is sent to all listeners and the list will be cleared.
-record(state,
        { listeners = [] :: [pid()]
        }).

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

alarm_obstacle() ->
    gen_server:cast(?SERVER, {alarm_obstacle, self()}).

ignore_obstacle() ->
    gen_server:cast(?SERVER, {ignore_obstacle, self()}).

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
    {ok, _} = chronos:start_link(sharp_ts),
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

%% handle_cast({alarm_obstacle, From}, #state{listeners=[]}=State) ->
%% %%    start_timer(),
%%     {noreply, State#state{listeners=[From]}};

handle_cast({alarm_obstacle, From}, #state{listeners=Ls}=State) ->
    start_timer(),
    {noreply, State#state{listeners=[From|Ls]}};

handle_cast({ignore_obstacle, From}, #state{listeners=Ls}=State) ->
    {noreply, State#state{listeners=lists:delete(From, Ls)}};

handle_cast(read_distance, #state{listeners=Ls}=State) ->
    case obstacle_present() of
        true ->
%%	    lists:foreach(fun() ->
            lists:map(fun(L) ->
			      L ! obstacle_present
		      end,
		      Ls),
            {noreply, State#state{listeners=[]}};
        false ->
	    start_timer(),
            {noreply, State}
    end;
         
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
    stop_timer(),
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

obstacle_present() ->
    case read_distance() of
        Distance when Distance =< 6.5 ->
            true;
        _ ->
            false
    end.

start_timer() ->
    chronos:start_timer(sharp_ts, read_timer, 10, {gen_server, cast, [?SERVER, read_distance]}).

stop_timer() ->
    chronos:stop_timer(sharp_ts, read_timer).
