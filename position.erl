-module(position).

-behaviour(gen_server).

%% API
-export([start_link/0, stop_server/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([go_to_point/2, position/0]).

-define(SERVER, ?MODULE). 

-define(D90, 535). %% 90 degrees in milliseconds
-define(D180, 1222). %% 180 degrees in milliseconds

-record(position, 
	{x, y, theta,
	 pending,
	 actions=[]}).

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

go_to_point(X, Y) ->
    gen_server:call(?SERVER, {call, go_to_point, X, Y}, infinity).

position() ->
    gen_server:call(?SERVER, {call, position}).

stop_server() ->
    gen_server:cast(?SERVER, {cast, stop}).
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
    i2c_servo:init(),
    i2c_servo:setPWMFreq(60),
    sharp:start_link(),
    motor:start_link(),
    {ok, #position{}}.

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
handle_call({call, position}, _From, State) ->
    Reply = {State#position.x, State#position.y, State#position.theta},
    {reply, Reply, State};

handle_call({call, go_to_point, X, Y}, _From, State) when State#position.x =< X, State#position.y =< Y ->
    DeltaX = X - State#position.x, 
    DeltaY = Y - State#position.y,

    %% Actions0 = [{set, theta, 0},
    %% 		{forward,DeltaX},
    %% 		{update,x,DeltaX}, 
    %% 		{rotate, left, ?D90}, 
    %% 		{forward, DeltaY},
    %% 	        {update, y, DeltaY},
    %% 		{set, theta, 90}],
    
    case State#position.theta of
	90 ->
	    motor:rotate(?D90, right);
	180 ->
	    motor:rotate(?D180, right);
	270 ->
	    motor:rotate(?D90, left);
	0 ->
	    ok
    end,
    sharp:alarm_obstacle(motor, stop, []),
    Xnew = motor:forward(DeltaX),
    motor:rotate(?D90, left),
    Ynew = motor:forward(DeltaY),
    NewState = #position{x = State#position.x + Xnew, y = State#position.y + Ynew, theta = 90},
    Reply = {Xnew, Ynew},
    {reply, Reply, NewState};
handle_call({call, go_to_point, X, Y}, _From, State) when State#position.x >= X, State#position.y >= Y ->
    DeltaX = State#position.x - X,
    DeltaY = State#position.y - Y,

    case State#position.theta of
	0 ->
	    motor:rotate(?D180, right);
	90 ->
	    motor:rotate(?D90, left);
	270 ->
	    motor:rotate(?D90, right);
	180 ->
	    ok
    end,
    sharp:alarm_obstacle(motor, stop, []),
    Xnew = motor:forward(DeltaX),
    motor:rotate(?D90, left),
    Ynew = motor:forward(DeltaY),
    NewState = #position{x = State#position.x - Xnew, y = State#position.y - Ynew, theta = 270},
    Reply = {Xnew, Ynew},
    {reply, Reply, NewState};
handle_call({call, go_to_point, X, Y}, _From, State) when State#position.x >= X, State#position.y =< Y ->
    DeltaX = State#position.x - X,
    DeltaY = Y - State#position.y,
    
    case State#position.theta of
	0 ->
	    motor:rotate(?D180, left);
	90 ->
	    motor:rotate(?D90, left);
	270 ->
	    motor:rotate(?D90, right);
	180 ->
	    ok
    end,
    sharp:alarm_obstacle(motor, stop, []),
    Xnew = motor:forward(DeltaX),
    motor:rotate(?D90, right),
    Ynew = motor:forward(DeltaY),
    NewState = #position{x = State#position.x - Xnew, y = State#position.y + Ynew, theta = 90},
    Reply = {Xnew, Ynew},
    {reply, Reply, NewState};
handle_call({call, go_to_point, X, Y}, _From, State) when State#position.x =< X, State#position.y >= Y ->
    DeltaX = X - State#position.x,
    DeltaY = State#position.y - Y,

    case State#position.theta of
	0 ->
	    ok;
	90 ->
	    motor:rotate(?D90, right);
	180 ->
	    motor:rotate(?D180, left);
	270 ->
	    motor:rotate(?D90, left)
    end,
    sharp:alarm_obstacle(motor, stop, []),
    Xnew = motor:forward(DeltaX),
    motor:rotate(?D90, right),
    Ynew = motor:forward(DeltaY),
    NewState = #position{x = State#position.x + Xnew, y = State#position.y - Ynew, theta = 270},
    Reply = {Xnew, Ynew},
    {reply, Reply, NewState}.


%% handle_call({call, stop}, _From, State#position{actions=[Next|Actions]}) ->
%%     motor_stop(),
%%     S1 = update_state(Next, State),
%%     case Actions of
%% 	[] ->
%% 	    Reply = ok,
%% 	    {reply, Reply, S1#position{actions=[]}};
%% 	[Action|Rest] ->
%% 	    start_action(Action), 
%% 	    {noreply, S1#position{actions=Rest}}
%%     end.

%% update_state({set, theta, V}, S) ->
%%     S#state{theta=V}.

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
handle_cast({cast, stop}, State) ->
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
