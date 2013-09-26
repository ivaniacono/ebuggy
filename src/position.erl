-module(position).

-behaviour(gen_server).

%% API
-export([start_link/0, stop_server/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([go_to_point/2, now/0, stop_motor/1]).

-define(SERVER, ?MODULE). 

-define(D90, 535). %% 90 degrees in milliseconds
-define(D180, 1222). %% 180 degrees in milliseconds

-record(position, 
	{x, y, theta,
	 requestor,
	 pending,
	 timeout,
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
    gen_server:call(?SERVER, {go_to_point, X, Y}, infinity).

now() ->
    gen_server:call(?SERVER, now).

stop_motor(Time) ->
    gen_server:call(?SERVER, {stop_motor, Time}).

stop_server() ->
    gen_server:cast(?SERVER, stop).
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
    {ok, _} = chronos:start_link(motor_ts),
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
handle_call(now, _From, State) ->
    Reply = {State#position.x, State#position.y, State#position.theta},
    {reply, Reply, State};

handle_call({go_to_point, X, Y}, From, State) when State#position.x =< X, State#position.y =< Y ->
    DeltaX = X - State#position.x, 
    DeltaY = Y - State#position.y,

    Actions1 = [{execute, theta, 0},
    		{execute, forward, DeltaX},
    		{update, x, DeltaX}, 
    		{execute, rotate, left, ?D90}, 
    		{execute, forward, DeltaY},
    	        {update, y, DeltaY},
    		{update, theta, 90}],

    NewState = start_actions(Actions1, State),
    {noreply, NewState#position{actions = Actions1, requestor = From, pending = actions1}};

handle_call({go_to_point, X, Y}, From, State) when State#position.x >= X, State#position.y >= Y ->
    DeltaX = State#position.x - X,
    DeltaY = State#position.y - Y,

    Actions2 = [{execute, theta, 180},
    		{execute, forward, DeltaX},
    		{update, x, DeltaX}, 
    		{execute, rotate, left, ?D90}, 
    		{execute, forward, DeltaY},
    	        {update, y, DeltaY},
    		{update, theta, 270}],    

    NewState = start_actions(Actions2, State),
    {noreply, NewState#position{actions = Actions2, requestor = From, pending = actions2}};

handle_call({go_to_point, X, Y}, From, State) when State#position.x >= X, State#position.y =< Y ->
    DeltaX = State#position.x - X,
    DeltaY = Y - State#position.y,

    Actions3 = [{execute, theta, 180},
    		{execute, forward, DeltaX},
    		{update, x, DeltaX}, 
    		{execute, rotate, right, ?D90}, 
    		{execute, forward, DeltaY},
    	        {update, y, DeltaY},
    		{update, theta, 90}],

    NewState = start_actions(Actions3, State),
    {noreply, NewState#position{actions=Actions3, requestor = From, pending = actions3}};

handle_call({go_to_point, X, Y}, From, State) when State#position.x =< X, State#position.y >= Y ->
    DeltaX = X - State#position.x,
    DeltaY = State#position.y - Y,

    Actions4 = [{execute, theta, 0},
    		{execute, forward, DeltaX},
    		{update, x, DeltaX}, 
    		{execute, rotate, right, ?D90}, 
    		{execute, forward, DeltaY},
    	        {update, y, DeltaY},
    		{update, theta, 270}],

    NewState = start_actions(Actions4, State),
    {noreply, NewState#position{actions=Actions4, requestor = From, pending = actions4}};

handle_call({update_state, NewState}, _From, _State) ->
    State = start_actions(NewState#position.actions, NewState),
    {noreply, State};

handle_call({stop_motor, Time}, _From, State#position{actions = [H | T]}) ->
    motor:stop(),
    case element(1, H) of
	execute ->
	    NewState = execute_action(H, State),
	    {noreply, NewState#position{actions = Actions, timeout = Time}};
	update ->
	    NewState = execute_action(H, State),
	    gen_server:call(?SERVER, {update_state, NewState})
    end;
handle_call({stop_motor, Time}, _From, State#position{actions = []}) ->
    motor:stop(),
    gen_server:reply(State#position.requestor, {State#position.x, State#position.y}),
    {reply, ok, State#position{timeout = Time}}.

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
handle_info(obstacle_present, State) ->
    {ok, Time} = chronos:stop_timer(motor_ts, stop_motor),
    gen_server:call(?SERVER, {stop_motor, Time}),
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

execute_action({execute, forward, Time}, State) ->
    sharp:alarm_obstacle(),
    chronos:start_timer(motor_ts, motor_stop, Time, {position, stop_motor, [Time]}),
    motor:forward(),
    State;
execute_action({execute, rotate, Direction, Time}, State) ->
    sharp:alarm_obstacle(),
    chronos:start_timer(motor_ts, motor_stop, Time, {position, stop_motor, [Time]}),
    motor:rotate(Direction),
    State;
execute_action({execute, theta, 0}, State) ->
    case State#position.theta of
       90 ->
	   execute_action({execute, rotate, right, ?D90}, State);
%%	   motor:rotate(?D90, right);
       180 ->
	   execute_action({execute, rotate, right, ?D180}, State);
%%	   motor:rotate(?D180, right);
       270 ->
	   execute_action({execute, rotate, left, ?D90}, State);
%%	   motor:rotate(?D90, left);
       0 ->
	   ok
    end;
execute_action({execute, theta, 180}, State) ->
    case State#position.theta of
	0 ->
	    execute_action({execute, rotate, right, ?D180}, State);
%%	    motor:rotate(?D180, right);
	90 ->
	    execute_action({execute, rotate, left, ?D90}, State);
%%	    motor:rotate(?D90, left);
	270 ->
	    execute_action({execute, rotate, right, ?D90}, State);
%%	    motor:rotate(?D90, right);
	180 ->
	    ok
    end;
%% update x: start point is =< of end point
execute_action({update, x, DeltaX}, 
	       State#position{x = Xold, timeout = Timeout, pending = Pending}) when Pending == actions1;
										    Pending == actions4 ->
    State#position{x = Xold + Timeout};
%% update x: start point is >= of end point
execute_action({update, x, DeltaX},
	       State#position{x = Xold, timeout = Timeout, pending = Pending}) when Pending == actions2;
										    Pending == actions3 ->
    State#position{x = Xold - Timeout};
%% update y: start point is =< of end point
execute_action({update, y, DeltaY}, 
	       State#position{y = Yold, timeout = Timeout, pending = Pending}) when Pending == actions1;
										    Pending == actions3 ->
    State#position{y = Yold + Timeout};
%% update y: start point is >= of end point
execute_action({update, y, DeltaY}, 
	       State#position{y = Yold, timeout = Timeout, pending = Pending}) when Pending == actions2;
										    Pending == actions4 ->
    State#position{y = Yold - Timeout};
%% update theta
execute_action({update, theta, V}, State) ->
    State#position{theta=V}.

start_actions([H | T], State) ->
    execute_action(H, State#position{actions = T}).
