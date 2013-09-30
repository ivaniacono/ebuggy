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
	{x = 0, y = 0, theta = 0,
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
    gen_server:cast(?SERVER, {stop_motor, Time}).

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
    application:start(gproc),
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
    		{update, x}, 
    		{execute, rotate, left, ?D90},
    		{execute, forward, DeltaY},
    	        {update, y},
    		{update, theta, 90}],

%%    NewState = start_actions(Actions1, State),
    chronos:start_timer(motor_ts, motor_stop, 50, {position, stop_motor, [0]}),
    {noreply, State#position{actions = Actions1, requestor = From, pending = actions1}};

handle_call({go_to_point, X, Y}, From, State) when State#position.x >= X, State#position.y >= Y ->
    DeltaX = State#position.x - X,
    DeltaY = State#position.y - Y,

    Actions2 = [{execute, theta, 180},
    		{execute, forward, DeltaX},
    		{update, x}, 
    		{execute, rotate, left, ?D90}, 
    		{execute, forward, DeltaY},
    	        {update, y},
    		{update, theta, 270}],    

%%    NewState = start_actions(Actions2, State),
    chronos:start_timer(motor_ts, motor_stop, 50, {position, stop_motor, [0]}),
    {noreply, State#position{actions = Actions2, requestor = From, pending = actions2}};

handle_call({go_to_point, X, Y}, From, State) when State#position.x >= X, State#position.y =< Y ->
    DeltaX = State#position.x - X,
    DeltaY = Y - State#position.y,

    Actions3 = [{execute, theta, 180},
    		{execute, forward, DeltaX},
    		{update, x}, 
    		{execute, rotate, right, ?D90}, 
    		{execute, forward, DeltaY},
    	        {update, y},
    		{update, theta, 90}],

%%    NewState = start_actions(Actions3, State),
    chronos:start_timer(motor_ts, motor_stop, 50, {position, stop_motor, [0]}),
    {noreply, State#position{actions=Actions3, requestor = From, pending = actions3}};

handle_call({go_to_point, X, Y}, From, State) when State#position.x =< X, State#position.y >= Y ->
    DeltaX = X - State#position.x,
    DeltaY = State#position.y - Y,

    Actions4 = [{execute, theta, 0},
    		{execute, forward, DeltaX},
    		{update, x}, 
    		{execute, rotate, right, ?D90}, 
    		{execute, forward, DeltaY},
    	        {update, y},
    		{update, theta, 270}],

%%    NewState = start_actions(Actions4, State),
    chronos:start_timer(motor_ts, motor_stop, 50, {position, stop_motor, [0]}),
    {noreply, State#position{actions=Actions4, requestor = From, pending = actions4}}.

%% handle_call({stop_motor, Time}, _From, State) when State#position.actions == []->
%%     motor:stop(),
%%     gen_server:reply(State#position.requestor, {State#position.x, State#position.y}),
%%     {reply, ok, State#position{timeout = Time}};
%% handle_call({stop_motor, Time}, _From, State) ->
%%     motor:stop(),
%%     [H | T] = State#position.actions,
%%     case element(1, H) of
%% 	execute ->
%%             io:format("executing: ~p~n",[H]),
%% 	    NewState = execute_action(H, State),
%% 	    {noreply, NewState#position{actions = T, timeout = Time}};
%% 	update ->
%% 	    io:format("executing: ~p~n",[H]),
%% 	    NewState = execute_action(H, State),
%% 	    gen_server:cast(?SERVER, {update_state, NewState#position{actions = T, timeout = 0}})
%%     end.

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
%% handle_cast({update_state, NewState}, _State) ->
%%     S = start_actions(NewState#position.actions, NewState),
%%     {noreply, S};

handle_cast({stop_motor, Time}, State) when State#position.actions == []->
    motor:stop(),
    gen_server:reply(State#position.requestor, {State#position.x, State#position.y}),
    {noreply, State#position{timeout = Time}};
handle_cast({stop_motor, Time}, State) ->
    motor:stop(),
    [H | T] = State#position.actions,
    S = State#position{actions = T, timeout = Time},
    case element(1, H) of
	execute ->
            io:format("executing: ~p~n",[H]),
%%	    NewState = execute_action(H, State),
	    execute_action(H, S),
%%	    {noreply, NewState#position{actions = T, timeout = Time}};
	    {noreply, S};
	update ->
	    io:format("executing: ~p~n",[H]),
	    UpdatedState = execute_action(H, S),
%%	    gen_server:cast(?SERVER, {update_state, NewState#position{actions = T, timeout = 0}})
	    chronos:start_timer(motor_ts, motor_stop, 50, {position, stop_motor, [Time]}),	    
%%	    {noreply, NewState#position{actions = T, timeout = 0}}
	    {noreply, UpdatedState}
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
	   chronos:start_timer(motor_ts, motor_stop, 0, {position, stop_motor, [0]})
%%	   ok
    end,
    State;
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
	    chronos:start_timer(motor_ts, motor_stop, 0, {position, stop_motor, [0]})
    end,
    State;
%% update x: start point is =< of end point
execute_action({update, x}, State) when State#position.pending == actions1; 
						State#position.pending == actions4 ->
    State#position{x = State#position.x + State#position.timeout, timeout = 0};

%% update x: start point is >= of end point
execute_action({update, x}, State) when State#position.pending == actions2;
						State#position.pending == actions3 ->
    State#position{x = State#position.x - State#position.timeout, timeout = 0};
%% update y: start point is =< of end point
execute_action({update, y}, State) when State#position.pending == actions1;
						State#position.pending == actions3 ->
    State#position{y = State#position.y + State#position.timeout, timeout = 0};
%% update y: start point is >= of end point
execute_action({update, y}, State) when State#position.pending == actions2;
						State#position.pending == actions4 ->
    State#position{y = State#position.y - State#position.timeout, timeout = 0};
%% update theta
execute_action({update, theta, V}, State) ->
    State#position{theta=V, timeout = 0}.

%% start_actions([H | T], State) ->
%%     io:format("executing: ~p~n",[H]),
%%     execute_action(H, State#position{actions = T}).
