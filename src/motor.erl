%%% @author Ivan Iacono <ivan.iacono@erlang-solutions.com> - Erlang Solutions Ltd
%%% @copyright (C) 2013, Erlang Solutions Ltd
%%% @doc This is the motor library implementation.
%%% @end

-module(motor).

-behaviour(gen_server).

%% API
-export([start_link/0, stop_server/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([forward/0, backward/0, rotate/1, stop/0]).

-define(SERVER, ?MODULE). 

%% -define(D90, 535). %% 90 degrees in milliseconds
%% -define(D180, 1222). %% 180 degrees in milliseconds

-record(state, {requestor = undefined, timeout = 0}).

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

forward() ->
    gen_server:call(?SERVER, {call, forward}, infinity).

backward() ->
    gen_server:call(?SERVER, {call, backward}, infinity).

rotate(left) ->
    gen_server:call(?SERVER, {call, rotate, left}, infinity);
rotate(right) ->
    gen_server:call(?SERVER, {call, rotate, right}, infinity).

stop() ->
    gen_server:call(?SERVER, {call, stop}).

stop_server() ->
    gen_server:cast(?SERVER, {cast, stop_server}).
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
%%    application:start(gproc),
%%    {ok, _} = chronos:start_link(motor_ts),
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

handle_call({call, forward}, _From, State) ->
%%    chronos:start_timer(motor_ts, stop_motor, T, {motor, stop, []}),
    motor_forward(),
  %%  NewState = #state{requestor = From, timeout = T},
    {reply, ok, State};

handle_call({call, backward}, _From, State) ->
%%    chronos:start_timer(motor_ts, stop_motor, T, {motor, stop, []}),
    motor_backward(),
%%    NewState = #state{requestor = From, timeout = T},
    {reply, ok, State};

handle_call({call, rotate, left}, _From, State) ->
%%    chronos:start_timer(motor_ts, stop_motor, T, {motor, stop, []}),
    motor_rotate(left),
%%    NewState = #state{requestor = From, timeout = T},
    {reply, ok, State};

handle_call({call, rotate, right}, _From, State) ->
%%    chronos:start_timer(motor_ts, stop_motor, T, {motor, stop, []}),
    motor_rotate(right),
%%    NewState = #state{requestor = From, timeout = T},
    {reply, ok, State};

handle_call({call, stop}, _From, State) ->
    motor_stop(),
    %% case chronos:stop_timer(motor_ts, stop_motor) of
    %% 	not_running ->
    %% 	    gen_server:reply(State#state.requestor, State#state.timeout);	    
    %% 	{ok, T} ->
    %% 	    gen_server:reply(State#state.requestor, T)
    %% end,
    %% NewState = #state{requestor = undefined, timeout = 0},
    Reply = ok,
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
handle_cast({cast, stop_server}, State) ->
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

motor_forward() ->
    timer:sleep(10),
    i2c_servo:setPWM(0,0,600),
    i2c_servo:setPWM(1,0,150).

motor_backward() ->
    timer:sleep(10),
    i2c_servo:setPWM(0,0,150),
    i2c_servo:setPWM(1,0,600).

motor_rotate(left) ->
    timer:sleep(10),
    i2c_servo:setPWM(0,0,150),
    i2c_servo:setPWM(1,0,150);
motor_rotate(right) ->
    timer:sleep(10),
    i2c_servo:setPWM(0,0,600),
    i2c_servo:setPWM(1,0,600).

motor_stop() ->
    i2c_servo:setPWM(0,0,0),
    i2c_servo:setPWM(1,0,0).
