%%%-------------------------------------------------------------------------------------
%%% @author Ivan Iacono <ivan.iacono@gmail.com>, Corrado Santoro <santoro@dmi.unict.it>
%%% @copyright (C) 2013, Ivan Iacono, Corrado Santoro
%%% @doc This module is the goal function set, includes the istructions of the Goals.
%%% Define your goals and insert its code here.
%%% @end
%%%-------------------------------------------------------------------------------------

%% ENIGMA GOALS FUNCTIONS SET MODULE

-module(goal_function).
-compile(export_all).

-include("Goal.hrl").

%%--------------------------------------------
%% @doc
%% Allow to the Goal to be scheduled.
%%
%% @spec goal_db:add_goal(goalname()).
%% @end
%%--------------------------------------------

init_goal() ->
    position:start_link(),
    goal_db:add_goal(pocket1()),
    goal_db:add_goal(pocket2()),    
    goal_db:add_goal(pocket3()),
    goal_db:add_goal(funny()).

%%---------------------------------------------------------------------------------
%% @doc
%% Define a Goal.
%%
%% @spec
%% Define a Simple Goal
%% goalname() ->
%%    #goal{id = goalname,
%%          type = simple,
%%          state = State,
%%          feasibility_function = {Module, Function},
%%          priority_function = {Module, Function},
%%          execute_function = {Module, Function},
%%          max_trials = int }.
%%
%% Define a Combined Goal
%% goalname() ->
%%    #goal{id = goalname,
%%          type = {combo, and_policy | xor_policy, [SubGoalId]},
%%          max_trials = int }.
%% @end 
%%---------------------------------------------------------------------------------

pocket1() ->
    #goal{id = pocket1,
	  type = simple,
	  state = state,
	  feasibility_function = {?MODULE, pocket1_feasibility},
	  priority_function = {?MODULE, pocket1_priority},
	  execute_function = {?MODULE, pocket1_execute},
	  max_trials = 2}.

pocket2() ->
    #goal{id = pocket2,
	  type = simple,
	  state = state,
	  feasibility_function = {?MODULE, pocket2_feasibility},
	  priority_function = {?MODULE, pocket2_priority},
	  execute_function = {?MODULE, pocket2_execute},
	  max_trials = 2}.

pocket3() ->
    #goal{id = pocket3,
	  type = simple,
	  state = state,
	  feasibility_function = {?MODULE, pocket3_feasibility},
	  priority_function = {?MODULE, pocket3_priority},
	  execute_function = {?MODULE, pocket3_execute},
	  max_trials = 2}.

funny() ->
    #goal{id = funny,
	  type = simple,
	  state = state,
	  feasibility_function = {?MODULE, funny_feasibility},
	  priority_function = {?MODULE, funny_priority},
	  execute_function = {?MODULE, funny_execute},
	  max_trials = 2}.

%%-----------------------------------------------------------------------
%% @doc
%% Define a Goal Feasibility function.
%%
%% goalname_feasibility(_Goal, State) -> {true, State} | {false, State}
%% @end
%%-----------------------------------------------------------------------

pocket1_feasibility(_Goal, State) ->
    %% {X, Y, _} = position:now(),
    %% position:go_to_point(X, Y, 0),
    %% case sharp:read() of
    %% 	Val when Val > 47; Val == {error, out_of_range} ->
	    {true, State}.
    %% 	_ ->
    %% 	    {false, State}
    %% end.

pocket2_feasibility(_Goal, State) ->
    %% {X, Y, _} = position:now(),
    %% position:go_to_point(X, Y, 90),
    %% case sharp:read() of
    %% 	Val when Val > 54; Val == {error, out_of_range} ->
	    {true, State}.
    %% 	_ ->
    %% 	    {false, State}
    %% end.

pocket3_feasibility(_Goal, State) ->
    %% {X, Y, _} = position:now(),
    %% position:go_to_point(X, Y, 180),
    %% case sharp:read() of
    %% 	Val when Val > 47; Val == {error, out_of_range} ->
	    {true, State}.
    %% 	_ ->
    %% 	    {false, State}
    %% end.

funny_feasibility(_Goal, State) ->
    {true, State}.

%%-----------------------------------------------------------------
%% @doc
%% Define a Goal Priority function.
%%
%% @spec goalname_priority(_Goal, State) -> {int, State}
%% @end
%%-----------------------------------------------------------------

pocket1_priority(_Goal, State) ->
    {1, State}.

pocket2_priority(_Goal, State) ->
    {2, State}.

pocket3_priority(_Goal, State) ->
    {3, State}.

funny_priority(_Goal, State) ->
    {4, State}.

%%--------------------------------------------------------------------------
%% @doc
%% Define a Goal Execute function.
%%
%% @spec goalname_execute(_Goal, State) -> {success, State} | {fail, State}
%% @end
%%--------------------------------------------------------------------------

pocket1_execute(_Goal, State) ->
    i2c_servo:setPWM(15, 0, 600),
    position:go_to_point(2300, 0, 0),
    case position:now() of
	{2300, 0, 0} ->
	    i2c_servo:setPWM(15, 0, 310),
	    timer:sleep(1000),
	    i2c_servo:setPWM(15, 0, 600),
	    {success, State};
	_ ->
	    {fail, State}
    end.

pocket2_execute(_Goal, State) ->
    i2c_servo:setPWM(15, 0, 600),
    position:go_to_point(2300, 2400, 90),
    case position:now() of
	{2300, 2400, 90} ->
	    i2c_servo:setPWM(15, 0, 150),
	    timer:sleep(1000),
	    i2c_servo:setPWM(15, 0, 600),
	    {success, State};
	_ ->
	{fail, State}
    end.

pocket3_execute(_Goal, State) ->
%%    position:go_to_point(300, 3500, 270),
    i2c_servo:setPWM(15, 0, 600),
    position:go_to_point(2300, 2700, 90),
    i2c_servo:setPWM(15, 0, 150),
    position:go_to_point(400, 2700, 180),
    %% i2c_servo:setPWM(15, 0, 150),
    %% timer:sleep(1000),
    i2c_servo:setPWM(15, 0, 600),
    case position:now() of
%%	{300, 3500, 270} ->
	{400, 2700, 180} ->
	    {success, State};
	_ ->
	{fail, State}
    end.

funny_execute(_Goal, State) ->
    i2c_servo:setPWM(15, 0, 600),
    position:go_to_point(2000, 2000, 270),
    motor:rotate(left),
    i2c_servo:setPWM(15, 0, 150),
    timer:sleep(500),
    i2c_servo:setPWM(15, 0, 600),
    timer:sleep(500),
    i2c_servo:setPWM(15, 0, 150),
    timer:sleep(500),
    i2c_servo:setPWM(15, 0, 600),
    timer:sleep(500),
    i2c_servo:setPWM(15, 0, 150),
    timer:sleep(500),
    i2c_servo:setPWM(15, 0, 600),
    timer:sleep(500),
    i2c_servo:setPWM(15, 0, 150),
    timer:sleep(500),
    i2c_servo:setPWM(15, 0, 600),
    motor:stop(),
    {success, State}.
