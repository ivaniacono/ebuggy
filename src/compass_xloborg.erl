%%% @author Ivan Iacono <ivan.iacono@erlang-solutions.com> - Erlang Solutions Ltd
%%% @copyright (C) 2013, Erlang Solutions Ltd
%%% @doc This is the erlang implementation for the xloborg module for raspberry pi.
%%% @end

-module(compass_xloborg).

-compile(export_all).

-define(I2C_CHANNEL, i2c1).
-define(ADDR, 16#0e).
-define(REG1, 16#11).
-define(REG2, 16#10).

init() ->
    i2c:start_link({?I2C_CHANNEL, "/dev/i2c-1"}),
    Data1 = (1 bsl 7) bor (1 bsl 5) bor (0 bsl 5),

    i2c:write(?I2C_CHANNEL, ?ADDR, {?REG1, Data1}),
    
    %% System operation
    Data2 = (0 bsl 5) bor (3 bsl 3) bor (0 bsl 2) bor (0 bsl 1) bor (1 bsl 0),
    i2c:write(?I2C_CHANNEL, ?ADDR, {?REG2, Data2}).

read() ->
    i2c:write(?I2C_CHANNEL, ?ADDR, {16#0}),
    timer:sleep(10),

%%    {_, BufIn1, BufIn2, BufIn3, BufIn4, BufIn5, BufIn6, _, _, _, _, _, _, _, _, _, _, _} = i2c:read(?I2C_CHANNEL, ?ADDR, 18),
    {_, BufIn1, BufIn2, BufIn3, BufIn4, _, _, _, _, _, _, _, _, _, _, _, _, _} = i2c:read(?I2C_CHANNEL, ?ADDR, 18),

    <<BufIn1Signed:16/signed>> = <<BufIn1:16/unsigned>>,
    <<BufIn2Signed:16/signed>> = <<BufIn2:16/unsigned>>,
    X = (BufIn1Signed bsl 8) bor BufIn2Signed,
    <<Xsigned:16/signed>> = <<X:16/unsigned>>,

    <<BufIn3Signed:16/signed>> = <<BufIn3:16/unsigned>>,
    <<BufIn4Signed:16/signed>> = <<BufIn4:16/unsigned>>,
    Y = (BufIn3Signed bsl 8) bor BufIn4Signed,
    <<Ysigned:16/signed>> = <<Y:16/unsigned>>,

%%    Z = (BufIn5 bsl 8) bor BufIn6,
%%    math:atan2(Ysigned, Xsigned) * (180 / math:pi()) + 180.
%%    {Xsigned, Ysigned}.
    
    Pi = 3.14159,
    Heading = (math:atan2(Ysigned, Xsigned) * 180) / Pi,
    if
	Heading < 0 ->
	    Heading + 360;	   
	Heading >= 0 ->
	    Heading
    end.
    
%%    case Heading < 0 of
%%	true ->
%%	   H =  Heading + (2 * math:pi());
%%	false ->
%%	    H = Heading
%%    end,    
%%    H * 180/math:pi().

%% angle= atan2((double)y,(double)x) * (180 / 3.14159265) + 180;
