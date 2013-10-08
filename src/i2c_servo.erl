%%% @author Ivan Iacono <ivan.iacono@erlang-solutions.com> - Erlang Solutions Ltd
%%% @copyright (C) 2013, Erlang Solutions Ltd
%%% @doc This is the implementation for the Adafruit 16-Channel 12-bit PWM/Servo Driver.
%%% @end

-module(i2c_servo).
-export([init/0, setPWMFreq/1, setPWM/3]).

-define(I2C_CHANNEL, i2c1).
-define(ADDR, 16#40).
-define(MODE1, 16#0).
-define(PRESCALE, 16#fe).
-define(LED0_ON_L, 16#06).
-define(LED0_ON_H, 16#07).
-define(LED0_OFF_L, 16#08).
-define(LED0_OFF_H, 16#09).

init() ->
    i2c:start_link({?I2C_CHANNEL, "/dev/i2c-1"}),
    i2c:write(?I2C_CHANNEL, ?ADDR, {?MODE1, 16#0}).

setPWMFreq(Freq) ->
    Prescaleval = ((25000000 / 4096) / Freq) - 1,
    Prescale = trunc(Prescaleval + 0.5),
    %% Specifies the register to read
    i2c:write(?I2C_CHANNEL, ?ADDR, {?MODE1}),
    timer:sleep(10),
    {Oldmode} = i2c:read(?I2C_CHANNEL, ?ADDR, 1),
    Newmode = (Oldmode band 16#7f) bor 16#10, %% sleep
    i2c:write(?I2C_CHANNEL, ?ADDR, {?MODE1, Newmode}), %% go to sleep
    i2c:write(?I2C_CHANNEL, ?ADDR, {?PRESCALE, trunc(Prescale)}),
    i2c:write(?I2C_CHANNEL, ?ADDR, {?MODE1, Oldmode}),
%%    timer:sleep(0.005),
    i2c:write(?I2C_CHANNEL, ?ADDR, {?MODE1, Oldmode bor 16#80}).

setPWM(Channel, On, Off) ->
    %% Sets a single PWM channel
    i2c:write(?I2C_CHANNEL, ?ADDR, {?LED0_ON_L + 4 * Channel, On band 16#ff}),
    i2c:write(?I2C_CHANNEL, ?ADDR, {?LED0_ON_H + 4 * Channel, On bsr 8}),
    i2c:write(?I2C_CHANNEL, ?ADDR, {?LED0_OFF_L + 4 * Channel, Off band 16#ff}),
    i2c:write(?I2C_CHANNEL, ?ADDR, {?LED0_OFF_H + 4 * Channel, Off bsr 8}).
