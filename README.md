= ebuggy =

ebuggy is a controller for a little buggy made using Raspberry Pi and
controlled by Erlang software.




== Communication ==

The communication pattern used inside ebuggy is as follows.

The motor process accepts commands (forward, backward,rotate) and when
it is done it will send a message `motor_action_complete` to the
requestor of that action.

The sharp process is responsible for detecting obstacles in front of
the buggy. All processes that are interested in knowing when there is
an obstacle in front of the buggy calls the `sharp:alarm_obstacle/0`
function.
When an obstacle is detected all processes will be sent the message
`obstacle_present`.
The "subscription" is a one-off, so processes have to call
`sharp:alarm_obstacle/0` again if they still wants to be alerted.
