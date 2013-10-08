EBuggy
======

EBuggy is a little robotic buggy made using Raspberry Pi and
controlled by Erlang software, utilising the embedded framework Erlang/ALE, a library for interfacing
Erlang with the hardware peripherals on embedded platforms.

# Goal

EBuggy is a pool playing robot. It aims to hit the pool balls and put them into the relevant pockets.
Each pocket is modeled as a single goal, scheduled using the Goal-Manager framework EN.I.GM.A.


# Hardware BOM (Bill of Materials)

- RaspberryPi rev.2
- Adafruit 16-Channel 12-bit PWM/Servo Driver (http://www.adafruit.com/products/815)
- Microchip MCP3002 Analog to Digital Converter
- Sharp 2Y0A21 IR Distance Sensor
- 2 x GWServo S03N STD continuous servo
- Small generic servo
- Wi-Fi USB Adapter

# Software

- Raspbian
- Erlang-mini Minimal Erlang/OTP distribution (https://www.erlang-solutions.com/downloads/download-erlang-otp#tabs-raspbian)
- Erlang/ALE Library for Embedded (https://github.com/esl/erlang_ale)
- EN.I.GM.A. Goal-Manager (https://github.com/ivaniacono/enigma)

# Communication

The communication pattern used inside ebuggy is as follows.

The `motor` process accepts commands (forward, backward, rotate).

The `sharp` process is responsible for detecting obstacles in front of
the buggy. All processes that are interested in knowing when there is
an obstacle in front of the buggy calls the `sharp:alarm_obstacle/0`
function.
When an obstacle is detected all processes will be sent the message
`obstacle_present`.
The "subscription" is a one-off, so processes have to call
`sharp:alarm_obstacle/0` again if they still wants to be alerted.
A process can delete its subscription calling `sharp:ignore_obstacle()`.

# Parts

Below a UML Component Diagram.

![](https://raw.github.com/ivaniacono/ebuggy/master/doc/parts_diagram.png)

# Getting started

Log in into RaspberryPi and fetch this software using git:

    git clone https://github.com/ivaniacono/ebuggy.git

Fetch the dependencies and build it:

    cd ebuggy && make

# Running

Use `make start` to start the robot.

# License

EBuggy is licensed under the Apache License, Version 2.0 (the "License");
You may not use this library except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.