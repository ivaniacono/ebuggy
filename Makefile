PROJECT = ebuggy

DEPS = chronos erlang_ale enigma

dep_erlang_ale = https://github.com/esl/erlang_ale master
dep_chronos = https://github.com/lehoff/chronos master
dep_enigma = https://github.com/ivaniacono/enigma master

include erlang.mk

REBAR_DEPS_DIR=${DEPS_DIR}

init:
	mkdir -p ebin
	ln -s deps/erlang_ale/priv
	rm -rf deps/enigma/ebin/goal_function.beam

all: init ebuggy

ebuggy:
	erlc -I deps/enigma/include -o ebin/ src/*.erl

shell: 
	sudo erl -pz deps/erlang_ale/deps/*/ebin -pz deps/*/ebin -pz ebin 

clean:
	rm -rf *.beam ebin/*.beam

start:
	sudo erl -noshell -pz deps/erlang_ale/deps/*/ebin -pz deps/*/ebin -pz ebin -s start_enigma start
	exit 0
