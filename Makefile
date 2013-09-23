PROJECT = ebuggy

DEPS = chronos

dep_erlang_ale = https://github.com/esl/erlang_ale master
dep_chronos = https://github.com/lehoff/chronos master
dep_enigma = https://github.com/ivaniacono/enigma master

include erlang.mk

REBAR_DEPS_DIR=${DEPS_DIR}

init:
	mkdir -p priv ebin

all: init ebuggy

ebuggy:
	erlc -o ebin/ src/*.erl

shell: 
	sudo erl -pz deps/*/ebin deps/erlang-ale/deps/*/ebin -pz ebin

clean:
	rm -rf *.beam ebin/*.beam
