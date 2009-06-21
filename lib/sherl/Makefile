.SUFFIXES: .erl .beam .yrl

%.beam: %.erl
	erlc -W -o ebin $<

%.erl : %.yrl
	erlc -W $<

vpath %.erl src
vpath %.beam ebin
vpath %.hrl src include

ERL = erl -boot start_clean

MODS = sherl sherl_db base62 sherl_app sherl_sup
BEAM = $(MODS:%=%.beam)
APP = sherl
TOP = $(realpath .)
EBIN_DIRS = $(TOP)/ebin
CT_DIR = $(TOP)/ct-results

all: compile $(TOP)/ebin/$(APP).app

compile: $(BEAM)

$(MODS) : $(BEAM)

$(TOP)/ebin/$(APP).app: src/$(APP).app
	@cp -v $< $@

test: $(MODS) $(CT_DIR)
	run_test -pa $(EBIN_DIRS) -dir $(TOP) -logdir ct-results

$(CT_DIR):
	mkdir -p $(CT_DIR)

shell:
	$(ERL) -pa $(EBIN_DIRS)

clean:
	@echo "removing:"
	@rm -fv ebin/*.beam ebin/*.app src/*.beam erl_crash.dump
	@rm -rf $(CT_DIR)
