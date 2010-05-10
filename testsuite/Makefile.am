# $Id$

ETEST = etest

ERL_FILES =

ERL_TESTS =

TESTS = dialyzer $(ERL_TESTS)

EXTRA_DIST = $(ERL_FILES)

CLEANFILES = $(TESTS) erl_crash.dump

$(ERL_TESTS): $(ETEST)
	@(test ! -e $@ -o $(ETEST) -nt $@ && echo "Create: $@" &&	\
	  cp $(ETEST) $@ && chmod 755 $@) || :
