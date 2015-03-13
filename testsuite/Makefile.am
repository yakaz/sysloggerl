if HAVE_TS_TOOLS

include @top_srcdir@/include.mk

MODULES = basic_SUITE.erl syslog_SUITE.erl syslog_wrapper_SUITE.erl

EBIN_DIR=@top_builddir@/testsuite
EBIN_FILES=$(MODULES:%.erl=$(EBIN_DIR)/%.beam)

LOG_DIR=@builddir@/logs

ERLC_FLAGS = $(ERLC_GENERIC_FLAGS) -DSHOW_LOG

include @top_srcdir@/erlang_deps.mk

ALL_SUITES = $(MODULES:%.erl=%)
SUITES     = all
CASES      = all

EXTRA_DIST = $(MODULES) testsuite.hrl


all-local: $(EBIN_FILES)

clean-local:
	$(AM_V_at)rm -fr $(EBIN_FILES) $(LOG_DIR)

$(EBIN_DIR)/%.beam: %.erl
	$(AM_V_at)$(ERLC) $(ERLC_FLAGS) -M -MF $(@:$(EBIN_DIR)/%.beam=$(DEPDIR)/%.Pbeam) -MT $@ $<
	$(AM_V_ERLC)$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

distclean-local:
	$(AM_V_at)rm -fr $(DEPDIR)

$(LOG_DIR):
	$(AM_V_at)mkdir -p $@

common_test: all $(LOG_DIR)
	$(AM_V_at)echo "  RUN     $@ [suites=$(SUITES) - cases=$(CASES)]"
	$(AM_V_at)@builddir@/run_common_test "$(ALL_SUITES)" "$(SUITES)" "$(CASES)"

check-local: common_test


.PHONY: common_test

else

check-local:
	$(AM_V_at)echo
	$(AM_V_at)echo "  SKIPPED - Tests cannot be executed because on or more required applications are missing"
	$(AM_V_at)echo
	$(AM_V_at)((test "x$(CT_RUN)" = "x")                            && echo "    - ct_run(1) is missing")                   || true
	$(AM_V_at)((test "$(ERLANG_LIB_DIR_common_test)" = "not found") && echo "    - \"common_test\" application is missing") || true
	$(AM_V_at)((test "$(ERLANG_LIB_DIR_eunit)" = "not found")       && echo "    - \"eunit\" application is missing")       || true
	$(AM_V_at)((test "$(ERLANG_LIB_DIR_tools)" = "not found")       && echo "    - \"tools\" application is missing")       || true
	$(AM_V_at)echo

endif
