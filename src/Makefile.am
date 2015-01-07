include @top_srcdir@/include.mk

MODULES = sysloggerl_app.erl		\
	  sysloggerl_sup.erl		\
	  syslog.erl			\
	  syslog_wrapper.erl		\
	  error_logger_syslog.erl

EBIN_DIR=@top_builddir@/ebin
EBIN_FILES=$(MODULES:%.erl=$(EBIN_DIR)/%.beam)

ERLC_FLAGS = $(ERLC_GENERIC_FLAGS)

erlsrcdir = $(ERLANG_INSTALL_LIB_DIR_sysloggerl)/src
dist_erlsrc_DATA = $(MODULES) sysloggerl.app.src sysloggerl.appup.src

include @top_srcdir@/erlang_deps.mk

all-local: $(EBIN_DIR)/sysloggerl.app $(EBIN_DIR)/sysloggerl.appup $(EBIN_FILES)

clean-local:
	$(AM_V_at)rm -f $(EBIN_DIR)/sysloggerl.app $(EBIN_DIR)/sysloggerl.appup $(EBIN_FILES)


$(EBIN_DIR)/sysloggerl.app: @srcdir@/sysloggerl.app.src
	$(AM_V_GEN)$(AWK) -v src="$(MODULES)"				\
	"/{vsn, *{cmd,/ {						\
		line = \$$0;						\
		sub(/{vsn.*/, \"{vsn, \\\"@SYSLOGGERL_VSN@\\\"},\", line);\
		print line;						\
		next;							\
        }								\
        /%% DO NOT EDIT: Modules list is generated/ {			\
		indent = index(\$$0, \"%\");				\
		indent = substr(\$$0, 1, indent - 1);			\
		split(src, mods);					\
		mods_count = 0;						\
		for (mod in mods) {					\
			mods_count++					\
		}							\
		for (i = 1; i <= mods_count; i++) {			\
			mod = mods[i];					\
			sub(/\.erl/, \"\", mod);			\
			if (i != mods_count)				\
				print indent mod \",\";			\
			else						\
				print indent mod			\
		}							\
                next;                                                   \
        }                                                               \
        { print; }" $< > $@

$(EBIN_DIR)/sysloggerl.appup: @srcdir@/sysloggerl.appup.src
	$(AM_V_GEN)$(SED) "s/%VSN%/@SYSLOGGERL_VSN@/g" < $< > $@

$(EBIN_DIR)/%.beam: %.erl
	$(AM_V_at)$(ERLC) $(ERLC_FLAGS) -M -MF $(@:$(EBIN_DIR)/%.beam=$(DEPDIR)/%.Pbeam) -MT $@ $<
	$(AM_V_ERLC)$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

distclean-local:
	$(AM_V_at)rm -fr $(DEPDIR)

# Local Variables:
#    tab-width: 8
# End:
