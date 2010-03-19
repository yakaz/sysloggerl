EXTRA_DIST = syslogger.erl syslogger_sup.erl syslog.erl \
			 syslog_wrapper.erl error_logger_syslog.erl

syslogsrcdir =  $(ERLANG_INSTALL_LIB_DIR_syslogger)/src
dist_syslogsrc_DATA = syslog_wrapper.erl

all-local:
	@$(ERL) $(ERL_PATH) -pa @top_builddir@/ebin -noshell			\
						-eval "case make:all() of					\
									up_to_date -> halt(0);			\
									Err ->							\
										io:format(\"~p~n\", [Err]),	\
										halt(1)						\
								end"

clean:
	rm -f erl_crash.dump
