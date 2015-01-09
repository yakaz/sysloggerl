ACLOCAL_FLAGS = -I m4
ACLOCAL_AMFLAGS = -I m4

SUBDIRS = src ebin include

EXTRA_DIST = README.md AUTHORS COPYING vsn.mk include.mk erlang_deps.mk	\
	     debian/changelog debian/compat debian/control 		\
	     debian/copyright debian/rules debian/source debian/watch

dialyzer: all
	$(AM_V_at)if [ -z $(DIALYZER) ]; then					\
		echo "  SKIP    dialyzer";					\
		echo "dialyzer(1) not found. Fill the DIALYZER variable with 	\
dialyzer(1) path if you want to use it."; 					\
		echo;								\
	else									\
		echo "  RUN     dialyzer";					\
		$(DIALYZER) -n -r @top_builddir@/ebin;				\
	fi

# Local Variables:
#    tab-width: 8
# End:
