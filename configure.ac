dnl ------------------------------------------------------------------
dnl Autoconf initialization.
dnl ------------------------------------------------------------------

AC_INIT([syslogger], [0.0.2], [christopher@yakaz.com], [syslogger])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([ac-aux])

AM_INIT_AUTOMAKE([foreign])

AC_PREREQ([2.60])
AC_REVISION([$Revision$])

ECHO=echo
COLORED_ECHO_INIT

dnl ------------------------------------------------------------------
dnl Tools.
dnl ------------------------------------------------------------------

AC_PROG_GREP
AC_PROG_SED

dnl ------------------------------------------------------------------
dnl Internal functions for this configure script.
dnl ------------------------------------------------------------------

dnl EMKOPTS is used by Emakefile(s).
append_to_EMKOPTS () {
	if test -z "[$]EMKOPTS"; then
		EMKOPTS="[$]1"
	else
		EMKOPTS="[$]{EMKOPTS% }, [$]1"
	fi
}

dnl Expand shell variables to have a nice output in the final report.
expand_var () {
	local v=`eval echo '$'[$]1`
	while test "`echo [$]v | grep [[$]] > /dev/null && echo nok`"; do
		v=`eval echo [$]v`
	done
	echo [$]v
}


dnl ------------------------------------------------------------------
dnl Options.
dnl ------------------------------------------------------------------

dnl Debugging option.
AC_ARG_ENABLE([debug],
	AC_HELP_STRING([--enable-debug],
	    [turn on debugging [[default=auto]]]),,
	enable_debug=no)
if test "x${enable_debug}" = "xyes"; then
	append_to_EMKOPTS "debug_info"
	append_to_EMKOPTS "{d, debug}"
fi

dnl Print any warnings.
append_to_EMKOPTS "report_warnings"
append_to_EMKOPTS "{warn_format, 1}"
append_to_EMKOPTS "warn_export_vars"
append_to_EMKOPTS "warn_shadow_vars"
append_to_EMKOPTS "warn_unused_import"

dnl ------------------------------------------------------------------
dnl Erlang environment.
dnl ------------------------------------------------------------------

echo
COLORED_ECHO([%BErlang environment%b])

dnl Available flags.
AC_ARG_WITH([erlang],
	AC_HELP_STRING([--with-erlang=PREFIX],
	    [prefix where build machine's Erlang is installed (optional)]),
	with_erlang=${withval%/},
	with_erlang="")


dnl erl(1) is used to compile Erlang modules.
if test "x${with_erlang}" = "x"; then
	AC_ERLANG_PATH_ERL
	AC_ERLANG_PATH_ERLC
else
	erl_path="${with_erlang}/bin"
	AC_ERLANG_PATH_ERL(, [$erl_path$PATH_SEPARATOR$PATH])
	AC_ERLANG_PATH_ERLC(, [$erl_path$PATH_SEPARATOR$PATH])
fi

if test "x${ERL}" = "x"; then
	AC_MSG_ERROR([
Erlang not found. Fill the ERL variable with erl(1) path or provide
Erlang prefix with --with-erlang.])
fi

dnl Declare ERL_LIBS as precious.
AC_ARG_VAR([ERL_LIBS], [Erlang/OTP applications search path [none]])

dnl Get Erlang $ROOT dir and lib dir.
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_LIB_DIR

dnl Get ERTS version.
ERLANG_CHECK_ERTS
ERLANG_CHECK_RELEASE

dnl Erlang R13B (ERTS 5.7) is required to build ejabberd_native_listener.
AX_COMPARE_VERSION([${ERLANG_ERTS_VER}], [ge], [5.7],
    [is_erlang_r13b="yes"],
    [is_erlang_r13b="no"])
if test "x${is_erlang_r13b}" = "xno"; then
        AC_MSG_ERROR([
Erlang R13B is required to build syslogger but only Erlang $ERLANG_RELEASE was found!])
fi

AC_ERLANG_CHECK_LIB([kernel],, [AC_MSG_ERROR([kenel was not found!])])
AC_ERLANG_CHECK_LIB([stdlib],, [AC_MSG_ERROR([stdlib was not found!])])

dnl Determine directories for installation.
if test "x${prefix}" = "xNONE"; then
	dnl Inside Erlang lib directory.
	ERLANG_INSTALL_LIB_DIR="${ERLANG_LIB_DIR}"
else
	dnl Under $prefix
	ERLANG_INSTALL_LIB_DIR="${prefix}"
fi

AC_ERLANG_SUBST_INSTALL_LIB_DIR
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(syslogger, AC_PACKAGE_VERSION)

dnl ------------------------------------------------------------------
dnl Finale substitutions.
dnl ------------------------------------------------------------------

AC_SUBST(EMKOPTS)

exp_ERLANG_INSTALL_LIB_DIR_syslogger=`expand_var ERLANG_INSTALL_LIB_DIR_syslogger`
AC_SUBST(exp_ERLANG_INSTALL_LIB_DIR_syslogger)

dnl ------------------------------------------------------------------
dnl Autoconf output.
dnl ------------------------------------------------------------------

echo
AM_CONFIG_HEADER([config.h])
AC_CONFIG_FILES([
	src/Makefile
	src/Emakefile
	ebin/Makefile
	ebin/syslogger.app
    priv/Makefile
	Makefile
])
AC_OUTPUT

dnl --------------------------------------------------
dnl Configuration report
dnl --------------------------------------------------

echo
COLORED_ECHO([ %B== syslogger ${PACKAGE_VERSION} ==%b])
echo
COLORED_ECHO([Configuration:])
COLORED_ECHO([    Prefix:             ${prefix}])
COLORED_ECHO([    Application dir.:   ${exp_ERLANG_INSTALL_LIB_DIR_syslogger}])
echo
COLORED_ECHO([    Erlang emulator:    ${ERL}])
COLORED_ECHO([    Erlang compiler:    ${ERLC}])
echo
COLORED_ECHO([    Debug:              ${enable_debug}])
echo
