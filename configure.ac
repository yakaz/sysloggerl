dnl ------------------------------------------------------------------
dnl Autoconf initialization.
dnl ------------------------------------------------------------------
dnl $Id$
m4_define([VERSION_NUMBER], m4_esyscmd([. ./vsn.mk && printf '%s' ${SYSLOGGER_VSN}]))
AC_INIT([syslogger], VERSION_NUMBER, [team@yakaz.com])

AC_CONFIG_SRCDIR([src/syslogger_app.erl])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([ac-aux])

AM_INIT_AUTOMAKE([foreign])

AC_PREREQ([2.64])

ECHO=echo
COLORED_ECHO_INIT

dnl ------------------------------------------------------------------
dnl Internal functions for this configure script.
dnl ------------------------------------------------------------------

dnl Expand shell variables to have a nice output in the final report.
expand_var () {
       local v=`eval echo '$'[$]1`
       while test "`echo [$]v | grep [[$]] > /dev/null && echo nok`"; do
               v=`eval echo [$]v`
       done
       echo [$]v
}

dnl ------------------------------------------------------------------
dnl Compiler and other tools.
dnl ------------------------------------------------------------------
AM_SILENT_RULES([yes])
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PROG_SED

AM_SET_DEPDIR

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

dnl escript(1) is used by the testsuite.
AC_ARG_VAR([ESCRIPT], [Erlang/OTP interpreter command [autodetected]])

if test "x${ESCRIPT}" = "x"; then
    if test "x${with_erlang}" = "x"; then
        AC_PATH_PROG([ESCRIPT], [escript],,)
    else
        erl_path="${with_erlang}/bin"
        AC_PATH_PROG([ESCRIPT], [escript],,
            [$erl_path$PATH_SEPARATOR$PATH])
    fi
else
    AC_MSG_CHECKING([for escript])
    AC_MSG_RESULT([$ESCRIPT])
fi

if test "x${ESCRIPT}" = "x"; then
    AC_MSG_WARN([
escript(1) not found. Fill the ESCRIPT variable with escript(1) path if
you want to use the testsuite.])
fi

dnl dialyzer(1).
AC_ARG_VAR([DIALYZER], [Erlang/OTP discrepancy analyzer [autodetected]])

if test "x${DIALYZER}" = "x"; then
   if test "x${with_erlang}" = "x"; then
      AC_PATH_PROG([DIALYZER], [dialyzer],,)
   else
        erl_path="${with_erlang}/bin"
        AC_PATH_PROG([DIALYZER], [dialyzer],,
            [$erl_path$PATH_SEPARATOR$PATH])
   fi
else
    AC_MSG_CHECKING([for dialyzer])
    AC_MSG_RESULT([$DIALYZER])
fi

if test "x${DIALYZER}" = "x"; then
    AC_MSG_WARN([
dialyzer(1) not found. Fill the DIALYZER variable with dialyzer(1) path if
you want to use it.])
fi

dnl Declare ERL_LIBS as precious.
AC_ARG_VAR([ERL_LIBS], [Erlang/OTP applications search path [none]])

dnl Get Erlang $ROOT dir and lib dir.
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_LIB_DIR

dnl Get ERTS version.
ERLANG_CHECK_ERTS
ERLANG_CHECK_RELEASE

dnl Erlang R14B02 (ERTS 5.8.3) or greater is required.
AX_COMPARE_VERSION([${ERLANG_ERTS_VER}], [ge], [5.8.3],
    [is_erlang_r14b02="yes"],
    [is_erlang_r14b02="no"])
if test "x${is_erlang_r14b02}" = "xno"; then
    AC_MSG_ERROR([
Erlang R14B02 or greater is required but only Erlang $ERLANG_RELEASE was found!])
fi

dnl Determine directories for installation.
if test "x${prefix}" != "xNONE" -a "x${ERLANG_INSTALL_LIB_DIR}" = "x"; then
    dnl Under $prefix
    ERLANG_INSTALL_LIB_DIR='${prefix}'
else
    dnl Under default erlang libdir
    ERLANG_INSTALL_LIB_DIR='${ERLANG_LIB_DIR}'
fi


AC_ERLANG_SUBST_INSTALL_LIB_DIR

. ${srcdir}/vsn.mk
AC_SUBST(SYSLOGGER_VSN)

AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(AC_PACKAGE_NAME, ${SYSLOGGER_VSN})


dnl ------------------------------------------------------------------
dnl Erlang applicatons.
dnl ------------------------------------------------------------------
AC_ERLANG_CHECK_LIB([kernel],,   [AC_MSG_ERROR(["kernel" Erlang application not found.])])
AC_ERLANG_CHECK_LIB([stdlib],,   [AC_MSG_ERROR(["stdlib" Erlang application not found.])])


dnl ------------------------------------------------------------------
dnl Finale substitutions.
dnl ------------------------------------------------------------------
exp_ERLANG_INSTALL_LIB_DIR_syslogger=`expand_var ERLANG_INSTALL_LIB_DIR_syslogger`
AC_SUBST(exp_ERLANG_INSTALL_LIB_DIR_syslogger)


dnl ------------------------------------------------------------------
dnl Autoconf output.
dnl ------------------------------------------------------------------
AC_SUBST([CONFIG_STATUS_DEPENDENCIES], ['$(top_srcdir)/vsn.mk'])

echo
AM_CONFIG_HEADER([config.h])
AC_CONFIG_FILES([
    src/Makefile
    ebin/Makefile
    priv/Makefile
    Makefile
])
AC_OUTPUT


dnl --------------------------------------------------
dnl Configuration report
dnl --------------------------------------------------
echo
COLORED_ECHO([ %B== ${PACKAGE_NAME} ${PACKAGE_VERSION}${APP_DISTSUFFIX} ==%b])
echo
COLORED_ECHO([Configuration:])
COLORED_ECHO([    Prefix:               ${prefix}])
COLORED_ECHO([    Application dir.:     ${exp_ERLANG_INSTALL_LIB_DIR_syslogger}])
echo
COLORED_ECHO([    Erlang emulator:      ${ERL}])
COLORED_ECHO([    Erlang compiler:      ${ERLC}])
COLORED_ECHO([    Erlang interpreter:   ${ESCRIPT}])
echo
