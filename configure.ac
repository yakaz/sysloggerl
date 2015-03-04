dnl ------------------------------------------------------------------
dnl Autoconf initialization.
dnl ------------------------------------------------------------------
dnl $Id$
m4_define([VERSION_NUMBER], m4_esyscmd([. ./vsn.mk && printf '%s' ${SYSLOGGERL_VSN}]))
AC_INIT([sysloggerl], VERSION_NUMBER)

AC_CONFIG_SRCDIR([src/sysloggerl_app.erl])
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

dnl ct_run(1).
AC_ARG_VAR([CT_RUN], [Erlang/OTP Program used for starting Common Test [autodetected]])

if test "x${CT_RUN}" = "x"; then
   if test "x${with_erlang}" = "x"; then
      AC_PATH_PROG([CT_RUN], [ct_run],,)
   else
        erl_path="${with_erlang}/bin"
        AC_PATH_PROG([CT_RUN], [ct_run],,
            [$erl_path$PATH_SEPARATOR$PATH])
   fi
else
    AC_MSG_CHECKING([for ct_run])
    AC_MSG_RESULT([$CT_RUN])
fi

if test "x${CT_RUN}" = "x"; then
    AC_MSG_WARN([
ct_run(1) not found. Fill the CT_RUN variable with ct_run(1) path if
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
AC_SUBST(SYSLOGGERL_VSN)

AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(AC_PACKAGE_NAME, ${SYSLOGGERL_VSN})


dnl ------------------------------------------------------------------
dnl Erlang applicatons.
dnl ------------------------------------------------------------------
AC_ERLANG_CHECK_LIB([kernel],,   [AC_MSG_ERROR(["kernel" Erlang application not found.])])
AC_ERLANG_CHECK_LIB([stdlib],,   [AC_MSG_ERROR(["stdlib" Erlang application not found.])])

AC_ERLANG_CHECK_LIB([common_test],, [AC_MSG_WARN(["common_test" Erlang application not found.])])
AC_ERLANG_CHECK_LIB([eunit],,       [AC_MSG_WARN(["eunit" Erlang application not found.])])
AC_ERLANG_CHECK_LIB([tools],,       [AC_MSG_WARN(["tools" Erlang application not found.])])

dnl ------------------------------------------------------------------
dnl Finale substitutions.
dnl ------------------------------------------------------------------
AM_CONDITIONAL(HAVE_TS_TOOLS, [(test "x${CT_RUN}"                    != "x")         &&
                               (test "${ERLANG_LIB_DIR_common_test}" != "not found") &&
                               (test "${ERLANG_LIB_DIR_eunit}"       != "not found") &&
                               (test "${ERLANG_LIB_DIR_tools}"       != "not found")])

exp_ERLANG_INSTALL_LIB_DIR_sysloggerl=`expand_var ERLANG_INSTALL_LIB_DIR_sysloggerl`
AC_SUBST(exp_ERLANG_INSTALL_LIB_DIR_sysloggerl)


dnl ------------------------------------------------------------------
dnl Autoconf output.
dnl ------------------------------------------------------------------
AC_SUBST([CONFIG_STATUS_DEPENDENCIES],
    ['$(top_srcdir)/vsn.mk                              \
      $(top_srcdir)/erlang_deps.mk                      \
      $(top_srcdir)/include.mk                          \
      $(top_srcdir)/testsuite/run_common_test.in        \
      $(top_srcdir)/testsuite/sysloggerl.coverspec.in   \
      $(top_srcdir)/testsuite/cover_to_html.sh.in       \
      $(top_srcdir)/testsuite/analyze_coverdata.escript.in'])

echo
AC_CONFIG_FILES([
    Makefile
    src/Makefile
    ebin/Makefile
    include/Makefile
    testsuite/Makefile
    testsuite/${PACKAGE_NAME}.coverspec
])

AC_CONFIG_FILES([testsuite/run_common_test], [chmod +x testsuite/run_common_test])
AC_CONFIG_FILES([testsuite/cover_to_html.sh], [chmod +x testsuite/cover_to_html.sh])
AC_CONFIG_FILES([testsuite/analyze_coverdata.escript], [chmod +x testsuite/analyze_coverdata.escript])

AC_OUTPUT

dnl --------------------------------------------------
dnl Configuration report
dnl --------------------------------------------------
echo
COLORED_ECHO([ %B== ${PACKAGE_NAME} ${SYSLOGGERL_VSN} ==%b])
echo
COLORED_ECHO([Configuration:])
COLORED_ECHO([ %BApplication dir.%b:     ${exp_ERLANG_INSTALL_LIB_DIR_sysloggerl}])
echo
COLORED_ECHO([ %BErlang root dir%b:      ${ERLANG_ROOT_DIR}])
COLORED_ECHO([ %BErlang lib dir%b:       ${ERLANG_LIB_DIR}])
COLORED_ECHO([ %BErlang emulator%b:      ${ERL}])
COLORED_ECHO([ %BErlang compiler%b:      ${ERLC}])
COLORED_ECHO([ %BErlang interpreter%b:   ${ESCRIPT}])
COLORED_ECHO([ %BErlang dialyzer tool%b: ${DIALYZER}])
COLORED_ECHO([ %BErlang ct_run tool%b:   ${CT_RUN}])
echo
