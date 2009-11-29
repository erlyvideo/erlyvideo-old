AC_INIT([erlang-amf], [1.0.1], [ruslan@babayev.com])

AC_PREREQ([2.59c])
AC_COPYRIGHT([Copyright (C) 2009 Ruslan Babayev])
AM_INIT_AUTOMAKE

AC_ERLANG_NEED_ERLC
AC_PREFIX_PROGRAM(erl)
AC_ERLANG_SUBST_LIB_DIR
AC_ERLANG_CHECK_LIB([kernel])
AC_ERLANG_CHECK_LIB([stdlib])
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR([amf], [1.0.1])

AC_CONFIG_FILES([Makefile src/amf.app src/overview.edoc src/Makefile])
AC_OUTPUT
