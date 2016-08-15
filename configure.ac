

AC_INIT([JWS], [1.0.2], [jasonswaataja@gmail.com])
AM_INIT_AUTOMAKE([-Wall foreign])
AM_SILENT_RULES([yes])

AC_PROG_CC
PKG_PROG_PKG_CONFIG
AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION([0.19])

PKG_CHECK_MODULES([JWS], [
                   glib-2.0
                   gobject-2.0
                   gio-2.0
                   ])

AC_CONFIG_FILES([
                 Makefile
                 src/Makefile
                 po/Makefile.in
                 ])
AC_OUTPUT
