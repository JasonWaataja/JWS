

#include <stdio.h>
#include <glib/gi18n.h>
#include <gio/gio.h>
#include "jwsapplication.h"

void
init_gettext ();

int
main (int argc,
      char *argv[])
{
  init_gettext ();

  JwsApplication *app;
  app = jws_application_new ();

  int status;
  status = g_application_run (G_APPLICATION (app), argc, argv);

  return status;
}

void
init_gettext ()
{
  setlocale (LC_ALL, "");
  bindtextdomain (PACKAGE, LOCALEDIR);
  textdomain (PACKAGE);
}
