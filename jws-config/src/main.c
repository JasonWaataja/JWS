

#include <stdio.h>
#include <stdlib.h>

#include <gtk/gtk.h>

#include "jwsconfigapplication.h"

int
main (int argc,
      char *argv[])
{
  JwsConfigApplication *app;
  app = jws_config_application_new ();

  int status;
  status = g_application_run (G_APPLICATION (app), argc, argv);

  return status;
}
