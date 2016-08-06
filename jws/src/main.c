

#include <stdio.h>
#include <glib/gi18n.h>
#include <gio/gio.h>
#include <signal.h>
#include <glib-unix.h>
#include "jwsapplication.h"

void
init_gettext ();

void
set_command_line_options (JwsApplication *app);

int
main (int argc,
      char *argv[])
{
  init_gettext ();

  JwsApplication *app;
  app = jws_application_new ();

  set_command_line_options (app);

  int status;
  status = g_application_run (G_APPLICATION (app), argc, argv);

  g_object_unref (G_OBJECT (app));

  return status;
}

void
init_gettext ()
{
  setlocale (LC_ALL, "");
  bindtextdomain (PACKAGE, LOCALEDIR);
  textdomain (PACKAGE);
}

static GOptionEntry option_entries[] = 
{
    {"config-file", 'c', 0, G_OPTION_ARG_FILENAME,
      NULL, "Specify file to load config from, default is ~/.jws",
      NULL},
    {"rotate-image", 'r', 0, G_OPTION_ARG_NONE,
      NULL, "Change the image", NULL},
    {"single-image", 'i', 0, G_OPTION_ARG_NONE,
      NULL, "Display one image only", NULL},
    {"randomize-order", 's', 0, G_OPTION_ARG_NONE,
      NULL, "Shuffle the images", NULL},
    {"in-order", 'o', 0, G_OPTION_ARG_NONE,
      NULL, "Display images in oredr", NULL},
    {"time", 't', 0, G_OPTION_ARG_INT, NULL,
      "Seconds between changing changing image", NULL},
    {NULL}
};

void
set_command_line_options (JwsApplication *app)
{
  jws_command_line_options *options;
  options = jws_application_get_command_line_options (app);
  option_entries[0].arg_data = &(options->config_file);
  option_entries[1].arg_data = &(options->rotate_image);
  option_entries[2].arg_data = &(options->single_image);
  option_entries[3].arg_data = &(options->randomize_order);
  option_entries[4].arg_data = &(options->in_order);
  option_entries[5].arg_data = &(options->time);
  g_application_add_main_option_entries (G_APPLICATION (app), option_entries);
}

