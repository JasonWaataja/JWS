/* main.c - main file for JWS

Copyright (C) 2016 Jason Waataja

This file is part of JWS.

JWS is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

JWS is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with JWS.  If not, see <http://www.gnu.org/licenses/>.  */

#include <stdio.h>
#include <glib/gi18n.h>
#include <gio/gio.h>
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
      NULL, "Display images in order", NULL},
    {"time", 't', 0, G_OPTION_ARG_STRING, NULL,
      "Time between changing image of the form [XXh][YYm][ZZs] or just a "
        "seconds value", NULL},
    {NULL}
};

void
set_command_line_options (JwsApplication *app)
{
  JwsCommandLineOptions *options;
  options = jws_application_get_command_line_options (app);
  option_entries[0].arg_data = &(options->config_file);
  option_entries[1].arg_data = &(options->rotate_image);
  option_entries[2].arg_data = &(options->single_image);
  option_entries[3].arg_data = &(options->randomize_order);
  option_entries[4].arg_data = &(options->in_order);
  option_entries[5].arg_data = &(options->rotate_time);
  g_application_add_main_option_entries (G_APPLICATION (app), option_entries);
}

