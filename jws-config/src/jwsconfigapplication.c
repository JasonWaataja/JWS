/* jwsconfigapplication.c - application class for jws-config

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

#include "jwsconfigapplication.h"
#include "jwsconfigwindow.h"
#include <gtk/gtk.h>

struct _JwsConfigApplication
{
  GtkApplication parent;
};

struct _JwsConfigApplicationClass
{
  GtkApplicationClass parent_class;
};

typedef struct _JwsConfigApplicationPrivate JwsConfigApplicationPrivate;

struct _JwsConfigApplicationPrivate
{
};

G_DEFINE_TYPE_WITH_PRIVATE (JwsConfigApplication, jws_config_application,
                            GTK_TYPE_APPLICATION);

static void
jws_config_application_activate (GApplication *app)
{
  g_print ("JwsConfigApplication\n");
  JwsConfigWindow *win;
  win = jws_config_window_new (JWS_CONFIG_APPLICATION (app));
  gtk_window_present (GTK_WINDOW (win));
}

static void
jws_config_application_init (JwsConfigApplication *app)
{
}

static void
jws_config_application_class_init (JwsConfigApplicationClass *kclass)
{
  G_APPLICATION_CLASS (kclass)->activate = jws_config_application_activate;
}

JwsConfigApplication *
jws_config_application_new ()
{
  return g_object_new (JWS_TYPE_CONFIG_APPLICATION,
                       "application-id", "com.waataja.jws-config",
                       NULL);
}
