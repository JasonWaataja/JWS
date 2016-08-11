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
  JwsConfigWindow *win;
};

G_DEFINE_TYPE_WITH_PRIVATE (JwsConfigApplication, jws_config_application,
                            GTK_TYPE_APPLICATION);

static void
jws_config_application_activate (GApplication *app)
{
  JwsConfigApplicationPrivate *priv;
  priv = jws_config_application_get_instance_private
    (JWS_CONFIG_APPLICATION (app));
  
  priv->win = jws_config_window_new (JWS_CONFIG_APPLICATION (app));
  gtk_window_present (GTK_WINDOW (priv->win));

  gchar *config_path;
  config_path = jws_get_default_config_file ();
  jws_config_window_load_file (priv->win, config_path);
  g_free (config_path);
}

static void
jws_config_application_init (JwsConfigApplication *app)
{
  JwsConfigApplicationPrivate *priv;
  priv = jws_config_application_get_instance_private (app);
  
  priv->win = NULL;
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

gchar *
jws_get_default_config_file ()
{
  GFile *home_dir = g_file_new_for_path (g_getenv ("HOME"));
  GFile *config_file = g_file_get_child (home_dir, ".jws");

  gchar *path = NULL;

  path = g_file_get_path (config_file);

  g_object_unref (G_OBJECT (home_dir));
  g_object_unref (G_OBJECT (config_file));

  return path;
}
