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
}

static void
jws_config_application_init (JwsConfigApplication *app)
{
}

static void
jws_config_application_class_init (JwsConfigApplicationClass *kclass)
{
}

