/* jwsconfigwindow.c - window for jws-config

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

#include "jwsconfigwindow.h"

struct _JwsConfigWindow
{
  GtkApplicationWindow parent;
};

struct _JwsConfigWindowClass
{
  GtkApplicationWindowClass parent_class;
};

typedef struct _JwsConfigWindowPrivate JwsConfigWindowPrivate;

struct _JwsConfigWindowPrivate
{
};

G_DEFINE_TYPE_WITH_PRIVATE (JwsConfigWindow, jws_config_window,
                            GTK_TYPE_APPLICATION_WINDOW);

void
jws_config_window_init (JwsConfigWindow *self)
{
  gtk_widget_init_template (GTK_WIDGET (self));
}

void
jws_config_window_class_init (JwsConfigWindowClass *kclass)
{
  gtk_widget_class_set_template_from_resource (GTK_WIDGET_CLASS (kclass),
                                               "/com/waataja/jwsconfig/"
                                               "ui/jwswindow.ui");
}

JwsConfigWindow *
jws_config_window_new (JwsConfigApplication *app)
{
  return g_object_new (JWS_TYPE_CONFIG_WINDOW,
                       "application", app,
                       NULL);
}

