/* jwsconfigapplication.h - header for the JwsConfigApplication class


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

#ifndef JWSCONFIGAPPLICATION_H
#define JWSCONFIGAPPLICATION_H

#include <gtk/gtk.h>

#define JWS_TYPE_CONFIG_APPLICATION (jws_config_application_get_type ())
#define JWS_CONFIG_APPLICATION(obj) \
  (G_TYPE_CHECK_INSTANCE_CAST ((obj), JWS_TYPE_CONFIG_APPLICATION, \
                               JwsConfigApplication))

typedef struct _JwsConfigApplication JwsConfigApplication;
typedef struct _JwsConfigApplicationClass JwsConfigApplicationClass;

GType
jws_config_application_get_type (void);

JwsConfigApplication *
jws_config_application_new ();

/* This value must be cleaned with g_free ().  */
gchar *
jws_get_default_config_file ();

#endif /* JWSCONFIGAPPLICATION_H */

