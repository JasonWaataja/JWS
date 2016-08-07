/* jwsconfigwindow.h - header for the JwsConfigWindow class

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

#ifndef JWSCONFIGWINDOW_H
#define JWSCONFIGWINDOW_H

#include <gtk/gtk.h>
#include "jwsconfigapplication.h"

#define JWS_TYPE_CONFIG_WINDOW (jws_config_window_get_type ())
#define JWS_CONFIG_WINDOW \
  (G_TYPE_CHECK_INSTANCE_CAST ((obj), JWS_TYPE_CONFIG_WINDOW, \
                               JwsConfigWindow))

typedef struct _JwsConfigWindow JwsConfigWindow;
typedef struct _JwsConfigWindowClass JwsConfigWindowClass;

GType
jws_config_window_get_type (void);

JwsConfigWindow *
jws_config_window_new (JwsConfigApplication *app);

#endif /* JWSCONFIGWINDOW_H */
