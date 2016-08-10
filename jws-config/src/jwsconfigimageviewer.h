/* jwsconfigimageviewer.h - header for JwsConfigImageViewer class

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

#ifndef JWSCONFIGIMAGEVIEWER_H
#define JWSCONFIGIMAGEVIEWER_H

#include <gtk/gtk.h>
#include "jwsconfigwindow.h"

#define JWS_TYPE_CONFIG_IMAGE_VIEWER (jws_config_image_viewer_get_type ())
#define JWS_CONFIG_IMAGE_VIEWER(obj) \
  (G_TYPE_CHECK_INSTANCE_CAST ((obj), JWS_TYPE_CONFIG_IMAGE_VIEWER, \
                               JwsConfigImageViewer))

/* The maximum width and height for a scaled image.  */
#define JWS_CONFIG_IMAGE_VIEWER_MAX_HEIGHT 540
#define JWS_CONFIG_IMAGE_VIEWER_MAX_WIDTH 960

typedef struct _JwsConfigImageViewer JwsConfigImageViewer;
typedef struct _JwsConfigImageViewerClass JwsConfigImageViewerClass;

GType
jws_config_image_viewer_get_type (void);

/* Internally stores start_row.  This means that your reference to start_row
 * need not be freed because that will be handled by the object.  */
JwsConfigImageViewer *
jws_config_image_viewer_new (JwsConfigWindow *win,
                             GtkTreeRowReference *start_row);

JwsConfigWindow *
jws_config_image_viewer_get_window (JwsConfigImageViewer *viewer);

void
jws_config_image_viewer_set_window (JwsConfigImageViewer *viewer,
                                    JwsConfigWindow *win);

GtkTreeRowReference *
jws_config_image_viewer_get_current_row (JwsConfigImageViewer *viewer);

void
jws_config_image_viewer_set_current_row (JwsConfigImageViewer *viewer,
                                         GtkTreeRowReference *row_ref);

void
jws_config_image_viewer_set_image_for_path (JwsConfigImageViewer *viewer,
                                            const char *path);

void
jws_config_image_viewer_previous (JwsConfigImageViewer *viewer);

void
jws_config_image_viewer_next (JwsConfigImageViewer *viewer);

void
jws_config_image_viewer_original_size (JwsConfigImageViewer *viewer);

void
jws_config_image_viewer_scaled_size (JwsConfigImageViewer *viewer);

/* Free the result with g_free ().  */
gchar *
jws_config_image_viewer_get_current_path (JwsConfigImageViewer *viewer);

void
jws_config_image_viewer_set_pixbufs_for_path (JwsConfigImageViewer *viewer,
                                              const char *path);

#endif /* JWSCONFIGIMAGEVIEWER_H */
