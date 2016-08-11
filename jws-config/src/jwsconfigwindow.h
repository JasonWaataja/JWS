/* jwsconfigwindow.h - header for the JwsConfigWindow class

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

#ifndef JWSCONFIGWINDOW_H
#define JWSCONFIGWINDOW_H

#include <gtk/gtk.h>
#include "jwsconfigapplication.h"

#define JWS_TYPE_CONFIG_WINDOW (jws_config_window_get_type ())
#define JWS_CONFIG_WINDOW(obj) \
  (G_TYPE_CHECK_INSTANCE_CAST ((obj), JWS_TYPE_CONFIG_WINDOW, \
                               JwsConfigWindow))

#define JWS_CONFIG_WINDOW_PREVIEW_HEIGHT 100

typedef struct _JwsConfigWindow JwsConfigWindow;
typedef struct _JwsConfigWindowClass JwsConfigWindowClass;

GType
jws_config_window_get_type (void);

JwsConfigWindow *
jws_config_window_new (JwsConfigApplication *app);

gboolean
jws_config_window_get_should_exit_thread (JwsConfigWindow *win);

void
jws_config_window_set_should_exit_thread (JwsConfigWindow *win,
                                          gboolean should_exit_thread);

void
jws_config_window_show_optional_side_buttons (JwsConfigWindow *win,
                                              gboolean visible);

void
jws_config_window_show_rotate_items (JwsConfigWindow *win,
                                     gboolean visible);

/* The file pointed to by path may be a regular file or a directory.  In the
 * case that it is a directory, a top level element will be added to the tree
 * view and that directory's children will be recursively added.  */
void
jws_config_window_add_file (JwsConfigWindow *win, const char *path);

void
jws_config_window_add_file_selection (JwsConfigWindow *win);

void
jws_config_window_add_directory_selection (JwsConfigWindow *win);

/* Free with g_free when finished with the result.  */
gchar *
jws_get_type_string (gboolean is_directory);

/* If width or height are positive, the pixbuf will have the dimension.  If one
 * of them are not set, that dimension will be scaled to the other.  If neither
 * are set, it will be the dimensions of the original.  Returns a new pixbuf
 * which must freed with g_object_free.  */
GdkPixbuf *
jws_create_scaled_pixbuf (GdkPixbuf *src,
                          int width,
                          int height);

/* Free with g_free ().  */
gchar *
jws_config_window_get_path_for_row (JwsConfigWindow *win,
                                    GtkTreeRowReference *row_ref);

void
jws_config_window_show_image_for_row (JwsConfigWindow *win,
                                      GtkTreeRowReference *row_ref);

GtkTreeRowReference *
jws_config_window_get_next_image_row (JwsConfigWindow *win,
                                      GtkTreeRowReference *row_ref);

GtkTreeRowReference *
jws_config_window_get_previous_image_row (JwsConfigWindow *win,
                                          GtkTreeRowReference *row_ref);

void
jws_config_window_load_file (JwsConfigWindow *win, const char *path);

void
jws_config_window_save_to_file (JwsConfigWindow *win, const char *path);

void
jws_config_window_set_gui_from_info (JwsConfigWindow *win);

void
jws_config_window_set_info_from_gui (JwsConfigWindow *win);

/* Returns whether or not the gui elemets create a coherent JwsInfo.  For
 * example, if the time box is less than zero.  Displays messages if anything
 * is off and returns TRUE if everything passes.  */
gboolean
jws_config_window_check_gui_consistency (JwsConfigWindow *win);

#endif /* JWSCONFIGWINDOW_H */
