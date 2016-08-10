/* jwsconfigimageviewer.c - image viewer with next and previous functions

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

#include "jwsconfigimageviewer.h"

#include <glib/gi18n.h>

struct _JwsConfigImageViewer
{
  GtkWindow parent;
};

struct _JwsConfigImageViewerClass
{
  GtkWindowClass parent_class;
};

typedef struct _JwsConfigImageViewerPrivate JwsConfigImageViewerPrivate;

struct _JwsConfigImageViewerPrivate
{
  JwsConfigWindow *win;
  GtkTreeRowReference *current_row;

  GdkPixbuf *original_pixbuf;
  GdkPixbuf *scaled_pixbuf;

  GtkWidget *image;
  GtkWidget *previous_button;
  GtkWidget *next_button;
  GtkWidget *original_size_button;
  GtkWidget *scaled_size_button;
  GtkWidget *cancel_button;
};

G_DEFINE_TYPE_WITH_PRIVATE (JwsConfigImageViewer, jws_config_image_viewer,
                            GTK_TYPE_WINDOW);

static void
jws_config_image_viewer_dispose (GObject *obj);

static void
jws_config_image_viewer_finalize (GObject *obj);

static void
jws_config_image_viewer_init (JwsConfigImageViewer *self)
{
  JwsConfigImageViewerPrivate *priv;
  priv = jws_config_image_viewer_get_instance_private (self);

  priv->original_pixbuf = NULL;
  priv->scaled_pixbuf = NULL;

  gtk_widget_init_template (GTK_WIDGET (self));
  
  priv->win = NULL;
  priv->current_row = NULL;

  g_signal_connect_swapped (priv->previous_button, "clicked",
                            G_CALLBACK (jws_config_image_viewer_previous),
                            self);

  g_signal_connect_swapped (priv->next_button, "clicked",
                            G_CALLBACK (jws_config_image_viewer_next),
                            self);

  g_signal_connect_swapped (priv->original_size_button, "clicked",
                            G_CALLBACK
                            (jws_config_image_viewer_original_size),
                            self);

  g_signal_connect_swapped (priv->scaled_size_button, "clicked",
                            G_CALLBACK
                            (jws_config_image_viewer_scaled_size),
                            self);

  g_signal_connect_swapped (priv->cancel_button, "clicked",
                            G_CALLBACK (gtk_widget_destroy),
                            self);
}

static void
jws_config_image_viewer_class_init (JwsConfigImageViewerClass *kclass)
{
  gtk_widget_class_set_template_from_resource (GTK_WIDGET_CLASS (kclass),
                                               "/com/waataja/jwsconfig/"
                                               "ui/imageviewer.ui");
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigImageViewer,
                                                cancel_button);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigImageViewer,
                                                scaled_size_button);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigImageViewer,
                                                original_size_button);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigImageViewer,
                                                next_button);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigImageViewer,
                                                previous_button);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigImageViewer,
                                                image);

  G_OBJECT_CLASS (kclass)->dispose = jws_config_image_viewer_dispose;
  G_OBJECT_CLASS (kclass)->finalize = jws_config_image_viewer_finalize;
}

static void
jws_config_image_viewer_dispose (GObject *obj)
{
  JwsConfigImageViewerPrivate *priv;
  priv = jws_config_image_viewer_get_instance_private
    (JWS_CONFIG_IMAGE_VIEWER (obj));

  g_clear_object (&priv->win);

  g_clear_object (&priv->original_pixbuf);
  g_clear_object (&priv->original_pixbuf);

  G_OBJECT_CLASS (jws_config_image_viewer_parent_class)->dispose (obj);
}

static void
jws_config_image_viewer_finalize (GObject *obj)
{
  JwsConfigImageViewerPrivate *priv;
  priv = jws_config_image_viewer_get_instance_private
    (JWS_CONFIG_IMAGE_VIEWER (obj));

  gtk_tree_row_reference_free (priv->current_row);

  G_OBJECT_CLASS (jws_config_image_viewer_parent_class)->finalize (obj);
}
  
  

JwsConfigImageViewer *
jws_config_image_viewer_new (JwsConfigWindow *win,
                             GtkTreeRowReference *start_row)
{
  JwsConfigImageViewer *viewer;
  viewer = JWS_CONFIG_IMAGE_VIEWER
    (g_object_new (JWS_TYPE_CONFIG_IMAGE_VIEWER,
                   NULL));

  JwsConfigImageViewerPrivate *priv;
  priv = jws_config_image_viewer_get_instance_private (viewer);

  priv->win = win;
  g_object_ref (win);

  priv->current_row = start_row;

  gtk_window_set_transient_for (GTK_WINDOW (viewer), GTK_WINDOW (win));
  gtk_window_set_destroy_with_parent (GTK_WINDOW (viewer), TRUE);

  gchar *path;
  path = jws_config_image_viewer_get_current_path (viewer);
  jws_config_image_viewer_set_pixbufs_for_path (viewer, path);

  gchar *title = g_strdup (_("Image"));
  GFile *as_file;
  as_file = g_file_new_for_path (path);
  if (as_file)
    {
      gchar *basename;
      basename = g_file_get_basename (as_file);
      if (basename)
        {
          g_free (title);
          title = g_strdup (basename);
        }
      g_free (basename);
    }
  gtk_window_set_title (GTK_WINDOW (viewer), title);

  g_free (title);
  g_object_unref (as_file);

  g_free (path);

  jws_config_image_viewer_scaled_size (viewer);

  return viewer;
}

JwsConfigWindow *
jws_config_image_viewer_get_window (JwsConfigImageViewer *viewer)
{
  JwsConfigImageViewerPrivate *priv;
  priv = jws_config_image_viewer_get_instance_private (viewer);
  
  return priv->win;
}

void
jws_config_image_viewer_set_window (JwsConfigImageViewer *viewer,
                                    JwsConfigWindow *win)
{
  JwsConfigImageViewerPrivate *priv;
  priv = jws_config_image_viewer_get_instance_private (viewer);

  g_object_unref (priv->win);
  
  priv->win = win;
  g_object_ref (G_OBJECT (win));
}

GtkTreeRowReference *
jws_config_image_viewer_get_current_row (JwsConfigImageViewer *viewer)
{
  JwsConfigImageViewerPrivate *priv;
  priv = jws_config_image_viewer_get_instance_private (viewer);
  
  return priv->current_row;
}

void
jws_config_image_viewer_set_current_row (JwsConfigImageViewer *viewer,
                                         GtkTreeRowReference *row_ref)
{
  JwsConfigImageViewerPrivate *priv;
  priv = jws_config_image_viewer_get_instance_private (viewer);

  gtk_tree_row_reference_free (priv->current_row);
  
  priv->current_row = row_ref;
}

void
jws_config_image_viewer_previous (JwsConfigImageViewer *viewer)
{
  JwsConfigImageViewerPrivate *priv;
  priv = jws_config_image_viewer_get_instance_private (viewer);
  
  GtkTreeRowReference *new_ref;
  new_ref = jws_config_window_get_previous_image_row (priv->win,
                                                      priv->current_row);

  if (new_ref)
    {
      gtk_tree_row_reference_free (priv->current_row);
      priv->current_row = new_ref;

      gchar *path;
      path = jws_config_image_viewer_get_current_path (viewer);
      jws_config_image_viewer_set_pixbufs_for_path (viewer, path);
      gchar *title = g_strdup (_("Image"));
      GFile *as_file;
      as_file = g_file_new_for_path (path);
      if (as_file)
        {
          gchar *basename;
          basename = g_file_get_basename (as_file);
          if (basename)
            {
              g_free (title);
              title = g_strdup (basename);
            }
          g_free (basename);
        }
      gtk_window_set_title (GTK_WINDOW (viewer), title);

      g_free (title);
      g_object_unref (as_file);

      g_free (path);

      jws_config_image_viewer_scaled_size (viewer);
    }
}

void
jws_config_image_viewer_next (JwsConfigImageViewer *viewer)
{
  JwsConfigImageViewerPrivate *priv;
  priv = jws_config_image_viewer_get_instance_private (viewer);
  
  GtkTreeRowReference *new_ref;
  new_ref = jws_config_window_get_next_image_row (priv->win,
                                                  priv->current_row);

  if (new_ref)
    {
      gtk_tree_row_reference_free (priv->current_row);
      priv->current_row = new_ref;

      gchar *path;
      path = jws_config_image_viewer_get_current_path (viewer);
      jws_config_image_viewer_set_pixbufs_for_path (viewer, path);
      gchar *title = g_strdup (_("Image"));
      GFile *as_file;
      as_file = g_file_new_for_path (path);
      if (as_file)
        {
          gchar *basename;
          basename = g_file_get_basename (as_file);
          if (basename)
            {
              g_free (title);
              title = g_strdup (basename);
            }
          g_free (basename);
        }
      gtk_window_set_title (GTK_WINDOW (viewer), title);

      g_free (title);
      g_object_unref (as_file);

      g_free (path);

      jws_config_image_viewer_scaled_size (viewer);
    }
}

void
jws_config_image_viewer_original_size (JwsConfigImageViewer *viewer)
{
  JwsConfigImageViewerPrivate *priv;
  priv = jws_config_image_viewer_get_instance_private (viewer);

  if (priv->original_pixbuf)
    {
      int width = gdk_pixbuf_get_width (priv->original_pixbuf);
      int height = gdk_pixbuf_get_height (priv->original_pixbuf);

      gtk_image_set_from_pixbuf (GTK_IMAGE (priv->image),
                                 priv->original_pixbuf);
      gtk_window_resize (GTK_WINDOW (viewer), width, height);
    }
  else
    {
      gtk_image_clear (GTK_IMAGE (priv->image));
    }
}

void
jws_config_image_viewer_scaled_size (JwsConfigImageViewer *viewer)
{
  JwsConfigImageViewerPrivate *priv;
  priv = jws_config_image_viewer_get_instance_private (viewer);

  if (priv->scaled_pixbuf)
    {
      int width = gdk_pixbuf_get_width (priv->scaled_pixbuf);
      int height = gdk_pixbuf_get_height (priv->scaled_pixbuf);

      gtk_image_set_from_pixbuf (GTK_IMAGE (priv->image),
                                 priv->scaled_pixbuf);
      gtk_window_resize (GTK_WINDOW (viewer), width, height);
    }
  else
    {
      gtk_image_clear (GTK_IMAGE (priv->image));
    }
}

gchar *
jws_config_image_viewer_get_current_path (JwsConfigImageViewer *viewer)
{
  JwsConfigImageViewerPrivate *priv;
  priv = jws_config_image_viewer_get_instance_private (viewer);
  
  gchar *path = NULL;
  path = jws_config_window_get_path_for_row (priv->win, priv->current_row);

  return path;
}

void
jws_config_image_viewer_set_pixbufs_for_path (JwsConfigImageViewer *viewer,
                                              const char *path)
{
  JwsConfigImageViewerPrivate *priv;
  priv = jws_config_image_viewer_get_instance_private (viewer);
  
  if (priv->original_pixbuf)
    g_object_unref (priv->original_pixbuf);

  if (priv->scaled_pixbuf)
    g_object_unref (priv->scaled_pixbuf);

  priv->original_pixbuf = gdk_pixbuf_new_from_file (path, NULL);
  if (priv->original_pixbuf)
    {
      int src_width = gdk_pixbuf_get_width (priv->original_pixbuf);
      int src_height = gdk_pixbuf_get_height (priv->original_pixbuf);

      int scaled_width_by_width = JWS_CONFIG_IMAGE_VIEWER_MAX_WIDTH;
      int scaled_width_by_height = (JWS_CONFIG_IMAGE_VIEWER_MAX_HEIGHT
                                    * src_width / src_height);

      int dest_width = MIN (scaled_width_by_width, scaled_width_by_height);

      priv->scaled_pixbuf = jws_create_scaled_pixbuf (priv->original_pixbuf,
                                                      dest_width,
                                                      -1);
    }
  else
    {
      /* The original pixbuf is already NULL.  */
      priv->scaled_pixbuf = NULL;
    }
}
