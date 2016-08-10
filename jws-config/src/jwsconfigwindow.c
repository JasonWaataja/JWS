/* jwsconfigwindow.c - window for jws-config

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

#include "jwsconfigwindow.h"

#include <glib/gi18n.h>

#include "jwsconfigimageviewer.h"

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
  GtkWidget *rotate_button;
  GtkWidget *apply_button;
  GtkWidget *add_button;
  GtkWidget *add_directory_button;
  GtkWidget *tree_view;

  GtkWidget *remove_button;
  GtkWidget *up_button;
  GtkWidget *down_button;
  GtkWidget *cancel_button;

  GtkWidget *rotate_items_box;

  GtkTreeStore *tree_store;
  GtkTreeSelection *tree_selection;

  /* Always use and accessors for this which use the mutex.  */
  gboolean should_exit_thread;
  GMutex should_exit_thread_mutex;

  GThread *preview_thread;
  GAsyncQueue *preview_queue;
};

G_DEFINE_TYPE_WITH_PRIVATE (JwsConfigWindow, jws_config_window,
                            GTK_TYPE_APPLICATION_WINDOW);

static void
jws_config_window_dispose (GObject *obj);

static void
jws_config_window_finalize (GObject *obj);

static void
on_rotate_button_toggled (JwsConfigWindow *win,
                          GtkToggleButton *rotate_button);

static void
jws_config_window_set_up_tree_view (JwsConfigWindow *win);

static void
type_column_data_func (GtkTreeViewColumn *tree_column,
                       GtkCellRenderer *cell,
                       GtkTreeModel *tree_model,
                       GtkTreeIter *iter,
                       gpointer data);

static void
jws_config_window_add_file_for_iter_recurse (JwsConfigWindow *win,
                                             const char *path,
                                             GtkTreeIter *parent_iter);

static void
destroy_row_reference (gpointer row_ref);

static void *
preview_thread_run (gpointer win);

/* Free with g_free when done.  */
static gchar *
get_home_directory ();

static void
on_row_activated (GtkTreeView *view,
                  GtkTreePath *path,
                  GtkTreeViewColumn *column,
                  gpointer user_data);

static void
on_selection_changed (GtkTreeSelection *selection,
                      JwsConfigWindow *win);

static GSList *
add_tree_path_to_slist (GSList *list, GtkTreeModel *model, GtkTreeIter *iter);

static void
on_remove_button_clicked (JwsConfigWindow *win);

static int
tree_path_sort_func (GtkTreePath *a, GtkTreePath *b);

static void
on_up_button_clicked (JwsConfigWindow *win);

static void
on_down_button_clicked (JwsConfigWindow *win);

enum tree_store_columns
{
  PATH_COLUMN = 0,
  NAME_COLUMN,
  IS_DIRECTORY_COLUMN,
  PREVIEW_COLUMN,
  N_COLUMNS
};

void
jws_config_window_init (JwsConfigWindow *self)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (self);

  gtk_widget_init_template (GTK_WIDGET (self));

  priv->tree_store = gtk_tree_store_new (N_COLUMNS,
                                         G_TYPE_STRING, /* 0, path */
                                         G_TYPE_STRING, /* 1, name */
                                         G_TYPE_BOOLEAN,/* 2, is directory */
                                         GDK_TYPE_PIXBUF);/* 3, preview */

  jws_config_window_set_up_tree_view (self);

  priv->preview_queue = g_async_queue_new_full (destroy_row_reference);

  g_mutex_init (&priv->should_exit_thread_mutex);
  jws_config_window_set_should_exit_thread (self, FALSE);

  priv->preview_thread = g_thread_new ("Preview Thread",
                                       preview_thread_run,
                                       self);


  g_signal_connect_swapped (priv->rotate_button, "toggled",
                            G_CALLBACK (on_rotate_button_toggled),
                            self);
  g_signal_connect_swapped (priv->add_button, "clicked",
                            G_CALLBACK (jws_config_window_add_file_selection),
                            self);
  g_signal_connect_swapped (priv->add_directory_button, "clicked",
                            G_CALLBACK
                            (jws_config_window_add_directory_selection),
                            self);
  g_signal_connect_swapped (priv->remove_button, "clicked",
                            G_CALLBACK (on_remove_button_clicked),
                            self);
  g_signal_connect_swapped (priv->up_button, "clicked",
                            G_CALLBACK (on_up_button_clicked),
                            self);
  g_signal_connect_swapped (priv->down_button, "clicked",
                            G_CALLBACK (on_down_button_clicked),
                            self);
  g_signal_connect_swapped (priv->cancel_button, "clicked",
                            G_CALLBACK (gtk_tree_selection_unselect_all),
                            priv->tree_selection);
  g_signal_connect (priv->tree_view, "row-activated",
                    G_CALLBACK (on_row_activated), self);
  g_signal_connect (priv->tree_selection, "changed",
                    G_CALLBACK (on_selection_changed), self);

  jws_config_window_show_optional_side_buttons (self, FALSE);
  jws_config_window_show_rotate_items (self, FALSE);
}

void
jws_config_window_class_init (JwsConfigWindowClass *kclass)
{
  gtk_widget_class_set_template_from_resource (GTK_WIDGET_CLASS (kclass),
                                               "/com/waataja/jwsconfig/"
                                               "ui/jwswindow.ui");
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                apply_button);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                remove_button);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                up_button);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                down_button);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                cancel_button);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                rotate_button);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                rotate_items_box);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                tree_view);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                add_button);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                add_directory_button);

  G_OBJECT_CLASS (kclass)->dispose = jws_config_window_dispose;
  G_OBJECT_CLASS (kclass)->finalize = jws_config_window_finalize;
}

static void
jws_config_window_dispose (GObject *obj)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (JWS_CONFIG_WINDOW (obj));

  /* Stopping the thread should happen first because operations inside inside
   * it might cause errors if objects don't exist.  */
  jws_config_window_set_should_exit_thread (JWS_CONFIG_WINDOW (obj), TRUE);
  g_thread_join (priv->preview_thread);

  g_mutex_clear (&priv->should_exit_thread_mutex);

  if (priv->preview_queue)
    g_async_queue_unref (priv->preview_queue);
  priv->preview_queue = NULL;

  g_clear_object (&priv->tree_store);



  G_OBJECT_CLASS (jws_config_window_parent_class)->dispose (obj);
}

static void
jws_config_window_finalize (GObject *obj)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (JWS_CONFIG_WINDOW (obj));
  
  G_OBJECT_CLASS (jws_config_window_parent_class)->finalize (obj);
}

JwsConfigWindow *
jws_config_window_new (JwsConfigApplication *app)
{
  return g_object_new (JWS_TYPE_CONFIG_WINDOW,
                       "application", app,
                       NULL);
}

void
jws_config_window_show_optional_side_buttons (JwsConfigWindow *win,
                                              gboolean visible)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);
  
  GtkWidget *side_buttons[] = 
    {
      priv->remove_button,
      priv->up_button,
      priv->down_button,
      priv->cancel_button
    };

  for (int i = 0; i < (sizeof (side_buttons) / sizeof (side_buttons[0])); i++)
    {
      gtk_widget_set_visible (side_buttons[i], visible);
    }
}

void
jws_config_window_show_rotate_items (JwsConfigWindow *win,
                                     gboolean visible)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);
  
  gtk_widget_set_visible (GTK_WIDGET (priv->rotate_items_box), visible);
}

static void
on_rotate_button_toggled (JwsConfigWindow *win,
                          GtkToggleButton *rotate_button)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);

  gboolean is_active;
  is_active = gtk_toggle_button_get_active (rotate_button);

  jws_config_window_show_rotate_items (win, is_active);
}

static void
type_column_data_func (GtkTreeViewColumn *tree_column,
                       GtkCellRenderer *cell,
                       GtkTreeModel *tree_model,
                       GtkTreeIter *iter,
                       gpointer data)
{
  gboolean is_directory;
  gtk_tree_model_get (tree_model, iter, IS_DIRECTORY_COLUMN, &is_directory,
                      -1);

  gchar *type_string;
  type_string = jws_get_type_string (is_directory);
  g_object_set (cell, "text", type_string, NULL);
  g_free (type_string);
}

static void
jws_config_window_set_up_tree_view (JwsConfigWindow *win)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);
  
  GtkTreeView *as_view = GTK_TREE_VIEW (priv->tree_view);
  gtk_tree_view_set_model (as_view, GTK_TREE_MODEL (priv->tree_store));

  GtkTreeViewColumn *name_column;
  GtkTreeViewColumn *type_column;
  GtkTreeViewColumn *preview_column;

  GtkCellRenderer *text_renderer;
  text_renderer = GTK_CELL_RENDERER (gtk_cell_renderer_text_new ());
  GtkCellRenderer *pixbuf_renderer;
  pixbuf_renderer = GTK_CELL_RENDERER (gtk_cell_renderer_pixbuf_new ());


  name_column = gtk_tree_view_column_new_with_attributes (_("Name"),
                                                          text_renderer,
                                                          "text", 1,
                                                          NULL);
  gtk_tree_view_insert_column (as_view, name_column, NAME_COLUMN);

  type_column = gtk_tree_view_column_new ();
  gtk_tree_view_column_set_title (type_column, _("Type"));
  gtk_tree_view_column_pack_start (type_column, text_renderer, TRUE);
  gtk_tree_view_column_set_cell_data_func (type_column, text_renderer,
                                           type_column_data_func, NULL, NULL);
  gtk_tree_view_append_column (as_view, type_column);

  preview_column = gtk_tree_view_column_new_with_attributes (_("Preview"),
                                                             pixbuf_renderer,
                                                             "pixbuf",
                                                             PREVIEW_COLUMN,
                                                             NULL);
  gtk_tree_view_insert_column (as_view, preview_column, PREVIEW_COLUMN);

  priv->tree_selection = gtk_tree_view_get_selection
    (GTK_TREE_VIEW (priv->tree_view));
  gtk_tree_selection_set_mode (priv->tree_selection, GTK_SELECTION_MULTIPLE);
}

static gchar *
get_home_directory ()
{
  return g_strdup (g_getenv ("HOME"));
}

void
jws_config_window_add_file_selection (JwsConfigWindow *win)
{
  GtkWidget *dialog;
  dialog = gtk_file_chooser_dialog_new (_("Choose File"),
                                        GTK_WINDOW (win),
                                        GTK_FILE_CHOOSER_ACTION_OPEN,
                                        _("Add"), GTK_RESPONSE_ACCEPT,
                                        _("Cancel"), GTK_RESPONSE_CANCEL,
                                        NULL);
  gchar *home_dir;
  home_dir = get_home_directory ();
  gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog), home_dir);
  g_free (home_dir);

  gtk_file_chooser_set_select_multiple (GTK_FILE_CHOOSER (dialog), TRUE);

  GtkFileFilter *filter;
  filter = gtk_file_filter_new ();

  gtk_file_filter_add_pixbuf_formats (filter);

  /* I don't think I have to free this because it's a floating reference.  */
  gtk_file_chooser_set_filter (GTK_FILE_CHOOSER (dialog), filter);

  int response;
  response = gtk_dialog_run (GTK_DIALOG (dialog));

  GSList *file_list = NULL;

  if (response == GTK_RESPONSE_ACCEPT)
    {
      file_list = gtk_file_chooser_get_filenames (GTK_FILE_CHOOSER (dialog));
    }

  gtk_widget_destroy (dialog);

  GSList *iter;
  for (iter = file_list; iter != NULL; iter = g_slist_next (iter))
    {
      jws_config_window_add_file (win, iter->data);
      g_free (iter->data);
    }
  g_slist_free (file_list);
}

void
jws_config_window_add_directory_selection (JwsConfigWindow *win)
{
  GtkWidget *dialog;
  dialog = gtk_file_chooser_dialog_new (_("Choose Directory"),
                                        GTK_WINDOW (win),
                                        GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER,
                                        _("Add"), GTK_RESPONSE_ACCEPT,
                                        _("Cancel"), GTK_RESPONSE_CANCEL,
                                        NULL);
  gchar *home_dir;
  home_dir = get_home_directory ();
  gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog), home_dir);
  g_free (home_dir);

  gtk_file_chooser_set_select_multiple (GTK_FILE_CHOOSER (dialog), TRUE);

  int response;
  response = gtk_dialog_run (GTK_DIALOG (dialog));

  GSList *file_list = NULL;

  if (response == GTK_RESPONSE_ACCEPT)
    {
      file_list = gtk_file_chooser_get_filenames (GTK_FILE_CHOOSER (dialog));
    }

  gtk_widget_destroy (dialog);

  GSList *iter;
  for (iter = file_list; iter != NULL; iter = g_slist_next (iter))
    {
      jws_config_window_add_file (win, iter->data);
      g_free (iter->data);
    }
  g_slist_free (file_list);
}

static void
jws_config_window_add_file_for_iter_recurse (JwsConfigWindow *win,
                                             const char *path,
                                             GtkTreeIter *parent_iter)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);

  GFile *file;
  file = g_file_new_for_path (path);

  GFileType file_type;
  file_type = g_file_query_file_type (file, G_FILE_QUERY_INFO_NONE, NULL);

  gchar *basename;
  basename = g_file_get_basename (file);

  /* This variable is redundant becuase I could just use path from the
   * arguments list but I felt like doing it this way just in case path is
   * formatted weirdly or something.  */
  gchar *file_path;
  file_path = g_file_get_path (file);

  gchar *type_string;

  GtkTreeIter iter;
  gtk_tree_store_append (priv->tree_store, &iter, parent_iter);

  if (file_type == G_FILE_TYPE_REGULAR)
    {
      type_string = jws_get_type_string (FALSE);

      gtk_tree_store_set (priv->tree_store, &iter,
                          PATH_COLUMN, file_path,
                          NAME_COLUMN, basename,
                          IS_DIRECTORY_COLUMN, FALSE,
                          PREVIEW_COLUMN, NULL,
                          -1);

      GtkTreePath *as_path;
      as_path = gtk_tree_model_get_path (GTK_TREE_MODEL (priv->tree_store),
                                         &iter);
      GtkTreeRowReference *row_ref;
      row_ref = gtk_tree_row_reference_new (GTK_TREE_MODEL (priv->tree_store),
                                            as_path);
      gtk_tree_path_free (as_path);

      g_async_queue_push (priv->preview_queue, row_ref);
    }
  else if (file_type == G_FILE_TYPE_DIRECTORY)
    {
      type_string = jws_get_type_string (TRUE);


      gtk_tree_store_set (priv->tree_store, &iter,
                          PATH_COLUMN, file_path,
                          NAME_COLUMN, basename,
                          IS_DIRECTORY_COLUMN, TRUE,
                          PREVIEW_COLUMN, NULL,
                          -1);

      GFileEnumerator *enumerator;
      enumerator = g_file_enumerate_children (file,
                                              "*",
                                              G_FILE_QUERY_INFO_NONE,
                                              NULL,
                                              NULL);

      if (enumerator)
        {
          GFile *child_file;

          gboolean iter_err = TRUE;
          iter_err = g_file_enumerator_iterate (enumerator,
                                                NULL,
                                                &child_file,
                                                NULL,
                                                NULL);

          while (iter_err && child_file != NULL)
            {
              gchar *child_path;
              child_path = g_file_get_path (child_file);
              jws_config_window_add_file_for_iter_recurse (win,
                                                           child_path,
                                                           &iter);
              g_free (child_path);
              iter_err = g_file_enumerator_iterate (enumerator,
                                                    NULL,
                                                    &child_file,
                                                    NULL,
                                                    NULL);
            }
        }
      g_object_unref (enumerator);
    }

  g_object_unref (file);
  g_free (type_string);
  g_free (basename);
  g_free (file_path);
}

void
jws_config_window_add_file (JwsConfigWindow *win, const char *path)
{
  jws_config_window_add_file_for_iter_recurse (win, path, NULL);
}

gchar *
jws_get_type_string (gboolean is_directory)
{
  gchar *type_string = NULL;
  if (is_directory)
    {
      type_string = g_strdup (_("Directory"));
    }
  else
    {
      type_string = g_strdup (_("File"));
    }
  return type_string;
}

static void
destroy_row_reference (gpointer row_ref)
{
  gtk_tree_row_reference_free (((GtkTreeRowReference *) row_ref));
}

static void *
preview_thread_run (gpointer win)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (JWS_CONFIG_WINDOW (win));

  GtkTreePath *tree_path;
  GtkTreeIter iter;

  gchar *path;

  while (!jws_config_window_get_should_exit_thread (JWS_CONFIG_WINDOW (win)))
    {
      GtkTreeRowReference *row_ref;
      row_ref = g_async_queue_try_pop (priv->preview_queue);

      if (row_ref && gtk_tree_row_reference_valid (row_ref))
        {
          tree_path = gtk_tree_row_reference_get_path (row_ref);
          if (tree_path)
            {
              gtk_tree_model_get_iter (GTK_TREE_MODEL (priv->tree_store),
                                       &iter,
                                       tree_path);
              gtk_tree_model_get (GTK_TREE_MODEL (priv->tree_store),
                                  &iter,
                                  PATH_COLUMN, &path,
                                  -1);
              GdkPixbuf *preview_src = gdk_pixbuf_new_from_file (path,
                                                                 NULL);
              GdkPixbuf *preview;
              preview =
                jws_create_scaled_pixbuf (preview_src,
                                          -1,
                                          JWS_CONFIG_WINDOW_PREVIEW_HEIGHT);
              g_object_unref (preview_src);
              gtk_tree_store_set (priv->tree_store, &iter,
                                  PREVIEW_COLUMN, preview,
                                  -1);
              g_object_unref (preview);
              gtk_tree_row_reference_free (row_ref);
            }
          gtk_tree_path_free (tree_path);
        }
    }
  return NULL;
}

GdkPixbuf *
jws_create_scaled_pixbuf (GdkPixbuf *src,
                          int width,
                          int height)
{
  int src_width;
  int src_height;

  GdkPixbuf *dest = NULL;;

  if (src)
    {
      src_width = gdk_pixbuf_get_width (src);
      src_height = gdk_pixbuf_get_height (src);

      int dest_width = src_width;
      int dest_height = src_height;

      if (width > 0)
        {
          dest_width = width;
          if (height <= 0)
            {
              dest_height = width * src_height / src_width;
            }
        }
      if (height > 0)
        {
          dest_height = height;
          if (width <= 0)
            {
              dest_width = height * src_width / src_height;
            }
        }

      dest = gdk_pixbuf_scale_simple (src, dest_width, dest_height,
                                      GDK_INTERP_HYPER);

    }

  return dest;
}

gboolean
jws_config_window_get_should_exit_thread (JwsConfigWindow *win)
{
  gboolean should_exit = FALSE;
  if (win)
    {
      JwsConfigWindowPrivate *priv;
      priv = jws_config_window_get_instance_private (win);

      g_mutex_lock (&priv->should_exit_thread_mutex);
      should_exit = priv->should_exit_thread;
      g_mutex_unlock (&priv->should_exit_thread_mutex);
    }

  return should_exit;
}

void
jws_config_window_set_should_exit_thread (JwsConfigWindow *win,
                                          gboolean should_exit_thread)
{
  if (win)
    {
      JwsConfigWindowPrivate *priv;
      priv = jws_config_window_get_instance_private (win);
      
      g_mutex_lock (&priv->should_exit_thread_mutex);
      priv->should_exit_thread = should_exit_thread;
      g_mutex_unlock (&priv->should_exit_thread_mutex);
    }
}

void
jws_config_window_show_image_for_row (JwsConfigWindow *win,
                                      GtkTreeRowReference *row_ref)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);

  JwsConfigImageViewer *viewer;
  viewer = jws_config_image_viewer_new (win, row_ref);
  gtk_widget_show (GTK_WIDGET (viewer));
}

gchar *
jws_config_window_get_path_for_row (JwsConfigWindow *win,
                                    GtkTreeRowReference *row_ref)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);
  
  gchar *path = NULL;

  GtkTreePath *tree_path;
  GtkTreeIter iter;

  tree_path = gtk_tree_row_reference_get_path (row_ref);
  gtk_tree_model_get_iter (GTK_TREE_MODEL (priv->tree_store), &iter,
                           tree_path);

  gtk_tree_model_get (GTK_TREE_MODEL (priv->tree_store), &iter,
                      PATH_COLUMN, &path,
                      -1);

  gtk_tree_path_free (tree_path);

  return g_strdup (path);
}

static void
on_row_activated (GtkTreeView *view,
                  GtkTreePath *path,
                  GtkTreeViewColumn *column,
                  gpointer win)
{
   GtkTreeModel *model;
   model = gtk_tree_view_get_model (view);

  GtkTreeRowReference *row_ref;
  row_ref = gtk_tree_row_reference_new (model,
                                        path);

  GtkTreeIter iter;
  gtk_tree_model_get_iter (model, &iter, path);

  gboolean is_directory;
  gtk_tree_model_get (model, &iter,
                      IS_DIRECTORY_COLUMN, &is_directory,
                      -1);

  if (!is_directory)
    jws_config_window_show_image_for_row (win, row_ref);
}

static void
on_selection_changed (GtkTreeSelection *selection,
                      JwsConfigWindow *win)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);

  GtkTreeModel *model;
  model = GTK_TREE_MODEL (priv->tree_store);
  
  GList *selected_paths;
  /* This isn't actually a list of row references.  This function returns a
   * list of paths.  */
  selected_paths = gtk_tree_selection_get_selected_rows (selection,
                                                         &model);

  gboolean show_side_buttons = FALSE;
  if (selected_paths)
    {
      show_side_buttons = TRUE;
    }
  else
    {
      show_side_buttons = FALSE;
    }
  jws_config_window_show_optional_side_buttons (win, show_side_buttons);

  g_list_free_full (selected_paths, (GDestroyNotify) gtk_tree_path_free);
}

static GSList *
add_tree_path_to_slist (GSList *list, GtkTreeModel *model, GtkTreeIter *iter)
{
  gboolean is_directory;

  gtk_tree_model_get (model, iter,
                      IS_DIRECTORY_COLUMN, &is_directory,
                      -1);

  GSList *new_list = list;

  if (!is_directory)
    {
      GtkTreePath *tree_path;
      tree_path = gtk_tree_model_get_path (model, iter);
      new_list = g_slist_append (new_list, tree_path);
      char *path;
      gtk_tree_model_get (model, iter,
                          PATH_COLUMN, &path,
                          -1);
    }
  else
    {
      if (gtk_tree_model_iter_has_child (model, iter))
        {
          GtkTreeIter child_iter;
          gboolean is_valid;
          is_valid = gtk_tree_model_iter_children (model,
                                                   &child_iter,
                                                   iter);
          for (; is_valid; is_valid = gtk_tree_model_iter_next (model,
                                                                &child_iter))
            {
              new_list = add_tree_path_to_slist (new_list,
                                                 model,
                                                 &child_iter);
            }
        }
    }
  return new_list;
}

GtkTreeRowReference *
jws_config_window_get_next_image_row (JwsConfigWindow *win,
                                      GtkTreeRowReference *row_ref)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);

  GtkTreeRowReference *new_ref = NULL;
  
  GtkTreeModel *tree_model;
  tree_model = GTK_TREE_MODEL (priv->tree_store);

  GSList *image_list = NULL;

  GtkTreeIter iter;
  gboolean is_empty;

  is_empty = !gtk_tree_model_get_iter_first(tree_model, &iter);

  if (!is_empty)
    {
      gboolean has_next;
      has_next = gtk_tree_model_get_iter_first (tree_model, &iter);
      for (; has_next;
           has_next = gtk_tree_model_iter_next (tree_model, &iter))
        {
          image_list = add_tree_path_to_slist (image_list, tree_model, &iter);
        }
      if (image_list)
        {
          if (gtk_tree_row_reference_valid (row_ref))
            {
              GtkTreePath *start_path;
              start_path = gtk_tree_row_reference_get_path (row_ref);
              GtkTreePath *current_path = NULL;
              GSList *start_node = NULL;

              GSList *list_iter = NULL;
              gboolean found_start = FALSE;
              for (list_iter = image_list; list_iter != NULL && !found_start;)
                {
                  current_path = list_iter->data;
                  if (gtk_tree_path_compare (start_path, current_path) == 0)
                    {
                      found_start = TRUE;
                      start_node = list_iter;
                    }
                  else
                    {
                      list_iter = g_slist_next (list_iter);
                    }
                }
              if (found_start)
                {
                  list_iter = start_node;
                  list_iter = g_slist_next (list_iter);

                  if (list_iter == NULL)
                    list_iter = image_list;

                  current_path = list_iter->data;

                  if (gtk_tree_path_compare (start_path, current_path) != 0)
                    {
                      new_ref = gtk_tree_row_reference_new (tree_model,
                                                            current_path);
                    }
                }
            }
          for (GSList *list_iter = image_list; list_iter;
               list_iter = g_slist_next (list_iter))
            {
              gtk_tree_path_free (list_iter->data);
            }
          g_slist_free (image_list);
        }
    }
  return new_ref;
}

GtkTreeRowReference *
jws_config_window_get_previous_image_row (JwsConfigWindow *win,
                                      GtkTreeRowReference *row_ref)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);

  GtkTreeRowReference *new_ref = NULL;
  
  GtkTreeModel *tree_model;
  tree_model = GTK_TREE_MODEL (priv->tree_store);

  GSList *image_list = NULL;

  GtkTreeIter iter;
  gboolean is_empty;

  is_empty = !gtk_tree_model_get_iter_first(tree_model, &iter);

  if (!is_empty)
    {
      gboolean has_next;
      has_next = gtk_tree_model_get_iter_first (tree_model, &iter);
      for (; has_next;
           has_next = gtk_tree_model_iter_next (tree_model, &iter))
        {
          image_list = add_tree_path_to_slist (image_list, tree_model, &iter);
        }
      if (image_list)
        {
          image_list = g_slist_reverse (image_list);
          if (gtk_tree_row_reference_valid (row_ref))
            {
              GtkTreePath *start_path;
              start_path = gtk_tree_row_reference_get_path (row_ref);
              GtkTreePath *current_path = NULL;
              GSList *start_node = NULL;

              GSList *list_iter = NULL;
              gboolean found_start = FALSE;
              for (list_iter = image_list; list_iter != NULL && !found_start;)
                {
                  current_path = list_iter->data;
                  if (gtk_tree_path_compare (start_path, current_path) == 0)
                    {
                      found_start = TRUE;
                      start_node = list_iter;
                    }
                  else
                    {
                      list_iter = g_slist_next (list_iter);
                    }
                }
              if (found_start)
                {
                  list_iter = start_node;
                  list_iter = g_slist_next (list_iter);

                  if (list_iter == NULL)
                    list_iter = image_list;

                  current_path = list_iter->data;

                  if (gtk_tree_path_compare (start_path, current_path) != 0)
                    {
                      new_ref = gtk_tree_row_reference_new (tree_model,
                                                            current_path);
                    }
                }
            }
          for (GSList *list_iter = image_list; list_iter;
               list_iter = g_slist_next (list_iter))
            {
              gtk_tree_path_free (list_iter->data);
            }
          g_slist_free (image_list);
        }
    }
  return new_ref;
}

static void
on_remove_button_clicked (JwsConfigWindow *win)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);

  GtkTreeModel *as_model = GTK_TREE_MODEL (priv->tree_store);
  
  GList *selected_list;
  selected_list = gtk_tree_selection_get_selected_rows (priv->tree_selection,
                                                        &as_model);
  GSList *row_list = NULL;

  gboolean all_valid = TRUE;
  
  GList *list_iter;
  for (list_iter = selected_list; list_iter && all_valid;
       list_iter = g_list_next (list_iter))
    {
      GtkTreePath *tree_path;
      tree_path = list_iter->data;
      
      int depth;
      depth = gtk_tree_path_get_depth (tree_path);

      if (depth > 1)
        {
          all_valid = FALSE;

          GtkWidget *dialog;
          dialog = gtk_message_dialog_new (GTK_WINDOW (win),
                                           GTK_DIALOG_MODAL,
                                           GTK_MESSAGE_WARNING,
                                           GTK_BUTTONS_OK,
                                           _("Can only remove items at "
                                             "the top level."));
          gtk_dialog_run (GTK_DIALOG (dialog));
          gtk_widget_destroy (dialog);
        }
      else
        {
          GtkTreeRowReference *row_ref;
          row_ref = gtk_tree_row_reference_new (as_model, list_iter->data);
          row_list = g_slist_append (row_list, row_ref);
        }
      /* I could free the path here but it's going to iterate again anyways
       * when it's free I think so I'll just do it the way it says online.  */
    }

  g_list_free_full (selected_list, (GDestroyNotify) gtk_tree_path_free);

  GtkTreeIter iter;

  GSList *slist_iter;
  for (slist_iter = row_list; slist_iter;
       slist_iter = g_slist_next (slist_iter))
    {
      if (all_valid)
        {
          GtkTreePath *tree_path;
          tree_path = gtk_tree_row_reference_get_path (slist_iter->data);

          gtk_tree_model_get_iter (as_model, &iter, tree_path);
          gtk_tree_store_remove (priv->tree_store, &iter);

          gtk_tree_path_free (tree_path);
        }

      gtk_tree_row_reference_free (slist_iter->data);
    }

  g_slist_free (row_list);
}

static int
tree_path_sort_func (GtkTreePath *a, GtkTreePath *b)
{
  int *a_indices;
  a_indices = gtk_tree_path_get_indices (a);

  int *b_indices;
  b_indices = gtk_tree_path_get_indices (b);

  /* Returns negative if the first value of a is less than the first value of b
   * and vica versa.  I also don't think I have to free these because the
   * documentation says so.  */
  return (*a_indices - *b_indices);
}

static void
on_up_button_clicked (JwsConfigWindow *win)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);
  
  GtkTreeModel *as_model;
  as_model = GTK_TREE_MODEL (priv->tree_store);

  GList *selected_list;
  selected_list = gtk_tree_selection_get_selected_rows (priv->tree_selection,
                                                        &as_model);

  gboolean all_valid = TRUE;
  GList *list_iter;
  for (list_iter = selected_list; list_iter && all_valid;
       list_iter = g_list_next (list_iter))
    {
      int depth;
      depth = gtk_tree_path_get_depth (list_iter->data);

      if (depth > 1)
        {
          all_valid = FALSE;
          GtkWidget *dialog;
          dialog = gtk_message_dialog_new (GTK_WINDOW (win),
                                           GTK_DIALOG_MODAL,
                                           GTK_MESSAGE_WARNING,
                                           GTK_BUTTONS_OK,
                                           _("Cannot move child item."));
          gtk_dialog_run (GTK_DIALOG (dialog));
          gtk_widget_destroy (dialog);
        }
      else
        {
          int *indices;
          indices = gtk_tree_path_get_indices (list_iter->data);

          if (*indices == 0)
            {
              all_valid = FALSE;
              GtkWidget *dialog;
              dialog = gtk_message_dialog_new (GTK_WINDOW (win),
                                               GTK_DIALOG_MODAL,
                                               GTK_MESSAGE_WARNING,
                                               GTK_BUTTONS_OK,
                                               _("Cannot move up item that's "
                                                 "first."));
              gtk_dialog_run (GTK_DIALOG (dialog));
              gtk_widget_destroy (dialog);
            }
        }
    }

  if (all_valid)
    {

      selected_list = g_list_sort (selected_list,
                                   (GCompareFunc) tree_path_sort_func);

      GtkTreeIter src;
      GtkTreeIter dest;

      for (list_iter = selected_list; list_iter;
           list_iter = g_list_next (list_iter))
        {
          GtkTreePath *tree_path;
          tree_path = list_iter->data;
          gtk_tree_model_get_iter (as_model, &src, tree_path);
          /* I'm not checking to see if it worked becuase that should've been
           * prevented from with the check from earlier.  */
          gtk_tree_path_prev (tree_path);
          gtk_tree_model_get_iter (as_model, &dest, tree_path);
          gtk_tree_store_move_before (priv->tree_store, &src, &dest);
        }
    }

  g_list_free_full (selected_list, (GDestroyNotify) gtk_tree_path_free);
}

static void
on_down_button_clicked (JwsConfigWindow *win)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);
  
  GtkTreeModel *as_model;
  as_model = GTK_TREE_MODEL (priv->tree_store);

  GList *selected_list;
  selected_list = gtk_tree_selection_get_selected_rows (priv->tree_selection,
                                                        &as_model);

  gboolean all_valid = TRUE;
  GList *list_iter;
  for (list_iter = selected_list; list_iter && all_valid;
       list_iter = g_list_next (list_iter))
    {
      int depth;
      depth = gtk_tree_path_get_depth (list_iter->data);

      if (depth > 1)
        {
          all_valid = FALSE;
          GtkWidget *dialog;
          dialog = gtk_message_dialog_new (GTK_WINDOW (win),
                                           GTK_DIALOG_MODAL,
                                           GTK_MESSAGE_WARNING,
                                           GTK_BUTTONS_OK,
                                           _("Cannot move child item."));
          gtk_dialog_run (GTK_DIALOG (dialog));
          gtk_widget_destroy (dialog);
        }
      else
        {
          int *indices;
          indices = gtk_tree_path_get_indices (list_iter->data);

          int length;
          length = gtk_tree_model_iter_n_children (as_model,
                                                   NULL);

          if (*indices >= length - 1)
            {
              all_valid = FALSE;
              GtkWidget *dialog;
              dialog = gtk_message_dialog_new (GTK_WINDOW (win),
                                               GTK_DIALOG_MODAL,
                                               GTK_MESSAGE_WARNING,
                                               GTK_BUTTONS_OK,
                                               _("Cannot move down item "
                                                 "that's last."));
              gtk_dialog_run (GTK_DIALOG (dialog));
              gtk_widget_destroy (dialog);
            }
        }
    }

  if (all_valid)
    {

      selected_list = g_list_sort (selected_list,
                                   (GCompareFunc) tree_path_sort_func);
      selected_list = g_list_reverse (selected_list);

      GtkTreeIter src;
      GtkTreeIter dest;

      for (list_iter = selected_list; list_iter;
           list_iter = g_list_next (list_iter))
        {
          GtkTreePath *tree_path;
          tree_path = list_iter->data;
          gtk_tree_model_get_iter (as_model, &src, tree_path);
          /* I'm not checking to see if it worked becuase that should've been
           * prevented from with the check from earlier.  */
          gtk_tree_path_next (tree_path);
          gtk_tree_model_get_iter (as_model, &dest, tree_path);
          gtk_tree_store_move_after (priv->tree_store, &src, &dest);
        }
    }

  g_list_free_full (selected_list, (GDestroyNotify) gtk_tree_path_free);
}
